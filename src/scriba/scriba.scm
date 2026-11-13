;; scriba

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; scriba is released under the MIT License.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.  See the
;; LICENSE file for the full text.

;;; Commentary:
;;
;; This is the fundamental engine core for the Scriba logging framework. It
;; manages internal logger records, logging level severities, dynamic execution
;; context (MDC), optimization shortcuts, and global configuration parameters.
;;
;; To minimize execution overhead from muted logs in high-throughput loops,
;; Scriba uses a level filtering system at instantiation time (`make-filtered-logger`).
;; If a severity level falls below the configured threshold, its logging procedure
;; is bound directly to a zero-overhead no-op function (`log-unit`). This avoids
;; unnecessary runtime level checks during iterative logging calls.

;; Thread/fiber-safe contextual tracing data can be appended dynamically using
;; the `with-log-context` and `with-log-tags` macros.

;;; Code:

(define-module (scriba scriba)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (make-filtered-logger log-trace
                                 log-message
                                 log-debug
                                 log-info
                                 log-success
                                 log-warning
                                 log-error
                                 log-critical
                                 log-levels
                                 iso-8601-time-format
                                 human-time-format
                                 log-friendly-time-format
                                 is-in-log-level?
                                 valid-log-level?
                                 valid-log-field?
                                 validate-log-layout
                                 text-layout-fields
                                 json-layout-fields
                                 syslog-layout-fields
                                 default-time-format
                                 default-log-level
                                 default-logger
                                 default-log-layout
                                 default-out-port
                                 default-flush-port?
                                 default-tag-filter
                                 default-context-filter
                                 default-line-ending
                                 write-log-line
                                 format-log-message
                                 with-log-context
                                 with-log-tags
                                 current-log-context
                                 current-log-tags
                                 current-log-exception
                                 memoize-logger
                                 maybe-log-proc
                                 tag-match?))

(define-syntax define-record-with-kw
  (syntax-rules ()
                ((_ (type-name constructor-name pred) kw-constructor-name
                    (field-name accessor-name) ...)
                 (begin
                   (define-record-type type-name
                     (constructor-name field-name ...) pred
                     (field-name accessor-name) ...)

                   (define* (kw-constructor-name #:key field-name ...)
                     (constructor-name field-name ...))))))

(define-record-with-kw (<logger> %make-logger logger?)
                       mk-logger
                       (name-val logger-name)
                       (level-val logger-level)
                       (context-filter-val logger-context-filter)
                       (tag-filter-val logger-tag-filter)
                       (log-message-proc logger-log-message)
                       (log-trace-proc logger-log-trace)
                       (log-debug-proc logger-log-debug)
                       (log-info-proc logger-log-info)
                       (log-success-proc logger-log-success)
                       (log-warning-proc logger-log-warning)
                       (log-error-proc logger-log-error)
                       (log-critical-proc logger-log-critical))

(define (log-unit msg . args)
  "A zero-overhead, no-op logging procedure.

Accepts a format string MSG and optional ARGS to satisfy standard logging
arity requirements, then immediately discards them and returns no values.
Used across the framework as a terminal sink for disabled log statements."
  (values))

(define (write-log-line port line line-ending)
  "Frames LINE with a newline and writes it to PORT.

LINE-ENDING decides where that newline goes: 'trailing (the default) appends
it, so log lines behave like ordinary text lines and the stream never starts
with a blank line. 'leading prepends it, which is handy when the log stream is
interleaved with prompts or other output that lacks a final newline.

`display' and `newline' are used instead of `format' because a format call
costs an order of magnitude more than the rest of a short log line."
  (case line-ending
    ((trailing)
     (display line port)
     (newline port))
    ((leading)
     (newline port)
     (display line port))
    (else (error "Scriba: invalid line-ending, expected 'trailing or 'leading"
           line-ending))))

(define (format-log-message msg args)
  "Applies MSG as a format string to ARGS, skipping the formatter for no ARGS.

Most log statements carry a literal message and no arguments, and Guile's
formatter is comparatively expensive, so a string is passed through untouched
when there is nothing to substitute. Messages with arguments are formatted as
usual. A side effect is that a lone message containing `~' is now literal
instead of being interpreted as a (likely invalid) format directive."
  (cond
    ((not (null? args))
     (apply format #f msg args))
    ((string? msg)
     msg)
    (else (format #f "~a" msg))))

(define (tag-match? filter-tags current-tags)
  "Determine if the current execution context satisfies the logger's tag filter.
Returns #t if FILTER-TAGS is empty (acting as a wildcard), or if there is a
lenient intersection (cross-comparing strings, symbols, and numbers) of at
least one element between FILTER-TAGS and CURRENT-TAGS."
  (or (null? filter-tags)
      (any (lambda (curr-tag)
             (any (lambda (filt-tag)
                    (loose-equal? curr-tag filt-tag)) filter-tags))
           current-tags)))

(define (loose-equal? a b)
  "Compare two values leniently. If they are structurally identical, returns #t.
If they are cross-boundary types (strings, symbols, or numbers), they are coerced
to strings for a loose value comparison."
  (cond
    ((equal? a b)
     #t)
    ;; Symbol vs String
    ((and (symbol? a)
          (string? b))
     (string=? (symbol->string a) b))
    ((and (string? a)
          (symbol? b))
     (string=? a
               (symbol->string b)))
    ;; Number vs String
    ((and (number? a)
          (string? b))
     (string=? (number->string a) b))
    ((and (string? a)
          (number? b))
     (string=? a
               (number->string b)))
    ;; Number vs Symbol
    ((and (number? a)
          (symbol? b))
     (string=? (number->string a)
               (symbol->string b)))
    ((and (symbol? a)
          (number? b))
     (string=? (symbol->string a)
               (number->string b)))
    (else #f)))

(define (context-match? filter-context current-context)
  "Determine if the current execution context satisfies the logger's context filter alist.

Returns #t if FILTER-CONTEXT is empty (acting as a wildcard that permits all logs),
or if at least one key-value pair specified in FILTER-CONTEXT matches loosely within
CURRENT-CONTEXT."
  (or (null? filter-context)
      (any (lambda (filter-pair)
             (let ((f-key (car filter-pair))
                   (f-val (cdr filter-pair)))
               ;; Check if this filter pair matches any pair in the current context
               (any (lambda (curr-pair)
                      (and (loose-equal? f-key
                                         (car curr-pair))
                           (loose-equal? f-val
                                         (cdr curr-pair)))) current-context)))
           filter-context)))

(define* (maybe-log-proc #:key level
                         target-level
                         context-filter
                         tag-filter
                         proc)
  "Conditionally evaluate and compile a logging procedure based on severity, tags, and context.

Compares TARGET-LEVEL against the logger's configured LEVEL. If the level is
explicitly disabled or falls below the required threshold, this returns the
`log-unit` no-op procedure directly, avoiding downstream runtime overhead.

If the severity level is active, it returns a higher-order wrapper closure.
When invoked, this closure inspects fluid parameters via `(current-log-tags)` and
`(current-log-context)`. If both constraints match their filters, it delegates formatting
to PROC; otherwise, it safely routes execution to the no-op `log-unit`."
  (let ((level-allowed? (cond
                          ;; #f is the explicit "never log" setting, so it
                          ;; wins even over level-less log-message calls.
                          ((eq? level #f)
                           #f)
                          ;; #t is the explicit "always log" setting, so every
                          ;; severity passes, not just log-message.
                          ((eq? level #t)
                           #t)
                          ;; log-message carries no severity and is otherwise
                          ;; only subject to the tag/context filters.
                          ((eq? target-level #t)
                           #t)
                          ((is-in-log-level? target-level level)
                           #t)
                          (else #f)))
        (no-filters? (and (null? tag-filter)
                          (null? context-filter))))
    (if level-allowed?
        (if no-filters? proc
            (lambda (msg . args)
              (if (and (tag-match? tag-filter
                                   (current-log-tags))
                       (context-match? context-filter
                                       (current-log-context)))
                  (apply proc msg args)
                  (apply log-unit msg args)))) log-unit)))

(define* (make-filtered-logger #:key name
                               level
                               context-filter
                               tag-filter
                               log-message-proc
                               log-trace-proc
                               log-debug-proc
                               log-info-proc
                               log-success-proc
                               log-warning-proc
                               log-error-proc
                               log-critical-proc)
  "Creates a logger, mapping excluded log levels and non-matching tags to log-unit."
  (validate-log-level level)
  (mk-logger #:name-val name
             #:level-val level
             #:context-filter-val context-filter
             #:tag-filter-val tag-filter
             #:log-message-proc (maybe-log-proc #:level level
                                                #:target-level #t
                                                #:context-filter
                                                context-filter
                                                #:tag-filter tag-filter
                                                #:proc log-message-proc)
             #:log-trace-proc (maybe-log-proc #:level level
                                              #:target-level 'trace
                                              #:context-filter context-filter
                                              #:tag-filter tag-filter
                                              #:proc log-trace-proc)
             #:log-debug-proc (maybe-log-proc #:level level
                                              #:target-level 'debug
                                              #:context-filter context-filter
                                              #:tag-filter tag-filter
                                              #:proc log-debug-proc)
             #:log-info-proc (maybe-log-proc #:level level
                                             #:target-level 'info
                                             #:context-filter context-filter
                                             #:tag-filter tag-filter
                                             #:proc log-info-proc)
             #:log-success-proc (maybe-log-proc #:level level
                                                #:target-level 'success
                                                #:context-filter
                                                context-filter
                                                #:tag-filter tag-filter
                                                #:proc log-success-proc)
             #:log-warning-proc (maybe-log-proc #:level level
                                                #:target-level 'warning
                                                #:context-filter
                                                context-filter
                                                #:tag-filter tag-filter
                                                #:proc log-warning-proc)
             #:log-error-proc (maybe-log-proc #:level level
                                              #:target-level 'error
                                              #:context-filter context-filter
                                              #:tag-filter tag-filter
                                              #:proc log-error-proc)
             #:log-critical-proc (maybe-log-proc #:level level
                                                 #:target-level 'critical
                                                 #:context-filter
                                                 context-filter
                                                 #:tag-filter tag-filter
                                                 #:proc log-critical-proc)))

(define-syntax-rule (define-logger-method method-name accessor docstring)
  "A helper macro to generate boilerplate domain logging functions.

It defines a new procedure METHOD-NAME that accepts a logger, a format string (MESSAGE),
and optional formatting arguments (ARGS). When invoked, the generated procedure uses ACCESSOR
to retrieve the specific logging implementation from the logger instance
and applies it to the message and arguments.

The provided DOCSTRING is automatically injected into the generated procedure."
  (define (method-name logger message . args)
    docstring
    (apply (accessor logger) message args)))

(define-logger-method log-message logger-log-message "Emits a log.")

(define-logger-method log-trace logger-log-trace
 "Emits a TRACE level log.

Use this for the most fine-grained, verbose informational events,
such as step-by-step execution traces, loops, or variable dumps.
MESSAGE is a format string applied to the optional ARGS.")

(define-logger-method log-debug logger-log-debug
 "Emits a DEBUG level log.

Use this for diagnostic information helpful to developers when troubleshooting,
but generally too noisy to be left enabled in production environments.
MESSAGE is a format string applied to the optional ARGS.")

(define-logger-method log-info logger-log-info
 "Emits an INFO level log.

Use this to highlight the normal, high-level progression of the application,
such as service startups, configuration loads, or periodic heartbeat events.
MESSAGE is a format string applied to the optional ARGS.")

(define-logger-method log-success logger-log-success
 "Emits a SUCCESS level log.

Use this to explicitly indicate the successful completion of a significant
operation, task, or transaction.
MESSAGE is a format string applied to the optional ARGS.")

(define-logger-method log-warning logger-log-warning
 "Emits a WARNING level log.

Use this to flag unexpected or potentially harmful situations that the
application can gracefully recover from, such as deprecated API usage or
temporary network retries.
MESSAGE is a format string applied to the optional ARGS.")

(define-logger-method log-error logger-log-error
 "Emits an ERROR level log.

Use this to record failed operations or exceptions that prevent a specific task
from completing, even if the application as a whole can continue running.
MESSAGE is a format string applied to the optional ARGS.")

(define-logger-method log-critical logger-log-critical
 "Emits a CRITICAL level log.

Use this for severe, unrecoverable failures that usually require immediate
attention, cause data loss, or force the application to abort.
MESSAGE is a format string applied to the optional ARGS.")

(define log-levels
  '(trace debug
          info
          success
          warning
          error
          critical))

(define (is-in-log-level? a b)
  "Is log level A allowed in a B context?"
  (and=> (memq b log-levels)
         (lambda (tail)
           (if (memq a tail) #t #f))))

(define (valid-log-level? level)
  "Is LEVEL a log level that Scriba understands?

#t means \"always log\", #f means \"never log\", and every symbol in
`log-levels` is a real severity."
  (or (boolean? level)
      (and (memq level log-levels) #t)))

(define (validate-log-level level)
  "Raise a descriptive error when LEVEL is not a usable log level.

Silently muting every log statement because of a typo (e.g. 'verbose) is a
great way to lose observability, so logger construction fails loudly instead."
  (unless (valid-log-level? level)
    (scm-error 'scriba-invalid-log-level
               'make-filtered-logger
               (format #f "Invalid log level ~a; expected #t, #f or one of ~a"
                       level log-levels)
               '() #f)) level)

(define text-layout-fields
  '(level tags
          time
          host
          name
          context
          message))

(define json-layout-fields
  '(level tags
          time
          host
          name
          message
          context))

(define syslog-layout-fields
  '(level tags
          time
          host
          name
          process-id
          message-id
          context
          message))

(define log-field-names
  (delete-duplicates (append text-layout-fields json-layout-fields
                             syslog-layout-fields)))

(define (valid-log-field? field)
  "Is FIELD something a logger knows how to render?"
  (and (memq field log-field-names) #t))

(define* (validate-log-layout layout
                              #:key (fields text-layout-fields))
  "Raise a descriptive error when LAYOUT is not a usable layout for a backend.

A mistyped field name used to be dropped silently, which turned a
misconfigured logger into one that writes blank lines. Failing at
construction time instead makes the typo impossible to miss."
  (unless (and (list? layout)
               (not (null? layout)))
    (scm-error 'scriba-invalid-layout
               'make-filtered-logger
               (format #f
                "Invalid log layout ~a; expected a non-empty list of ~a"
                layout fields)
               '() #f))
  (for-each (lambda (field)
              (unless (memq field fields)
                (scm-error 'scriba-invalid-layout
                           'make-filtered-logger
                           (format #f
                            "Unknown log layout field ~a; expected one of ~a"
                            field fields)
                           '() #f))) layout) layout)

(define (resolve-cache-defaults kwargs defaults)
  "Returns KWARGS extended with the current value of every entry in DEFAULTS.

DEFAULTS is an alist of `(keyword . thunk)` pairs. A default is only added when
its KEYWORD is missing from KWARGS, so explicit call-site arguments always win.
Thunks are called on every lookup, which is what keeps memoized loggers honest
about dynamically scoped settings: a `(parameterize ((default-out-port port))
(scriba-console-logger))` must not reuse the cached logger of another port."
  (fold (lambda (entry acc)
          (if (memq (car entry) acc) acc
              (append acc
                      (list (car entry)
                            ((cdr entry)))))) kwargs defaults))

(define-syntax-rule (memoize-logger factory-proc
                                    (kw thunk) ...)
  "Wraps a logger FACTORY-PROC with a caching layer.

Returns a new variadic procedure that accepts arbitrary keyword arguments (KWARGS).
When called, it checks a private hash table to see if a logger has already been
instantiated with the exact same configuration. If so, it returns the cached
instance. If not, it calls FACTORY-PROC, caches the resulting logger, and
returns it.

The optional (KW THUNK) pairs declare the defaults that FACTORY-PROC derives
from dynamically scoped parameters. Their current values are evaluated on every
call and merged into the cache key (unless the caller passed KW explicitly), so
parameterized defaults such as `default-out-port` correctly produce distinct
loggers."
  (let ((cache (make-hash-table))
        (defaults (list (cons kw thunk) ...)))
    (lambda kwargs
      (let* ((effective-kwargs (resolve-cache-defaults kwargs defaults))
             (cached-logger (hash-ref cache effective-kwargs)))
        (if cached-logger cached-logger
            (let ((new-logger (apply factory-proc effective-kwargs)))
              (hash-set! cache effective-kwargs new-logger) new-logger))))))

(define current-log-context
  (make-parameter '()))

(define current-log-tags
  (make-parameter '()))

;; The exception report of the line being logged, bound by
;; `(scriba exception)' around the call to a backend so each one can render it
;; in its own idiom.
(define current-log-exception
  (make-parameter #f))

(define-syntax-rule (with-log-context ctx body ...)
  "Dynamically append CTX (an alist) to the current log context for the duration of BODY."
  (parameterize ((current-log-context (append ctx
                                              (current-log-context))))
    body ...))

(define-syntax-rule (with-log-tags tags body ...)
  "Dynamically append TAGS (a list) to the current log tags for the duration of BODY."
  (parameterize ((current-log-tags (append tags
                                           (current-log-tags))))
    body ...))

(define human-time-format
  "%F %T %Z")

(define iso-8601-time-format
  "%Y-%m-%dT%H:%M:%S%z")

(define log-friendly-time-format
  "%F %T %z")

(define default-log-level
  (make-parameter 'info))

(define default-tag-filter
  (make-parameter '()))

(define default-context-filter
  (make-parameter '()))

(define default-time-format
  (make-parameter human-time-format))

(define default-logger
  (make-parameter 'console))

(define default-out-port
  (make-parameter (current-output-port)))

(define default-flush-port?
  (make-parameter #t))

(define default-line-ending
  (make-parameter 'trailing))

(define default-log-layout
  (make-parameter '(level tags time name message context)))
