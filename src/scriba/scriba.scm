;; scriba

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; scriba is free software: you can redistribute it and/or modify
;; it under the terms of the Lesser GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; scriba is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; Lesser GNU General Public License for more details.

;; You should have received a copy of the Lesser GNU General Public License
;; along with scriba.  If not, see <https://www.gnu.org/licenses/>.

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
                                 default-time-format
                                 default-log-level
                                 default-logger
                                 default-log-layout
                                 default-out-port
                                 default-flush-port?
                                 default-tag-filter
                                 with-log-context
                                 with-log-tags
                                 current-log-context
                                 current-log-tags
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

(define (tag-match? filter-tags current-tags)
  "Determine if the current execution context satisfies the logger's tag filter.

Returns #t if FILTER-TAGS is empty (acting as a wildcard that permits all logs),
or if there is an intersection of at least one element between FILTER-TAGS and
CURRENT-TAGS. Returns #f if a non-empty filter shares no tags with the current tags."
  (or (null? filter-tags)
      (let loop
        ((tags current-tags))
        (cond
          ((null? tags)
           #f)
          ((memq (car tags) filter-tags)
           #t)
          (else (loop (cdr tags)))))))

(define* (maybe-log-proc #:key level target-level tag-filter proc)
  "Conditionally evaluate and compile a logging procedure based on severity and tags.

Compares TARGET-LEVEL against the logger's configured LEVEL. If the level is
explicitly disabled or falls below the required threshold, this returns the
`log-unit` no-op procedure directly, avoiding downstream runtime overhead.

If the severity level is active, it returns a higher-order wrapper closure.
When invoked, this closure inspects the fluid context via `(current-log-tags)`.
If the tags match TAG-FILTER, it delegates formatting to PROC; otherwise, it
safely routes execution to `log-unit`."
  (let ((level-allowed? (cond
                          ((eq? target-level #t)
                           #t)
                          ((eq? target-level #f)
                           #f)
                          ((is-in-log-level? target-level level)
                           #t)
                          (else #f))))
    (if level-allowed?
        (if (null? tag-filter) proc
            (lambda (msg . args)
              (if (tag-match? tag-filter
                              (current-log-tags))
                  (apply proc msg args)
                  (apply log-unit msg args)))) log-unit)))

(define* (make-filtered-logger #:key name
                               level
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
  (mk-logger #:name-val name
             #:level-val level
             #:tag-filter-val tag-filter
             #:log-message-proc (maybe-log-proc #:level level
                                                #:target-level #t
                                                #:tag-filter tag-filter
                                                #:proc log-message-proc)
             #:log-trace-proc (maybe-log-proc #:level level
                                              #:target-level 'trace
                                              #:tag-filter tag-filter
                                              #:proc log-trace-proc)
             #:log-debug-proc (maybe-log-proc #:level level
                                              #:target-level 'debug
                                              #:tag-filter tag-filter
                                              #:proc log-debug-proc)
             #:log-info-proc (maybe-log-proc #:level level
                                             #:target-level 'info
                                             #:tag-filter tag-filter
                                             #:proc log-info-proc)
             #:log-success-proc (maybe-log-proc #:level level
                                                #:target-level 'success
                                                #:tag-filter tag-filter
                                                #:proc log-success-proc)
             #:log-warning-proc (maybe-log-proc #:level level
                                                #:target-level 'warning
                                                #:tag-filter tag-filter
                                                #:proc log-warning-proc)
             #:log-error-proc (maybe-log-proc #:level level
                                              #:target-level 'error
                                              #:tag-filter tag-filter
                                              #:proc log-error-proc)
             #:log-critical-proc (maybe-log-proc #:level level
                                                 #:target-level 'critical
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

(define-syntax-rule (memoize-logger factory-proc)
  "Wraps a logger FACTORY-PROC with a caching layer.

Returns a new variadic procedure that accepts arbitrary keyword arguments (KWARGS).
When called, it checks a private hash table to see if a logger has already been
instantiated with the exact same KWARGS. If so, it returns the cached instance.
If not, it calls FACTORY-PROC with the KWARGS, caches the resulting new logger,
and returns it.

This ensures that multiple requests for identically configured loggers (e.g.,
the same name, level, and format) resolve instantly to the exact same object
in memory, preventing redundant instantiations and reducing overhead."
  (let ((cache (make-hash-table)))
    (lambda kwargs
      (let ((cached-logger (hash-ref cache kwargs)))
        (if cached-logger cached-logger
            (let ((new-logger (apply factory-proc kwargs)))
              (hash-set! cache kwargs new-logger) new-logger))))))

(define current-log-context
  (make-parameter '()))

(define current-log-tags
  (make-parameter '()))

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

(define default-time-format
  (make-parameter human-time-format))

(define default-logger
  (make-parameter 'console))

(define default-out-port
  (make-parameter (current-output-port)))

(define default-flush-port?
  (make-parameter #t))

(define default-log-layout
  (make-parameter '(level tags time name message context)))
