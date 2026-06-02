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
;; the `with-log-context` macro. This wraps execution scopes and links key-value
;; pairs cleanly to downstream logging outputs without manual context drilling.

;;; Code:

(define-module (scriba scriba)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (make-filtered-logger log-trace
                                 log
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
                                 with-log-context
                                 with-log-tags
                                 current-log-context
                                 current-log-tags
                                 memoize-logger))

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
                       (log-proc logger-log)
                       (log-trace-proc logger-log-trace)
                       (log-debug-proc logger-log-debug)
                       (log-info-proc logger-log-info)
                       (log-success-proc logger-log-success)
                       (log-warning-proc logger-log-warning)
                       (log-error-proc logger-log-error)
                       (log-critical-proc logger-log-critical))

(define log-unit
  (lambda (msg . args)
    (values)))

(define* (make-filtered-logger #:key name
                               level
                               log-proc
                               log-trace-proc
                               log-debug-proc
                               log-info-proc
                               log-success-proc
                               log-warning-proc
                               log-error-proc
                               log-critical-proc)
  "Creates a logger, mapping excluded log levels to log-unit."
  (define (maybe-proc target-level proc)
    (cond
      ((eq? target-level #t)
       proc)
      ((eq? target-level #f)
       log-unit)
      ((is-in-log-level? target-level level)
       proc)
      (else log-unit)))

  (mk-logger #:name-val name
             #:level-val level
             #:log-proc (maybe-proc #t log-proc)
             #:log-trace-proc (maybe-proc 'trace log-trace-proc)
             #:log-debug-proc (maybe-proc 'debug log-debug-proc)
             #:log-info-proc (maybe-proc 'info log-info-proc)
             #:log-success-proc (maybe-proc 'success log-success-proc)
             #:log-warning-proc (maybe-proc 'warning log-warning-proc)
             #:log-error-proc (maybe-proc 'error log-error-proc)
             #:log-critical-proc (maybe-proc 'critical log-critical-proc)))

(define-syntax-rule (define-logger-method method-name accessor docstring)
  "A helper macro to generate boilerplate domain logging functions.

It defines a new procedure METHOD-NAME that accepts a logger, a format string (MESSAGE),
and optional formatting arguments (ARGS). When invoked, the generated procedure uses ACCESSOR
to retrieve the specific logging implementation from the logger instance
and applies it to the message and arguments.

The provided DOCSTRING is automatically injected into the generated procedure."
  (define (method-name logger message . args)
    docstring
    (apply (accessor logger)
           (cons message args))))

(define-logger-method log logger-log "Emits a log.")

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
  (parameterize ((current-log-context (append (current-log-context) ctx)))
    body ...))

(define-syntax-rule (with-log-tags tags body ...)
  "Dynamically append TAGS (a list) to the current log tags for the duration of BODY."
  (parameterize ((current-log-tags (append (current-log-tags) tags)))
    body ...))

(define human-time-format
  "%F %T %Z")

(define iso-8601-time-format
  "%Y-%m-%dT%H:%M:%S%z")

(define log-friendly-time-format
  "%F %T %z")

(define default-log-level
  (make-parameter 'info))

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
