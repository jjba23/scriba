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

(define-module (scriba scriba)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (make-filtered-logger log-trace
                                 log-debug
                                 log-info
                                 log-warn
                                 log-error
                                 log-levels
                                 iso-8601-time-format
                                 human-time-format
                                 log-friendly-time-format
                                 is-in-log-level?
                                 default-time-format
                                 default-log-level
                                 with-log-context
                                 current-log-context
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
                       (log-trace-proc logger-log-trace)
                       (log-debug-proc logger-log-debug)
                       (log-info-proc logger-log-info)
                       (log-warn-proc logger-log-warn)
                       (log-error-proc logger-log-error))

(define log-unit
  (lambda (msg . args)
    (values)))

(define* (make-filtered-logger #:key name
                               level
                               log-trace-proc
                               log-debug-proc
                               log-info-proc
                               log-warn-proc
                               log-error-proc)
  "Creates a logger, mapping excluded log levels to log-unit."
  (define (maybe-proc target-level proc)
    (if (is-in-log-level? target-level level) proc log-unit))

  (mk-logger #:name-val name
             #:level-val level
             #:log-trace-proc (maybe-proc 'trace log-trace-proc)
             #:log-debug-proc (maybe-proc 'debug log-debug-proc)
             #:log-info-proc (maybe-proc 'info log-info-proc)
             #:log-warn-proc (maybe-proc 'warn log-warn-proc)
             #:log-error-proc (maybe-proc 'error log-error-proc)))

(define-syntax define-logger-method
  (syntax-rules ()
                ((_ method-name accessor docstring)
                 (define (method-name logger message . args)
                   docstring
                   (apply (accessor logger)
                          (cons message args))))))

(define-logger-method log-trace logger-log-trace "Log TRACE")
(define-logger-method log-debug logger-log-debug "Log DEBUG")
(define-logger-method log-info logger-log-info "Log INFO")
(define-logger-method log-warn logger-log-warn "Log WARN")
(define-logger-method log-error logger-log-error "Log ERROR")

(define log-levels
  '(trace debug info warn error))

(define default-log-level
  (make-parameter 'info))

(define (is-in-log-level? a b)
  "Is log level A allowed in a B context?"
  (and=> (memq b log-levels)
         (lambda (tail)
           (if (memq a tail) #t #f))))

;; Wraps a logger FACTORY-PROC with a caching layer.
(define-syntax memoize-logger
  (syntax-rules ()
                ((_ factory-proc)
                 (let ((cache (make-hash-table)))
                   (lambda kwargs
                     (let ((cached-logger (hash-ref cache kwargs)))
                       (if cached-logger cached-logger
                           (let ((new-logger (apply factory-proc kwargs)))
                             (hash-set! cache kwargs new-logger) new-logger))))))))

(define human-time-format
  "%F %T %Z")

(define iso-8601-time-format
  "%Y-%m-%dT%H:%M:%S%z")

(define log-friendly-time-format
  "%F %T %z")

(define default-time-format
  (make-parameter human-time-format))

(define current-log-context
  (make-parameter '()))

(define-syntax-rule (with-log-context ctx body ...)
  "Dynamically append CTX (an alist) to the current log context for the duration of BODY."
  (parameterize ((current-log-context (append (current-log-context) ctx)))
    body ...))
