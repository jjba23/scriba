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
  #:export (get-logger scriba-console-logger
                       log-trace
                       log-debug
                       log-info
                       log-warn
                       log-error))

(define-syntax define-record-with-kw
  (syntax-rules ()
                ((_ (type-name constructor-name pred) kw-constructor-name
                    (field-name accessor-name) ...)
                 (begin
                   ;; Define the standard SRFI-9 record
                   (define-record-type type-name
                     (constructor-name field-name ...) pred
                     (field-name accessor-name) ...)

                   ;; Define the keyword-argument constructor
                   (define* (kw-constructor-name #:key field-name ...)
                     (constructor-name field-name ...))

                   ;; Auto-export members
                   ;; (export type-name pred kw-constructor-name accessor-name
                   ;; ...)
                   ))))

(define-record-with-kw (<logger> %make-logger logger?)
                       mk-logger
                       (name-val logger-name)
                       (log-trace-proc logger-log-trace)
                       (log-debug-proc logger-log-debug)
                       (log-info-proc logger-log-info)
                       (log-warn-proc logger-log-warn)
                       (log-error-proc logger-log-error))

(define-syntax define-logger-method
  (syntax-rules ()
                ((_ method-name accessor docstring)
                 ;; standard 'define' supports dot notation for rest args just fine
                 (define (method-name logger message . args)
                   docstring
                   (apply (accessor logger)
                          (cons message args))))))

(define-logger-method log-trace logger-log-trace "Log TRACE")
(define-logger-method log-debug logger-log-debug "Log DEBUG")
(define-logger-method log-info logger-log-info "Log INFO")
(define-logger-method log-warn logger-log-warn "Log WARN")
(define-logger-method log-error logger-log-error "Log ERROR")

(define log-unit
  (lambda (msg . args)
    (values)))

(define supported-log-levels
  '(trace debug info warn error))

(define (is-in-log-level? a b)
  "Is log level A allowed in a B context?"
  (and=> (memq b supported-log-levels)
         (lambda (tail)
           (if (memq a tail) #t #f))))

(define* (scriba-console-logger #:key (name #f)
                                (level 'info))
  (define logger-name-fragment
    (if name
        (format #f "[~a] " name) ""))

  (define log-trace
    (if (is-in-log-level? 'trace level)
        (lambda (msg . args)
          (apply format #t
                 (string-append "\n[trace] " logger-name-fragment msg) args))
        log-unit))

  (define log-debug
    (if (is-in-log-level? 'debug level)
        (lambda (msg . args)
          (apply format #t
                 (string-append "\n[debug] " logger-name-fragment msg) args))
        log-unit))

  (define log-info
    (if (is-in-log-level? 'info level)
        (lambda (msg . args)
          (apply format #t
                 (string-append "\n[info] " logger-name-fragment msg) args))
        log-unit))

  (define log-warn
    (if (is-in-log-level? 'warn level)
        (lambda (msg . args)
          (apply format #t
                 (string-append "\n[warn] " logger-name-fragment msg) args))
        log-unit))

  (define log-error
    (if (is-in-log-level? 'error level)
        (lambda (msg . args)
          (apply format #t
                 (string-append "\n[error] " logger-name-fragment msg) args))
        log-unit))

  (mk-logger #:log-trace-proc log-trace
             #:log-debug-proc log-debug
             #:log-info-proc log-info
             #:log-warn-proc log-warn
             #:log-error-proc log-error))
