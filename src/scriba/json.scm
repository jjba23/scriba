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

(define-module (scriba json)
  #:use-module (json)
  #:use-module (scriba scriba)
  #:export (scriba-json-logger))

(define (get-time-field with-timestamp? format-string)
  "Returns a timestamp alist entry, or an empty list if disabled."
  (if with-timestamp?
      `((time unquote
              (strftime format-string
                        (localtime (current-time)))))
      '()))

(define (get-name-field name)
  "Returns a name alist entry, or an empty list if unnamed."
  (if name
      `((name unquote name))
      '()))

(define* (%make-scriba-json-logger #:key name
                                   (level (default-log-level))
                                   pretty?
                                   (with-timestamp? #t)
                                   (timestamp-format (default-time-format)))
  
  (define name-field
    (get-name-field name))

  (define (make-json-printer lvl)
    (let ((level-field `((level unquote
                                (string-upcase (symbol->string lvl))))))
      (lambda (msg . args)
        (let* ((user-msg (apply format #f msg args))
               (base-field `((message unquote user-msg)))
               (time-field (get-time-field with-timestamp? timestamp-format))
               (dynamic-ctx (current-log-context))

               (fields (append level-field name-field time-field base-field
                               dynamic-ctx))

               (serialized-log (scm->json-string fields
                                                 #:pretty pretty?)))
          
          (format #t "\n~a" serialized-log)))))

  (make-filtered-logger #:name name
                        #:level level
                        #:log-trace-proc (make-json-printer 'trace)
                        #:log-debug-proc (make-json-printer 'debug)
                        #:log-info-proc (make-json-printer 'info)
                        #:log-success-proc (make-json-printer 'success)
                        #:log-warning-proc (make-json-printer 'warning)
                        #:log-error-proc (make-json-printer 'error)
                        #:log-critical-proc (make-json-printer 'critical)))

(define scriba-json-logger
  (memoize-logger %make-scriba-json-logger))
