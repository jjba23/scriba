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

(define* (%make-scriba-json-logger #:key name
                                   (level (default-log-level))
                                   pretty?
                                   (with-timestamp? #t)
                                   (timestamp-format (default-time-format)))
  (define (make-json-printer lvl)
    (lambda (msg . args)
      (let* ((m (apply format #f msg args))
             (level-fields `((level unquote lvl)))
             (base-fields `((message unquote m)))
             (name-fields (if name
                              `((name unquote name))
                              '()))
             (time-fields (if with-timestamp?
                              `((time unquote
                                      (strftime timestamp-format
                                                (localtime (current-time)))))
                              '()))
             (dynamic-ctx (current-log-context))
             (fields (append level-fields name-fields base-fields time-fields dynamic-ctx))
             (j (scm->json-string fields
                                  #:pretty pretty?)))
        (format #t "\n~a" j))))

  (make-filtered-logger #:name name
                        #:level level
                        #:log-trace-proc (make-json-printer 'trace)
                        #:log-debug-proc (make-json-printer 'debug)
                        #:log-info-proc (make-json-printer 'info)
                        #:log-warn-proc (make-json-printer 'warn)
                        #:log-error-proc (make-json-printer 'error)))

(define scriba-json-logger
  (memoize-logger %make-scriba-json-logger))
