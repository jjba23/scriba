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

(define-module (scriba console)
  #:use-module (scriba scriba)
  #:export (scriba-console-logger))

(define* (%make-scriba-console-logger #:key name level)
  (define logger-name-fragment
    (if name
        (format #f "[~a] " name) ""))

  (define (make-console-printer lvl)
    (let ((pref (string-append "\n["
                               (symbol->string lvl) "] " logger-name-fragment)))
      (lambda (msg . args)
        (apply format #t
               (string-append pref msg) args))))

  (make-filtered-logger #:name name
                        #:level level
                        #:log-trace-proc (make-console-printer 'trace)
                        #:log-debug-proc (make-console-printer 'debug)
                        #:log-info-proc (make-console-printer 'info)
                        #:log-warn-proc (make-console-printer 'warn)
                        #:log-error-proc (make-console-printer 'error)))

(define scriba-console-logger
  (memoize-logger %make-scriba-console-logger))
