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

(define (format-timestamp with-timestamp? format-string)
  "Returns a formatted timestamp string, or an empty string."
  (if with-timestamp?
      (format #f "[~a] "
              (strftime format-string
                        (localtime (current-time)))) ""))

(define (format-context ctx)
  "Converts an alist context into a bracketed string layout: [k=v] [k=v]."
  (let ((mdc (map (lambda (pair)
                    (format #f "[~a=~a]"
                            (car pair)
                            (cdr pair))) ctx)))
    (if (nil? ctx) ""
        (string-append (string-join mdc " ") " "))))

(define (format-name name)
  "Returns the formatted logger name, or an empty string."
  (if name
      (format #f "[~a] " name) ""))

(define* (%make-scriba-console-logger #:key name
                                      (level (default-log-level))
                                      (with-timestamp? #t)
                                      (timestamp-format (default-time-format)))
  (define name-str
    (format-name name))

  (define (make-console-printer lvl)
    (let ((level-str (format #f "[~a] "
                             (symbol->string lvl))))
      (lambda (msg . args)
        (let* ((time-str (format-timestamp with-timestamp? timestamp-format))
               (ctx-str (format-context (current-log-context)))
               (user-msg (apply format #f msg args)))
          
          (format #t
                  "\n~a~a~a~a~a"
                  level-str
                  time-str
                  name-str
                  ctx-str
                  user-msg)))))

  (make-filtered-logger #:name name
                        #:level level
                        #:log-trace-proc (make-console-printer 'trace)
                        #:log-debug-proc (make-console-printer 'debug)
                        #:log-info-proc (make-console-printer 'info)
                        #:log-warn-proc (make-console-printer 'warn)
                        #:log-error-proc (make-console-printer 'error)))

(define scriba-console-logger
  (memoize-logger %make-scriba-console-logger))
