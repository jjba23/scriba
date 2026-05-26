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
  #:export (scriba-console-logger %make-scriba-console-logger))

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
                                      (with-timestamp? (default-with-timestamp?))
                                      (timestamp-format (default-time-format))
                                      (out-port (default-out-port))
                                      (flush-port? (default-flush-port?))
                                      ;; 1. Add layout parameter with a default order
                                      (layout '(level time name context
                                                      message)))
  (define name-str
    (format-name name))

  (define (make-console-printer lvl)
    (let ((level-str (format #f "[~a] "
                             (string-upcase (symbol->string lvl)))))
      (lambda (msg . args)
        (let* ((time-str (format-timestamp with-timestamp? timestamp-format))
               (ctx-str (format-context (current-log-context)))
               (user-msg (apply format #f msg args))

               (parts-alist `((level unquote level-str)
                              (time unquote time-str)
                              (name unquote name-str)
                              (context unquote ctx-str)
                              (message unquote user-msg)))

               (ordered-strings (map (lambda (sym)
                                       (or (assq-ref parts-alist sym) ""))
                                     layout))

               (final-output (apply string-append ordered-strings)))
          (format out-port "\n~a" final-output)
          (when flush-port?
            (force-output out-port))))))

  (make-filtered-logger #:name name
                        #:level level
                        #:log-trace-proc (make-console-printer 'trace)
                        #:log-debug-proc (make-console-printer 'debug)
                        #:log-info-proc (make-console-printer 'info)
                        #:log-success-proc (make-console-printer 'success)
                        #:log-warning-proc (make-console-printer 'warning)
                        #:log-error-proc (make-console-printer 'error)
                        #:log-critical-proc (make-console-printer 'critical)))

(define scriba-console-logger
  (memoize-logger %make-scriba-console-logger))
