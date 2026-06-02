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
;; This module implements a logging backend structured to conform with
;; syslog protocol specifications (such as RFC 5424 concepts). It generates
;; string messages featuring priority framing headers, process identifier tracking,
;; and standardized structured metadata fields.

;;; Code:

(define-module (scriba syslog)
  #:use-module (srfi srfi-1)
  #:use-module (scriba scriba)
  #:export (scriba-syslog-logger %make-scriba-syslog-logger))

(define (format-timestamp format-string)
  "Returns a formatted timestamp string."
  (format #f "~a"
          (strftime format-string
                    (localtime (current-time)))))

(define (format-context ctx)
  "Converts an alist context into a bracketed string layout: [k=v] [k=v]."
  (let ((mdc (map (lambda (pair)
                    (format #f "~a=\"~a\""
                            (car pair)
                            (cdr pair))) ctx)))
    (if (nil? ctx) #f
        (format #f "[~a]"
                (string-join mdc " ")))))

(define (format-tags tags)
  "Converts a list of tags into a bracketed string layout: [#a #b #c]."
  (let ((mdc (map (lambda (x)
                    (format #f "#~a" x)) tags)))
    (if (nil? tags) #f
        (format #f "[~a]"
                (string-join mdc " ")))))

(define (format-name name)
  "Returns the formatted logger name, or an empty string."
  (if name
      (format #f "[~a]" name) #f))

(define* (level-to-priority lvl
                            #:key (facility 1))
  "Translates a scriba log level symbol to a Syslog PRI integer based on the facility."
  (let ((severity (case lvl
                    ((trace debug)
                     7)
                    ((info)
                     6)
                    ((success)
                     5)
                    ((warning)
                     4)
                    ((error)
                     3)
                    ((critical)
                     2)
                    (else 6))))
    (+ (* facility 8) severity)))

(define* (%make-scriba-syslog-logger #:key name
                                     (level (default-log-level))
                                     (timestamp-format iso-8601-time-format)
                                     (out-port (default-out-port))
                                     (flush-port? (default-flush-port?))
                                     (layout '(level time
                                                     host
                                                     name
                                                     process-id
                                                     message-id
                                                     context
                                                     message
                                                     tags)))
  (define name-str
    (format-name name))

  (define (make-syslog-printer lvl)
    (let ((level-str (format #f "<~a>2"
                             (level-to-priority lvl)))
          (host-str (gethostname)))
      (lambda (msg . args)
        (let* ((time-str (if (memq 'time layout)
                             (format-timestamp timestamp-format) #f))
               (ctx-str (format-context (current-log-context)))
               (tags-str (format-tags (current-log-tags)))
               (user-msg (apply format #f msg args))
               (parts-alist `((level unquote level-str)
                              (tags unquote tags-str)
                              (time unquote time-str)
                              (host unquote host-str)
                              (name unquote name-str)
                              (process-id unquote
                                          (number->string (getpid)))
                              (message-id . "-")
                              (context unquote ctx-str)
                              (message unquote user-msg)))

               (ordered-strings (filter-map (lambda (sym)
                                              (assq-ref parts-alist sym))
                                            layout))

               (final-output (string-join ordered-strings " ")))
          (format out-port "\n~a" final-output)
          (when flush-port?
            (force-output out-port))))))

  (make-filtered-logger #:name name
                        #:level level
                        #:log-proc (make-syslog-printer #t)
                        #:log-trace-proc (make-syslog-printer 'trace)
                        #:log-debug-proc (make-syslog-printer 'debug)
                        #:log-info-proc (make-syslog-printer 'info)
                        #:log-success-proc (make-syslog-printer 'success)
                        #:log-warning-proc (make-syslog-printer 'warning)
                        #:log-error-proc (make-syslog-printer 'error)
                        #:log-critical-proc (make-syslog-printer 'critical)))

(define scriba-syslog-logger
  (memoize-logger %make-scriba-syslog-logger))
