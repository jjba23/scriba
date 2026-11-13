;; scriba

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; scriba is released under the MIT License.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.  See the
;; LICENSE file for the full text.

;;; Commentary:
;;
;; This module implements a logging backend structured to conform with
;; syslog protocol specifications (such as RFC 5424 concepts). It generates
;; string messages featuring priority framing headers, process identifier tracking,
;; and standardized structured metadata fields.

;;; Code:

(define-module (scriba syslog)
  #:use-module (srfi srfi-1)
  #:use-module ((scriba layout)
                #:select (format-plain-time display->string))
  #:use-module (scriba exception)
  #:use-module (scriba scriba)
  #:export (scriba-syslog-logger %make-scriba-syslog-logger
                                 default-syslog-time-format))

(define (format-timestamp format-string)
  "Returns a formatted timestamp string."
  (format-plain-time format-string))

(define default-syslog-time-format
  (make-parameter iso-8601-time-format))

(define (format-context ctx)
  "Converts an alist context into a bracketed string layout: [k=v] [k=v]."
  (if (null? ctx) #f
      (string-append "["
                     (string-join (map (lambda (pair)
                                         (string-append (display->string (car
                                                                          pair))
                                                        "=\""
                                                        (display->string (cdr
                                                                          pair))
                                                        "\"")) ctx) " ") "]")))

(define (format-tags tags)
  "Converts a list of tags into a bracketed string layout: [#a #b #c]."
  (if (null? tags) #f
      (string-append "["
                     (string-join (map (lambda (x)
                                         (string-append "#"
                                                        (display->string x)))
                                       tags) " ") "]")))

(define (format-name name)
  "Returns the formatted logger name, or an empty string."
  (if name
      (string-append "["
                     (display->string name) "]") #f))

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
                                     (context-filter (default-context-filter))
                                     (tag-filter (default-tag-filter))
                                     (timestamp-format (default-syslog-time-format))
                                     (out-port (default-out-port))
                                     (flush-port? (default-flush-port?))
                                     (line-ending (default-line-ending))
                                     (layout '(level time
                                                     host
                                                     name
                                                     process-id
                                                     message-id
                                                     context
                                                     message
                                                     tags)))
  (validate-log-layout layout
                       #:fields syslog-layout-fields)
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
               (report (current-log-exception))
               ;; RFC 5424 frames one message per line, so the trace is joined
               ;; onto it instead of indented under it.
               (user-msg (if report
                             (format-exception-line report)
                             (format-log-message msg args)))
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
          (write-log-line out-port final-output line-ending)
          (when flush-port?
            (force-output out-port))))))

  (make-filtered-logger #:name name
                        #:level level
                        #:context-filter context-filter
                        #:tag-filter tag-filter
                        #:log-message-proc (make-syslog-printer #t)
                        #:log-trace-proc (make-syslog-printer 'trace)
                        #:log-debug-proc (make-syslog-printer 'debug)
                        #:log-info-proc (make-syslog-printer 'info)
                        #:log-success-proc (make-syslog-printer 'success)
                        #:log-warning-proc (make-syslog-printer 'warning)
                        #:log-error-proc (make-syslog-printer 'error)
                        #:log-critical-proc (make-syslog-printer 'critical)))

(define scriba-syslog-logger
  (memoize-logger %make-scriba-syslog-logger
                  (#:level default-log-level)
                  (#:context-filter default-context-filter)
                  (#:tag-filter default-tag-filter)
                  (#:timestamp-format default-syslog-time-format)
                  (#:out-port default-out-port)
                  (#:flush-port? default-flush-port?)
                  (#:line-ending default-line-ending)))
