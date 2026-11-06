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
;; This module provides a standard plaintext, uncolorized console logging
;; backend for the Scriba logging framework. It outputs logs cleanly structured
;; by brackets, making it optimal for production log-aggregation sidecars,
;; file appending, or basic interactive terminal sessions.

;;; Code:

(define-module (scriba console)
  #:use-module (scriba scriba)
  #:use-module (scriba layout)
  #:export (scriba-console-logger %make-scriba-console-logger))

(define* (%make-scriba-console-logger #:key name
                                      (level (default-log-level))
                                      (context-filter (default-context-filter))
                                      (tag-filter (default-tag-filter))
                                      (timestamp-format (default-time-format))
                                      (out-port (default-out-port))
                                      (flush-port? (default-flush-port?))
                                      (layout (default-log-layout))
                                      (line-ending (default-line-ending)))
  (let ((printer (make-text-printer #:name name
                                    #:layout layout
                                    #:timestamp-format timestamp-format
                                    #:emit (lambda (line)
                                             (write-log-line out-port line
                                                             line-ending)
                                             (when flush-port?
                                               (force-output out-port))))))
    (make-filtered-logger #:name name
                          #:level level
                          #:context-filter context-filter
                          #:tag-filter tag-filter
                          #:log-message-proc (printer #t)
                          #:log-trace-proc (printer 'trace)
                          #:log-debug-proc (printer 'debug)
                          #:log-info-proc (printer 'info)
                          #:log-success-proc (printer 'success)
                          #:log-warning-proc (printer 'warning)
                          #:log-error-proc (printer 'error)
                          #:log-critical-proc (printer 'critical))))

(define scriba-console-logger
  (memoize-logger %make-scriba-console-logger
                  (#:level default-log-level)
                  (#:context-filter default-context-filter)
                  (#:tag-filter default-tag-filter)
                  (#:timestamp-format default-time-format)
                  (#:out-port default-out-port)
                  (#:flush-port? default-flush-port?)
                  (#:layout default-log-layout)
                  (#:line-ending default-line-ending)))
