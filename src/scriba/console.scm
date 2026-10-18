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
