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

(define-module (scriba syslog)
  #:use-module (srfi srfi-1)
  #:use-module (scriba scriba)
  #:export (scriba-syslog-logger %make-scriba-syslog-logger))

;; RFC 5424 syslog logger

;;     Numerical             Facility
;;     Code

;;               0             kernel messages
;;               1             user-level messages
;;               2             mail system
;;               3             system daemons
;;               4             security/authorization messages
;;               5             messages generated internally by syslogd
;;               6             line printer subsystem
;;               7             network news subsystem
;;               8             UUCP subsystem
;;               9             clock daemon
;;              10             security/authorization messages
;;              11             FTP daemon
;;              12             NTP subsystem
;;              13             log audit
;;              14             log alert
;;              15             clock daemon (note 2)
;;              16             local use 0  (local0)
;;              17             local use 1  (local1)
;;              18             local use 2  (local2)
;;              19             local use 3  (local3)
;;              20             local use 4  (local4)
;;              21             local use 5  (local5)
;;              22             local use 6  (local6)
;;              23             local use 7  (local7)

;; severity table:
;; Numerical         Severity
;; code
;;               0                 Emergency: system is unusable
;;               1                 Alert: action must be taken immediately
;;               2                 Critical: critical conditions
;;               3                 Error: error conditions
;;               4                 Warning: warning conditions
;;               5                 Notice: normal but significant condition
;;               6                 Informational: informational messages
;;               7                 Debug: debug-level messages

(define (format-timestamp with-timestamp? format-string)
  "Returns a formatted timestamp string, or an empty string."
  (if with-timestamp?
      (format #f "~a"
              (strftime format-string
                        (localtime (current-time)))) #f))

(define (format-context ctx)
  "Converts an alist context into a bracketed string layout: [k=v] [k=v]."
  (let ((mdc (map (lambda (pair)
                    (format #f "~a=\"~a\""
                            (car pair)
                            (cdr pair))) ctx)))
    (if (nil? ctx) #f
        (format #f "[~a]"
                (string-join mdc " ")))))

(define (format-name name)
  "Returns the formatted logger name, or an empty string."
  (if name
      (format #f "[~a]" name) #f))

(define* (level-to-priority lvl
                            (facility 1))
  "Translates a scriba log level symbol to a Syslog PRI integer based on the facility."
  (let ((severity (cond
                    ((eq? lvl
                          'trace)
                     7)
                    ((eq? lvl
                          'debug)
                     7)
                    ((eq? lvl
                          'info)
                     6)
                    ((eq? lvl
                          'success)
                     5)
                    ((eq? lvl
                          'warning)
                     4)
                    ((eq? lvl
                          'error)
                     3)
                    ((eq? lvl
                          'critical)
                     2)
                    (else 6))))
    ;; Default fallback to Informational
    (+ (* facility 8) severity)))

;; This is the modern format defined in RFC 5424, providing more structured and standardized data for syslog messages.
;; <34>1 2026-05-29T09:05:00.123+02:00 mymachine.example.com su 12345 ID47 [exampleSD@32473 user="admin"] 'su root' failed

;; <PRI>VERSION TIMESTAMP HOSTNAME APP-NAME PROCID MSGID [STRUCTURED-DATA] MESSAGE
;; <PRI>: The priority field, combining the facility and severity level.
;; VERSION: The version of the syslog protocol (e.g., ).
;; TIMESTAMP: The timestamp of the event in the format YYYY-MM-DDThh:mm:ss.sssZ.
;; HOSTNAME: The hostname or IP address of the device generating the message.
;; APP-NAME: The name of the application or process generating the message.
;; PROCID: The process ID associated with the application or process.
;; MSGID: The ID to identify the type of message (e.g., "ID12345" for specific events).
;; [STRUCTURED-DATA]: Optional structured data containing additional information in key-value pairs.
;; MESSAGE: The actual log message.

(define* (%make-scriba-syslog-logger #:key name
                                     (level (default-log-level))
                                     (with-timestamp? (default-with-timestamp?))
                                     (timestamp-format (default-time-format))
                                     (out-port (default-out-port))
                                     (flush-port? (default-flush-port?))
                                     (layout '(level time name message context)))
  (define name-str
    (format-name name))

  (define (make-syslog-printer lvl)
    (let ((level-str (format #f "<~a>2"
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

               (ordered-strings (filter-map (lambda (sym)
                                              (assq-ref parts-alist sym))
                                            layout))

               (final-output (string-join ordered-strings " ")))
          (format out-port "\n~a" final-output)
          (when flush-port?
            (force-output out-port))))))

  (make-filtered-logger #:name name
                        #:level level
                        #:log-trace-proc (make-syslog-printer 'trace)
                        #:log-debug-proc (make-syslog-printer 'debug)
                        #:log-info-proc (make-syslog-printer 'info)
                        #:log-success-proc (make-syslog-printer 'success)
                        #:log-warning-proc (make-syslog-printer 'warning)
                        #:log-error-proc (make-syslog-printer 'error)
                        #:log-critical-proc (make-syslog-printer 'critical)))

(define scriba-syslog-logger
  (memoize-logger %make-scriba-syslog-logger))
