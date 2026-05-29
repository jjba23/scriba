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

(define-module (scriba auto)
  #:use-module (ice-9 match)
  #:use-module (scriba scriba)
  #:use-module (scriba json)
  #:use-module (scriba color-console)
  #:use-module (scriba console)
  #:use-module (scriba syslog)
  #:export (scriba-auto-logger))

(define (get-env-log-level)
  "Parses the log level from the environment, falling back to default-level."
  (let ((env-val (getenv "SCRIBA_LOG_LEVEL")))
    (if env-val
        (string->symbol (string-downcase env-val))
        (default-log-level))))

(define (get-env-logger)
  "Parses the log format ('console', 'color-console', 'syslog' or 'json') from the environment."
  (let ((env-val (getenv "SCRIBA_LOGGER")))
    (if env-val
        (string->symbol (string-downcase env-val))
        (default-logger))))

(define (get-env-layout)
  "Parses the log layout from the environment."
  (let ((env-val (getenv "SCRIBA_LOG_LAYOUT")))
    (if env-val
        (map string->symbol
             (string-split env-val #\,))
        (default-log-layout))))

(define (get-env-timestamp-format)
  "Parses the log timestamp format from the environment."
  (let ((env-val (getenv "SCRIBA_TIMESTAMP_FORMAT")))
    (if env-val env-val
        (default-time-format))))

(define (get-env-json-pretty?)
  "Checks if the JSON logger should pretty-print based on the environment."
  (let ((env-val (getenv "SCRIBA_JSON_PRETTY")))
    (if env-val
        (or (string=? (string-downcase env-val) "true")
            (string=? env-val "1"))
        (default-json-pretty?))))

(define (get-env-flush-port?)
  "Checks if the logger should flush the port every log action, based on the environment."
  (let ((env-val (getenv "SCRIBA_FLUSH_PORT")))
    (if env-val
        (or (string=? (string-downcase env-val) "true")
            (string=? env-val "1"))
        (default-flush-port?))))

(define (get-env-out-port)
  "Parses the output port based on the environment."
  (let ((env-val (getenv "SCRIBA_OUT_PORT")))
    (if env-val
        (match (string->symbol (string-downcase env-val))
          (('stdout)
           (current-output-port))
          (('stderr)
           (current-error-port))
          (_ (default-out-port)))
        (default-out-port))))

(define* (%make-scriba-auto-logger #:key name
                                   (level (get-env-log-level))
                                   (out-port (get-env-out-port))
                                   (timestamp-format (get-env-timestamp-format))
                                   (flush-port? (get-env-flush-port?))
                                   (pretty? (get-env-json-pretty?))
                                   (layout (get-env-layout)))
  "Creates a logger automatically configured by environment variables."
  (let ((log-format (get-env-logger)))
    (cond
      ((eq? log-format
            'json)
       (scriba-json-logger #:name name
                           #:level level
                           #:out-port out-port
                           #:pretty? pretty?
                           #:flush-port? flush-port?
                           #:layout layout
                           #:timestamp-format timestamp-format))
      ((eq? log-format
            'color-console)
       (scriba-color-console-logger #:name name
                                    #:level level
                                    #:out-port out-port
                                    #:flush-port? flush-port?
                                    #:layout layout
                                    #:timestamp-format timestamp-format))
      ((eq? log-format
            'syslog)
       (scriba-syslog-logger #:name name
                             #:level level
                             #:out-port out-port
                             #:flush-port? flush-port?
                             ;; probably syslog always needs all its fields
                             ;; to be compliant to RFC-5424
                             ;; #:layout layout
                             #:timestamp-format timestamp-format))
      (else (scriba-console-logger #:name name
                                   #:level level
                                   #:out-port out-port
                                   #:flush-port? flush-port?
                                   #:layout layout
                                   #:timestamp-format timestamp-format)))))

(define scriba-auto-logger
  (memoize-logger %make-scriba-auto-logger))
