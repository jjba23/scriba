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
  #:use-module (scriba scriba)
  #:use-module (scriba json)
  #:use-module (scriba console)
  #:export (scriba-auto-logger))

(define (get-env-log-level)
  "Parses the log level from the environment, falling back to default-level."
  (let ((env-val (or (getenv "SCRIBA_LOG_LEVEL")
                     (getenv "LOG_LEVEL"))))
    (if env-val
        (string->symbol (string-downcase env-val))
        (default-log-level))))

(define (get-env-logger)
  "Parses the log format ('console' or 'json') from the environment."
  (let ((env-val (or (getenv "SCRIBA_LOGGER")
                     (getenv "LOGGER"))))
    (if env-val
        (string-downcase env-val)
        (default-logger))))

(define (get-env-timestamp-format)
  "Parses the log timestamp format from the environment."
  (let ((env-val (or (getenv "SCRIBA_TIMESTAMP_FORMAT")
                     (getenv "LOG_TIMESTAMP_FORMAT"))))
    (if env-val env-val
        (default-time-format))))

(define (get-env-json-pretty?)
  "Checks if the JSON logger should pretty-print based on the environment."
  (let ((env-val (or (getenv "SCRIBA_JSON_PRETTY")
                     (getenv "LOG_JSON_PRETTY"))))
    (if env-val
        (or (string=? (string-downcase env-val) "true")
            (string=? env-val "1"))
        (default-json-pretty?))))

(define (get-env-with-timestamp?)
  "Checks if the logger should print timestamps based on the environment."
  (let ((env-val (or (getenv "SCRIBA_WITH_TIMESTAMP")
                     (getenv "LOG_WITH_TIMESTAMP"))))
    (if env-val
        (or (string=? (string-downcase env-val) "true")
            (string=? env-val "1"))
        (default-with-timestamp?))))

(define* (%make-scriba-auto-logger #:key name
                                   (level (get-env-log-level))
                                   (with-timestamp? (get-env-with-timestamp?))
                                   (timestamp-format (get-env-timestamp-format))
                                   (pretty? (get-env-json-pretty?)))
  "Creates a logger automatically configured by environment variables."
  (let ((log-format (get-env-logger)))
    (if (string=? log-format "json")
        (scriba-json-logger #:name name
                            #:level level
                            #:pretty? pretty?
                            #:with-timestamp? with-timestamp?
                            #:timestamp-format timestamp-format)
        (scriba-console-logger #:name name
                               #:level level
                               #:with-timestamp? with-timestamp?
                               #:timestamp-format timestamp-format))))

(define scriba-auto-logger
  (memoize-logger %make-scriba-auto-logger))
