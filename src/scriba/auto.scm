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
;; This module provides zero-configuration initialization for the Scriba
;; logging framework. It dynamically bootstraps, configures, and memoizes
;; an appropriate logger backend based on system environment variables.

;;; Code:

(define-module (scriba auto)
  #:use-module (ice-9 match)
  #:use-module (scriba scriba)
  #:use-module (scriba json)
  #:use-module (scriba color-console)
  #:use-module (scriba console)
  #:use-module (scriba syslog)
  #:export (scriba-auto-logger))

(define (get-env-with-default var-name default-proc transform)
  "Retrieves an environment variable, transforming it if found, or falling back to a default."
  (let ((env-val (getenv var-name)))
    (if env-val
        (transform env-val)
        (default-proc))))

(define (string->boolean str)
  "Normalizes string truthiness for env configurations."
  (let ((down (string-downcase str)))
    (or (string=? down "true")
        (string=? down "1"))))

(define (string->downcase-symbol str)
  (string->symbol (string-downcase str)))

(define (get-env-log-level)
  "Parses the log level from the environment, falling back to default-level."
  (get-env-with-default "SCRIBA_LOG_LEVEL" default-log-level
                        string->downcase-symbol))

(define (get-env-logger)
  "Parses the logger ('console', 'color-console', 'syslog' or 'json') from the environment."
  (get-env-with-default "SCRIBA_LOGGER" default-logger string->downcase-symbol))

(define (get-env-layout)
  "Parses the log layout from the environment."
  (get-env-with-default "SCRIBA_LOG_LAYOUT" default-log-layout
                        (lambda (val)
                          (map string->symbol
                               (string-split val #\,)))))

(define (get-env-tag-filter)
  "Parses the log tag filter from the environment."
  (get-env-with-default "SCRIBA_TAG_FILTER" default-tag-filter
                        (lambda (val)
                          (map string->symbol
                               (string-split val #\,)))))

(define (get-env-timestamp-format)
  "Parses the log timestamp format from the environment."
  (get-env-with-default "SCRIBA_TIMESTAMP_FORMAT" default-time-format identity))

(define (get-env-json-pretty?)
  "Checks if the JSON logger should pretty-print based on the environment."
  (get-env-with-default "SCRIBA_JSON_PRETTY" default-json-pretty?
                        string->boolean))

(define (get-env-flush-port?)
  "Checks if the logger should flush the port every log action, based on the environment."
  (get-env-with-default "SCRIBA_FLUSH_PORT" default-flush-port?
                        string->boolean))

(define (get-env-out-port)
  "Parses the output port based on the environment."
  (get-env-with-default "SCRIBA_OUT_PORT" default-out-port
                        (lambda (val)
                          (match (string->downcase-symbol val)
                            ('stdout (current-output-port))
                            ('stderr (current-error-port))
                            (_ (default-out-port))))))

(define* (%make-scriba-auto-logger #:key name
                                   (level (get-env-log-level))
                                   (tag-filter (get-env-tag-filter))
                                   (out-port (get-env-out-port))
                                   (timestamp-format (get-env-timestamp-format))
                                   (flush-port? (get-env-flush-port?))
                                   (pretty? (get-env-json-pretty?))
                                   (layout (get-env-layout)))
  "Creates a logger automatically configured by environment variables."
  (let* ((log-format (get-env-logger))
         (constructor (match log-format
                        ('json scriba-json-logger)
                        ('color-console scriba-color-console-logger)
                        ('syslog scriba-syslog-logger)
                        (_ scriba-console-logger)))
         (base-args (list #:name name
                          #:level level
                          #:tag-filter tag-filter
                          #:out-port out-port
                          #:flush-port? flush-port?
                          #:timestamp-format timestamp-format))
         (args-with-layout (if (eq? log-format
                                    'syslog) base-args
                               (append base-args
                                       (list #:layout layout))))
         (final-args (if (eq? log-format
                              'json)
                         (append args-with-layout
                                 (list #:pretty? pretty?)) args-with-layout)))

    (apply constructor final-args)))

(define scriba-auto-logger
  (memoize-logger %make-scriba-auto-logger))
