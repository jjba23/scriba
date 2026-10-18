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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (scriba scriba)
  #:use-module (scriba json)
  #:use-module (scriba color-console)
  #:use-module (scriba console)
  #:use-module (scriba file)
  #:use-module (scriba syslog)
  #:export (scriba-auto-logger %make-scriba-auto-logger))

(define (get-env-with-default var-name default-proc transform)
  "Retrieves an environment variable, transforming it if found, or falling back to a default."
  (let ((env-val (getenv var-name)))
    (if env-val
        (transform env-val)
        (default-proc))))

(define (string->boolean str)
  "Normalizes string truthiness for env configurations."
  (let ((down (string-downcase (string-trim-both str))))
    (or (string=? down "true")
        (string=? down "1"))))

(define (string->downcase-symbol str)
  "Trims and downcases STR before turning it into a symbol."
  (string->symbol (string-downcase (string-trim-both str))))

(define (split-env-list str)
  "Splits STR on commas, trimming entries and dropping empty ones.

This makes `SCRIBA_TAG_FILTER=\"a, b ,,c\"` behave as `(a b c)` instead of
producing confusing empty symbols."
  (filter-map (lambda (entry)
                (let ((entry (string-trim-both entry)))
                  (if (string-null? entry) #f entry)))
              (string-split str #\,)))

(define (get-env-log-level)
  "Parses the log level from the environment, falling back to default-level."
  (get-env-with-default "SCRIBA_LOG_LEVEL" default-log-level
                        string->downcase-symbol))

(define (get-env-logger)
  "Parses the logger backend from the environment.

Known backends are 'console, 'color-console, 'syslog, 'json and 'file; an
unknown value falls back to the console rather than leaving the program
without a logger."
  (get-env-with-default "SCRIBA_LOGGER" default-logger string->downcase-symbol))

(define (get-env-layout)
  "Parses the log layout from the environment."
  (get-env-with-default "SCRIBA_LOG_LAYOUT" default-log-layout
                        (lambda (val)
                          (map string->symbol
                               (split-env-list val)))))

(define (get-env-tag-filter)
  "Parses the log tag filter from the environment."
  (get-env-with-default "SCRIBA_TAG_FILTER" default-tag-filter
                        (lambda (val)
                          (map string->symbol
                               (split-env-list val)))))

(define (get-env-context-filter)
  "Parses the log context filter from the environment.

The expected format is a comma-separated list of `key;value` pairs, such as
`request-id;abc123,tenant;acme`. Each entry becomes a proper alist pair, which
is what `context-match?` compares against."
  (get-env-with-default "SCRIBA_CONTEXT_FILTER" default-context-filter
                        (lambda (val)
                          (map (lambda (entry)
                                 (match (string-split entry #\;)
                                   ((key value . _) (cons (string-trim-both
                                                           key)
                                                          (string-trim-both
                                                           value)))
                                   ((key . _) (cons (string-trim-both key) ""))))
                               (split-env-list val)))))

(define (get-env-timestamp-format)
  "Parses the log timestamp format from the environment."
  (get-env-with-default "SCRIBA_TIMESTAMP_FORMAT" default-time-format
                        (lambda (val)
                          (string-trim-both val))))

(define (get-auto-timestamp-format)
  "Resolves the timestamp format for the selected backend.

Syslog is RFC 5424 shaped, so it keeps an ISO-8601 timestamp unless the user
explicitly sets SCRIBA_TIMESTAMP_FORMAT. Other backends use the human readable
default."
  (if (getenv "SCRIBA_TIMESTAMP_FORMAT")
      (get-env-timestamp-format)
      (if (eq? (get-env-logger)
               'syslog)
          (default-syslog-time-format)
          (get-env-timestamp-format))))

(define (get-env-json-pretty?)
  "Checks if the JSON logger should pretty-print based on the environment."
  (get-env-with-default "SCRIBA_JSON_PRETTY" default-json-pretty?
                        string->boolean))

(define (get-env-flush-port?)
  "Checks if the logger should flush the port every log action, based on the environment."
  (get-env-with-default "SCRIBA_FLUSH_PORT" default-flush-port?
                        string->boolean))

(define (get-env-line-ending)
  "Parses the newline placement of each log line from the environment."
  (get-env-with-default "SCRIBA_LINE_ENDING" default-line-ending
                        (lambda (val)
                          (let ((ending (string->downcase-symbol val)))
                            (if (memq ending
                                      '(trailing leading)) ending
                                (error
                                 "Scriba: SCRIBA_LINE_ENDING must be 'trailing or 'leading"
                                 val))))))

(define (get-env-out-port)
  "Parses the output port based on the environment."
  (get-env-with-default "SCRIBA_OUT_PORT" default-out-port
                        (lambda (val)
                          (match (string->downcase-symbol val)
                            ('stdout (current-output-port))
                            ('stderr (current-error-port))
                            (_ (default-out-port))))))

(define (get-env-file-path)
  "Parses the destination path of the file backend.

Blank values count as unset, so an empty SCRIBA_FILE_PATH does not silently
produce a logger pointed at the current directory. When the variable is
absent, `default-file-path' is used."
  (get-env-with-default "SCRIBA_FILE_PATH" default-file-path
                        (lambda (val)
                          (let ((path (string-trim-both val)))
                            (if (string-null? path) #f path)))))

(define (get-env-file-max-size)
  "Parses the rotation threshold of the file backend in bytes.

`off', `none', `false' or `0' disable rotation; anything else must be a
positive whole number of bytes."
  (get-env-with-default "SCRIBA_FILE_MAX_SIZE" default-file-max-size
                        (lambda (val)
                          (let* ((trimmed (string-downcase (string-trim-both
                                                            val)))
                                 (size (string->number trimmed)))
                            (cond
                              ((member trimmed
                                       '("off" "none" "false" "0"))
                               #f)
                              ((and (integer? size)
                                    (positive? size))
                               size)
                              (else (error
                                     "Scriba: SCRIBA_FILE_MAX_SIZE must be a positive byte count or 'off'"
                                     val)))))))

(define (get-env-file-max-backups)
  "Parses how many rotated backups the file backend keeps."
  (get-env-with-default "SCRIBA_FILE_MAX_BACKUPS" default-file-max-backups
                        (lambda (val)
                          (let ((count (string->number (string-trim-both val))))
                            (if (and (integer? count)
                                     (>= count 0)) count
                                (error
                                 "Scriba: SCRIBA_FILE_MAX_BACKUPS must be a non-negative integer"
                                 val))))))

(define* (%make-scriba-auto-logger #:key name
                                   (level (get-env-log-level))
                                   (context-filter (get-env-context-filter))
                                   (tag-filter (get-env-tag-filter))
                                   (out-port (get-env-out-port))
                                   (timestamp-format (get-auto-timestamp-format))
                                   (flush-port? (get-env-flush-port?))
                                   (pretty? (get-env-json-pretty?))
                                   (layout (get-env-layout))
                                   (line-ending (get-env-line-ending))
                                   (path (get-env-file-path))
                                   (max-size (get-env-file-max-size))
                                   (max-backups (get-env-file-max-backups))
                                   (log-format (get-env-logger)))
  "Creates a logger automatically configured by environment variables.

Each backend only receives the keywords it understands: the file backend
takes a path and rotation options instead of an output port, and syslog
keeps its own RFC 5424 layout."
  (define common-args
    (list #:name name
          #:level level
          #:context-filter context-filter
          #:tag-filter tag-filter
          #:timestamp-format timestamp-format
          #:flush-port? flush-port?
          #:line-ending line-ending))
  (case log-format
    ((json)
     (apply scriba-json-logger
            (append common-args
                    (list #:out-port out-port
                          #:layout layout
                          #:pretty? pretty?))))
    ((file)
     (unless path
       (error
        "Scriba: SCRIBA_LOGGER=file needs SCRIBA_FILE_PATH (or the #:path argument)"))
     (apply scriba-file-logger
            (append common-args
                    (list #:path path
                          #:max-size max-size
                          #:max-backups max-backups
                          #:layout layout))))
    ((syslog)
     (apply scriba-syslog-logger
            (append common-args
                    (list #:out-port out-port))))
    ((color-console)
     (apply scriba-color-console-logger
            (append common-args
                    (list #:out-port out-port
                          #:layout layout))))
    (else (apply scriba-console-logger
                 (append common-args
                         (list #:out-port out-port
                               #:layout layout))))))

(define scriba-auto-logger
  (memoize-logger %make-scriba-auto-logger
                  (#:level get-env-log-level)
                  (#:context-filter get-env-context-filter)
                  (#:tag-filter get-env-tag-filter)
                  (#:out-port get-env-out-port)
                  (#:timestamp-format get-auto-timestamp-format)
                  (#:flush-port? get-env-flush-port?)
                  (#:pretty? get-env-json-pretty?)
                  (#:layout get-env-layout)
                  (#:line-ending get-env-line-ending)
                  (#:path get-env-file-path)
                  (#:max-size get-env-file-max-size)
                  (#:max-backups get-env-file-max-backups)
                  (#:log-format get-env-logger)))
