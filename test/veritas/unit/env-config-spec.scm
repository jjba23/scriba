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

(define-module (veritas unit env-config-spec)
  #:use-module (veritas veritas)
  #:use-module (scriba scriba)
  #:use-module (scriba auto)
  #:export (spec))

(define env-vars
  '("SCRIBA_LOGGER" "SCRIBA_LOG_LEVEL"
    "SCRIBA_LOG_LAYOUT"
    "SCRIBA_CONTEXT_FILTER"
    "SCRIBA_TAG_FILTER"
    "SCRIBA_TIMESTAMP_FORMAT"
    "SCRIBA_LINE_ENDING"
    "SCRIBA_OUT_PORT"
    "SCRIBA_JSON_PRETTY"
    "SCRIBA_FLUSH_PORT"))

(define (with-restored-environment thunk)
  "Runs THUNK with the SCRIBA_* environment variables restored afterwards."
  (let ((saved (map (lambda (var)
                      (cons var
                            (getenv var))) env-vars)))
    (dynamic-wind (lambda ()
                    #t) thunk
                  (lambda ()
                    (for-each (lambda (entry)
                                (if (cdr entry)
                                    (setenv (car entry)
                                            (cdr entry))
                                    (unsetenv (car entry)))) saved)))))

(define (prepare-environment!)
  (for-each unsetenv
            '("SCRIBA_LOG_LAYOUT" "SCRIBA_TAG_FILTER"
              "SCRIBA_TIMESTAMP_FORMAT"
              "SCRIBA_LINE_ENDING"
              "SCRIBA_OUT_PORT"
              "SCRIBA_JSON_PRETTY"
              "SCRIBA_FLUSH_PORT"))
  (setenv "SCRIBA_LOGGER" "console")
  (setenv "SCRIBA_LOG_LEVEL" "info"))

(define (capture-auto-output configure! log!)
  "Builds an auto-logger configured by CONFIGURE!, runs LOG! on it and
returns everything that reached the logger's string port."
  (with-restored-environment (lambda ()
                               (prepare-environment!)
                               (configure!)
                               (let ((port (open-output-string)))
                                 (log! (scriba-auto-logger #:out-port port
                                                           #:layout '(level
                                                                      message
                                                                      context)))
                                 (get-output-string port)))))

(define (configure-context-filter!)
  (setenv "SCRIBA_CONTEXT_FILTER" "type;performance"))

(define (log-context-sample logger)
  (with-log-context '((type . performance))
                    (log-info logger "kept"))
  (with-log-context '((type . other))
                    (log-info logger "dropped")))

(define (configure-leading-newline!)
  (setenv "SCRIBA_LINE_ENDING" "leading"))

(define (log-hello logger)
  (log-info logger "hello"))

(define (configure-syslog!)
  (setenv "SCRIBA_LOGGER" "syslog"))

(define (log-syslog-line logger)
  (log-info logger "syslog shaped"))

(define (contains-timestamp-separator? str)
  (and (memv #\T
             (string->list str)) #t))

(define env-suite
  (suite "Environment Driven Auto-Logger Configuration"
         (test "Environment context filters match structured context"
               (assert-equal #:expect "[INFO] kept [type=performance]\n"
                #:got (capture-auto-output configure-context-filter!
                                           log-context-sample)
                #:name "Parses key;value environment pairs into a real alist"))

         (test "Environment line ending is honored"
               (assert-equal #:expect "\n[INFO] hello"
                             #:got (capture-auto-output
                                    configure-leading-newline! log-hello)
                             #:name
                             "Routes SCRIBA_LINE_ENDING into the backend"))

         (test "Auto syslog keeps an RFC 5424 timestamp by default"
               (assert-true (contains-timestamp-separator? (capture-auto-output
                                                            configure-syslog!
                                                            log-syslog-line))
                #:name
                "Does not leak the human console timestamp format into syslog"))))

(define (spec)
  (veritas-run env-suite))
