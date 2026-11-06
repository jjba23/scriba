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
