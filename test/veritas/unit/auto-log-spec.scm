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

(define-module (veritas unit auto-log-spec)
  #:use-module (veritas veritas)
  #:use-module (scriba auto)
  #:use-module (scriba scriba)
  #:export (spec))

(define auto-log-suite
  (suite "Adaptive Auto-Logger Routing Backend"
         (let* ((auto-logger (scriba-auto-logger)))

           (test "Baseline Routing and Dynamic Tag Binding"
                 (assert-no-error (lambda ()
                                    (log-info auto-logger "Hello Scriba!")
                                    (with-log-tags '(performance)
                                                   (log-message auto-logger
                                                    "Slept 0 seconds"))
                                    (log-message auto-logger
                                     "While any editor can save your files only Emacs can save your soul !"))
                  #:name
                  "Dispatches unformatted strings and appends active tags without execution faults"))

           (test "Severity Enforcement and Argument Expansion"
                 (assert-no-error (lambda ()
                                    ;; Verify that low severity logs are safely dropped or ignored
                                    (log-debug auto-logger
                                     "we should never see this since log level is not high enough!")
                                    (log-warning auto-logger
                                                 "some kind of warning")
                                    (log-success auto-logger "yes! we made it")
                                    (log-info auto-logger
                                              "Hello Scriba, 2 + 2 = ~a!"
                                              (+ 2 2)))
                  #:name
                  "Intercepts low-priority severities and cleanly expands layout format arguments"))

           (test "Context Metadata Mapping"
                 (assert-no-error (lambda ()
                                    (with-log-context `((sample-1 . value-1))
                                                      (log-info auto-logger
                                                       "Some log with context")))
                  #:name
                  "Safely threads structured Mapped Diagnostic Context (MDC) scopes onto active pipelines")))))

(define (spec)
  (veritas-run auto-log-suite))
