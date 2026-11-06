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

(define-module (veritas unit syslog-spec)
  #:use-module (veritas veritas)
  #:use-module (scriba syslog)
  #:use-module (scriba scriba)
  #:export (spec))

(define syslog-suite
  (suite "Syslog Integration Backend"
         (let* ((syslog-logger (scriba-syslog-logger)))

           (test "Baseline Plain-Text Logging"
                 (assert-no-error (lambda ()
                                    (log-info syslog-logger "Hello Scriba!"))
                  #:name "Emits a standard, unformatted string without errors"))

           (test "Severity Handlers and String Formatting"
                 (assert-no-error (lambda ()
                                    ;; Muted level check
                                    (log-debug syslog-logger
                                     "we should never see this since log level is not high enough!")
                                    ;; Active levels checks
                                    (log-warning syslog-logger
                                                 "some kind of warning")
                                    ;; String interpolation check
                                    (log-success syslog-logger "yes! ~a"
                                                 "we made it")
                                    (log-info syslog-logger
                                              "Hello Scriba, 2 + 2 = ~a!"
                                              (+ 2 2)))
                  #:name
                  "Processes multiple severities and expands format arguments cleanly"))

           (test "Dynamic Context and Tag Propagation"
                 (assert-no-error (lambda ()
                                    ;; MDC Context checks
                                    (with-log-context '((sample-1 . value-1))
                                                      (log-info syslog-logger
                                                       "Some log with context"))
                                    (with-log-context '((sample-1 . value-1))
                                                      (log-message
                                                       syslog-logger
                                                       "Some log with context"))
                                    ;; Dynamic tag filter checks
                                    (with-log-tags '(performance benchmark)
                                                   (log-info syslog-logger
                                                    "Not gonna sleep!")))
                  #:name
                  "Safely threads structured metadata alists and active tag lists to output")))))

(define (spec)
  (veritas-run syslog-suite))
