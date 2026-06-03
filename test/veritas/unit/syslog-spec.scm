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
