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

(define-module (veritas unit log-spec)
  #:use-module (veritas veritas)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 iconv)
  #:use-module (scriba scriba)
  #:use-module (scriba console)
  #:use-module (scriba color-console)
  #:use-module (scriba json)
  #:export (spec))

(define log-suite
  (suite "Core Engine - Text & JSON Backends"
         (let* ((console-logger (scriba-console-logger #:layout '(time level
                                                                  tags message
                                                                  context)))
                (color-logger (scriba-color-console-logger #:layout '(time
                                                                      level
                                                                      tags
                                                                      message
                                                                      context))))
           
           (test "Standard and Colorized Console Output Handling"
                 (assert-no-error (lambda ()
                                    (log-info console-logger "Hello Scriba!")
                                    (with-log-tags '(hello world)
                                                   (log-info color-logger
                                                             "Hello World!")
                                                   (with-log-context `((planet . earth))
                                                                     (log-info
                                                                      color-logger
                                                                      "Yes!")))
                                    (with-log-tags '(performance)
                                                   (log-message console-logger
                                                    "Slept 0 seconds")))
                  #:name
                  "Processes baseline strings, colorized streams, and multi-layered tag/context parameters without faults")

                 (assert-no-error (lambda ()
                                    ;; Low level debug should be intercepted and muted
                                    (log-debug console-logger
                                     "we should never see this since log level is not high enough!")
                                    (log-warning console-logger
                                                 "some kind of warning")
                                    (log-critical color-logger
                                                  "IN VINO VERITAS")
                                    (with-log-context `(("SANITAS" . "CARPE DIEM"))
                                                      (log-critical
                                                       color-logger "IN AQUA"))
                                    (log-success color-logger
                                                 "ja, wir schaffen das!")
                                    (log-success console-logger
                                                 "yes! we made it")
                                    (log-message console-logger
                                     "While any editor can save your files only Emacs can save your soul !")
                                    (log-info console-logger
                                              "Hello Scriba, 2 + 2 = ~a!"
                                              (+ 2 2)))
                  #:name
                  "Enforces strict severity thresholds and evaluates format string arguments across console writers")

                 (assert-no-error (lambda ()
                                    (with-log-context `((sample-1 . value-1))
                                                      (log-info console-logger
                                                       "Some log with context")))
                  #:name
                  "Safely captures and binds flat associative context pairs during console emission")))

         (let* ((json-logger (scriba-json-logger)))
           (test "JSON Structured Map Output Pipeline"
                 (assert-no-error (lambda ()
                                    (log-info json-logger "Hello Scriba!"))
                  #:name
                  "Serializes clean, unformatted top-level message keys to JSON text fields")

                 (assert-no-error (lambda ()
                                    ;; Low level debug should be intercepted and muted
                                    (log-debug json-logger
                                     "we should never see this since log level is not high enough!")
                                    (log-warning json-logger
                                                 "some kind of warning")
                                    (log-success json-logger "yes! we made it")
                                    (with-log-tags '(performance)
                                                   (log-message json-logger
                                                    "Slept 0 seconds"))
                                    (log-message json-logger
                                     "While any editor can save your files only Emacs can save your soul !")
                                    (log-info json-logger
                                     "Hello Scriba, current module is: ~a!"
                                     (current-module)))
                  #:name
                  "Mutes low-severity data and marshals dynamic evaluation states into complex JSON trees")

                 (assert-no-error (lambda ()
                                    (with-log-context `((sample-2 . value-2))
                                                      (log-info json-logger
                                                       "Some log with context")))
                  #:name
                  "Encodes active metadata maps as nested JSON objects within the parent scheme payload")))))

(define log-level-suite
  (suite "Severity Threshold Hierarchy Validation"
         (test "Log Level Precedence Visibility Matrix"
               (for-each (lambda (case-record)
                           (let ((target-level (car case-record))
                                 (configured-context (cadr case-record))
                                 (should-allow? (caddr case-record)))
                             (if should-allow?
                                 (assert-true (is-in-log-level? target-level
                                               configured-context)
                                              #:name (format #f
                                                      "Permit: [~a] visibility should be active inside a [~a] logging threshold"
                                                      target-level
                                                      configured-context))
                                 (assert-false (is-in-log-level? target-level
                                                configured-context)
                                               #:name (format #f
                                                       "Suppress: [~a] visibility should be discarded inside a [~a] logging threshold"
                                                       target-level
                                                       configured-context)))))
                           '((trace trace #t)
                             (trace debug #f)
                             (debug critical #f)
                             (info trace #t)
                             (info error #f)
                             (success info #t)
                             (warning trace #t)
                             (warning debug #t)
                             (critical info #t)
                             (error warning #t)
                             (error error #t))))))

(define (spec)
  (veritas-run log-suite log-level-suite))
