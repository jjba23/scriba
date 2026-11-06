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

(define-module (veritas unit file-log-spec)
  #:use-module (veritas veritas)
  #:use-module (scriba console)
  #:use-module (scriba scriba)
  #:export (spec))

(define file-port
  (open-output-file "test.log"))

(define file-log-suite
  (suite "Console Backend - File System Destination"
         (let* ((file-logger (scriba-console-logger #:out-port file-port)))

           (test "File Destination Logging Operations"
                 (assert-no-error (lambda ()
                                    ;; Verify muted thresholds are still handled safely on file ports
                                    (log-debug file-logger
                                     "we should never see this since log level is not high enough!")
                                    (log-warning file-logger
                                                 "some kind of warning")
                                    (log-success file-logger "yes! we made it")
                                    (log-info file-logger
                                              "Hello Scriba, 2 + 2 = ~a!"
                                              (+ 2 2))
                                    (with-log-tags '(performance)
                                                   (log-message file-logger
                                                    "Slept 0 seconds"))
                                    (log-message file-logger
                                     "While any editor can save your files only Emacs can save your soul !"))
                  #:name
                  "Enforces severity thresholds and drops formatted payloads to a file stream without faults")

                 (assert-no-error (lambda ()
                                    (with-log-context `((sample-1 . value-1))
                                                      (log-info file-logger
                                                       "Some log with context")))
                  #:name
                  "Appends dynamic Mapped Diagnostic Context (MDC) properties onto a file-bound record")))))

(define (spec)
  (veritas-run file-log-suite)
  (close-output-port file-port))
