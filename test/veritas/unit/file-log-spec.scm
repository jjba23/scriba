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
