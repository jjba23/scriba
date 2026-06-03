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
