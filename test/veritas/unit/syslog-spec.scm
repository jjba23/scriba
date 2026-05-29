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
  (suite "syslog"
         (let* ((s (scriba-syslog-logger)))
           (test "scriba-syslog-logger"
                 (assert-no-error (lambda ()
                                    (log-info s "Hello Scriba!"))
                                  #:name
                                  "default configuration and simple message")
                 (assert-no-error (lambda ()
                                    (log-debug s
                                     "we should never see this since log level is not high enough!")
                                    (log-warning s "some kind of warning")
                                    (log-success s "yes! we made it")
                                    (log-info s "Hello Scriba, 2 + 2 = ~a!"
                                              (+ 2 2)))
                  #:name "default configuration and formatted message")
                 (assert-no-error (lambda ()
                                    (with-log-context `((sample-1 . value-1))
                                                      (log-info s
                                                       "Some log with context")))
                                  #:name
                                  "default configuration and logging context")))))

(define (spec)
  (veritas-run syslog-suite))
