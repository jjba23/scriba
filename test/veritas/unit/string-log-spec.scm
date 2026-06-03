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

(define-module (veritas unit string-log-spec)
  #:use-module (veritas veritas)
  #:use-module (scriba console)
  #:use-module (scriba scriba)
  #:export (spec))

(define string-log-suite
  (suite "String"
         (let* ((string-port (open-output-string))
                (s (scriba-console-logger #:out-port string-port
                                          #:layout '(level tags name message
                                                           context))))
           (test "scriba-console-logger to string"
                 (assert-no-error (lambda ()
                                    (with-log-context `((sample-1 . value-1))
                                                      (log-info s
                                                       "Some log with context"))
                                    (let ((r (get-output-string string-port)))
                                      (unless (string-prefix-ci?
                                               "\n[INFO] Some log with context"
                                               r)
                                        (throw 'unexpected-log-message r))))
                                  #:name
                                  "default configuration and logging context")))))

(define tag-log-suite
  (suite "Tag log suite"
         (let* ((string-port (open-output-string))
                (s (scriba-console-logger #:out-port string-port
                                          #:tag-filter '(performance is quite
                                                         important)
                                          #:layout '(level tags name message
                                                           context))))
           (test "scriba-console-logger with tagging"
                 (assert-no-error (lambda ()
                                    (with-log-tags '(performance is quite
                                                                 important)
                                                   (log-message s
                                                    "Some log without level but with tags with context"))
                                    (let ((r (get-output-string string-port)))
                                      (unless (string-prefix-ci?
                                               "
[#performance #is #quite #important] Some log without level but with tags with context"
                                               r)
                                        (throw 'unexpected-log-message r))))
                                  #:name
                                  "default configuration and logging context")))
         (let* ((string-port (open-output-string))
                (s (scriba-console-logger #:out-port string-port
                                          #:tag-filter '(performance is quite
                                                         important)
                                          #:layout '(level tags name message
                                                           context))))
           (test "scriba-console-logger with tagging"
                 (assert-no-error (lambda ()
                                    (with-log-tags '(irrelevant tag)
                                                   (log-message s
                                                    "Some log without level but with irrelevanttags"))
                                    (let ((r (get-output-string string-port)))
                                      (unless (equal? "" r)
                                        (throw 'unexpected-log-message r))))
                                  #:name
                                  "default configuration and logging context")))))

(define (spec)
  (veritas-run string-log-suite tag-log-suite))
