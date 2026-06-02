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
  (suite "Log"
         (let* ((s (scriba-console-logger #:layout '(time level tags message
                                                          context)))
                (sc (scriba-color-console-logger #:layout '(time level tags
                                                                 message
                                                                 context))))
           (test "scriba-console-logger"
                 (assert-no-error (lambda ()
                                    (log-info s "Hello Scriba!")
                                    (with-log-tags '(hello world) (log-info sc "Hello World!")
                                                   (with-log-context `((planet . earth))
                                                                     (log-info sc "Yes!")))
                                    (with-log-tags '(performance)
                                                   (log s "Slept 0 seconds")))
                                  #:name
                                  "default configuration and simple message")
                 (assert-no-error (lambda ()
                                    (log-debug s
                                     "we should never see this since log level is not high enough!")
                                    (log-warning s "some kind of warning")
                                    (log-critical sc "IN VINO VERITAS")
                                    (with-log-context `(("SANITAS" . "CARPE DIEM"))
                                                      (log-critical sc
                                                                    "IN AQUA"))
                                    (log-success sc "ja, wir schaffen das!")
                                    (log-success s "yes! we made it")
                                    (log s
                                     "While any editor can save your files only Emacs can save your soul !")
                                    (log-info s "Hello Scriba, 2 + 2 = ~a!"
                                              (+ 2 2)))
                  #:name "default configuration and formatted message")
                 (assert-no-error (lambda ()
                                    (with-log-context `((sample-1 . value-1))
                                                      (log-info s
                                                       "Some log with context")))
                                  #:name
                                  "default configuration and logging context")))
         (let* ((s (scriba-json-logger)))
           (test "scriba-json-logger"
                 (assert-no-error (lambda ()
                                    (log-info s "Hello Scriba!"))
                                  #:name
                                  "default configuration and simple message")
                 (assert-no-error (lambda ()
                                    (log-debug s
                                     "we should never see this since log level is not high enough!")
                                    (log-warning s "some kind of warning")
                                    (log-success s "yes! we made it")
                                    (with-log-tags '(performance)
                                                   (log s "Slept 0 seconds"))
                                    (log s
                                     "While any editor can save your files only Emacs can save your soul !")
                                    (log-info s
                                     "Hello Scriba, current module is: ~a!"
                                     (current-module)))
                  #:name "default configuration and formatted message")
                 (assert-no-error (lambda ()
                                    (with-log-context `((sample-2 . value-2))
                                                      (log-info s
                                                       "Some log with context")))
                                  #:name
                                  "default configuration and logging context")))))

(define log-level-suite
  (suite "Log Level"
         (apply test "is-in-log-level?"
                (map (lambda (c)
                       (if (caddr c)
                           (assert-true (is-in-log-level? (car c)
                                                          (cadr c))
                                        #:name (format #f
                                                       "~a is allowed in ~a"
                                                       (car c)
                                                       (cadr c)))
                           (assert-false (is-in-log-level? (car c)
                                                           (cadr c))
                                         #:name (format #f
                                                 "~a is not allowed in ~a"
                                                 (car c)
                                                 (cadr c)))))
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
