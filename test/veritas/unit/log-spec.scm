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
  #:use-module (scriba json)
  #:export (spec))

(define log-suite
  (suite "Log"
         (test "scriba-console-logger"
               (let* ((s (scriba-console-logger)))
                 (assert-true (begin
                                (log-info s "Hello Scriba!") #t)
                              #:name
                              "default configuration and simple message")
                 (assert-true (begin
                                (log-info s
                                 "Hello Scriba, current time is: ~a, current module is: ~a!"
                                 (current-time)
                                 (current-module)) #t)
                              #:name
                              "default configuration and formatted message")))
         (test "scriba-json-logger"
               (let* ((s (scriba-json-logger)))
                 (assert-true (begin
                                (log-info s "Hello Scriba!") #t)
                              #:name
                              "default configuration and simple message")
                 (assert-true (begin
                                (log-info s
                                 "Hello Scriba, current time is: ~a, current module is: ~a!"
                                 (current-time)
                                 (current-module)) #t)
                              #:name
                              "default configuration and formatted message")))))

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
                       (trace info #f)
                       (trace warn #f)
                       (trace error #f)
                       (debug trace #t)
                       (debug debug #t)
                       (debug info #f)
                       (debug warn #f)
                       (debug error #f)
                       (info trace #t)
                       (info debug #t)
                       (info info #t)
                       (info warn #f)
                       (info error #f)
                       (warn trace #t)
                       (warn debug #t)
                       (warn info #t)
                       (warn warn #t)
                       (warn error #f)
                       (error trace #t)
                       (error debug #t)
                       (error info #t)
                       (error warn #t)
                       (error error #t))))))

(define (spec)
  (veritas-run log-suite log-level-suite))
