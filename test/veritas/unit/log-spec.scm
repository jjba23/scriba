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

(define simple-log-suite
  (suite "Simple log suite"
         (test "console log tests"
               (let* ((s (scriba-console-logger)))
                 (assert-true (begin
                                (log-info s "Hello Scriba!") #t)
                  #:name
                  "console logger with default configuration and simple message works")
                 (assert-true (begin
                                (log-info s
                                 "Hello Scriba, current time is: ~a, current module is: ~a!"
                                 (current-time)
                                 (current-module)) #t)
                  #:name
                  "console logger with default configuration and formatted message works")))
         (test "json log tests"
               (let* ((s (scriba-json-logger)))
                 (assert-true (begin
                                (log-info s "Hello Scriba!") #t)
                  #:name
                  "json logger with default configuration and simple message works")
                 (assert-true (begin
                                (log-info s
                                 "Hello Scriba, current time is: ~a, current module is: ~a!"
                                 (current-time)
                                 (current-module)) #t)
                  #:name
                  "json logger with default configuration and formatted message works")))))

(define (spec)
  (veritas-run simple-log-suite))
