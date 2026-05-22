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

(define-module (scriba main)
  #:use-module (scriba scriba)
  #:use-module (scriba console)
  #:use-module (scriba json))

(let* ((s (scriba-console-logger #:name 'console-logger
                                 #:level 'info)))
  (log-info s "1 + 1 = ~a"
            (+ 1 1))
  (log-error s "some error happened"))

(newline)

(let* ((s (scriba-json-logger #:name 'json-logger
                              #:level 'info
                              #:pretty #f)))
  (log-info s "2 + 2 = ~a"
            (+ 2 2))
  (log-warn s "something suspicious happened"))
