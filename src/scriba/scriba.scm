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

(define-module (scriba scriba)
  #:export (get-logger))

;;
;; logging helpers
;;

;; (define (log-message msg)
;;   (display (format #f "\n~a\n" msg)))

;; (define (log msg . args)
;;   (log-message (apply format
;;                       (append (list #f msg) args))))

;; (define (log-info msg . args)
;;   (log-message (format #f "[INFO] ~a"
;;                        (apply format
;;                               (append (list #f msg) args)))))
