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

;;; Commentary:
;;
;; This module implements a structured JSON logging backend for the Scriba
;; framework using Guile's (json) module serialization. It is intended
;; for production environments that require schema-compliant logs ready to be
;; parsed by log shippers and indexers (e.g., Loki, Elasticsearch, Logstash, Fluentd).

;;; Code:

(define-module (scriba json)
  #:use-module (json)
  #:use-module (scriba scriba)
  #:export (scriba-json-logger default-json-pretty? %make-scriba-json-logger))

(define (get-time-field format-string)
  "Returns a timestamp alist entry."
  `((time unquote
          (strftime format-string
                    (localtime (current-time))))))

(define (get-name-field name)
  "Returns a name alist entry, or an empty list if unnamed."
  (if name
      `((name unquote name))
      '()))

(define* (%make-scriba-json-logger #:key name
                                   (level (default-log-level))
                                   pretty?
                                   (timestamp-format (default-time-format))
                                   (out-port (default-out-port))
                                   (flush-port? (default-flush-port?))
                                   (layout '(level time host name message
                                                   context)))

  (define name-field
    (get-name-field name))

  (define (make-json-printer lvl)
    (let ((level-field `((level unquote
                                (string-upcase (symbol->string lvl)))))
          (host-field `((host unquote
                              (gethostname)))))
      (lambda (msg . args)
        (let* ((user-msg (apply format #f msg args))
               (base-field `((message unquote user-msg)))
               (time-field (if (memq 'time layout)
                               (get-time-field timestamp-format) #f))
               (dynamic-ctx (current-log-context))
               (fragments `((level unquote level-field)
                            (time unquote time-field)
                            (host unquote host-field)
                            (name unquote name-field)
                            (message unquote base-field)
                            (context unquote dynamic-ctx)))

               (ordered-fragments (map (lambda (sym)
                                         (let ((frag (assq-ref fragments sym)))
                                           (if frag frag
                                               '()))) layout))
               (fields (apply append ordered-fragments))
               (serialized-log (scm->json-string fields
                                                 #:pretty pretty?)))
          (format out-port "\n~a" serialized-log)
          (when flush-port?
            (force-output out-port))))))

  (make-filtered-logger #:name name
                        #:level level
                        #:log-trace-proc (make-json-printer 'trace)
                        #:log-debug-proc (make-json-printer 'debug)
                        #:log-info-proc (make-json-printer 'info)
                        #:log-success-proc (make-json-printer 'success)
                        #:log-warning-proc (make-json-printer 'warning)
                        #:log-error-proc (make-json-printer 'error)
                        #:log-critical-proc (make-json-printer 'critical)))

(define scriba-json-logger
  (memoize-logger %make-scriba-json-logger))

(define default-json-pretty?
  (make-parameter #f))
