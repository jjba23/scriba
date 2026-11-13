;; scriba

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; scriba is released under the MIT License.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.  See the
;; LICENSE file for the full text.

;;; Commentary:
;;
;; This module implements a structured JSON logging backend for the Scriba
;; framework using Guile's (json) module serialization. It is intended
;; for production environments that require schema-compliant logs ready to be
;; parsed by log shippers and indexers (e.g., Loki, Elasticsearch, Logstash, Fluentd).

;;; Code:

(define-module (scriba json)
  #:use-module (json)
  #:use-module ((scriba layout)
                #:select (format-plain-time))
  #:use-module (scriba exception)
  #:use-module (scriba scriba)
  #:export (scriba-json-logger default-json-pretty? %make-scriba-json-logger))

(define default-json-pretty?
  (make-parameter #f))

(define (exception->json report)
  "Returns the `exception' member of a line logged for an exception.

The trace becomes an array rather than one string, so an indexer can group by
frame without parsing the text of a message."
  (list (cons 'exception
              (list (cons 'type
                          (symbol->string (exception-report-type report)))
                    (cons 'message
                          (exception-report-message report))
                    (cons 'trace
                          (list->vector (exception-report-trace report)))))))

(define (get-time-field format-string)
  "Returns a timestamp alist entry."
  `((time unquote
          (format-plain-time format-string))))

(define (get-name-field name)
  "Returns a name alist entry, or an empty list if unnamed."
  (if name
      `((name unquote name))
      '()))

(define* (%make-scriba-json-logger #:key name
                                   (level (default-log-level))
                                   (context-filter (default-context-filter))
                                   (tag-filter (default-tag-filter))
                                   (pretty? (default-json-pretty?))
                                   (timestamp-format (default-time-format))
                                   (out-port (default-out-port))
                                   (flush-port? (default-flush-port?))
                                   (line-ending (default-line-ending))
                                   (layout '(level tags
                                                   time
                                                   host
                                                   name
                                                   message
                                                   context)))

  (validate-log-layout layout
                       #:fields json-layout-fields)

  (define name-field
    (get-name-field name))

  (define (make-json-printer lvl)
    (let ((level-field (cond
                         ((symbol? lvl)
                          `((level unquote
                                   (string-upcase (symbol->string lvl)))))
                         ((boolean? lvl)
                          '())))
          (host-field `((host unquote
                              (gethostname)))))
      (lambda (msg . args)
        (let* ((user-msg (format-log-message msg args))
               (base-field `((message unquote user-msg)))
               (time-field (if (memq 'time layout)
                               (get-time-field timestamp-format) #f))
               (dynamic-ctx (current-log-context))
               (dynamic-tags (if (null? (current-log-tags)) #f
                                 `((tags unquote
                                         (list->vector (current-log-tags))))))
               (fragments `((level unquote level-field)
                            (tags unquote dynamic-tags)
                            (time unquote time-field)
                            (host unquote host-field)
                            (name unquote name-field)
                            (message unquote base-field)
                            (context unquote dynamic-ctx)))
               (ordered-fragments (map (lambda (sym)
                                         (let ((frag (assq-ref fragments sym)))
                                           (if frag frag
                                               '()))) layout))
               (report (current-log-exception))
               (fields (append (apply append ordered-fragments)
                               (if report
                                   (exception->json report)
                                   '())))
               (serialized-log (scm->json-string fields
                                                 #:pretty pretty?)))
          (write-log-line out-port serialized-log line-ending)
          (when flush-port?
            (force-output out-port))))))

  (make-filtered-logger #:name name
                        #:level level
                        #:context-filter context-filter
                        #:tag-filter tag-filter
                        #:log-message-proc (make-json-printer #t)
                        #:log-trace-proc (make-json-printer 'trace)
                        #:log-debug-proc (make-json-printer 'debug)
                        #:log-info-proc (make-json-printer 'info)
                        #:log-success-proc (make-json-printer 'success)
                        #:log-warning-proc (make-json-printer 'warning)
                        #:log-error-proc (make-json-printer 'error)
                        #:log-critical-proc (make-json-printer 'critical)))

(define scriba-json-logger
  (memoize-logger %make-scriba-json-logger
                  (#:level default-log-level)
                  (#:context-filter default-context-filter)
                  (#:tag-filter default-tag-filter)
                  (#:pretty? default-json-pretty?)
                  (#:timestamp-format default-time-format)
                  (#:out-port default-out-port)
                  (#:flush-port? default-flush-port?)
                  (#:line-ending default-line-ending)))
