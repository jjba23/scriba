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

(define-module (veritas unit string-log-spec)
  #:use-module (veritas veritas)
  #:use-module (scriba console)
  #:use-module (scriba scriba)
  #:export (spec))

(define string-log-suite
  (suite "Console Backend - String Stream Output"
         (let* ((string-port (open-output-string))
                (logger (scriba-console-logger #:out-port string-port
                                               #:layout '(level tags name
                                                                message
                                                                context))))

           (test "Layout and Context Layout Rendering"
                 (assert-no-error (lambda ()
                                    (with-log-context '((sample-1 . value-1))
                                                      (log-info logger
                                                       "Some log with context"))
                                    (let ((result (get-output-string
                                                   string-port)))
                                      (unless (equal?
                                               "[INFO] Some log with context [sample-1=value-1]\n"
                                               result)
                                        (throw 'unexpected-log-message result))))
                  #:name
                  "Formats severe levels and layout records to raw string buffers accurately")))))

(define tag-log-suite
  (suite "Dynamic Tag Filtering Pipeline"
         ;; Matching tags
         (let* ((string-port (open-output-string))
                (logger (scriba-console-logger #:out-port string-port
                                               #:tag-filter '(performance is
                                                              quite important)
                                               #:layout '(level tags name
                                                                message
                                                                context))))

           (test "Matching Tag Interception"
                 (assert-no-error (lambda ()
                                    (with-log-tags '(performance is quite
                                                                 important)
                                                   (log-message logger
                                                    "Some log without level but with tags with context"))
                                    (let ((result (get-output-string
                                                   string-port)))
                                      (unless (string-prefix-ci?
                                               "[#performance #is #quite #important] Some log without level but with tags with context"
                                               result)
                                        (throw 'unexpected-log-message result))))
                  #:name
                  "Passes execution and prepends active tags when context matches the filter hash")))

         ;; Non-matching tags
         (let* ((string-port (open-output-string))
                (logger (scriba-console-logger #:out-port string-port
                                               #:tag-filter '(performance is
                                                              quite important)
                                               #:layout '(level tags name
                                                                message
                                                                context))))

           (test "Non-Matching Tag Suppression"
                 (assert-no-error (lambda ()
                                    (with-log-tags '(irrelevant tag)
                                                   (log-message logger
                                                    "Some log without level but with irrelevanttags"))
                                    (let ((result (get-output-string
                                                   string-port)))
                                      (unless (equal? "" result)
                                        (throw 'unexpected-log-message result))))
                  #:name
                  "Intercepts unlisted tag streams and silently drops them via log-unit redirection")))))

(define context-log-suite
  (suite "Dynamic Context Filtering Pipeline"
         ;; Matching contexts
         (let* ((string-port (open-output-string))
                (logger (scriba-console-logger #:out-port string-port
                                               #:context-filter '((type . performance))
                                               #:layout '(level name message
                                                                context))))

           (test "Matching Context Interception"
                 (assert-no-error (lambda ()
                                    (with-log-context '((type . performance))
                                                      (log-message logger
                                                       "Some log without level but with context"))
                                    (let ((result (get-output-string
                                                   string-port)))
                                      (unless (string-prefix-ci?
                                               "Some log without level but with context [type=performance]"
                                               result)
                                        (throw 'unexpected-log-message result))))
                  #:name
                  "Passes execution and prepends active contexts when context matches the filter hash")))

         ;; Non-matching contexts
         (let* ((string-port (open-output-string))
                (logger (scriba-console-logger #:out-port string-port
                                               #:context-filter '((type . performance))
                                               #:layout '(level name message
                                                                context))))

           (test "Non-Matching Context Suppression"
                 (assert-no-error (lambda ()
                                    (with-log-context '((irrelevant . context))
                                                      (log-message logger
                                                       "Some log without level but with irrelevant contexts"))
                                    (let ((result (get-output-string
                                                   string-port)))
                                      (unless (equal? "" result)
                                        (throw 'unexpected-log-message result))))
                  #:name
                  "Intercepts unlisted context streams and silently drops them via log-unit redirection")))))

(define (spec)
  (veritas-run string-log-suite tag-log-suite context-log-suite))
