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

(define-module (veritas unit redaction-spec)
  #:use-module (srfi srfi-13)
  #:use-module (veritas veritas)
  #:use-module (scriba scriba)
  #:use-module (scriba console)
  #:use-module (scriba json)
  #:export (spec))

(define (console-into port)
  (scriba-console-logger #:out-port port
                         #:layout '(level name message context)))

(define (json-into port)
  (scriba-json-logger #:out-port port))

(define (line-of make-logger redacted-keys ctx log-call)
  "Returns the line LOG-CALL gets its logger to write for CTX.

REDACTED-KEYS is in force for that line, the way a program would set it."
  (let ((port (open-output-string)))
    (parameterize ((default-redacted-keys redacted-keys))
      (with-log-context ctx
                        (log-call (make-logger port))))
    (get-output-string port)))

(define redaction-suite
  (suite "Context Redaction"
         (test "A redacted context key is masked before the layout"
               (assert-equal #:expect
                             "[INFO] Signed in [token=***] [user=joe]\n"
                             #:got (line-of console-into
                                            '("token")
                                            '((token . "s3cr3t")
                                              (user . "joe"))
                                            (lambda (logger)
                                              (log-info logger "Signed in")))
                             #:name
                             "The listed value is masked and the other pairs keep their order in the line"))

         (test "An unlisted context key keeps its value"
               (assert-equal #:expect
                             "[INFO] Kept [token=s3cr3t]\n"
                             #:got (line-of console-into
                                            '("password")
                                            '((token . "s3cr3t"))
                                            (lambda (logger)
                                              (log-info logger "Kept")))
                             #:name
                             "A narrow list masks nothing it was not given"))

         (test "A context filter still matches on the real value"
               (assert-equal #:expect
                             "Filtered [token=***]\n"
                             #:got (line-of (lambda (port)
                                              (scriba-console-logger #:out-port
                                                                     port
                                                                     #:context-filter
                                                                     '((token
                                                                        . "s3cr3t"))
                                                                     #:layout
                                                                     '(message
                                                                       context)))
                                            '("token")
                                            '((token . "s3cr3t"))
                                            (lambda (logger)
                                              (log-info logger "Filtered")))
                             #:name
                             "Redaction follows the filter, so a filter naming a secret still matches"))

         (test "Every backend redacts, not just the text layout"
               (assert-false (string-contains (line-of json-into
                                                       '("token")
                                                       '((token . "s3cr3t"))
                                                       (lambda (logger)
                                                         (log-info logger
                                                                   "Structured")))
                                              "s3cr3t")
                             #:name
                             "A JSON line carries no masked value either"))))

(define (spec)
  (veritas-run redaction-suite))
