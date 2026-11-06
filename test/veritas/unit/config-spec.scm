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

(define-module (veritas unit config-spec)
  #:use-module (veritas veritas)
  #:use-module (scriba scriba)
  #:use-module (scriba console)
  #:use-module (scriba json)
  #:use-module (scriba syslog)
  #:export (spec))

;; Veritas collects the assertions that sit directly inside a `test' form,
;; so the state these checks share lives in module level bindings rather than
;; in a `let*' (whose inner assertions would never run).
(define memo-port-1
  #f)
(define memo-port-2
  #f)
(define memo-logger-1
  #f)
(define memo-logger-2
  #f)
(define memo-logger-1-again
  #f)

(define memoization-suite
  (suite "Memoization Honors Dynamically Scoped Defaults"
         (test "Cached loggers follow the parameterized out-port"
               (set! memo-port-1
                     (open-output-string))
               (set! memo-port-2
                     (open-output-string))
               (set! memo-logger-1
                     (parameterize ((default-out-port memo-port-1))
                       (scriba-console-logger #:layout '(message))))
               (set! memo-logger-2
                     (parameterize ((default-out-port memo-port-2))
                       (scriba-console-logger #:layout '(message))))
               (set! memo-logger-1-again
                     (parameterize ((default-out-port memo-port-1))
                       (scriba-console-logger #:layout '(message))))
               (log-info memo-logger-1 "first port")
               (log-info memo-logger-2 "second port")
               (assert-false (eq? memo-logger-1 memo-logger-2)
                #:name
                "Different default-out-port values must not share a cached logger")
               (assert-true (eq? memo-logger-1 memo-logger-1-again)
                #:name
                "The same default-out-port value must resolve to the cached logger")
               (assert-equal #:expect "first port\n"
                             #:got (get-output-string memo-port-1)
                             #:name
                             "The first logger keeps writing to its own port")
               (assert-equal #:expect "second port\n"
                #:got (get-output-string memo-port-2)
                #:name "The second logger keeps writing to its own port"))

         (test "Explicit keyword arguments bypass parameter defaults"
               (let* ((port-1 (open-output-string))
                      (port-2 (open-output-string))
                      (logger (parameterize ((default-out-port port-1))
                                (scriba-console-logger #:out-port port-2
                                                       #:layout '(message)))))
                 (log-info logger "explicit")
                 (assert-equal #:expect "explicit\n"
                  #:got (get-output-string port-2)
                  #:name
                  "Explicit out-port wins over the parameterized default")))))

(define line-ending-suite
  (suite "Configurable Log Line Termination"
         (test "Trailing newline is the default"
               (let* ((port (open-output-string))
                      (logger (scriba-console-logger #:out-port port
                                                     #:layout '(level message))))
                 (log-info logger "hello")
                 (assert-equal #:expect "[INFO] hello\n"
                  #:got (get-output-string port)
                  #:name
                  "Log lines end with a newline and no leading blank line")))

         (test "Leading newline remains available"
               (let* ((port (open-output-string))
                      (logger (scriba-console-logger #:out-port port
                                                     #:line-ending 'leading
                                                     #:layout '(level message))))
                 (log-info logger "hello")
                 (assert-equal #:expect "\n[INFO] hello"
                  #:got (get-output-string port)
                  #:name "Opting into 'leading restores the prefixed newline")))

         (test "Invalid line endings are rejected"
               (let* ((port (open-output-string))
                      (logger (scriba-console-logger #:out-port port
                                                     #:line-ending 'middle
                                                     #:layout '(message))))
                 (assert-error (lambda ()
                                 (log-info logger "nope"))
                  #:name "Logging with an unknown line-ending raises an error")))))

(define validation-suite
  (suite "Constructor Argument Validation"
         (test "Unknown log levels are rejected"
               (assert-error (lambda ()
                               (scriba-console-logger #:level 'verbose))
                #:name "A typo such as 'verbose fails at construction time"))

         (test "Boolean and known levels are accepted"
               (assert-true (valid-log-level? #t)
                            #:name "#t means always log")
               (assert-true (valid-log-level? #f)
                            #:name "#f means never log")
               (assert-true (valid-log-level? 'warning)
                            #:name "Known severities are valid")
               (assert-false (valid-log-level? 'verbose)
                             #:name "Unknown severities are invalid"))

         (test "JSON pretty printing follows its parameter"
               (let* ((port (open-output-string))
                      (logger (parameterize ((default-json-pretty? #t))
                                (scriba-json-logger #:out-port port))))
                 (log-info logger "pretty")
                 (assert-true (and (memv #\newline
                                         (string->list (get-output-string port)))
                                   #t)
                  #:name "default-json-pretty? drives the JSON logger default")))

         (test "Unknown layout fields are rejected"
               (assert-error (lambda ()
                               (scriba-console-logger #:layout '(level
                                                                 nonsense)))
                #:name
                "A mistyped field fails instead of silently writing blank lines")
               (assert-error (lambda ()
                               (scriba-console-logger #:layout (list (string->symbol
                                                                      "level message"))))
                #:name
                "A space separated SCRIBA_LOG_LAYOUT becomes one unknown field")
               (assert-error (lambda ()
                               (scriba-console-logger #:layout '()))
                #:name
                "An empty layout would log empty lines, so it is rejected"))

         (test "Layout fields are backend specific"
               (assert-error (lambda ()
                               (scriba-json-logger #:layout '(process-id)))
                             #:name "The process id is not a JSON field")
               (assert-true (and (scriba-syslog-logger #:layout '(level
                                                                  process-id
                                                                  message-id))
                                 #t)
                            #:name "The process id is a syslog field")
               (assert-true (valid-log-field? 'message)
                            #:name "message is a known field")
               (assert-false (valid-log-field? 'nonsense)
                             #:name "nonsense is not"))))

(define (spec)
  (veritas-run memoization-suite line-ending-suite validation-suite))
