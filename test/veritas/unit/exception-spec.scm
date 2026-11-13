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
;; Coverage for logging Guile exceptions: the report a caught exception turns
;; into, the trace captured at the frame that raised, and how each backend
;; renders that report. Veritas only collects assertions that sit directly
;; inside a `test' form, so the helpers below return plain values and every
;; check is a single assertion.

(define-module (veritas unit exception-spec)
  #:use-module (veritas veritas)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (scriba scriba)
  #:use-module (scriba exception)
  #:use-module (scriba console)
  #:use-module (scriba color-console)
  #:use-module (scriba json)
  #:use-module (scriba syslog)
  #:export (spec))

;;; Helpers

;; Two frames, so the trace has a frame that raised and a caller above it. The
;; second expression keeps the call out of tail position, which is what keeps
;; the caller's frame in the stack.
(define (deep-car x)
  (car x))

(define (deep-fail)
  (deep-car 5)
  'unreachable)

(define* (report-of thunk
                    #:key (depth 8))
  "Returns the exception report of THUNK, capturing the stack at the raise."
  (let ((stack #f))
    (catch #t thunk
           (lambda (key . args)
             (make-exception-report key
                                    args
                                    #:stack stack
                                    #:depth depth))
           (lambda (key . args)
             (set! stack
                   (make-stack #t 2))))))

(define (deep-report)
  (report-of deep-fail))

(define (report-message thunk)
  (exception-report-message (report-of thunk)))

(define (report-trace thunk)
  (exception-report-trace (report-of thunk)))

(define (log-deep-fail logger)
  (log-exception logger
                 (deep-report)))

(define (log-boom logger)
  (log-exception logger
                 (make-exception-report 'app-error
                                        (list "disk on fire"))))

(define* (console-of thunk
                     #:key (level 'trace)
                     (layout '(level message)))
  "Runs THUNK on a console logger in a string port and returns the output."
  (let ((port (open-output-string)))
    (thunk (scriba-console-logger #:out-port port
                                  #:level level
                                  #:layout layout))
    (get-output-string port)))

(define* (color-of thunk
                   #:key (level 'trace))
  (let ((port (open-output-string)))
    (thunk (scriba-color-console-logger #:out-port port
                                        #:level level
                                        #:layout '(level message)))
    (get-output-string port)))

(define* (syslog-of thunk
                    #:key (level 'trace))
  (let ((port (open-output-string)))
    (thunk (scriba-syslog-logger #:out-port port
                                 #:level level
                                 #:timestamp-format "FIXED"
                                 #:layout '(time message)))
    (get-output-string port)))

(define* (json-of thunk
                  #:key (level 'trace))
  (let ((port (open-output-string)))
    (thunk (scriba-json-logger #:out-port port
                               #:level level
                               #:layout '(level message)))
    (json-string->scm (get-output-string port))))

(define (field object key)
  "Looks KEY up in a parsed JSON object, as a symbol or as a string."
  (cond
    ((not (list? object))
     #f)
    ((assq key object)
     => cdr)
    ((assoc (symbol->string key) object)
     => cdr)
    (else #f)))

(define (lines-of string)
  (filter (lambda (line)
            (not (string-null? line)))
          (string-split string #\newline)))

(define (first-line string)
  (car (lines-of string)))

(define (contains? haystack needle)
  (and (string-contains haystack needle) #t))

(define (rethrow thunk)
  "Runs THUNK, swallowing whatever it raises, and returns #f."
  (catch #t thunk
         (lambda (key . args)
           #f)))

(define (critical-line)
  "The first line of an exception logged at critical level."
  (first-line (console-of (lambda (logger)
                            (log-exception logger
                                           (deep-report)
                                           #:level 'critical)))))

(define (context-line)
  "The first line of an exception logged inside a log context."
  (first-line (console-of (lambda (logger)
                            (with-log-context '((tenant . acme))
                                              (log-boom logger)))
                          #:layout '(level context message))))

(define (logged-with-exception-logging thunk)
  "The output of THUNK wrapped in `with-exception-logging' on a console logger."
  (console-of (lambda (logger)
                (rethrow (lambda ()
                           (with-exception-logging logger
                                                   (thunk)))))))

;;; Suites

(define report-suite
  (suite "Exception Reports"
         (test "A caught Guile error becomes a report"
               (assert-true (exception-report? (deep-report))
                            #:name "The constructor returns a report")
               (assert-equal #:expect 'wrong-type-arg
                             #:got (exception-report-type (deep-report))
                             #:name "The exception key names the type")
               (assert-true (contains? (report-message deep-fail)
                                       "Wrong type argument")
                            #:name "The message is Guile's own rendering"))

         (test "A non-Guile throw still has a message"
               (assert-equal #:expect 'app-error
                             #:got (exception-report-type (report-of (lambda ()
                                                                       (throw 'app-error
                                                                        "boom"
                                                                        7))))
                             #:name "The key is kept as the type")
               (assert-true (contains? (report-message (lambda ()
                                                         (throw 'app-error
                                                                "boom" 7)))
                                       "boom")
                            #:name "The arguments of an unknown key print"))

         (test "The trace is captured at the frame that raised"
               (assert-true (string-prefix? "at "
                                            (car (report-trace deep-fail)))
                            #:name "A frame reads as `at NAME (FILE:LINE)'")
               (assert-true (contains? (car (report-trace deep-fail))
                                       "exception-spec.scm")
                            #:name "The frame names the file it came from")
               (assert-true (contains? (car (report-trace deep-fail)) ".scm:")
                            #:name "The frame names the line it came from"))

         (test "The trace honours the depth limit"
               (assert-equal #:expect 1
                             #:got (length (exception-report-trace (report-of
                                                                    deep-fail
                                                                    #:depth 1)))
                             #:name "depth 1 keeps one frame"))

         (test "A report built without a stack has no trace"
               (assert-equal #:expect '()
                             #:got (exception-report-trace (make-exception-report 'misc-error
                                                            (list #f "~A"
                                                                  '("boom") #f)))
                             #:name "No stack, no frames"))))

(define format-suite
  (suite "Report Formatting"
         (test "The text trace is indented, one frame per line"
               (assert-true (string-prefix? "  at "
                                            (format-exception-trace (deep-report)))
                            #:name "Frames start with two spaces and `at'")
               (assert-equal #:expect (length (report-trace deep-fail))
                             #:got (length (lines-of (format-exception-trace (deep-report))))
                             #:name "Every frame gets its own line"))

         (test "An empty trace formats as the empty string"
               (assert-equal #:expect ""
                             #:got (format-exception-trace (make-exception-report 'app-error
                                                            (list "boom")))
                             #:name "Text backends have nothing to append"))

         (test "The single-line form has no newline in it"
               (assert-equal #:expect 0
                             #:got (string-count (format-exception-line (deep-report))
                                                 #\newline)
                             #:name "Line-framed backends stay on one line")
               (assert-true (contains? (format-exception-line (deep-report))
                                       " | at ")
                            #:name "The frames are joined with a separator"))))

(define logging-suite
  (suite "Logging an Exception"
         (test "A console line carries the level, the message and the trace"
               (assert-true (string-prefix? "[ERROR] "
                                            (first-line (console-of
                                                         log-deep-fail)))
                            #:name "The line is logged at error level")
               (assert-true (contains? (first-line (console-of log-deep-fail))
                                       "Wrong type argument")
                            #:name "The message is on the first line")
               (assert-true (string-prefix? "  at "
                                            (cadr (lines-of (console-of
                                                             log-deep-fail))))
                            #:name "The trace follows it, indented"))

         (test "The level of an exception is configurable"
               (assert-true (string-prefix? "[CRITICAL] "
                                            (critical-line))
                            #:name "critical replaces error")
               (assert-equal #:expect ""
                             #:got (console-of log-deep-fail
                                               #:level 'critical)
                             #:name "A level below the threshold stays muted"))

         (test "Log context reaches the exception line"
               (assert-true (string-prefix? "[ERROR] [tenant=acme] "
                                            (context-line))
                            #:name "The exception stays inside the context"))

         (test "The JSON backend structures the exception"
               (assert-equal #:expect "wrong-type-arg"
                             #:got (field (field (json-of log-deep-fail)
                                                 'exception)
                                          'type)
                             #:name "The type is a JSON member")
               (assert-true (string? (field (field (json-of log-deep-fail)
                                                   'exception)
                                            'message))
                            #:name "The message is a JSON string")
               (assert-true (vector? (field (field (json-of log-deep-fail)
                                                   'exception)
                                            'trace))
                            #:name "The trace is a JSON array")
               (assert-equal #:expect "ERROR"
                             #:got (field (json-of log-deep-fail)
                                          'level)
                             #:name "The level is still a normal member"))

         (test "A plain JSON line has no exception member"
               (assert-equal #:expect #f
                             #:got (field (json-of (lambda (logger)
                                                     (log-info logger "hello")))
                                          'exception)
                             #:name "Exceptions are not invented"))

         (test "The syslog backend keeps the exception on one line"
               (assert-equal #:expect 1
                             #:got (string-count (syslog-of log-deep-fail)
                                                 #\newline)
                             #:name "RFC 5424 framing is one message per line")
               (assert-true (contains? (syslog-of log-deep-fail)
                                       "Wrong type argument")
                            #:name "The message survives the flattening"))

         (test "The colour console keeps both the message and the trace"
               (assert-true (contains? (color-of log-deep-fail)
                                       "Wrong type argument")
                            #:name "The message is still in the output")
               (assert-true (contains? (color-of log-deep-fail) "  at ")
                            #:name "The trace is still in the output"))))

(define macro-suite
  (suite "Catching, Logging and Re-raising"
         (test "A body that returns logs nothing of its own"
               (assert-equal #:expect ""
                             #:got (console-of (lambda (logger)
                                                 (with-exception-logging
                                                  logger
                                                  (+ 1 1))))
                             #:name "Only raising bodies produce reports"))

         (test "The exception is logged and then re-raised"
               (assert-error (lambda ()
                               (console-of (lambda (logger)
                                             (with-exception-logging logger
                                                                     (deep-fail)))))
                             #:name "The exception still reaches the caller")
               (assert-true (contains? (logged-with-exception-logging
                                        deep-fail) "Wrong type argument")
                            #:name "The report was logged before the re-raise"))

         (test "A quit is not an error to log"
               (assert-equal #:expect ""
                             #:got (logged-with-exception-logging (lambda ()
                                                                    (throw 'quit
                                                                           0)))
                             #:name "Leaving the program is not a failure"))

         (test "The function form takes a level and re-raises"
               (assert-error (lambda ()
                               (call-with-exception-log (scriba-console-logger
                                                         #:out-port (open-output-string))
                                                        deep-fail
                                                        #:level 'critical))
                             #:name "call-with-exception-log re-raises too"))))

(define (spec)
  (veritas-run report-suite format-suite logging-suite macro-suite))
