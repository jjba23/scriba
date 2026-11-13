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
;; Turning a Guile exception into something a log line can carry.
;;
;; A Guile exception is a key plus arguments: `car' raising `wrong-type-arg',
;; `error' raising `misc-error', `(throw 'app-error ...)' raising
;; `app-error'. Rendering that well is Guile's own job - `print-exception'
;; knows every key's message shape, including the strerror text of a system
;; error and the location of a syntax error - so the message here is exactly
;; what Guile would print, without reimplementing any of those cases.
;;
;; What Guile does not lend is the trace, and that is what makes an exception
;; legible in a log: a handler installed before unwinding still has the frames
;; that raised, and the report keeps them as `at NAME (FILE:LINE)' lines,
;; innermost first, with Guile's own machinery and this module's plumbing
;; dropped. Backends then render as much of the report as their destination can
;; carry: the text backends indent the trace under the message, JSON makes an
;; object of it, and syslog, which frames one message per line, joins it with
;; separators.

;;; Code:

(define-module (scriba exception)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-9)
  #:use-module (system vm program)
  #:use-module (scriba scriba)
  #:export (make-exception-report exception-report?
                                  exception-report-type
                                  exception-report-message
                                  exception-report-trace
                                  format-exception-trace
                                  format-exception-line
                                  log-exception
                                  call-with-exception-log
                                  with-exception-logging))

(define-record-type <exception-report>
  (%make-exception-report type message trace) exception-report?
  (type exception-report-type)
  (message exception-report-message)
  (trace exception-report-trace))

;; Frames of Guile's own implementation, and of this module's plumbing, say
;; nothing about the application that failed: what a reader wants is the code
;; that led to the raise, not the machinery that caught it.
(define internal-source-prefixes
  '("ice-9/" "system/" "language/"))

(define internal-source-fragments
  '("/ice-9/" "/system/vm/" "scriba/exception.scm"))

(define (internal-source? file)
  "Is FILE one of Guile's own sources, or this module?"
  (and file
       (or (any (lambda (prefix)
                  (string-prefix? prefix file)) internal-source-prefixes)
           (any (lambda (fragment)
                  (string-contains file fragment)) internal-source-fragments))))

(define (frame-source-of frame)
  "Returns the source of FRAME, or #f when it has none."
  (false-if-exception (frame-source frame)))

(define (frame->line frame)
  "Returns `at NAME (FILE:LINE)' for FRAME, or #f when it is not worth a line.

A frame from Guile's own sources, or from this module's plumbing, says nothing
about the application that failed; a frame without a source is a compiled
procedure Guile cannot place, and a guess about it would be worse than leaving
it out."
  (let* ((source (frame-source-of frame))
         (file (and source
                    (false-if-exception (source:file source))))
         (line (and source
                    (false-if-exception (source:line source)))))
    (if (or (not file)
            (internal-source? file)
            (not line)) #f
        (format #f
                "at ~a (~a:~a)"
                (or (false-if-exception (frame-procedure-name frame))
                    "anonymous")
                file
                ;; `source:line' counts from zero, a reader counts from one.
                (+ 1 line)))))

(define (raise-site-frame start)
  "Returns the frame of the code that raised, given the innermost frame.

The innermost frames of a captured stack belong to the handler that caught the
raise - `make-stack', Guile's error dispatch, this module's own lambda - and the
code that raised starts just below the innermost `raise-exception'. Frames are
matched by procedure name, which is an order of magnitude cheaper than reading
their source, and a stack with no such frame is walked as it is."
  (let loop
    ((frame start))
    (cond
      ((not frame)
       start)
      ((eq? 'raise-exception
            (false-if-exception (frame-procedure-name frame)))
       (frame-previous frame))
      (else (loop (frame-previous frame))))))

(define* (exception-trace stack
                          #:key (depth 8))
  "Returns at most DEPTH trace lines from STACK, innermost first.

Reading the source of a frame of a captured stack is far from free in Guile -
a few hundred microseconds a frame on the machine this was measured on - so the
walk goes outwards one frame at a time and stops as soon as DEPTH lines are in
hand, instead of materialising the whole stack and reading all of it."
  (if (or (not stack)
          (zero? (stack-length stack)))
      '()
      (let loop
        ((frame (raise-site-frame (stack-ref stack 0)))
         (lines '()))
        (if (or (not frame)
                (>= (length lines) depth))
            (reverse lines)
            (let ((line (frame->line frame)))
              (loop (frame-previous frame)
                    (if line
                        (cons line lines) lines)))))))

(define (exception-message-text key args)
  "Returns the rendering Guile itself gives the exception KEY and ARGS.

Going through Guile's printer keeps every message shape - the strerror text of
a system error, the location of a syntax error, the arguments of an unknown
key - in Guile, rather than reimplemented and fallen behind here."
  (string-trim-both (call-with-output-string (lambda (port)
                                               (print-exception port #f key
                                                                args)))))

(define* (make-exception-report key args
                                #:key stack
                                (depth 8))
  "Summarises the exception KEY and ARGS, with a trace of STACK.

STACK is what a handler installed before unwinding captures with
`(make-stack #t 2)'. Without one the report still carries the type and the
message, and only the trace is missing."
  (%make-exception-report key
                          (exception-message-text key args)
                          (exception-trace stack
                                           #:depth depth)))

(define* (format-exception-trace report
                                 #:key (indent "  "))
  "Returns the trace of REPORT indented for a text backend, or the empty string."
  (string-join (map (lambda (frame)
                      (string-append indent frame))
                    (exception-report-trace report)) "\n"))

(define (single-line text)
  "Returns TEXT with every line break replaced by a space.

A syntax error message spans several lines, and a backend that frames one
message per line cannot carry that break."
  (string-join (string-split text #\newline) " "))

(define (format-exception-line report)
  "Returns REPORT on one line, for backends that frame one message per line."
  (string-join (cons (single-line (exception-report-message report))
                     (exception-report-trace report)) " | "))

(define (level-procedure level)
  "Returns the logging procedure for LEVEL, or raises when LEVEL is not one."
  (case level
    ((trace)
     log-trace)
    ((debug)
     log-debug)
    ((info)
     log-info)
    ((success)
     log-success)
    ((warning)
     log-warning)
    ((error)
     log-error)
    ((critical)
     log-critical)
    (else (error
           "Scriba: an exception is logged at a real level, expected one of (trace debug info success warning error critical), got"
           level))))

(define* (log-exception logger report
                        #:key (level 'error))
  "Logs REPORT on LOGGER at LEVEL, leaving the rendering to the backend.

The report is bound to `current-log-exception' around the call, which is what
each backend reads to indent the trace, nest it in JSON, or flatten it onto one
line. The message of the report is the log message itself, so a logger's level,
tag and context filters apply to an exception as they do to any other line."
  (parameterize ((current-log-exception report))
    ((level-procedure level)
     logger
     (exception-report-message report))))

(define* (call-with-exception-log logger thunk
                                  #:key (level 'error)
                                  (depth 8))
  "Calls THUNK, logging an exception it raises before re-raising it.

The handler runs before the stack is unwound, so `(make-stack #t 2)' still
holds the frames that raised; an unwinding handler would see only its own.

A `quit' - how a Scheme program leaves - is passed on unlogged: exiting is not
a failure to report. Logging must never hide the failure it reports, so a log
statement that itself raises is swallowed, and the original exception still
propagates with the same key and arguments it arrived with."
  (with-exception-handler (lambda (exn)
                            (unless (eq? 'quit
                                         (exception-kind exn))
                              (catch #t
                                     (lambda ()
                                       (log-exception logger
                                                      (make-exception-report (exception-kind
                                                                              exn)
                                                       (exception-args exn)
                                                       #:stack (make-stack #t
                                                                           2)
                                                       #:depth depth)
                                                      #:level level))
                                     (lambda (key . args)
                                       #f)))
                            (raise-exception exn)) thunk))

(define-syntax-rule (with-exception-logging logger body ...)
  "Logs an exception raised by BODY on LOGGER, then re-raises it.

@example
(with-exception-logging log
  (process-request request))
@end example"
  (call-with-exception-log logger
                           (lambda ()
                             body ...)))
