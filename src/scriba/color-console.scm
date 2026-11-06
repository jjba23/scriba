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
;; This module provides an interactive console logger utilizing ANSI escape
;; sequences to colorize terminal output. Text elements are visually prioritized
;; by log severity levels, while metadata blocks are systematically dimmed.

;;; Code:

(define-module (scriba color-console)
  #:use-module (srfi srfi-1)
  #:use-module ((scriba layout)
                #:select (format-plain-time display->string))
  #:use-module (scriba scriba)
  #:export (scriba-color-console-logger %make-scriba-color-console-logger))

(define ansi-reset
  "\x1b[0m")
(define ansi-dim
  "\x1b[2m")

(define (level->color lvl)
  "Maps a log level symbol to its corresponding ANSI color escape code."
  (case lvl
    ((trace)
     "\x1b[90m") ;Bright Black / Gray
    ((debug)
     "\x1b[36m") ;Cyan
    ((info)
     "\x1b[34m") ;Blue
    ((success)
     "\x1b[32m") ;Green
    ((warning)
     "\x1b[33m") ;Yellow
    ((error)
     "\x1b[31m") ;Red
    ((critical)
     "\x1b[1;31m") ;Bold Red
    (else "")))
; Default

(define (format-timestamp format-string)
  "Returns a formatted timestamp string."
  (string-append ansi-dim "["
                 (format-plain-time format-string) "]" ansi-reset))

(define (format-context ctx)
  "Converts an alist context into a bracketed string layout."
  (if (null? ctx) #f
      (string-append ansi-dim
                     (string-join (map (lambda (pair)
                                         (string-append "["
                                                        (display->string (car
                                                                          pair))
                                                        "="
                                                        (display->string (cdr
                                                                          pair))
                                                        "]")) ctx) " ")
                     ansi-reset)))

(define (format-tags tags)
  "Converts an list of tags into a bracketed string layout."
  (if (null? tags) #f
      (string-append ansi-dim "["
                     (string-join (map (lambda (x)
                                         (string-append "#"
                                                        (display->string x)))
                                       tags) " ") "]" ansi-reset)))

(define (format-name name)
  "Returns the formatted logger name, or an empty string."
  (if name
      (string-append ansi-dim
                     "["
                     (display->string name)
                     "]"
                     ansi-reset
                     " ") #f))

(define* (%make-scriba-color-console-logger #:key name
                                            (level (default-log-level))
                                            (context-filter (default-context-filter))
                                            (tag-filter (default-tag-filter))
                                            (timestamp-format (default-time-format))
                                            (out-port (default-out-port))
                                            (flush-port? (default-flush-port?))
                                            (layout (default-log-layout))
                                            (line-ending (default-line-ending)))
  (validate-log-layout layout
                       #:fields text-layout-fields)
  (define name-str
    (format-name name))

  (define (make-color-printer lvl)
    (let* ((color (level->color lvl))
           (level-str (cond
                        ((symbol? lvl)
                         (format #f "~a[~a]~a" color
                                 (string-upcase (symbol->string lvl))
                                 ansi-reset))
                        ((boolean? lvl)
                         #f)))
           (host-str (gethostname)))
      (lambda (msg . args)
        (let* ((time-str (if (memq 'time layout)
                             (format-timestamp timestamp-format) #f))
               (ctx-str (format-context (current-log-context)))
               (tags-str (format-tags (current-log-tags)))
               (raw-user-msg (format-log-message msg args))

               (user-msg (string-append color raw-user-msg ansi-reset))

               (parts-alist `((level unquote level-str)
                              (tags unquote tags-str)
                              (time unquote time-str)
                              (host unquote host-str)
                              (name unquote name-str)
                              (context unquote ctx-str)
                              (message unquote user-msg)))

               (ordered-strings (filter-map (lambda (sym)
                                              (assq-ref parts-alist sym))
                                            layout))

               (final-output (string-join ordered-strings " ")))

          (write-log-line out-port final-output line-ending)

          (when flush-port?
            (force-output out-port))))))

  (make-filtered-logger #:name name
                        #:level level
                        #:context-filter context-filter
                        #:tag-filter tag-filter
                        #:log-message-proc (make-color-printer #t)
                        #:log-trace-proc (make-color-printer 'trace)
                        #:log-debug-proc (make-color-printer 'debug)
                        #:log-info-proc (make-color-printer 'info)
                        #:log-success-proc (make-color-printer 'success)
                        #:log-warning-proc (make-color-printer 'warning)
                        #:log-error-proc (make-color-printer 'error)
                        #:log-critical-proc (make-color-printer 'critical)))

(define scriba-color-console-logger
  (memoize-logger %make-scriba-color-console-logger
                  (#:level default-log-level)
                  (#:context-filter default-context-filter)
                  (#:tag-filter default-tag-filter)
                  (#:timestamp-format default-time-format)
                  (#:out-port default-out-port)
                  (#:flush-port? default-flush-port?)
                  (#:layout default-log-layout)
                  (#:line-ending default-line-ending)))
