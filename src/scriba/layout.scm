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
;; This module holds the plaintext layout shared by Scriba's text backends
;; (the console and the file logger). Fields are formatted as bracketed
;; blocks and joined in the order requested by the logger's `#:layout`, so a
;; backend only has to decide where the finished line goes.
;;
;; Timestamps are cached per format string and per wall-clock second.
;; Formatting a time is by far the most expensive part of a log line (it
;; consults the timezone database), while a second-resolution format cannot
;; change within the same second, so the cache removes that cost from every
;; line but the first one of each second.

;;; Code:

(define-module (scriba layout)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (scriba scriba)
  #:export (format-timestamp format-plain-time
                             format-context
                             format-tags
                             format-name
                             display->string
                             make-text-printer))

(define (display->string value)
  "Returns VALUE rendered the way `display' would render it.

Strings are returned untouched, so the common case allocates nothing."
  (if (string? value) value
      (call-with-output-string (lambda (port)
                                 (display value port)))))

;; Maps a strftime format string to `(second . formatted-string)'.
(define timestamp-cache
  (make-hash-table))

(define (current-second)
  "Returns the current wall-clock time as whole seconds.

Guile releases disagree on how many values @code{current-time} returns, so
this accepts a single integer, a `(seconds . nanoseconds)' pair, or the two
values form."
  (call-with-values current-time
                    (lambda values
                      (let ((first (car values)))
                        (if (pair? first)
                            (car first) first)))))

(define (format-plain-time format-string)
  "Returns the current time formatted with FORMAT-STRING, without brackets.

The result is reused for every log line within the same second, because
@code{strftime} and @code{localtime} are much more expensive than the rest of
a log line and a second-resolution format cannot change faster than that."
  (let* ((second (current-second))
         (cached (hash-ref timestamp-cache format-string)))
    (if (and cached
             (= (car cached) second))
        (cdr cached)
        (let ((formatted (strftime format-string
                                   (localtime second))))
          (hash-set! timestamp-cache format-string
                     (cons second formatted)) formatted))))

(define (format-timestamp format-string)
  "Returns a formatted, bracketed timestamp string."
  (string-append "["
                 (format-plain-time format-string) "]"))

(define (format-context ctx)
  "Converts an alist context into a bracketed string layout: [k=v] [k=v]."
  (if (null? ctx) #f
      (string-join (map (lambda (pair)
                          (string-append "["
                                         (display->string (car pair)) "="
                                         (display->string (cdr pair)) "]"))
                        ctx) " ")))

(define (format-tags tags)
  "Converts an list of tags into a bracketed string layout: [#a #b #c]."
  (if (null? tags) #f
      (string-append "["
                     (string-join (map (lambda (x)
                                         (string-append "#"
                                                        (display->string x)))
                                       tags) " ") "]")))

(define (format-name name)
  "Returns the formatted logger name, or an empty string."
  (if name
      (string-append "["
                     (display->string name) "]") #f))

(define* (make-text-printer #:key name layout timestamp-format emit)
  "Returns a procedure that turns a log level into a logging procedure.

The returned procedure maps a level symbol (or #t for level-less messages)
to a `(lambda (msg . args) ...)' procedure. When called, that procedure
formats MSG with ARGS, builds the configured LAYOUT and hands the finished
line, without any newline, to EMIT. Backends use EMIT to choose their
destination, newline placement and flushing behaviour."
  (validate-log-layout layout
                       #:fields text-layout-fields)
  (let ((name-str (format-name name))
        (host-str (gethostname))
        (with-time? (if (memq 'time layout) #t #f)))
    (lambda (lvl)
      (let ((level-str (cond
                         ((symbol? lvl)
                          (format #f "[~a]"
                                  (string-upcase (symbol->string lvl))))
                         ((boolean? lvl)
                          #f))))
        (lambda (msg . args)
          (let* ((time-str (if with-time?
                               (format-timestamp timestamp-format) #f))
                 (ctx-str (format-context (current-log-context)))
                 (tags-str (format-tags (current-log-tags)))
                 (user-msg (format-log-message msg args))
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
            (emit final-output)))))))
