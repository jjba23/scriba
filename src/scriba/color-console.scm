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
;; This module provides an interactive console logger utilizing ANSI escape
;; sequences to colorize terminal output. Text elements are visually prioritized
;; by log severity levels, while metadata blocks are systematically dimmed.

;;; Code:

(define-module (scriba color-console)
  #:use-module (srfi srfi-1)
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
  (format #f "~a[~a]~a" ansi-dim
          (strftime format-string
                    (localtime (current-time))) ansi-reset))

(define (format-context ctx)
  "Converts an alist context into a bracketed string layout."
  (let ((mdc (map (lambda (pair)
                    (format #f "[~a=~a]"
                            (car pair)
                            (cdr pair))) ctx)))
    (if (nil? ctx) #f
        (string-append ansi-dim
                       (string-join mdc " ") ansi-reset))))

(define (format-tags tags)
  "Converts an list of tags into a bracketed string layout."
  (let ((mdc (map (lambda (x)
                    (format #f "#~a" x)) tags)))
    (if (nil? tags) #f
        (string-append ansi-dim
                       (format #f "[~a]"
                               (string-join mdc " ")) ansi-reset))))

(define (format-name name)
  "Returns the formatted logger name, or an empty string."
  (if name
      (format #f "~a[~a]~a " ansi-dim name ansi-reset) #f))

(define* (%make-scriba-color-console-logger #:key name
                                            (level (default-log-level))
                                            (tag-filter (default-tag-filter))
                                            (timestamp-format (default-time-format))
                                            (out-port (default-out-port))
                                            (flush-port? (default-flush-port?))
                                            (layout (default-log-layout)))
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
               (raw-user-msg (apply format #f msg args))

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

          (format out-port "\n~a" final-output)

          (when flush-port?
            (force-output out-port))))))

  (make-filtered-logger #:name name
                        #:level level
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
  (memoize-logger %make-scriba-color-console-logger))
