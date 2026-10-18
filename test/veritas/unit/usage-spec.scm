;; scriba

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; scriba is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
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
;; End-to-end coverage of the public logging API: severity thresholds,
;; tag/context filter semantics, layout rendering, JSON and syslog
;; structure, color output, auto-logger routing and line framing.
;;
;; Veritas only collects assertions that sit directly inside a `test' form,
;; so assertions are never nested a level deeper than the test itself; the
;; helpers below make each check self-contained instead.

(define-module (veritas unit usage-spec)
  #:use-module (veritas veritas)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (scriba scriba)
  #:use-module (scriba console)
  #:use-module (scriba color-console)
  #:use-module (scriba json)
  #:use-module (scriba syslog)
  #:use-module (scriba auto)
  #:export (spec))

;;; Helpers

(define* (console-output thunk
                         #:key (level 'info)
                         (layout '(level message))
                         (name #f)
                         (tag-filter '())
                         (context-filter '())
                         (timestamp-format (default-time-format)))
  "Runs THUNK, a procedure of one logger, and returns what it logged.

THUNK receives a console logger writing into a string port, configured with
the given filters, name and layout."
  (let ((port (open-output-string)))
    (thunk (scriba-console-logger #:out-port port
                                  #:level level
                                  #:name name
                                  #:tag-filter tag-filter
                                  #:context-filter context-filter
                                  #:layout layout
                                  #:timestamp-format timestamp-format))
    (get-output-string port)))

(define default-json-layout
  '(level tags
          time
          host
          name
          message
          context))

(define* (json-output thunk
                      #:key (pretty? #f)
                      (level 'info)
                      (layout default-json-layout))
  "Runs THUNK on a JSON logger in a string port and returns the raw output."
  (let ((port (open-output-string)))
    (thunk (scriba-json-logger #:out-port port
                               #:level level
                               #:pretty? pretty?
                               #:layout layout))
    (get-output-string port)))

(define* (json-object thunk
                      #:key (pretty? #f)
                      (level 'info)
                      (layout default-json-layout))
  "Runs THUNK on a JSON logger and returns the parsed log object."
  (json-string->scm (json-output thunk
                                 #:pretty? pretty?
                                 #:level level
                                 #:layout layout)))

(define (field object key)
  "Looks KEY up in a parsed JSON object.

Guile-json hands back object keys as symbols or as strings depending on how
it was loaded, so both are accepted."
  (cond
    ((not (list? object))
     #f)
    ((assq key object)
     => cdr)
    ((assoc (symbol->string key) object)
     => cdr)
    (else #f)))

(define* (color-output thunk
                       #:key (layout '(level message))
                       (timestamp-format (default-time-format)))
  "Runs THUNK on a color console logger in a string port."
  (let ((port (open-output-string)))
    (thunk (scriba-color-console-logger #:out-port port
                                        #:layout layout
                                        #:timestamp-format timestamp-format))
    (get-output-string port)))

(define (log-all-levels logger)
  "Emits one message at every severity, in order."
  (log-trace logger "trace")
  (log-debug logger "debug")
  (log-info logger "info")
  (log-success logger "success")
  (log-warning logger "warning")
  (log-error logger "error")
  (log-critical logger "critical"))

(define (log-hello logger)
  (log-info logger "hello"))

(define (log-two-levels logger)
  (log-info logger "dropped")
  (log-warning logger "kept"))

(define (log-with-tags logger)
  (with-log-tags '(web db)
                 (log-info logger "kept"))
  (with-log-tags '(web)
                 (log-info logger "dropped")))

(define (log-nested-tags logger)
  (with-log-tags '(outer)
                 (with-log-tags '(inner)
                                (log-info logger "nested"))))

(define (log-scoped-context logger)
  (with-log-context '((tenant . acme))
                    (log-info logger "inside"))
  (log-info logger "outside"))

(define (log-flexible-tags logger)
  (with-log-tags '(web)
                 (log-info logger "web"))
  (with-log-tags '(other)
                 (log-info logger "other")))

(define (lines-of string)
  (filter (lambda (line)
            (not (string-null? line)))
          (string-split string #\newline)))

;;; Suites

(define severity-suite
  (suite "Severity Thresholds"
         (test "Severity ordering is consistent"
               (assert-true (is-in-log-level? 'error
                                              'warning)
                            #:name "error passes a warning threshold")
               (assert-false (is-in-log-level? 'info
                                               'warning)
                             #:name "info is dropped by a warning threshold")
               (assert-true (is-in-log-level? 'warning
                                              'warning)
                            #:name "The threshold itself is included")
               (assert-true (is-in-log-level? 'critical
                                              'trace)
                            #:name
                            "critical passes the most permissive threshold")
               (assert-false (is-in-log-level? 'trace
                                               'critical)
                             #:name
                             "trace is dropped by the strictest threshold"))

         (test "A warning threshold keeps warning and above"
               (assert-equal #:expect '("[WARNING] warning" "[ERROR] error"
                                        "[CRITICAL] critical" "unlevelled")
                             #:got (lines-of (console-output (lambda (logger)
                                                               (log-all-levels
                                                                logger)
                                                               (log-message
                                                                logger
                                                                "unlevelled"))
                                                             #:level 'warning))
                             #:name
                             "Exactly the allowed severities reach the port"))

         (test "level #f disables every statement"
               (assert-equal #:expect ""
                             #:got (console-output (lambda (logger)
                                                     (log-all-levels logger)
                                                     (log-message logger
                                                      "unlevelled"))
                                                   #:level #f)
                             #:name "Not even level-less messages are emitted"))

         (test "level #t enables every statement"
               (assert-equal #:expect 8
                             #:got (length (lines-of (console-output (lambda (logger)
                                                                       (log-all-levels
                                                                        logger)
                                                                       (log-message
                                                                        logger
                                                                        "unlevelled"))
                                                                     #:level
                                                                     #t)))
                             #:name
                             "Every severity plus log-message is emitted"))))

(define filtering-suite
  (suite "Tag and Context Filtering"
         (test "Empty filters are wildcards"
               (assert-equal #:expect
                             "[INFO] [#anything] kept [tenant=acme]\n"
                             #:got (console-output (lambda (logger)
                                                     (with-log-tags '(anything)
                                                                    (with-log-context '
                                                                     ((tenant . acme))
                                                                     (log-info
                                                                      logger
                                                                      "kept"))))
                                                   #:layout '(level tags
                                                                    message
                                                                    context))
                             #:name "A logger without filters logs everything"))

         (test "Tag filters match on any intersection"
               (assert-equal #:expect "kept\n"
                             #:got (console-output log-with-tags
                                                   #:layout '(message)
                                                   #:tag-filter '(db))
                             #:name "One matching tag is enough"))

         (test "Tag matching is lenient across types"
               (assert-equal #:expect "kept\n"
                             #:got (console-output (lambda (logger)
                                                     (with-log-tags '(db)
                                                                    (log-info
                                                                     logger
                                                                     "kept")))
                                                   #:layout '(message)
                                                   #:tag-filter '("db"))
                             #:name "A string filter matches a symbol tag"))

         (test "Nested tags accumulate within a scope"
               (assert-equal #:expect "[#inner #outer] nested\n"
                             #:got (console-output log-nested-tags
                                                   #:layout '(tags message))
                             #:name
                             "Inner tags are prepended to the outer ones"))

         (test "Context filters match a single pair"
               (assert-equal #:expect "kept\n"
                             #:got (console-output (lambda (logger)
                                                     (with-log-context '((tenant . acme)
                                                                         (extra . 1))
                                                                       (log-info
                                                                        logger
                                                                        "kept")))
                                                   #:layout '(message)
                                                   #:context-filter '((tenant . acme)))
                             #:name "The other context pairs are irrelevant"))

         (test "Context filters are restored after their scope"
               (assert-equal #:expect "inside\n"
                             #:got (console-output log-scoped-context
                                                   #:layout '(message)
                                                   #:context-filter '((tenant . acme)))
                             #:name
                             "A log outside the scope is filtered again"))

         (test "Context filters compare numbers loosely"
               (assert-equal #:expect "kept\n"
                             #:got (console-output (lambda (logger)
                                                     (with-log-context '((code . "42"))
                                                                       (log-info
                                                                        logger
                                                                        "kept")))
                                                   #:layout '(message)
                                                   #:context-filter '((code . 42)))
                             #:name "42 matches \"42\""))))

(define layout-suite
  (suite "Layout Rendering"
         (test "Fields follow the requested order"
               (assert-equal #:expect (format #f
                                       "[FIXED] [INFO] [app] [#db] ~a hello [tenant=acme]
"
                                       (gethostname))
                             #:got (console-output (lambda (logger)
                                                     (with-log-tags '(db)
                                                                    (with-log-context '
                                                                     ((tenant . acme))
                                                                     (log-info
                                                                      logger
                                                                      "hello"))))
                                                   #:name 'app
                                                   #:timestamp-format "FIXED"
                                                   #:layout '(time level
                                                                   name
                                                                   tags
                                                                   host
                                                                   message
                                                                   context))
                             #:name "Every field lands in layout order"))

         (test "Unlisted fields are omitted"
               (assert-equal #:expect "just this\n"
                #:got (console-output (lambda (logger)
                                        (with-log-tags '(db)
                                                       (log-info logger
                                                                 "just this")))
                                      #:layout '(message))
                #:name "A message-only layout renders the message alone"))

         (test "Timestamp formats are passed through"
               (assert-equal #:expect "[FIXED] hello\n"
                             #:got (console-output log-hello
                                                   #:layout '(time message)
                                                   #:timestamp-format "FIXED")
                             #:name "A literal strftime format is reproduced"))

         (test "Names are bracketed"
               (assert-equal #:expect "[worker] started\n"
                             #:got (console-output (lambda (logger)
                                                     (log-info logger
                                                               "started"))
                                                   #:name 'worker
                                                   #:layout '(name message))
                             #:name "The logger name gets its own bracket"))))

(define json-suite
  (suite "JSON Backend Structure"
         (test "Emits one valid JSON object per line"
               (assert-true (string-suffix? "\n"
                                            (json-output log-hello))
                            #:name "JSON lines end with a newline")
               (assert-equal #:expect 1
                             #:got (string-count (json-output log-hello)
                                                 #\newline)
                             #:name "A compact JSON log is a single line"))

         (test "Stores level, message and host"
               (assert-equal #:expect "INFO"
                             #:got (field (json-object log-hello)
                                          'level)
                             #:name "The level is an uppercase string")
               (assert-equal #:expect "hello"
                             #:got (field (json-object log-hello)
                                          'message)
                             #:name "The message is a JSON string")
               (assert-true (string? (field (json-object log-hello)
                                            'host))
                            #:name "The host is present"))

         (test "Serializes tags as an array"
               (assert-equal #:expect 2
                             #:got (vector-length (field (json-object (lambda 
                                                                              (logger)
                                                                        (with-log-tags '
                                                                         (db
                                                                          performance)
                                                                         (log-info
                                                                          logger
                                                                          "query"))))
                                                         'tags))
                             #:name "Both tags reach the array"))

         (test "Merges context members into the log object"
               (assert-equal #:expect "acme"
                             #:got (field (json-object (lambda (logger)
                                                         (with-log-context '((tenant . acme))
                                                                           (log-info
                                                                            logger
                                                                            "query"))))
                                          'tenant)
                             #:name
                             "Context pairs become top level JSON members"))

         (test "Pretty printing is controlled by #:pretty?"
               (assert-equal #:expect 1
                             #:got (string-count (json-output log-hello
                                                              #:pretty? #f)
                                                 #\newline)
                             #:name "Compact output is one line")
               (assert-true (> (string-count (json-output log-hello
                                                          #:pretty? #t)
                                             #\newline) 1)
                            #:name "Pretty output spans several lines"))

         (test "JSON respects the level threshold"
               (assert-equal #:expect '("kept")
                             #:got (map (lambda (line)
                                          (field (json-string->scm line)
                                                 'message))
                                        (lines-of (json-output (lambda (logger)
                                                                 (log-info
                                                                  logger
                                                                  "dropped")
                                                                 (log-error
                                                                  logger
                                                                  "kept"))
                                                               #:layout '(level
                                                                          message)
                                                               #:level 'error)))
                             #:name "Only the allowed level is serialized"))))

(define syslog-suite
  (suite "Syslog Backend Structure"
         (test "Severities map to RFC 5424 priorities"
               (assert-equal #:expect '("<15>2" "<15>2"
                                        "<14>2"
                                        "<13>2"
                                        "<12>2"
                                        "<11>2"
                                        "<10>2")
                #:got (map (lambda (line)
                             (string-take line 5))
                           (lines-of (let ((port (open-output-string)))
                                       (log-all-levels (scriba-syslog-logger
                                                        #:out-port port
                                                        #:level 'trace
                                                        #:timestamp-format
                                                        "FIXED"
                                                        #:layout '(level
                                                                   message)))
                                       (get-output-string port))))
                #:name "trace/debug/info/success/warning/error/critical"))

         (test "Includes the process id and the given timestamp"
               (assert-equal #:expect (format #f "FIXED ~a hello\n"
                                              (getpid))
                             #:got (let ((port (open-output-string)))
                                     (log-info (scriba-syslog-logger
                                                                     #:out-port
                                                                     port
                                                                     #:timestamp-format
                                                                     "FIXED"
                                                                     #:layout '
                                                                     (time
                                                                      process-id
                                                                      message))
                                               "hello")
                                     (get-output-string port))
                             #:name "Timestamp and pid are rendered"))))

(define color-suite
  (suite "Color Console Backend"
         (test "Severities get distinct ANSI colors"
               (assert-true (and (string-contains (color-output (lambda (logger)
                                                                  (log-info
                                                                   logger
                                                                   "info")
                                                                  (log-critical
                                                                   logger
                                                                   "critical")))
                                                  "\x1b[34m") #t)
                            #:name "INFO is blue")
               (assert-true (and (string-contains (color-output (lambda (logger)
                                                                  (log-critical
                                                                   logger
                                                                   "critical")))
                                                  "\x1b[1;31m") #t)
                            #:name "CRITICAL is bold red")
               (assert-true (and (string-contains (color-output (lambda (logger)
                                                                  (log-info
                                                                   logger
                                                                   "info")))
                                                  "\x1b[0m") #t)
                            #:name "Colors are reset"))

         (test "Timestamps are dimmed"
               (assert-true (and (string-contains (color-output log-hello
                                                                #:layout '(time
                                                                           message)
                                                                #:timestamp-format
                                                                "FIXED")
                                                  "\x1b[2m[FIXED]\x1b[0m") #t)
                            #:name
                            "The timestamp is wrapped in the dim escape"))))

;;; Auto logger

(define env-vars
  '("SCRIBA_LOGGER" "SCRIBA_LOG_LEVEL"
    "SCRIBA_LOG_LAYOUT"
    "SCRIBA_TAG_FILTER"
    "SCRIBA_CONTEXT_FILTER"
    "SCRIBA_JSON_PRETTY"
    "SCRIBA_TIMESTAMP_FORMAT"
    "SCRIBA_LINE_ENDING"))

(define (with-restored-environment thunk)
  "Runs THUNK with the SCRIBA_* variables in env-vars restored afterwards."
  (let ((saved (map (lambda (var)
                      (cons var
                            (getenv var))) env-vars)))
    (dynamic-wind (lambda ()
                    #t) thunk
                  (lambda ()
                    (for-each (lambda (entry)
                                (if (cdr entry)
                                    (setenv (car entry)
                                            (cdr entry))
                                    (unsetenv (car entry)))) saved)))))

(define (prepare-environment!)
  (for-each unsetenv
            '("SCRIBA_LOG_LAYOUT" "SCRIBA_TAG_FILTER" "SCRIBA_CONTEXT_FILTER"
              "SCRIBA_JSON_PRETTY" "SCRIBA_TIMESTAMP_FORMAT"
              "SCRIBA_LINE_ENDING"))
  (setenv "SCRIBA_LOGGER" "console")
  (setenv "SCRIBA_LOG_LEVEL" "info"))
(define* (capture-auto configure! thunk
                       #:key (layout '(level message)))
  "Configures the environment, builds an auto logger writing into a string
port, runs THUNK on it and returns what reached the port.

A LAYOUT of #f leaves the layout to the environment."
  (with-restored-environment (lambda ()
                               (prepare-environment!)
                               (configure!)
                               (let ((port (open-output-string)))
                                 (thunk (if layout
                                            (scriba-auto-logger #:out-port
                                                                port
                                                                #:layout
                                                                layout)
                                            (scriba-auto-logger #:out-port
                                                                port)))
                                 (get-output-string port)))))

(define (configure-json!)
  (setenv "SCRIBA_LOGGER" "json"))

(define (configure-color!)
  (setenv "SCRIBA_LOGGER" "color-console"))

(define (configure-syslog!)
  (setenv "SCRIBA_LOGGER" "syslog"))

(define (configure-unknown!)
  (setenv "SCRIBA_LOGGER" "not-a-backend"))

(define (configure-warning!)
  (setenv "SCRIBA_LOG_LEVEL" "warning"))

(define (configure-tag-filter!)
  (setenv "SCRIBA_TAG_FILTER" " db , ,web "))

(define (configure-pretty!)
  (setenv "SCRIBA_LOGGER" "json")
  (setenv "SCRIBA_JSON_PRETTY" "true"))

(define (configure-layout!)
  (setenv "SCRIBA_LOG_LAYOUT" "level,message,context"))

(define (log-context-hello logger)
  (with-log-context '((tenant . acme))
                    (log-info logger "hello")))

(define auto-suite
  (suite "Auto Logger Routing"
         #:concurrent? #f
         (test "JSON backend is selected by the environment"
               (assert-equal #:expect "INFO"
                             #:got (field (json-string->scm (string-trim-right
                                                             (capture-auto
                                                              configure-json!
                                                              log-hello)
                                                             #\newline))
                                          'level)
                             #:name "SCRIBA_LOGGER=json produces JSON"))

         (test "Auto JSON pretty printing is honored"
               (assert-true (> (string-count (capture-auto configure-pretty!
                                                           log-hello)
                                             #\newline) 1)
                            #:name
                            "SCRIBA_JSON_PRETTY=true reaches the backend"))

         (test "Color console backend is selected by the environment"
               (assert-true (and (string-contains (capture-auto
                                                   configure-color! log-hello)
                                                  "\x1b[") #t)
                            #:name
                            "SCRIBA_LOGGER=color-console emits ANSI escapes"))

         (test "Syslog backend is selected by the environment"
               (assert-true (string-prefix? "<"
                                            (capture-auto configure-syslog!
                                                          log-hello))
                            #:name
                            "SCRIBA_LOGGER=syslog emits an RFC 5424 header"))

         (test "Unknown backends fall back to the console"
               (assert-equal #:expect "[INFO] hello\n"
                #:got (capture-auto configure-unknown! log-hello)
                #:name "A typo does not leave the program without a logger"))

         (test "Environment level filters the stream"
               (assert-equal #:expect "[WARNING] kept\n"
                             #:got (capture-auto configure-warning!
                                                 log-two-levels)
                             #:name "SCRIBA_LOG_LEVEL is enforced"))

         (test "Tag filters are trimmed and empty entries dropped"
               (assert-equal #:expect "[INFO] web\n"
                             #:got (capture-auto configure-tag-filter!
                                                 log-flexible-tags)
                             #:name "Only the matching tag stream survives"))

         (test "SCRIBA_LOG_LAYOUT is honored"
               (assert-equal #:expect "[INFO] hello [tenant=acme]\n"
                             #:got (capture-auto configure-layout!
                                                 log-context-hello
                                                 #:layout #f)
                             #:name
                             "Comma separated fields drive the rendered line"))

         (test "A space separated SCRIBA_LOG_LAYOUT is rejected"
               (assert-error (lambda ()
                               (capture-auto (lambda ()
                                               (setenv "SCRIBA_LOG_LAYOUT"
                                                       "level message"))
                                             log-hello
                                             #:layout #f))
                #:name "Fields are comma separated, so a typo fails loudly"))))

(define memoization-suite
  (suite "Auto Logger Memoization"
         #:concurrent? #f
         (test "Stable environments reuse one logger"
               (assert-true (with-restored-environment (lambda ()
                                                         (prepare-environment!)
                                                         (eq? (scriba-auto-logger)
                                                              (scriba-auto-logger))))
                            #:name "The same configuration is cached"))

         (test "Changing a setting produces a new logger"
               (assert-false (with-restored-environment (lambda ()
                                                          (prepare-environment!)
                                                          (let ((info-logger (scriba-auto-logger)))
                                                            (setenv
                                                             "SCRIBA_LOG_LEVEL"
                                                             "debug")
                                                            (eq? info-logger
                                                                 (scriba-auto-logger)))))
                             #:name "The level is part of the cache key"))))

(define framing-suite
  (suite "Framing and Message Formatting"
         (test "Messages without arguments are literal"
               (assert-equal #:expect "100% done ~ here"
                             #:got (format-log-message "100% done ~ here"
                                                       '())
                             #:name "No stray format interpretation"))

         (test "Messages with arguments are formatted"
               (assert-equal #:expect "2 + 2 = 4"
                             #:got (format-log-message "2 + 2 = ~a"
                                                       '(4))
                             #:name "Format arguments are applied"))

         (test "Non-string messages are displayed"
               (assert-equal #:expect "42"
                             #:got (format-log-message 42
                                                       '())
                             #:name
                             "Objects are printed with display semantics"))

         (test "write-log-line supports both newline placements"
               (assert-equal #:expect "line\n"
                             #:got (let ((port (open-output-string)))
                                     (write-log-line port "line"
                                                     'trailing)
                                     (get-output-string port))
                             #:name "Trailing mode appends the newline")
               (assert-equal #:expect "\nline"
                             #:got (let ((port (open-output-string)))
                                     (write-log-line port "line"
                                                     'leading)
                                     (get-output-string port))
                             #:name "Leading mode prepends it"))

         (test "Unknown newline placements are rejected"
               (assert-error (lambda ()
                               (write-log-line (open-output-string) "x"
                                               'middle))
                             #:name "An invalid line-ending raises"))

         (test "The default-line-ending parameter is honored"
               (assert-equal #:expect "\nhello"
                             #:got (let ((port (open-output-string)))
                                     (parameterize ((default-line-ending 'leading))
                                       (let ((logger (scriba-console-logger
                                                      #:out-port port
                                                      #:layout '(message))))
                                         (log-info logger "hello")))
                                     (get-output-string port))
                             #:name
                             "New loggers pick up the parameterized default"))))

(define (spec)
  (veritas-run severity-suite
               filtering-suite
               layout-suite
               json-suite
               syslog-suite
               color-suite
               auto-suite
               memoization-suite
               framing-suite))
