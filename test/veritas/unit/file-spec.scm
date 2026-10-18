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
;; Coverage for the rotating file backend: writing, layout, size based
;; rotation, validation and the SCRIBA_FILE_* configuration read by the auto
;; logger.
;;
;; Veritas only collects assertions that sit directly inside a `test' form,
;; so every check below is a top level form; preparatory work goes in a form
;; whose value is not a procedure. Each test writes to its own file inside
;; one throwaway directory, which keeps the checks independent of each other
;; even though suites run concurrently.

(define-module (veritas unit file-spec)
  #:use-module (veritas veritas)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-13)
  #:use-module (scriba scriba)
  #:use-module (scriba file)
  #:use-module (scriba auto)
  #:export (spec))

(define test-directory
  (mkdtemp "/tmp/scriba-file-spec-XXXXXX"))

(define (test-path name)
  (format #f "~a/~a.log" test-directory name))

(define (backup-of path index)
  (format #f "~a.~a" path index))

(define (file-bytes path)
  (if (file-exists? path)
      (stat:size (stat path)) 0))

(define (read-file path)
  "Returns the whole content of PATH as a string, or the empty string."
  (if (file-exists? path)
      (call-with-input-file path
        get-string-all) ""))

(define (file-lines path)
  "Returns the lines of PATH, without the trailing newline split artifact."
  (let ((content (read-file path)))
    (if (string-null? content)
        '()
        (string-split (string-trim-right content #\newline) #\newline))))

(define (line-count path)
  (length (file-lines path)))

(define (log-lines logger count message)
  "Logs MESSAGE COUNT times with LOGGER."
  (let loop
    ((remaining count))
    (when (positive? remaining)
      (log-info logger message)
      (loop (- remaining 1)))))

(define writing-suite
  (suite "File Backend - Plaintext Writing"
         (test "Creates the file and appends formatted lines"
               (let ((logger (scriba-file-logger #:path (test-path "basic")
                                                 #:layout '(level message))))
                 (log-info logger "hello")
                 (log-warning logger "careful"))
               (assert-true (file-exists? (test-path "basic"))
                            #:name "Logging creates a missing log file")
               (assert-equal #:expect '("[INFO] hello" "[WARNING] careful")
                             #:got (file-lines (test-path "basic"))
                             #:name
                             "Lines are appended in order with their levels"))

         (test "Honors level thresholds"
               (let ((logger (scriba-file-logger #:path (test-path "levels")
                                                 #:level 'error
                                                 #:layout '(level message))))
                 (log-debug logger "hidden debug")
                 (log-info logger "hidden info")
                 (log-warning logger "hidden warning")
                 (log-error logger "kept error"))
               (assert-equal #:expect '("[ERROR] kept error")
                #:got (file-lines (test-path "levels"))
                #:name "Only lines at or above the threshold reach the file"))

         (test "Renders name, tags and context in layout order"
               (let ((logger (scriba-file-logger #:path (test-path "layout")
                                                 #:name 'app
                                                 #:layout '(level name tags
                                                                  message
                                                                  context))))
                 (with-log-tags '(db performance)
                                (with-log-context '((tenant . acme))
                                                  (log-info logger "done"))))
               (assert-equal #:expect
                "[INFO] [app] [#db #performance] done [tenant=acme]"
                #:got (car (file-lines (test-path "layout")))
                #:name
                "The file backend uses the same formatting as the console"))

         (test "Supports leading newline placement"
               (let ((logger (scriba-file-logger #:path (test-path "leading")
                                                 #:line-ending 'leading
                                                 #:layout '(message))))
                 (log-info logger "first")
                 (log-info logger "second"))
               (assert-equal #:expect "\nfirst\nsecond"
                             #:got (read-file (test-path "leading"))
                             #:name
                             "Leading mode prefixes each line with a newline"))

         (test "Leaves a literal message untouched"
               (let ((logger (scriba-file-logger #:path (test-path "literal")
                                                 #:layout '(message))))
                 (log-info logger "100% done ~ not a directive"))
               (assert-equal #:expect "100% done ~ not a directive\n"
                             #:got (read-file (test-path "literal"))
                             #:name
                             "Messages without arguments skip the formatter"))

         (test "Formats messages carrying arguments"
               (let ((logger (scriba-file-logger #:path (test-path "args")
                                                 #:layout '(message))))
                 (log-info logger "2 + 2 = ~a"
                           (+ 2 2)))
               (assert-equal #:expect "2 + 2 = 4\n"
                             #:got (read-file (test-path "args"))
                             #:name
                             "Format arguments are substituted as usual"))

         (test "Appends to a file that already exists"
               (let ((logger (scriba-file-logger #:path (test-path "append")
                                                 #:layout '(message))))
                 (log-info logger "first run"))
               (let ((logger (scriba-file-logger #:path (test-path "append")
                                                 #:layout '(message))))
                 (log-info logger "second run"))
               (assert-equal #:expect '("first run" "second run")
                             #:got (file-lines (test-path "append"))
                             #:name "Logging never truncates an existing file"))))

(define rotation-suite
  (suite "File Backend - Size Based Rotation"
         (test "Rotates before a line would exceed the limit"
               (let ((logger (scriba-file-logger #:path (test-path
                                                         "rotate-basic")
                                                 #:max-size 100
                                                 #:max-backups 2
                                                 #:layout '(message))))
                 (log-lines logger 10 "0123456789"))
               (assert-true (file-exists? (backup-of (test-path "rotate-basic")
                                                     1))
                            #:name
                            "A full file is renamed to its first backup")
               (assert-equal #:expect 9
                #:got (line-count (backup-of (test-path "rotate-basic") 1))
                #:name "The backup holds the lines written before rotation")
               (assert-equal #:expect 1
                             #:got (line-count (test-path "rotate-basic"))
                             #:name
                             "The current file starts over after rotation")
               (assert-true (<= (file-bytes (test-path "rotate-basic")) 100)
                            #:name
                            "The current file stays within the size limit"))

         (test "Keeps only the configured number of backups"
               (let ((logger (scriba-file-logger #:path (test-path
                                                         "rotate-backups")
                                                 #:max-size 30
                                                 #:max-backups 2
                                                 #:layout '(message))))
                 (log-lines logger 20 "0123456789"))
               (assert-true (file-exists? (backup-of (test-path
                                                      "rotate-backups") 1))
                            #:name "The first backup is kept")
               (assert-true (file-exists? (backup-of (test-path
                                                      "rotate-backups") 2))
                            #:name "The second backup is kept")
               (assert-false (file-exists? (backup-of (test-path
                                                       "rotate-backups") 3))
                             #:name "Backups beyond the limit are dropped"))

         (test "Never rotates when max-size is #f"
               (let ((logger (scriba-file-logger #:path (test-path
                                                         "no-rotation")
                                                 #:max-size #f
                                                 #:layout '(message))))
                 (log-lines logger 100 "0123456789"))
               (assert-false (file-exists? (backup-of (test-path "no-rotation")
                                                      1))
                             #:name "No backup file appears")
               (assert-equal #:expect 100
                             #:got (line-count (test-path "no-rotation"))
                             #:name "Every line stays in the one file"))

         (test "max-backups 0 truncates instead of keeping history"
               (let ((logger (scriba-file-logger #:path (test-path
                                                         "no-history")
                                                 #:max-size 40
                                                 #:max-backups 0
                                                 #:layout '(message))))
                 (log-lines logger 20 "0123456789"))
               (assert-false (file-exists? (backup-of (test-path "no-history")
                                                      1))
                             #:name "No backups are left behind")
               (assert-true (<= (file-bytes (test-path "no-history")) 40)
                            #:name
                            "The file is truncated whenever it fills up"))

         (test "rotate-file! moves the current file into its first backup"
               (let ((logger (scriba-file-logger #:path (test-path "manual")
                                                 #:max-size #f
                                                 #:layout '(message))))
                 (log-info logger "one")
                 (log-info logger "two")
                 (rotate-file! (test-path "manual")))
               (assert-equal #:expect '("one" "two")
                             #:got (file-lines (backup-of (test-path "manual")
                                                          1))
                             #:name "The written lines move into the backup")
               (assert-equal #:expect 0
                             #:got (file-bytes (test-path "manual"))
                             #:name "The current file is empty after rotation"))

         (test "rotate-file! reports success"
               (let ((logger (scriba-file-logger #:path (test-path
                                                         "manual-return")
                                                 #:layout '(message))))
                 (log-info logger "hello"))
               (assert-true (rotate-file! (test-path "manual-return"))
                            #:name "rotate-file! returns #t"))

         (test "rotate-file! refuses an unknown path"
               (assert-error (lambda ()
                               (rotate-file! (test-path "never-opened")))
                #:name
                "Rotating a file Scriba does not write needs a logger first"))

         (test "Counts UTF-8 bytes, not characters"
               ;; "héllo" is 5 characters but 6 UTF-8 bytes, so each line
               ;; takes 7 bytes and only five fit before a 40 byte rotation.
               (let ((logger (scriba-file-logger #:path (test-path "multibyte")
                                                 #:max-size 40
                                                 #:max-backups 1
                                                 #:layout '(message))))
                 (log-lines logger 6 "héllo"))
               (assert-equal #:expect 5
                             #:got (line-count (backup-of (test-path
                                                           "multibyte") 1))
                             #:name
                             "The threshold accounts for encoded length"))

         (test "A later logger can change the rotation policy"
               (let ((first (scriba-file-logger #:path (test-path
                                                        "policy-change")
                                                #:max-size #f
                                                #:layout '(message))))
                 (log-info first "one"))
               (let ((second (scriba-file-logger #:path (test-path
                                                         "policy-change")
                                                 #:max-size 20
                                                 #:layout '(message))))
                 (log-info second "0123456789")
                 (log-info second "0123456789"))
               (assert-true (file-exists? (backup-of (test-path
                                                      "policy-change") 1))
                            #:name
                            "Reconfiguring a path updates its shared sink"))))

(define validation-suite
  (suite "File Backend - Argument Validation"
         (test "Requires a path"
               (assert-error (lambda ()
                               (scriba-file-logger))
                             #:name "No path, no file logger")
               (assert-error (lambda ()
                               (scriba-file-logger #:path "   "))
                             #:name "A blank path is rejected"))

         (test "Rejects impossible rotation settings"
               (assert-error (lambda ()
                               (scriba-file-logger #:path (test-path "invalid")
                                                   #:max-size 0))
                             #:name "A zero byte limit is rejected")
               (assert-error (lambda ()
                               (scriba-file-logger #:path (test-path "invalid")
                                                   #:max-size -5))
                             #:name "A negative byte limit is rejected")
               (assert-error (lambda ()
                               (scriba-file-logger #:path (test-path "invalid")
                                                   #:max-size 'ten))
                             #:name "A non-numeric byte limit is rejected")
               (assert-error (lambda ()
                               (scriba-file-logger #:path (test-path "invalid")
                                                   #:max-backups -1))
                             #:name "A negative backup count is rejected")
               (assert-error (lambda ()
                               (scriba-file-logger #:path (test-path "invalid")
                                                   #:max-backups "3"))
                             #:name "A non-numeric backup count is rejected"))

         (test "Honors the default-file-path parameter"
               (parameterize ((default-file-path (test-path "parameter-path")))
                 (let ((logger (scriba-file-logger #:layout '(message))))
                   (log-info logger "from parameter")))
               (assert-equal #:expect '("from parameter")
                             #:got (file-lines (test-path "parameter-path"))
                             #:name
                             "default-file-path supplies the destination"))))

;;; Environment driven configuration

(define env-vars
  '("SCRIBA_LOGGER" "SCRIBA_LOG_LEVEL" "SCRIBA_LOG_LAYOUT" "SCRIBA_FILE_PATH"
    "SCRIBA_FILE_MAX_SIZE" "SCRIBA_FILE_MAX_BACKUPS"))

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

(define (prepare-file-environment! path)
  (for-each unsetenv
            '("SCRIBA_LOG_LEVEL" "SCRIBA_FILE_MAX_SIZE"
              "SCRIBA_FILE_MAX_BACKUPS"))
  (setenv "SCRIBA_LOGGER" "file")
  (setenv "SCRIBA_FILE_PATH" path)
  (setenv "SCRIBA_LOG_LAYOUT" "level,message"))

(define auto-logger-a
  #f)
(define auto-logger-b
  #f)

(define auto-file-suite
  (suite "File Backend - Environment Driven Configuration"
         #:concurrent? #f
         (test "SCRIBA_LOGGER=file writes to SCRIBA_FILE_PATH"
               (with-restored-environment (lambda ()
                                            (prepare-file-environment! (test-path
                                                                        "auto-basic"))
                                            (let ((logger (scriba-auto-logger)))
                                              (log-info logger
                                                        "from environment")
                                              (log-error logger "second"))))
               (assert-equal #:expect '("[INFO] from environment"
                                        "[ERROR] second")
                             #:got (file-lines (test-path "auto-basic"))
                             #:name
                             "The auto logger routes to the file backend"))

         (test "A missing SCRIBA_FILE_PATH is reported"
               (assert-error (lambda ()
                               (with-restored-environment (lambda ()
                                                            (prepare-file-environment!
                                                             (test-path
                                                              "unused"))
                                                            (unsetenv
                                                             "SCRIBA_FILE_PATH")
                                                            (scriba-auto-logger))))
                             #:name "file logging without a path fails loudly"))

         (test "SCRIBA_FILE_MAX_SIZE=off disables rotation"
               (with-restored-environment (lambda ()
                                            (prepare-file-environment! (test-path
                                                                        "auto-off"))
                                            (setenv "SCRIBA_FILE_MAX_SIZE"
                                                    "off")
                                            (log-lines (scriba-auto-logger) 20
                                                       "0123456789")))
               (assert-false (file-exists? (backup-of (test-path "auto-off") 1))
                             #:name "Off really means no backups"))

         (test "SCRIBA_FILE_MAX_SIZE rotates the env configured file"
               (with-restored-environment (lambda ()
                                            (prepare-file-environment! (test-path
                                                                        "auto-rotation"))
                                            (setenv "SCRIBA_FILE_MAX_SIZE"
                                                    "25")
                                            (setenv "SCRIBA_FILE_MAX_BACKUPS"
                                                    "1")
                                            (log-lines (scriba-auto-logger) 4
                                                       "0123456789")))
               (assert-true (file-exists? (backup-of (test-path
                                                      "auto-rotation") 1))
                            #:name "The environment threshold drives rotation"))

         (test "An invalid SCRIBA_FILE_MAX_SIZE is reported"
               (assert-error (lambda ()
                               (with-restored-environment (lambda ()
                                                            (prepare-file-environment!
                                                             (test-path
                                                              "auto-invalid"))
                                                            (setenv
                                                             "SCRIBA_FILE_MAX_SIZE"
                                                             "banana")
                                                            (scriba-auto-logger))))
                             #:name "A malformed size is not silently ignored"))

         (test "Changing SCRIBA_FILE_PATH yields a different logger"
               (with-restored-environment (lambda ()
                                            (prepare-file-environment! (test-path
                                                                        "auto-first"))
                                            (set! auto-logger-a
                                                  (scriba-auto-logger))
                                            (log-info auto-logger-a "first")
                                            (prepare-file-environment! (test-path
                                                                        "auto-second"))
                                            (set! auto-logger-b
                                                  (scriba-auto-logger))
                                            (log-info auto-logger-b "second")))
               (assert-false (eq? auto-logger-a auto-logger-b)
                             #:name "The path is part of the memoization key")
               (assert-equal #:expect '("[INFO] first")
                             #:got (file-lines (test-path "auto-first"))
                             #:name "The old logger keeps writing to its file")
               (assert-equal #:expect '("[INFO] second")
                             #:got (file-lines (test-path "auto-second"))
                             #:name "The new logger writes to the new file"))))

(define (cleanup-test-directory)
  "Removes the files written by this spec and the directory holding them."
  (for-each (lambda (name)
              (unless (member name
                              '("." ".."))
                (delete-file (string-append test-directory "/" name))))
            (scandir test-directory))
  (rmdir test-directory))

(define (spec)
  (veritas-run writing-suite rotation-suite validation-suite auto-file-suite)
  (cleanup-test-directory))
