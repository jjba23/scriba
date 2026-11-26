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

(define-module (veritas unit reconfigure-spec)
  #:use-module (ice-9 threads)
  #:use-module (veritas veritas)
  #:use-module (scriba scriba)
  #:use-module (scriba console)
  #:export (spec))

(define* (console-into port #:key (level 'info))
  "A console logger writing LEVEL to PORT."
  (scriba-console-logger #:out-port port
                         #:layout '(level message)
                         #:level level))

(define (with-swapped logger thunk)
  "Runs THUNK with LOGGER resolved, restoring whatever was resolved before."
  (let ((previous (current-logger)))
    (dynamic-wind (lambda () (set-current-logger! logger))
                  thunk
                  (lambda () (set-current-logger! previous)))))

(define (wait-for-logger target)
  "Waits up to two seconds for TARGET to be the resolved logger."
  (let loop ((remaining 100))
    (cond ((eq? target (current-logger)) #t)
          ((zero? remaining) #f)
          (else (usleep 20000) (loop (- remaining 1))))))

;; The seam is one process-wide value, so these cases must not run at the same
;; time as each other.
(define reconfigure-suite
  (suite "Hot Reconfiguration"
         #:concurrent? #f

         (test "A swap changes the logger the next line resolves"
               (let* ((first-port (open-output-string))
                      (second-port (open-output-string))
                      (first (console-into first-port))
                      (second (console-into second-port)))
                 (with-swapped first
                               (lambda ()
                                 (log-info (current-logger) "before")
                                 (set-current-logger! second)
                                 (log-info (current-logger) "after")))
                 (assert-equal #:expect "[INFO] before\n"
                               #:got (get-output-string first-port)
                               #:name "The line before the swap went to the older logger")
                 (assert-equal #:expect "[INFO] after\n"
                               #:got (get-output-string second-port)
                               #:name "The line after the swap went to the newer logger")))

         (test "A muted level is still a no-op once the logger is resolved"
               (let ((port (open-output-string)))
                 (with-swapped (console-into port #:level 'warning)
                               (lambda ()
                                 (log-debug (current-logger) "muted")
                                 (log-warning (current-logger) "shown")))
                 (assert-equal #:expect "[WARNING] shown\n"
                               #:got (get-output-string port)
                               #:name "Resolving the logger per line does not unmute a filtered level")))

         (test "A signal handler swap reaches the thread that logs"
               (let* ((loud-port (open-output-string))
                      (quiet-port (open-output-string))
                      (quiet (console-into quiet-port #:level 'warning)))
                 (with-swapped (console-into loud-port)
                               (lambda ()
                                 (log-info (current-logger) "before")
                                 (sigaction SIGUSR1
                                            (lambda (signal)
                                              (set-current-logger! quiet)))
                                 (system (format #f "kill -USR1 ~a"
                                                 (getpid)))
                                 (assert-true (wait-for-logger quiet)
                                              #:name
                                              "The handler ran and its swap is visible to this thread")
                                 (log-warning (current-logger) "after")
                                 (sigaction SIGUSR1 SIG_DFL)))
                 (assert-equal #:expect "[INFO] before\n"
                               #:got (get-output-string loud-port)
                               #:name "Nothing after the swap reached the previous logger")
                 (assert-equal #:expect "[WARNING] after\n"
                               #:got (get-output-string quiet-port)
                               #:name "The line after the signal went through the swapped-in logger")))

         (test "A swap refuses anything that is not a logger"
               (assert-error (lambda () (set-current-logger! 'console))
                             #:name
                             "Naming a backend, rather than passing its logger, raises instead of muting everything"))))

(define (spec)
  (veritas-run reconfigure-suite))
