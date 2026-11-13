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
;; The Scriba half of the cross-language comparison in docs/comparisons.org,
;; written so that its numbers can be read next to python-bench.py and
;; node-bench.cjs: the same message, the same call counts and the same
;; destination, /dev/null, so the measurement is the logger and not the disk.
;;
;;   guix shell -m manifest.scm -- guile -L ./src benchmarks/compare/scriba-bench.scm
;;
;; Each workload runs three times and the median is reported, in nanosecond per
;; call, which is the unit the other two programs report. The logger keeps its
;; default layout, so the line carries a level, a cached timestamp and the
;; message, as the other three programs' lines carry theirs.

(define-module (scriba-bench)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (scriba scriba)
  #:use-module (scriba console)
  #:use-module (scriba json))

(define (now)
  "Returns a monotonic-ish wall clock in seconds."
  (match (gettimeofday)
    ((seconds . microseconds) (+ seconds
                                 (/ microseconds 1000000.0)))))

(define (repeat times thunk)
  (let loop
    ((remaining times))
    (when (positive? remaining)
      (thunk)
      (loop (- remaining 1)))))

(define* (report name calls thunk
                 #:key (runs 3))
  "Runs THUNK CALLS times, RUNS times, and prints the median cost per call."
  (let* ((seconds (map (lambda (run)
                         (let ((start (now)))
                           (repeat calls thunk)
                           (- (now) start)))
                       (iota runs)))
         (median (list-ref (sort seconds <)
                           (quotient runs 2))))
    (format #t
            "scriba\t~a\t~a\t~,3f\t~,1f~%"
            name
            calls
            median
            (/ (* median 1.0e9) calls))))

(define sink
  (open-file "/dev/null" "a"))

(define console
  (scriba-console-logger #:out-port sink
                         #:level 'info))

(define json-logger
  (scriba-json-logger #:out-port sink
                      #:level 'info))

(report "text literal" 200000
        (lambda ()
          (log-info console "user 42 logged in")))

(report "text 2 args" 100000
        (lambda ()
          (log-info console "user ~a took ~a ms" 42 7)))

(report "json literal" 100000
        (lambda ()
          (log-info json-logger "user 42 logged in")))

(report "muted debug" 4000000
        (lambda ()
          (log-debug console "user ~a took ~a ms" 42 7)))
