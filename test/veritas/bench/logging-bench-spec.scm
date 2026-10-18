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
;; Benchmarks for the cost of a single log statement, written with Veritas'
;; `benchmark' form so the project has one runner instead of two.
;;
;;   maak bench
;;
;; or directly, from the project root:
;;
;;   VERITAS_TEST_TYPE=bench guile -L ./src -L ./test \
;;     -c '((@ (veritas runner) run-tests))'
;;
;; Veritas measures with Guile's `statprof', so each run reports a profile
;; rather than a single number. Benchmarks run in their own pass, one at a
;; time, instead of alongside the test fibers.
;;
;; Each benchmark repeats its operation a fixed, explicit number of times (the
;; count is in the benchmark name, so it appears in the report). Dividing the
;; reported total by that count gives the cost per call; the profile next to it
;; says where the time went. The most important number is the muted one: a
;; program that leaves `log-debug' calls in a hot loop should pay almost
;; nothing for them, and the no-op benchmark at the same call count is the
;; baseline that "almost nothing" is measured against.
;;
;; Every destination is /dev/null so the measurement is dominated by Scriba
;; rather than by disk or terminal speed.

(define-module (veritas bench logging-bench-spec)
  #:use-module (veritas veritas)
  #:use-module (scriba scriba)
  #:use-module (scriba console)
  #:use-module (scriba json)
  #:use-module (scriba file)
  #:export (spec))

;;; Helpers

(define (repeat times thunk)
  "Calls THUNK TIMES times."
  (let loop
    ((remaining times))
    (when (positive? remaining)
      (thunk)
      (loop (- remaining 1)))))

;; A single file descriptor that swallows everything, so the I/O cost stays
;; constant instead of growing with the benchmark.
(define sink-port
  (open-file "/dev/null" "a"))

(define console
  (scriba-console-logger #:out-port sink-port))

(define console-no-flush
  (scriba-console-logger #:out-port sink-port
                         #:flush-port? #f))

(define json-logger
  (scriba-json-logger #:out-port sink-port))

(define file-logger
  (scriba-file-logger #:path "/dev/null"
                      #:max-size #f))

(define muted-debug
  (scriba-console-logger #:out-port sink-port
                         #:level 'info))

(define disabled
  (scriba-console-logger #:out-port sink-port
                         #:level #f))

(define tag-miss
  (scriba-console-logger #:out-port sink-port
                         #:tag-filter '(db)))

(define context-miss
  (scriba-console-logger #:out-port sink-port
                         #:context-filter '((tenant . acme))))

;;; Benchmarks

;; The baseline: what the loop itself costs, so the muted numbers below can be
;; read as "this much more than doing nothing". Same call count as the muted
;; benchmarks, so the difference is directly meaningful.
(define no-op-benchmark
  (benchmark #:name "no-op thunk, 4000000 calls"
             #:runs 3
             #:thunk (lambda ()
                       (repeat 4000000
                               (lambda ()
                                 #t)))))

(define muted-level-benchmark
  (benchmark #:name "muted log-debug (below the level), 4000000 calls"
             #:runs 3
             #:thunk (lambda ()
                       (repeat 4000000
                               (lambda ()
                                 (log-debug muted-debug "hidden ~a" 1))))))

(define disabled-level-benchmark
  (benchmark #:name "muted log-info (level #f), 4000000 calls"
             #:runs 3
             #:thunk (lambda ()
                       (repeat 4000000
                               (lambda ()
                                 (log-info disabled "hidden ~a" 1))))))

(define tag-miss-benchmark
  (benchmark #:name "tag filter miss (log-info), 2000000 calls"
             #:runs 3
             #:thunk (lambda ()
                       (repeat 2000000
                               (lambda ()
                                 (log-info tag-miss "filtered out ~a" 1))))))

(define context-miss-benchmark
  (benchmark #:name "context filter miss (log-info), 2000000 calls"
             #:runs 3
             #:thunk (lambda ()
                       (repeat 2000000
                               (lambda ()
                                 (log-info context-miss "filtered out ~a" 1))))))

(define console-literal-benchmark
  (benchmark #:name "console INFO, literal message, 100000 calls"
             #:runs 3
             #:thunk (lambda ()
                       (repeat 100000
                               (lambda ()
                                 (log-info console "health check ok"))))))

(define console-args-benchmark
  (benchmark #:name "console INFO with 2 format arguments, 20000 calls"
             #:runs 3
             #:thunk (lambda ()
                       (repeat 20000
                               (lambda ()
                                 (log-info console "request ~a took ~a ms" 42
                                           7))))))

(define console-no-flush-benchmark
  (benchmark #:name "console INFO with flushing disabled, 20000 calls"
             #:runs 3
             #:thunk (lambda ()
                       (repeat 20000
                               (lambda ()
                                 (log-info console-no-flush
                                           "request ~a took ~a ms" 42 7))))))

(define json-benchmark
  (benchmark #:name "JSON INFO -> /dev/null, 12000 calls"
             #:runs 3
             #:thunk (lambda ()
                       (repeat 12000
                               (lambda ()
                                 (log-info json-logger "request ~a took ~a ms"
                                           42 7))))))

(define file-benchmark
  (benchmark #:name "file INFO -> /dev/null, 20000 calls"
             #:runs 3
             #:thunk (lambda ()
                       (repeat 20000
                               (lambda ()
                                 (log-info file-logger "request ~a took ~a ms"
                                           42 7))))))

(define tags-context-benchmark
  (benchmark #:name "console INFO with tags and context, 12000 calls"
             #:runs 3
             #:thunk (lambda ()
                       (repeat 12000
                               (lambda ()
                                 (with-log-tags '(performance db)
                                                (with-log-context '((tenant . acme)
                                                                    (request-id . "abc123"))
                                                                  (log-info
                                                                   console
                                                                   "request ~a took ~a ms"
                                                                   42 7))))))))

(define memoized-lookup-benchmark
  (benchmark #:name "memoized logger lookup, 150000 calls"
             #:runs 3
             #:thunk (lambda ()
                       (repeat 150000
                               (lambda ()
                                 (scriba-console-logger #:out-port sink-port
                                                        #:level 'info))))))

(define raw-construction-benchmark
  (benchmark #:name "raw logger construction, 8000 calls"
             #:runs 3
             #:thunk (lambda ()
                       (repeat 8000
                               (lambda ()
                                 (%make-scriba-console-logger #:out-port
                                                              sink-port
                                                              #:level 'info))))))

(define (spec)
  (veritas-run #:shuffle? #f
               no-op-benchmark
               muted-level-benchmark
               disabled-level-benchmark
               tag-miss-benchmark
               context-miss-benchmark
               console-literal-benchmark
               console-args-benchmark
               console-no-flush-benchmark
               json-benchmark
               file-benchmark
               tags-context-benchmark
               memoized-lookup-benchmark
               raw-construction-benchmark))
