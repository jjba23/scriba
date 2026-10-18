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
;; This module implements a plaintext file logging backend with size based
;; rotation. Log lines keep the same bracketed layout as the console backend,
;; so a file written by Scriba can be tailed, grepped and shipped by log
;; aggregators without any transformation.
;;
;; Rotation happens when the next line would push the file past `#:max-size'
;; bytes. The current file is renamed to `<path>.1', older backups are shifted
;; up, and the oldest backup beyond `#:max-backups' is removed. As a result a
;; file never grows past `#:max-size' unless a single line is larger than it.
;; Setting
;; `#:max-size' to #f disables rotation entirely, and `#:max-backups' to 0
;; keeps no history (the file is simply truncated). `rotate-file!' rotates on
;; demand, for example from a SIGHUP handler or a cron job.
;;
;; One sink, meaning one open port and one rotation policy, is kept per path.
;; Loggers created for the same path share it, which keeps independent
;; loggers (say one for `app' and one for `db') from fighting over rotation
;; and from reopening the file on every message. Rotation is a property of
;; the file rather than of a logger, so when a later logger asks for a
;; different policy on the same path, that policy takes over for everyone.

;;; Code:

(define-module (scriba file)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (scriba scriba)
  #:use-module (scriba layout)
  #:export (scriba-file-logger %make-scriba-file-logger
                               rotate-file!
                               file-sink-open?
                               default-file-path
                               default-file-max-size
                               default-file-max-backups))

(define default-file-path
  (make-parameter #f))

;; Ten mebibytes, a size most log shippers and editors are comfortable with.
(define default-file-max-size
  (make-parameter (* 10 1024 1024)))

(define default-file-max-backups
  (make-parameter 5))

(define-record-type <file-sink>
  (%make-file-sink path port bytes max-size max-backups)
  file-sink?
  (path sink-path)
  (port sink-port set-sink-port!)
  (bytes sink-bytes set-sink-bytes!)
  (max-size sink-max-size set-sink-max-size!)
  (max-backups sink-max-backups set-sink-max-backups!))

;; One writer per path, so rotation has a single owner. Keys are the path
;; strings exactly as the caller passed them.
(define file-sinks
  (make-hash-table))

(define (file-size path)
  "Returns the size of PATH in bytes, or 0 when it does not exist."
  (if (file-exists? path)
      (stat:size (stat path)) 0))

(define (utf8-byte-length str)
  "Returns the number of bytes STR occupies when written as UTF-8."
  (bytevector-length (string->utf8 str)))

(define (validate-rotation-options max-size max-backups)
  "Raises an error unless the rotation options make sense."
  (unless (or (not max-size)
              (and (integer? max-size)
                   (positive? max-size)))
    (error
     "Scriba: #:max-size must be #f (never rotate) or a positive integer number of bytes, got"
     max-size))
  (unless (and (integer? max-backups)
               (>= max-backups 0))
    (error "Scriba: #:max-backups must be a non-negative integer, got"
           max-backups)) #t)

(define (backup-path path index)
  "Returns the path of the INDEX-th rotated backup of PATH."
  (format #f "~a.~a" path index))

(define (delete-if-exists path)
  "Deletes PATH when it is there."
  (when (file-exists? path)
    (delete-file path)))

(define (rename-if-exists source target)
  "Renames SOURCE onto TARGET, replacing TARGET, when SOURCE is there."
  (when (file-exists? source)
    (delete-if-exists target)
    (rename-file source target)))

(define (rotate-files! path max-backups)
  "Shifts the backups of PATH and renames PATH to its first backup.

With zero MAX-BACKUPS the current file is deleted instead, leaving no
history behind."
  (if (zero? max-backups)
      (delete-if-exists path)
      (begin
        ;; Drop the oldest backup, then walk down so each rename has a free
        ;; destination.
        (delete-if-exists (backup-path path max-backups))
        (let loop
          ((index (- max-backups 1)))
          (when (>= index 1)
            (rename-if-exists (backup-path path index)
                              (backup-path path
                                           (+ index 1)))
            (loop (- index 1))))
        (rename-if-exists path
                          (backup-path path 1)))))

(define (open-sink-port! path)
  "Opens PATH for appending, creating it when missing."
  (open-file path "a"))

(define (get-file-sink path max-size max-backups)
  "Returns the sink for PATH, opening it on first use.

The rotation policy belongs to the file, not to a logger. A later logger
for the same path with a different policy therefore adopts that policy for
the shared sink, so reconfiguring a running program never leaves two
writers rotating the same file under each other's feet."
  (let ((existing (hash-ref file-sinks path)))
    (if existing
        (begin
          (set-sink-max-size! existing max-size)
          (set-sink-max-backups! existing max-backups) existing)
        (let ((sink (%make-file-sink path
                                     (open-sink-port! path)
                                     (file-size path) max-size max-backups)))
          (hash-set! file-sinks path sink) sink))))

(define (rotate-sink! sink)
  "Rotates SINK immediately and keeps it usable.

The port is reopened even if a rename fails, so a transient filesystem error
does not silently turn the logger into a black hole."
  (let ((path (sink-path sink)))
    (close-port (sink-port sink))
    (catch #t
           (lambda ()
             (rotate-files! path
                            (sink-max-backups sink)))
           (lambda (key . args)
             (set-sink-port! sink
                             (open-sink-port! path))
             (set-sink-bytes! sink
                              (file-size path))
             (apply throw key args)))
    (set-sink-port! sink
                    (open-sink-port! path))
    (set-sink-bytes! sink 0) sink))

(define (sink-write! sink line line-ending flush-port?)
  "Writes LINE to SINK, rotating first when the line would not fit.

The byte count is tracked from what Scriba writes instead of `stat'-ing the
file for every line, which keeps the hot path free of extra syscalls. A line
that is larger than `#:max-size' on its own is still written to an empty
file, so a single huge message cannot spin the rotation forever."
  (let* ((max-size (sink-max-size sink))
         (line-bytes (+ (utf8-byte-length line) 1))
         (current-bytes (sink-bytes sink)))
    (when (and max-size
               (positive? current-bytes)
               (> (+ current-bytes line-bytes) max-size))
      (rotate-sink! sink))
    (write-log-line (sink-port sink) line line-ending)
    (set-sink-bytes! sink
                     (+ (sink-bytes sink) line-bytes))
    (when flush-port?
      (force-output (sink-port sink)))) sink)

(define (file-sink-open? path)
  "Is a Scriba file sink currently open for PATH?"
  (if (hash-ref file-sinks path) #t #f))

(define (rotate-file! path)
  "Rotates PATH right now, returning #t.

PATH must be a path that a Scriba file logger has already opened; that is
what makes the open port and the rotated files agree on which file is
current."
  (let ((sink (hash-ref file-sinks path)))
    (unless sink
      (error
       "Scriba: no file sink is open for this path, create a file logger first"
       path))
    (rotate-sink! sink) #t))

(define* (%make-scriba-file-logger #:key (path (default-file-path))
                                   name
                                   (level (default-log-level))
                                   (context-filter (default-context-filter))
                                   (tag-filter (default-tag-filter))
                                   (timestamp-format (default-time-format))
                                   (flush-port? (default-flush-port?))
                                   (layout (default-log-layout))
                                   (line-ending (default-line-ending))
                                   (max-size (default-file-max-size))
                                   (max-backups (default-file-max-backups)))
  "Creates a logger appending lines to the file at PATH.

PATH is required (there is no sensible default file) and its parent
directory must already exist. MAX-SIZE and MAX-BACKUPS control rotation, see
the module commentary."
  (unless (and (string? path)
               (not (string-null? (string-trim-both path))))
    (error "Scriba: a file logger needs #:path (or default-file-path)" path))
  (validate-rotation-options max-size max-backups)
  (let* ((sink (get-file-sink path max-size max-backups))
         (printer (make-text-printer #:name name
                                     #:layout layout
                                     #:timestamp-format timestamp-format
                                     #:emit (lambda (line)
                                              (sink-write! sink line
                                                           line-ending
                                                           flush-port?)))))
    (make-filtered-logger #:name name
                          #:level level
                          #:context-filter context-filter
                          #:tag-filter tag-filter
                          #:log-message-proc (printer #t)
                          #:log-trace-proc (printer 'trace)
                          #:log-debug-proc (printer 'debug)
                          #:log-info-proc (printer 'info)
                          #:log-success-proc (printer 'success)
                          #:log-warning-proc (printer 'warning)
                          #:log-error-proc (printer 'error)
                          #:log-critical-proc (printer 'critical))))

(define scriba-file-logger
  (memoize-logger %make-scriba-file-logger
                  (#:path default-file-path)
                  (#:level default-log-level)
                  (#:context-filter default-context-filter)
                  (#:tag-filter default-tag-filter)
                  (#:timestamp-format default-time-format)
                  (#:flush-port? default-flush-port?)
                  (#:layout default-log-layout)
                  (#:line-ending default-line-ending)
                  (#:max-size default-file-max-size)
                  (#:max-backups default-file-max-backups)))
