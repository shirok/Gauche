;;;
;;; Profiler - profiler interface
;;;  
;;;   Copyright (c) 2005-2007  Shiro Kawai  <shiro@acm.org>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  
;;;  $Id: profiler.scm,v 1.7 2007-03-02 07:39:10 shirok Exp $
;;;

(define-module gauche.vm.profiler
  (use srfi-1)
  (use srfi-13)
  (use util.list)
  (use util.match)
  (extend gauche.internal)
  (export profiler-show profiler-get-result
          profiler-show-load-stats)
  )
(select-module gauche.vm.profiler)

;;;==========================================================
;;; External API
;;;

;;
;; Returns a portable representation of the current profiler result
;;
(define (profiler-get-result)
  (cond
   ;; NB: this part depends on the result object of profiler-raw-result,
   ;; which may be changed later.  Keep this in sync with src/prof.c.
   ((profiler-raw-result)
    => (cut hash-table-map <>
            (lambda (k v) (cons (entry-name k) v))))
   (else #f)))

;;
;; Show the profiler result.
;;
;;  Keyword args:
;;    :results - give a list of results returned by profiler-get-result
;;               If not given, the current result is used.
;;    :sort-by - either one of 'time, 'count, or 'time-per-call
;;    :max-rows - # of rows to be shown.  #f to show everything.
;;
(define (profiler-show . opts)
  (let-keywords opts ((results #f)
                      (sort-by 'time)
                      (max-rows 50))
    (if (not results)
      ;; use the current result
      (cond
       ((profiler-get-result) => (cut show-stats <> sort-by max-rows))
       (else (print "No profiling data has been gathered.")))
      ;; gather all the results
      (let1 ht (make-hash-table 'equal?)
        ;; gather stats
        (dolist (r results)
          (dolist (e r)
            (let1 p (hash-table-get ht (car e) '(0 . 0))
              (hash-table-put! ht (car e)
                               (cons (+ (cadr e) (car p))
                                     (+ (cddr e) (cdr p)))))))
        ;; show 'em.
        (show-stats (hash-table-map ht cons) sort-by max-rows)))))

;; *EXPERIMENTAL*
;; Show the load statistics.
;; Called from the cleanup routine of main.c.  Passed STATS is a list of
;; accumulated load stats info; see load.c for th exact format.
;; This routine should be in sync of it.
(define (profiler-show-load-stats stats)
  (let ((results '())) ;; [(<filename> . <time>)]
    (let/cc return
      (define (start stats)
        (match stats
          (()  (show-results))
          (((f . t) . more)
           (receive (_ rest) (cumulate f t more) (start rest)))
          ((_ . more) (start more)) ;; this can't happen, but tolerate
          ))
      ;; cumulate :: String, Integer, [Stat] -> Integer, [Stat]
      (define (cumulate filename start-time stats)
        (match stats
          (() (show-results))  ;; premature stats data; we discard current fn.
          (((f . t) . more)
           (receive (time-spent rest) (cumulate f t more)
             (cumulate filename (+ start-time time-spent) rest)))
          ((t . more)
           (set! results
                 (cons (cons filename (- t start-time)) results))
           (values (- t start-time) more))))
      (define (show-results)
        (print "Load statistics:")
        (print "Time(us)    File")
        (print "--------+-------------------------------------------------------------------")
        (for-each (lambda (p)
                    (format #t "~8d ~a\n" (cdr p) (car p)))
                  (sort results (lambda (a b) (> (cdr a) (cdr b)))))
        (return #f))
      (start (reverse stats)))))

;;;==========================================================
;;; Internal routines
;;;

;; Show the result in a comprehensive way
(define (show-stats stat sort-by max-rows)
  (let* ((num-samples (fold (lambda (entry cnt) (+ (cddr entry) cnt)) 0 stat))
         (sum-time (* num-samples 0.01))
         (sorter (case sort-by
                   ((time)
                    (lambda (a b)
                      (or (> (cddr a) (cddr b))
                          (and (= (cddr a) (cddr b))
                               (> (cadr a) (cadr b))))))
                   ((count)
                    (lambda (a b)
                      (or (> (cadr a) (cadr b))
                          (and (= (cadr a) (cadr b))
                               (> (cddr a) (cddr b))))))
                   ((time-per-call)
                    (lambda (a b)
                      (> (/ (cddr a) (cadr a)) (/ (cddr b) (cadr b)))))
                   (else
                    (error "profiler-show: sort-by argument must be either one of time, count, or time-per-call, but got:" sort-by))))
         (sorted (sort stat sorter)))

    (print "Profiler statistics (total "num-samples" samples, "
           sum-time " seconds)")
    (print "                                                    num    time/    total")
    (print "Name                                                calls  call(ms) samples")
    (print "---------------------------------------------------+------+-------+-----------")
    (for-each
     (lambda (e)
       (let* ((name (car e))
              (samples (cddr e))
              (ncalls  (cadr e))
              )
         (format #t "~50a ~7d ~5a ~5d(~3d%)\n"
                 name ncalls (time/call samples ncalls) samples
                 (if (zero? num-samples)
                   0
                   (inexact->exact (round (* 100 (/ samples num-samples))))))))
     (if (integer? max-rows)
       (take* sorted max-rows)
       sorted)))
  )

;; Get a fixed-decimal notation of time/call (in us)
;; If the time is under 100ms:  ##.####
;; If the time is under 10^6ms: ###.### - ######.
;; Else print as is.
(define (time/call samples ncalls)
  (let1 time (* 10.0 (/ samples ncalls)) ;; in ms
    (receive (frac int) (modf (* time 10000))
      (let1 val (inexact->exact (if (>= frac 0.5) (+ int 1) int))
        (receive (q r) (quotient&remainder val 10000)
          (format "~2d.~4,'0d" q r))))))

;; Return a 'printable' notation of sampled code location
(define (entry-name obj)
  (cond
   ((and (procedure? obj) (subr? obj))
    (ref obj 'info))
   ((is-a? obj <compiled-code>)
    (ref obj 'full-name))
   ((is-a? obj <generic>)
    `(GF ,(ref obj 'name)))
   ((is-a? obj <method>)
    `(METHOD ,(ref (ref obj 'generic) 'name)
             ,(map class-name (ref obj 'specializers))))
   (else (write-to-string obj))))

(provide "gauche/vm/profiler")
