;;;
;;; Profiler - profiler interface
;;;
;;;   Copyright (c) 2005-2019  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.vm.profiler
  (use srfi-13)
  (use util.match)
  (extend gauche.internal)
  (export profiler-show profiler-get-result
          profiler-show-load-stats with-profiler)
  )
(select-module gauche.vm.profiler)

;;;==========================================================
;;; External API
;;;

;;
;; Returns a portable representation of the current profiler result
;;
(define (profiler-get-result)
  ;; NB: this part depends on the result object of profiler-raw-result,
  ;; which may be changed later.  Keep this in sync with src/prof.c.
  (if-let1 r (profiler-raw-result)
    (hash-table-map r (^(k v) (cons (entry-name k) v)))
    #f))

;;
;; Show the profiler result.
;;
;;  Keyword args:
;;    :results - give a list of results returned by profiler-get-result
;;               If not given, the current result is used.
;;    :sort-by - either one of 'time, 'count, or 'time-per-call
;;    :max-rows - # of rows to be shown.  #f to show everything.
;;
(define (profiler-show :key (results #f) (sort-by 'time) (max-rows 50))
  (if (not results)
    ;; use the current result
    (if-let1 r (profiler-get-result)
      (show-stats r sort-by max-rows)
      (print "No profiling data has been gathered."))
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
      (show-stats (hash-table-map ht cons) sort-by max-rows))))

;; *EXPERIMENTAL*
;; Show the load statistics.
;; Called from the cleanup routine of main.c.  Passed STATS is a list of
;; accumulated load stats info; see load.c for th exact format.
;; This routine should be in sync of it.
(define (profiler-show-load-stats stats)
  (let1 results '() ; [(<filename> . <time>)]
    (let/cc return
      (define (start stats)
        (match stats
          [()  (show-results)]
          [((f . t) . more)
           (receive (_ rest) (cumulate f t 0 more) (start rest))]
          [(_ . more) (start more)] ;; this can't happen, but tolerate
          ))
      ;; cumulate :: String, Integer, Integer, [Stat] -> Integer, [Stat]
      (define (cumulate filename start-time exclude stats)
        (match stats
          [() (show-results)]  ;; premature stats data; we discard current fn.
          [((f . t) . more)
           (receive (time-spent rest) (cumulate f t 0 more)
             (cumulate filename start-time (+ exclude time-spent) rest))]
          [(t . more)
           (set! results (cons (cons filename (- t start-time exclude)) results))
           (values (- t start-time) more)]))
      (define (show-results)
        (print "Load statistics:")
        (print "Time(us)    File")
        (print "--------+-------------------------------------------------------------------")
        (let1 total (fold (^[p sum]
                            (format #t "~8d ~a\n" (cdr p) (car p))
                            (+ sum (cdr p)))
                          0 (sort-by results cdr >))
          (print "--------+-------------------------------------------------------------------")
          (format #t "~8d Total\n" total))
        (return #f))
      (start (reverse stats)))))

;; Convenience API
(define (with-profiler thunk)
  (receive vals (dynamic-wind
                  profiler-start
                  thunk
                  profiler-stop)
    (profiler-show)
    (profiler-reset)
    (apply values vals)))

;;;==========================================================
;;; Internal routines
;;;

;; Show the result in a comprehensive way
(define (show-stats stat sort-by max-rows)
  (let* ([num-samples (fold (^(entry cnt) (+ (cddr entry) cnt)) 0 stat)]
         [sum-time (* num-samples 0.01)]
         [sorter (case sort-by
                   [(time)
                    (^(a b) (or (> (cddr a) (cddr b))
                                (and (= (cddr a) (cddr b))
                                     (> (cadr a) (cadr b)))))]
                   [(count)
                    (^(a b) (or (> (cadr a) (cadr b))
                                (and (= (cadr a) (cadr b))
                                     (> (cddr a) (cddr b)))))]
                   [(time-per-call)
                    (^(a b) (> (/ (cddr a) (cadr a)) (/ (cddr b) (cadr b))))]
                   [else
                    (error "profiler-show: sort-by argument must be either one of time, count, or time-per-call, but got:" sort-by)])]
         [sorted (sort stat sorter)])

    (print "Profiler statistics (total "num-samples" samples, "
           sum-time " seconds)")
    (print "                                                    num    time/    total")
    (print "Name                                                calls  call(ms) samples")
    (print "---------------------------------------------------+------+-------+-----------")
    (dolist [e (if (integer? max-rows) (take* sorted max-rows) sorted)]
      (match-let1 (name ncalls . samples) e
        (format #t "~50a ~7d ~5a ~5d(~3d%)\n"
                name ncalls (time/call samples ncalls) samples
                (if (zero? num-samples)
                  0
                  (exact (round (* 100 (/ samples num-samples))))))))
    ))


;; Get a fixed-decimal notation of time/call (in us)
;; If the time is under 100ms:  ##.####
;; If the time is under 10^6ms: ###.### - ######.
;; Else print as is.
(define (time/call samples ncalls)
  (let1 time (* 10.0 (/ samples ncalls)) ;; in ms
    (receive (frac int) (modf (* time 10000))
      (let1 val (exact (if (>= frac 0.5) (+ int 1) int))
        (receive (q r) (quotient&remainder val 10000)
          (format "~2d.~4,'0d" q r))))))

;; Return a 'printable' notation of sampled code location
(define (entry-name obj)
  (cond
   [(and (procedure? obj) (subr? obj))
    (match (~ obj'info)
      ;; Kludge: case-lambda-dispatcher contains #<closure> in its dispatch
      ;; vector.   It is unprintable, so we omit it.  The format of info
      ;; in case-lambda is not fixed yet; see make-case-lambda-dispatcher
      ;; in intlib.stub, and keep this in sync.
      [('case-lambda-dispatcher min-req vec) `(case-lambda ,min-req)]
      [other other])]
   [(is-a? obj <compiled-code>)
    (~ obj'full-name)]
   [(is-a? obj <generic>)
    `(GF ,(~ obj'name))]
   [(is-a? obj <method>)
    `(METHOD ,(~ obj'generic'name)
             ,(map class-name (~ obj'specializers)))]
   [else (write-to-string obj)]))

