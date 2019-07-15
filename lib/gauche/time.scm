;;;
;;; gauche/time.scm - time the procedure
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.time
  (use srfi-11)
  (use gauche.parameter)
  (use gauche.record)
  (use util.match)
  (export time time-this time-these report-time-results time-these/report
          <time-result> time-result+ time-result-
          time-result-real time-result-user time-result-sys
          <time-counter> <real-time-counter> <user-time-counter>
          <system-time-counter> <process-time-counter>
          time-counter-start! time-counter-stop! time-counter-reset!
          time-counter-value with-time-counter
          time-counter-get-current-time time-counter-get-delta)
  )
(select-module gauche.time)

;; Time, a simple measurement ------------------------------

;; TODO: Drop these once we support sane formatting of flonums in format.
(define (format-flonum val mincol digs)
  (if (finite? val)
    (let* ([scale (expt 10 digs)]
           [n (round->exact (* val scale))])
      (format "~vd.~v,'0d" mincol (div n scale) digs (mod n scale)))
    (format "~a" val)))

(define (format-delta-time delta) (format-flonum delta 3 3))

;; common stuff for 'time' macro and benchmarking
(define-syntax %with-times
  (syntax-rules ()
    [(_ expr exprs do-result)
     (begin
       (gc)
       (let*-values ([(stimes) (sys-times)]
                     [(sreal-sec sreal-msec) (sys-gettimeofday)]
                     [r (begin expr . exprs)]
                     [(etimes) (sys-times)]
                     [(ereal-sec ereal-msec) (sys-gettimeofday)])
         (let ([real (- (+ ereal-sec (/. ereal-msec 1000000))
                        (+ sreal-sec (/. sreal-msec 1000000)))]
               [user (/. (- (list-ref etimes 0) (list-ref stimes 0))
                         (list-ref stimes 4))]
               [sys  (/. (- (list-ref etimes 1) (list-ref stimes 1))
                         (list-ref stimes 4))])
           (do-result r real user sys))))]))

(define-syntax time
  (syntax-rules ()
    [(_ expr . exprs)
     (%with-times expr exprs
                  (^(r real user sys)
                    (format (current-error-port)
                            ";~,,,,75:s\n; real ~a\n; user ~a\n; sys  ~a\n"
                            '(time expr . exprs)
                            (format-delta-time real)
                            (format-delta-time user)
                            (format-delta-time sys))
                    (apply values r)))]
    [(_)
     (syntax-error "usage: (time expr expr2 ...); or you meant sys-time?")]))

;; Benchmarking ---------------------------------------

(define-record-type <time-result> make-time-result time-result?
  (count time-result-count)
  (real  time-result-real)
  (user  time-result-user)
  (sys   time-result-sys))

(define-method write-object ((obj <time-result>) port)
  (format port "#<time-result ~a times/~a real/~a user/~a sys>"
          (~ obj'count)
          (format-delta-time (~ obj'real))
          (format-delta-time (~ obj'user))
          (format-delta-time (~ obj'sys))))

(define (%time-result-op op t1 t2 with-count)
  (make-time-result (if with-count
                      (op (time-result-count t1) (time-result-count t2))
                      (time-result-count t1))
                    (op (time-result-real t1) (time-result-real t2))
                    (op (time-result-user t1) (time-result-user t2))
                    (op (time-result-sys  t1) (time-result-sys  t2))))

(define (time-result+ t1 t2 :key (with-count #f))
  (%time-result-op + t1 t2 with-count))

(define (time-result- t1 t2 :key (with-count #f))
  (%time-result-op - t1 t2 with-count))

;; one sample.
;; config := <integer>     ; number of repetition
;;        |  (cpu <real>)  ; at least <real> seconds in cpu time
(define (time-this config thunk)
  (define (with-times count thunk)
    (%with-times (thunk) ()
                 (^(_ real user sys) (make-time-result count real user sys))))
  (define (run count)
    (let* ([body (with-times count (^() (dotimes [n count] (thunk))))]
           [skin (with-times count (^() (dotimes [n count] #f)))])
      (time-result- body skin)))
  (define (cputime result)
    (+ (time-result-user result) (time-result-sys result)))

  ;; Determine the unit repetition count which takes at least 0.3 cpu secs.
  (define (estimate rep)
    (let* ([result (run rep)]
           [t      (cputime result)])
      (cond [(< t 0.01) (estimate (* rep 50))]
            [(< t 0.05) (estimate (* rep 10))]
            [else (ceiling->exact (* (/. rep t) 0.3))])))

  (match config
    [(? integer?) (run config)]
    [('cpu (? real? secs))
     (when (< secs 1.0)
       (error "Benchmark duration (cpu time) should be greater than 1.0 for \
               reliable measurement.  We got:" config))
     (let1 unit (estimate 1)
       (let loop ([cumu 0] [r #f])
         (if (>= cumu secs)
           r
           (let1 r1 (run unit)
             (loop (+ cumu (cputime r1))
                   (if r
                     (time-result+ r r1 :with-count #t)
                     r1))))))]))

;; multiple samples.
;; samples : ((key . thunk) ...)
;; returns : (config (key . result) ...)
(define (time-these config samples)
  (cons config
        (map (^s (cons (car s) (time-this config (cdr s)))) samples)))

(define (time-these/report config samples)
  (report-time-results (time-these config samples)))

;; Show the result of time-these nicely.
(define (report-time-results result)
  ;; returns list of formatted strings and maximum width
  (define (fmt+w formatter vals)
    (let1 fs (map formatter vals)
      (values fs (apply max (map string-length fs)))))
  (define (ff val dig) (format-flonum val 0 dig))
  (define (realtime result) (~ result'real))
  (define (usertime result) (~ result'user))
  (define (systime result)  (~ result'sys))
  (define (cputime result)  (+ (usertime result) (systime result)))
  (define (rate result)     (/. (~ result'count) (cputime result)))
  ;; compare rates.  x, y :: <time-result>
  (define (ratio x y) (ff (/ (rate x) (rate y)) 3))
  ;; show the report
  (define (show)
    (match-let1 (config . alist) result
      ;; header
      (format #t "Benchmark: ran ~a, each for ~a.\n"
              (string-join (map (.$ x->string car) alist) ", ")
              (match config
                [('cpu secs) (format "at least ~a cpu seconds" secs)]
                [count       (format "~a times" count)]))
      ;; individual results
      (let*-values
          ([(ts) (map cdr alist)] ;<time-result>s
           [(ks kw) (fmt+w (.$ x->string car) alist)]    ;key
           [(rs rw) (fmt+w (^t (ff (realtime t) 3)) ts)]  ;real time
           [(ps pw) (fmt+w (^t (ff (cputime  t)  3)) ts)] ;cpu time
           [(us uw) (fmt+w (^t (ff (usertime t) 3)) ts)]  ;user time
           [(ss sw) (fmt+w (^t (ff (systime  t) 3)) ts)]  ;sys time
           [(cs cw) (fmt+w (^t (ff (rate t) 2)) ts)]      ;count/s
           )
        (for-each
         (^(k r p u s c n)
           (format #t
                   "  ~v@a: ~v@a real, ~v@a cpu (~v@a user + ~v@a sys)@~v@a/s n=~a\n"
                   kw k rw r pw p uw u sw s cw c n))
         ks rs ps us ss cs (map (cut ref <> 'count) ts))
        ;; matrix
        (let1 mat (map (^y (map (^x (if (eq? x y) "--" (ratio y x))) ts)) ts)
          (receive (Cs Cw) (fmt+w (^t (if (finite? (rate t))
                                        ($ x->string $ x->integer $ rate t)
                                        ($ x->string $ rate t)))
                                  ts)
            (let1 Cw (max (+ Cw 2) 4)   ; minimum width for "Rate"
              (format #t "\n  ~v@a ~v@a" kw "" Cw "Rate")
              (let1 col-widths (map (pa$ apply max)
                                    (map (pa$ map string-length)
                                         (map cons ks (apply map list mat))))
                (for-each (^(key col-width) (format #t " ~v@a" col-width key))
                          ks col-widths)
                (for-each (^(key row C)
                            (format #t "\n  ~v@a ~v@a/s" kw key (- Cw 2) C)
                            (for-each (^(col col-width)
                                        (format #t " ~v@a" col-width col))
                                      row col-widths))
                          ks mat Cs))))))
      (newline)))
  ;; check valid format of the alist part
  (define (valid-alist? alist)
    (and (every pair? alist) (every (.$ time-result? cdr) alist)))

  (match result
    [((? integer?) . (? valid-alist? alist))     (show)]
    [(('cpu (? real?)) . (? valid-alist? alist)) (show)]
    [else (error "the argument doesn't seem like a time-these result:" result)]))
;; Timers ---------------------------------------------

(define-class <time-counter> ()
  ((value   :init-value 0)    ;accured time, in seconds
   (start   :init-value #f)   ;start time.  format depends on the subclass.
   (nesting :init-value 0))   ;nesting level.
  )

(define-method initialize ((self <time-counter>) initargs)
  (next-method)
  (when (eq? (class-of self) <time-counter>)
    (error "you can't instantiate <time-counter> class directly; use either <real-time-counter>, <user-time-counter> or <process-time-counter>"))
  )

(define-method time-counter-start! ((self <time-counter>))
  (let1 nesting (slot-ref self 'nesting)
    (when (zero? nesting)
      (slot-set! self 'start (time-counter-get-current-time self)))
    (slot-set! self 'nesting (+ nesting 1))))


(define-method time-counter-stop! ((self <time-counter>))
  (let1 nesting (slot-ref self 'nesting)
    (cond
     [(= nesting 1)
      (inc! (slot-ref self 'value) (time-counter-get-delta self))
      (set! (slot-ref self 'nesting) 0)]
     [(> nesting 1)
      (set! (slot-ref self 'nesting) (- nesting 1))]
     [else
      ;; sprious stop.  make sure the state is sane
      (set! (slot-ref self 'nesting) 0)])))

(define-method time-counter-get-delta ((self <time-counter>))
  ;; This default method assumes time-counter-get-current-time returns
  ;; a real number of seconds.  A subclass that uses different representation
  ;; should override this method as well.
  (let1 end (time-counter-get-current-time self)
    (- end (slot-ref self 'start))))

(define-method time-counter-value ((self <time-counter>))
  (slot-ref self 'value))

(define-method time-counter-reset! ((self <time-counter>))
  (slot-set! self 'value 0)
  (slot-set! self 'nesting 0))

(define-method write-object ((self <time-counter>) port)
  (format port "#<~a ~a>"
          (class-name (class-of self))
          (format-delta-time (slot-ref self 'value))))

(define-syntax with-time-counter
  (syntax-rules ()
    [(_ counter . exprs)
     (let1 c counter
       (dynamic-wind
           (lambda () (time-counter-start! c))
           (lambda () . exprs)
           (lambda () (time-counter-stop! c))))]))

;; 'real' time counter
(define-class <real-time-counter> (<time-counter>)
  ())

(define-method time-counter-get-current-time ((self <real-time-counter>))
  (receive r (sys-gettimeofday) r))

(define-method time-counter-get-delta ((self <real-time-counter>))
  (receive (sec usec) (sys-gettimeofday)
    (+ (- sec (car (slot-ref self 'start)))
       (/. (- usec (cadr (slot-ref self 'start)))
           1000000))))

;; 'user' time counter
(define-class <user-time-counter> (<time-counter>)
  ())

(define-method time-counter-get-current-time ((self <user-time-counter>))
  (let1 times (sys-times)
    (/. (list-ref times 0) (list-ref times 4))))

;; 'system' time counter
(define-class <system-time-counter> (<time-counter>)
  ())

(define-method time-counter-get-current-time ((self <system-time-counter>))
  (let1 times (sys-times)
    (/. (list-ref times 1) (list-ref times 4))))

;; 'process' time counter - 'user' + 'sys'
(define-class <process-time-counter> (<time-counter>)
  ())

(define-method time-counter-get-current-time ((self <process-time-counter>))
  (let1 times (sys-times)
    (/. (+ (list-ref times 0) (list-ref times 1))
        (list-ref times 4))))

