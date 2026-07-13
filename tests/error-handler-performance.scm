;;
;; error-handler-performance.scm
;;
;; Micro-benchmark for the cost of with-error-handler when we move
;; continuation frames to the heap eagerly.
;;
;; Although with-error-handler can be implemented with call/cc and
;; with-exception-handler (see "Exception handling" section in vm.c).
;; we provide it as a primitive, as the continuation used for
;; error escape is one-shot, and does not need to save cont frames
;; to the heap at capturing time.  (These "lightweight continuation"
;; is chained to vm->floatingEscapePoints).
;;
;; The existnce of these "lightweight continuation" makes continuation
;; handling complicated, and it has been debated that we may just save
;; continuation frames to heap at capture time.  This benchmark
;; is to estimate the cost of switching to eager saving model.
;;
;; The eager variant is compiled in only when src/vm.c is built with
;;   -DGAUCHE_EP_EAGER_SAVECONT
;;
;; Compiled gosh with and without -DGAUCHE_EP_EAGER_SAVECONT, and run
;; this script:
;;
;; gosh error-handler-performance.scm [<depth> <iters>]
;;
;; <depth> is the # of in-stack continuation frames below with-error-handler.
;;
;; Results
;; --------------------
;;
;; One run, x86-64 Linux, gcc -O2, iters=200000, best of 3.
;;
;;   depth   baseline    eager save_cont   delta      delta/frame   slowdown
;;           (ns/call)      (ns/call)      (ns)         (ns)
;;   ----------------------------------------------------------------------
;;      0        368            411          43           -           1.1x
;;      8        489           1056         568          71.0         2.2x
;;     32        770           3061        2291          71.6         4.0x
;;    128       1993          10844        8852          69.2         5.4x
;;    512       6785          45353       38568          75.3         6.7x
;;
;; It appares our hypothesis is correct - eager save_cont does tax
;; the performance.   However, in more realistic code, the overhead
;; may be buried in other variances.

(use util.match)

;; Handler is never invoked; the thunk returns a number so make-frames' (+ 0 ..)
;; wrapper stays valid.
(define (one-weh)
  (with-error-handler
      (lambda (e) 0)
      (lambda () 0)))

;; Build `d` non-tail continuation frames on the stack, then run `thunk` at the
;; bottom.  The (+ 0 ...) keeps each recursive call non-tail so a real cont
;; frame is pushed, giving `thunk` `d` in-stack cont frames below it.
(define (make-frames d thunk)
  (if (= d 0)
      (thunk)
      (+ 0 (make-frames (- d 1) thunk))))

;; Rebuild fresh in-stack frames on every iteration.  This matters: the eager
;; save_cont heapifies those frames, so if we reused them across iterations only
;; the first install would pay the copy cost.
(define (bench iters depth)
  (let loop ((i iters))
    (when (> i 0)
      (make-frames depth one-weh)
      (loop (- i 1)))))

(define (now)
  (receive (s u) (sys-gettimeofday) (+ s (* u 1e-6))))

(define (best-of n thunk)
  (let loop ((n n) (best #f))
    (if (= n 0)
        best
        (let ((t0 (now)))
          (thunk)
          (let ((dt (- (now) t0)))
            (loop (- n 1) (if (or (not best) (< dt best)) dt best)))))))

(define (run-one depth iters)
  (bench (quotient iters 10) depth)          ; warm up
  (let ((t (best-of 3 (lambda () (bench iters depth)))))
    (format #t "~8d ~10d ~12,4f ~10,1f~%" depth iters t (/ (* t 1e9) iters))))

(define (header)
  (format #t "~8a ~10a ~12a ~10a~%" "depth" "iters" "best(s)" "ns/call"))

(define *default-iter* 200000)

(define (main args)
  (match (cdr args)
    [() ;; Run with varying depths
     (header)
     (dolist [d '(0 8 32 128 512)]
       (run-one d *default-iter*))]
    [(depth)
     (header)
     (run-one (string->number depth) *default-iter*)]
    [(depth iter)
     (header)
     (run-one (string->number depth) (string->number iter))]
    [_ (exit 1 "Usage: gosh error-handler-performance.scm [depth iteration]")])
  0)
