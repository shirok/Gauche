;;;
;;; control.pmap - parallel mapper
;;;
;;;   Copyright (c) 2018-2022  Shiro Kawai  <shiro@acm.org>
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

(define-module control.pmap
  (use gauche.threads)
  (use gauche.sequence)
  (use gauche.generator)
  (use gauche.mop.singleton)
  (use data.queue)
  (use scheme.list)
  (use control.thread-pool)
  (use control.job)
  (export pmap pfind pany
          sequential-mapper
          make-static-mapper
          make-pool-mapper
          make-fully-concurrent-mapper))
(select-module control.pmap)

;; MAPPER abstracts the parallelization strategy.  We provide several
;; predefined mappers.
;;
;;   sequential-mapper - A sigleton mapper that runs in a single (current) thread.
;;      If the running system is single-core, this is the default mapper.
;;
;;   pool-mapper - Use thread pool.  This is ideal when each task requies
;;      some processing time, so that the overhead of thread pool is
;;      negligible.
;;
;;   static-mapper - Create as many threads as the available cores, and
;;      distribute elements evenly.  Low overhead, good when each task is
;;      lightweight and the execution time won't fractuate much.
;;
;;   full-concurrent-mapper - Creates as many threads as the number of
;;      elements and run concurrently.  Relatively large overhead per element,
;;      but works better if (1) the number of elements are not very large,
;;      e.g. up to a few dozens, and (2) each task is expected to block
;;      on I/O.
;;
;; A mapper class implements the following methods.  This is a provisional
;; interface.  The protocol should be kept private, for we may change as
;; we support more operations.
;;
;;   run-map <mapper> proc <collection>
;;      Apply proc on each element of <collection> in parallel,
;;      gather the result into a list.
;;
;;   run-select <mapper> proc <collection>
;;      PROC must return two values, stop? and value.  The method applies
;;      proc on each element of <collection>.  As soon as one of the proc
;;      returns true in stop?, it cancels other operations and returns
;;      the value.

;;
;; The high-level API, pmap, can be used without knowing underlying
;; thread models; it uses threads if running Gauche has the thread support
;; and the system has more than one cores.   Otherwise, it just runs
;; ordinary map (see sequential-mapper).
;;
;; Concurrent execution can be done in either (1) staically split
;; the task according to the number of available threads, or (2) dynamically
;; split it using an existing thread pool.   We don't make thread
;; pool every time pmap is called, for the overhead would be too big.


;; Abstract class.
(define-class <mapper> () ())

;; Utilities

;; Start threads in the list.
(define (%start-threads threads)
  (for-each thread-try-start! threads))

;;
;; sequential-mapper
;;
(define-class <sequential-mapper> (<mapper> <singleton-mixin>) ())

(define (sequential-mapper) (instance-of <sequential-mapper>))

(define-method run-map ((mapper <sequential-mapper>) proc coll)
  (map proc coll))

(define-method run-select ((mapper <sequential-mapper>) proc coll)
  (with-iterator (coll end? next)
    (let loop ()
      (if (end?)
        #f
        (receive (s? r) (proc (next))
          (if s? r (loop)))))))

;;
;; static-mapper
;;

(define-class <static-mapper> (<mapper>)
  ((num-threads :init-keyword :num-threads)))

(define (make-static-mapper :optional (num-threads (sys-available-processors)))
  (make <static-mapper> :num-threads num-threads))

(define-method run-map ((mapper <static-mapper>) proc coll)
  (let* ([cols (%split-collection coll (~ mapper'num-threads))]
         [ts (filter-map (^c (and (pair? c)
                                  (make-thread (cut map proc c))))
                         cols)])
    (%start-threads ts)
    (append-map thread-join! ts)))

;; (define (%split-collection coll n)
;;   (define qs (list-tabulate n (^_ (make-queue))))
;;   (define qgen (apply circular-generator qs))
;;   (with-iterator (coll end? next)
;;     (until (end?)
;;       (enqueue! (qgen) (next))))
;;   (map dequeue-all! qs))

(define (%split-collection coll n)
  (receive (q r) (quotient&remainder (size-of coll) n)
    ($ remove null?
       (values-ref (map-accum (^[k lis] (split-at lis k))
                              (coerce-to <list> coll)
                              (append (make-list r (+ q 1))
                                      (make-list (- n r) q)))
                   0))))

(define-method run-select ((mapper <static-mapper>) proc coll)
  (define signaled (atom #f))
  (define (with-stopper proc elts threads)
    (let loop ([elts elts])
      (if (null? elts)
        #f
        (receive (s? r) (proc (car elts))
          (if s?
            (values-ref
             ($ atomic-update! signaled
                (^f (or f
                        (begin
                          (dolist (t threads)
                            (unless (eq? t (current-thread))
                              (thread-terminate! t)))
                          (values #t r)))))
             1)
            (loop (cdr elts)))))))
  (letrec ([cols (%split-collection coll (~ mapper'num-threads))]
           [ts (filter-map (^c (and (pair? c)
                                    (make-thread
                                     (cut with-stopper proc c ts))))
                           cols)])
    (%start-threads ts)
    (do ([ts ts (cdr ts)]
         [r #f (guard (e [(<terminated-thread-exception> e) r])
                 (or (thread-join! (car ts)) r))])
        [(null? ts) r])))

;;
;; pool mapper
;;

;; NB: You can pass an exsting thread pool and it well be reused.
;; If you do so, it is your responsibility to shut down the pool properly.

(define-class <pool-mapper> (<mapper>)
  ((external-pool :init-keyword :external-pool
                  :init-value #f)))

(define (make-pool-mapper :optional (pool #f))
  (make <pool-mapper> :external-pool pool))

(define-method run-map ((mapper <pool-mapper>) proc coll)
  (define (run pool)
    ($ for-each-with-index
       (^[i e] (add-job! pool (^[] (cons i (proc e))) #t))
       coll)
    ($ map cdr $ (cut sort <> < car)
       $ map (^_ (job-result (dequeue/wait! (thread-pool-results pool))))
       $ liota (size-of coll)))
  (if-let1 pool (~ mapper'external-pool)
    (run pool)
    (let1 pool (make-thread-pool (sys-available-processors))
      (unwind-protect
          (run pool)
        (terminate-all! pool)))))

(define-method run-select ((mapper <pool-mapper>) proc coll)
  (define result (atom #f #f))
  (define start-latch (make-latch 1))
  (define finish-latch (make-latch (size-of coll)))
  (define (do-job elt jobs)
    (latch-await start-latch)
    (receive (s? r) (proc elt)
      (if s?
        (receive [found? result notify?]
            (atomic-update! result (^[f v]
                                     (if f
                                       (values f v #f)
                                       (values #t r #t))))
          (when notify?
            (for-each (^j (job-mark-killed! j "solution selected"))
                      jobs)
            (latch-clear! finish-latch))) ;ensure to open the latch
        (latch-dec! finish-latch))))
  (define (run pool)
    (letrec ([jobs (map (^e (add-job! pool (cut do-job e jobs))) coll)])
      (latch-dec! start-latch)
      (latch-await finish-latch)
      (atom-ref result 1)))
  (if-let1 pool (~ mapper'external-pool)
    (run pool)
    (let1 pool (make-thread-pool (sys-available-processors))
      (unwind-protect
          (run pool)
        (terminate-all! pool :force-timeout 0)))))

;;
;; fully concurrent mapper
;;

(define-class <fully-concurrent-mapper> (<mapper>)
  ((timeout :init-keyword :timeout :init-value #f)
   (timeout-val :init-keyword :timeout-val :init-value #f)))

(define (make-fully-concurrent-mapper :optional (timeout #f) (timeout-val #f))
  (make <fully-concurrent-mapper> :timeout timeout :timeout-val timeout-val))

(define-method run-map ((mapper <fully-concurrent-mapper>) proc coll)
  (let ([ts (map (^e (make-thread (^[] (proc e)))) coll)]
        [timeout (~ mapper'timeout)]
        [timeout-val (~ mapper'timeout-val)])
    (%start-threads ts)
    (map (cut thread-join! <> timeout timeout-val) ts)))

(define-method run-select ((mapper <fully-concurrent-mapper>) proc coll)
  (define signaled (atom #f))
  (define (task e threads)
    (receive (s? r) (proc e)
      (if s?
        (values-ref
         ($ atomic-update! signaled
            (^f (or f
                    (begin
                      (dolist [t threads]
                        (unless (eq? t (current-thread))
                          (thread-terminate! t)))
                      (values #t r)))))
         1)
        #f)))
  (letrec ([ts (map (^e (make-thread (^[] (task e ts)))) coll)])
    (%start-threads ts)
    (do ([ts ts (cdr ts)]
         [r #f (guard (e [(<terminated-thread-exception> e) r])
                 (or (thread-join! (car ts)) r))])
        [(null? ts) r])))

;;
;; default mapper
;;

(define default-mapper
  (make-parameter
   (cond-expand
    [gauche.sys.threads
     (if (= 1 (sys-available-processors))
       (sequential-mapper)
       (make-static-mapper))]
    [else
     (sequential-mapper)])))

;;;
;;; High-level API
;;;

(define (pmap proc coll :key (mapper (default-mapper)))
  (run-map mapper proc coll))

(define (pfind pred coll :key (mapper (default-mapper)))
  (run-select mapper
              (^e (if (pred e)
                    (values #t e)
                    (values #f #f)))
              coll))

(define (pany pred coll :key (mapper (default-mapper)))
  (run-select mapper
              (^e (if-let1 r (pred e)
                    (values #t r)
                    (values #f #f)))
              coll))
