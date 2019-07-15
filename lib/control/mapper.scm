;;;
;;; control.mapper - parallel mapper
;;;
;;;   Copyright (c) 2018-2019  Shiro Kawai  <shiro@acm.org>
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

(define-module control.mapper
  (use gauche.threads)
  (use gauche.sequence)
  (use gauche.generator)
  (use data.queue)
  (use scheme.list)
  (use control.thread-pool)
  (use control.job)
  (export pmap
          single-mapper make-thread-mapper make-pool-mapper))
(select-module control.mapper)

;; MAPPER allows a procedure to run on set of objects, possibly
;; in parallel.  It abstracts the execution mechanism so that the
;; same code can run on both single-threaded and multi-threaded
;; processes.
;;
;; The high-level API, pmap, can be used without knowing underlying
;; thread models; it uses threads

(define (single-mapper proc coll)
  (map proc coll))

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

(define (make-thread-mapper :optional (num-threads #f))
  (let1 nt (or num-threads (sys-available-processors))
    (^[proc coll]
      (let* ([cols (%split-collection coll nt)]
             [ts (filter-map (^c (and (pair? c)
                                      (make-thread (^[] (map proc c)))))
                             cols)])
        (for-each thread-start! ts)
        (append-map thread-join! ts)))))

(define (make-pool-mapper pool)
  (^[proc coll]
    (for-each-with-index (^[i e] (add-job! pool (^[] (cons i (proc e))) #t))
                         coll)
    ($ map cdr $ (cut sort <> < car)
       $ map (^_ (job-result (dequeue/wait! (thread-pool-results pool))))
       $ liota (size-of coll))))

(define (default-mapper)
  (cond-expand
   [gauche.sys.threads
    (if (= 1 (sys-available-processors))
      single-mapper
      (make-thread-mapper))]
   [else
    single-mapper]))

(define (pmap proc coll :key (mapper (default-mapper)))
  (mapper proc coll))
                                
  
  
    





