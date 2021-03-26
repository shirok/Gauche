;;;
;;;liblazy.scm - lazy constructs
;;;
;;;   Copyright (c) 2000-2021  Shiro Kawai  <shiro@acm.org>
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

(select-module gauche.internal)

(declare (keep-private-macro lcons lcons* llist*))

;;;
;;; delay/force/lazy/eager
;;;

;; NB: delay and lazy is recognized by the compiler and directly
;; expanded into PROMISE instruction.

(select-module scheme)
(define-cproc force (p) Scm_VMForce)

(select-module gauche)
(define-cproc promise? (obj) ::<boolean> :constant
  (return (SCM_XTYPEP obj SCM_CLASS_PROMISE)))
(define-cproc eager (obj)              ;srfi-45
  (return (Scm_MakePromise TRUE obj)))
(define-cproc promise-kind (p::<promise>)
  (setter (p::<promise> obj) ::<void> (set! (-> p kind) obj))
  (return (-> p kind)))

;;;
;;; lazy sequence
;;;

(select-module gauche.internal)

;; A primitive for corecursion.
;; See libmacro.scm for lcons macro.
(define-cproc %lcons (item thunk :optional (attrs ()))
  (return (Scm_LazyCons item thunk attrs)))

;; lazy sequence primitives
;;   These are so fundamental that they deserve to be in core.
;;   Auxiliary utilities are provided in gauche.lazy module.

(select-module gauche)

;; Fundamental constructor
;; generator->lseq generator
;; generator->lseq item ... generator
(define-cproc generator->lseq (item :rest args)
  (if (SCM_NULLP args)
    (return (Scm_GeneratorToLazyPair item)) ;item is a generator
    (let* ([h SCM_NIL] [t SCM_NIL])
      (for ()
        (when (SCM_NULLP (SCM_CDR args))
          (if (SCM_NULLP t)
            (return (Scm_MakeLazyPair item (SCM_CAR args) SCM_NIL))
            (begin
              (SCM_SET_CDR t (Scm_MakeLazyPair item (SCM_CAR args) SCM_NIL))
              (return h))))
        (SCM_APPEND1 h t item)
        (set! item (SCM_CAR args))
        (set! args (SCM_CDR args))))))

;; For convenience.
(define (lrange start :optional (end +inf.0) (step 1))
  (cond [(or (and (> step 0) (>= start end))
             (and (< step 0) (<= start end))) '()]
        [(= step 0) (generator->lseq (^[] start))]
        [(and (exact? start) (exact? step))
         (generator->lseq start
                          (if (> step 0)
                            (^[] (inc! start step)
                              (if (< start end) start (eof-object)))
                            (^[] (inc! start step)
                              (if (> start end) start (eof-object)))))]
        [else
         (generator->lseq (inexact start)
                          (let1 c 0
                            (if (> step 0)
                              (^[] (inc! c)
                                (let1 r (+ start (* c step))
                                  (if (< r end) r (eof-object))))
                              (^[] (inc! c)
                                (let1 r (+ start (* c step))
                                  (if (> r end) r (eof-object))))
                              )))]))

(define (liota :optional (count +inf.0) (start 0) (step 1))
  (let1 count (if (< count 0) +inf.0 count) ; like stream-iota
    (define gen
      (if (and (exact? start) (exact? step))
        (if (infinite? count)
          (^[] (rlet1 v start (inc! start step)))
          (^[] (if (<= count 0)
                 (eof-object)
                 (rlet1 v start (inc! start step) (dec! count)))))
        (let1 k 0
          (if (infinite? count)
            (^[] (rlet1 v (+ start (* k step)) (inc! k)))
            (^[] (if (<= count 0)
                   (eof-object)
                   (rlet1 v (+ start (* k step)) (inc! k) (dec! count))))))))
    (generator->lseq gen)))

(define (port->char-lseq :optional (port (current-input-port)))
  (generator->lseq (cut read-char port)))
(define (port->byte-lseq :optional (port (current-input-port)))
  (generator->lseq (cut read-byte port)))
(define (port->string-lseq :optional (port (current-input-port)))
  (generator->lseq (cut read-line port)))
(define (port->sexp-lseq :optional (port (current-input-port)))
  (generator->lseq (cut read port)))
