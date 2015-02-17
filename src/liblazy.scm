;;;
;;;liblazy.scm - lazy constructs
;;;
;;;   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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
(define-cproc force (p) Scm_Force)

(select-module gauche)
(define-cproc promise? (obj) ::<boolean> :constant
  (result (SCM_XTYPEP obj SCM_CLASS_PROMISE)))
(define-cproc eager (obj)              ;srfi-45
  (result (Scm_MakePromise TRUE obj)))
(define-cproc promise-kind (p::<promise>)
  (setter (p::<promise> obj) ::<void> (set! (-> p kind) obj))
  (result (-> p kind)))

;;;
;;; lazy sequence
;;;

(select-module gauche.internal)

(define-cproc %make-lazy-pair (item generator) Scm_MakeLazyPair)

(define-cproc %decompose-lazy-pair (obj) :: (<top> <top>)
  (let* ([item] [generator])
    (let* ([r::int (Scm_DecomposeLazyPair obj (& item) (& generator))])
      (cond [r (result item generator)]
            ;; NB: there's a possibility that obj has been forced by
            ;; some other thread; handle it.
            [else (result SCM_EOF SCM_FALSE)]))))

(define-cproc %force-lazy-pair (lp)
  (if (SCM_LAZY_PAIR_P lp)
    (result (Scm_ForceLazyPair (SCM_LAZY_PAIR lp)))
    (result lp)))

;; A primitive for corecursion.
;; See lib/gauche/common-macros.scm for the lcons macro.
(define (%lcons item thunk)
  (%make-lazy-pair item (^[] (%decompose-lazy-pair (thunk)))))

;; lazy sequence primitives
;;   These are so fundamental that they deserve to be in core.
;;   Auxiliary utilities are provided in gauche.lazy module.

;; Fundamental constructor
;; generator->lseq generator
;; generator->lseq item ... generator
(define-in-module gauche (generator->lseq item . args)
  (if (null? args)
    (let ([r (item)]) ; item is a generator
      (if (eof-object? r)
        '()
        (%make-lazy-pair r item)))
    (let rec ([item item] [args args])
      (if (null? (cdr args))
        (%make-lazy-pair item (car args))
        (cons item (rec (car args) (cdr args)))))))

;; For convenience.
(define-in-module gauche (lrange start :optional (end +inf.0) (step 1))
  ;; Exact numbers.  Fast way.
  (define (gen-exacts)
    (set! start (+ start step))
    (if (< start end)
      start
      (eof-object)))
  ;; Inexact numbers.  We use multiplication to avoid accumulating errors
  (define c 0)
  (define (gen-inexacts)
    (set! c (+ c 1))
    (let1 r (+ start (* c step))
      (if (< r end)
        r
        (eof-object))))

  (cond [(>= start end) '()]
        [(and (exact? start) (exact? step)) (generator->lseq start gen-exacts)]
        [else (generator->lseq (inexact start) gen-inexacts)]))

(define-in-module gauche (liota :optional (count +inf.0) (start 0) (step 1))
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

(define-in-module gauche (port->char-lseq :optional (port (current-input-port)))
  (generator->lseq (cut read-char port)))
(define-in-module gauche (port->byte-lseq :optional (port (current-input-port)))
  (generator->lseq (cut read-byte port)))
(define-in-module gauche (port->string-lseq :optional (port (current-input-port)))
  (generator->lseq (cut read-line port)))
(define-in-module gauche (port->sexp-lseq :optional (port (current-input-port)))
  (generator->lseq (cut read port)))

(select-module gauche)
(define-macro (lcons a b)
  ;; poor man's explicit renaming.
  ;; don't copy---we'll have real ER-transformer in future.
  (let1 %lcons ((with-module gauche.internal make-identifier)
                '%lcons
                (find-module 'gauche.internal)
                '())
    `(,%lcons ,a (lambda () ,b))))

(define-macro (lcons* x . args)
  (let1 %lcons ((with-module gauche.internal make-identifier)
                '%lcons
                (find-module 'gauche.internal)
                '())
    (cond [(null? args) x]
          [(null? (cdr args)) `(,%lcons ,x (lambda () ,(car args)))]
          [else `(cons ,x (lcons* ,@args))])))
(define-macro (llist* . args) `(lcons* ,@args))

;; Once we are able to precompile hygineic macros ...
;; (define-syntax lcons*
;;   (syntax-rules ()
;;     [(_ x) x]
;;     [(_ x y) (lcons x y)]
;;     [(_ x y z ...) (cons x (lcons* y z ...))]))
