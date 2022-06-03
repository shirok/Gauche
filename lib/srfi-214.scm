;;;
;;; srfi-214 - flexvector
;;;
;;;   Copyright (c) 2021-2022  Shiro Kawai  <shiro@acm.org>
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

;; We use data.ring-buffer to implement flexvector
;;
(define srfi-214
  (use srfi-42)
  (use data.ring-buffer)
  (export make-flexvector flexvector
          flexvector-unfold flexvector-unfold-right
          flexvector-copy flexvector-reverse-copy
          flexvector-append flexvector-concatenate
          flexvector-append-subvectors

          flexvector? flexvector-empty? flexvector=?

          flexvector-ref flexvector-front flexvector-back
          flexvector-length

          flexvector-add! flexvector-add-front! flexvector-add-back!
          flexvector-add-all! flexvector-append! flexvector-remove!
          flexvector-remove-front! flexvector-remove-back!
          flexvector-remove-range!
          flexvector-clear! flexvector-set!
          flexvector-swap! flexvector-fill! flexvector-reverse!
          flexvector-copy! flexvector-reverse-copy!

          flexvector-fold flexvector-fold-right
          flexvector-map flexvector-map/index
          flexvector-append-map flexvector-append-map/index
          flexvector-filter flexvector-filter/index
          flexvector-filter! flexvector-filter/index!
          flexvector-for-each flexvector-for-each/index
          flexvector-count flexvector-cumulate

          flexvector-index flexvector-index-right
          flexvector-skip flexvector-skip-right
          flexvector-binary-search
          flexvector-any flexvector-every flexvector-partition

          flexvector->vector vector->flexvector
          flexvector->list list->flexvector reverse-flexvector->list
          flexvector->string string->flexvector
          generator->flexvector))
(select-module srfi-214)

;;
;; Constructors
;;
(define (make-flexvector size :optional fill)
  (make-ring-buffer (make-vector size fill)
                    :initial-tail-index size))
(define (flexvector . xs)
  (let1 v (list->vector xs)
    (make-ring-buffer v :initial-tail-index (vector-length v))))

(define (flexvector-unfold p f g . seeds)
  (rlet1 fv (make-ring-buffer)
    (let loop ((seeds seeds))
      (unless (apply p seeds)
        (ring-buffer-add-back! fv (apply f seeds))
        (loop (values->list (apply g seeds)))))))

(define (flexvector-unfold-right p f g . seeds)
  (rlet1 fv (make-ring-buffer)
    (let loop ((seeds seeds))
      (unless (apply p seeds)
        (ring-buffer-add-front! fv (apply f seeds))
        (loop (values->list (apply g seeds)))))))

(define (flexvector-copy fv :optional (start 0)
                                      (end (flexvector-length fv)))
  (assume-type fv <ring-buffer>)
  (make-ring-buffer (ring-buffer->flat-vector fv start end)
                    :initial-tail-index (- end start)))

(define (flexvector-reverse-copy fv :optional (start 0)
                                              (end (flexvector-length fv)))
  (assume-type fv <ring-buffer>)
  ;; Can be more efficient, but for now.
  ;; (We want to treat ring-buffer as opaque, so avoid accessing storage
  ;; diractly).
  (let1 vec (make-vector (- end start))
    (dotimes [k (- end start)]
      (vector-set! vec k (ring-buffer-ref fv (- end k 1))))
    (make-ring-buffer vec :initial-tail-index (- end start))))

(define (flexvector-append . fvs) (flexvector-concatenate fvs))

(define (flexvector-concatenate fvs)
  (define len (fold (^[fv s] (+ (flexvector-length fv) s)) 0 fvs))
  (rlet1 dest (make-vector len)
    (let loop ([fvs fvs] [k 0])
      (if (null? fvs)
        (make-ring-buffer dest :initial-tail-index len)
        (begin (ring-buffer->flat-vector! dest k (car fvs))
               (loop (cdr fvs) (+ k (flexvector-length (car fvs)))))))))

;; flexvector-append-subvectors

;;
;; Predicates
;;
(define (flexvector? obj) (ring-buffer? obj))

(define (flexvector-empty? obj) (ring-buffer-empty? obj))

(define flexvector=?
  (case-lambda
    [(elt=?) #t]
    [(elt=? fv) (assume (flexvector? fv)) #t]
    [(elt=? fv . fvs)
     (let1 len (flexvetor-length fv)
       (every (^[fv2]
                (and (= len (flexvector-length fv2))
                     (every-ec (: i len)
                               (elt=? (flexvector-ref a i)
                                      (flexvector-ref b i)))))
              fvs))]))

;;
;; Selectors
;;
(define (flexvector-ref fv i) (ring-buffer-ref fv i))
(define (flexvector-front fv) (ring-buffer-front fv))
(define (flexvector-back fv) (ring-buffer-back fv))
(define (flexvector-length fv) (ring-buffer-num-entries fv))

;;
;; Mutators
;;

(define (flexvector-add! fv i . xs)
  (ring-buffer-insert-all! fv i xs))
(define (flexvector-add-all! f i xs)
  (ring-buffer-insert-all! fv i xs))

(define (flexvector-add-front! fv . xs)
  (for-each (cut ring-buffer-add-front! fv <>) (reverse xs)))
(define (flexvector-add-back! fv . xs)
  (for-each (cut ring-buffer-add-back! fv <>) xs))

;; flexvector-append!
;; flexvector-remove!

(define (flexvector-remove-front! fv) (ring-buffer-remove-front! fv))
(define (flexvector-remove-back! fv) (ring-buffer-remove-back! fv))

;; flexvector-remove-range!

(define (flexvector-clear! fv) (ring-buffer-clear! fv))

(define (flexvector-set! fv i obj) (ring-buffer-set! fv i obj))

;; flexvector-swap!
;; flexvector-fill!
;; flexvector-reverse!
;; flexvector-copy!
;; flexvector-reverse-copy!

;;
;; Iteration
;;
