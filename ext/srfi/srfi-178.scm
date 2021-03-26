;;;
;;; srfi-178 - bitvectors
;;;
;;;   Copyright (c) 2020-2021  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi-178
  (use util.match)
  (use gauche.sequence)
  (export
   ;; bit conversion
   bit->integer bit->boolean            ;built-in

   ;; constructors
   make-bitvector bitvector             ;built-in
   bitvector-unfold bitvector-unfold-right
   bitvector-copy                       ;built-in
   bitvector-reverse-copy
   bitvector-append bitvector-concatenate bitvector-append-subbitvectors

   ;; predicates
   bitvector?                           ;built-in
   bitvector-empty?
   bitvector=?

   ;; selectors
   bitvector-ref/int bitvector-ref/bool ;built-in
   bitvector-length                     ;built-in

   ;; iteration
   bitvector-take bitvector-take-right
   bitvector-drop bitvector-drop-right
   bitvector-segment
   bitvector-fold/int bitvector-fold/bool
   bitvector-fold-right/int bitvector-fold-right/bool
   bitvector-map/int bitvector-map/bool
   bitvector-map!/int bitvector-map!/bool
   bitvector-map->list/int bitvector-map->list/bool
   bitvector-for-each/int bitvector-for-each/bool

   ;; prefixes, suffixes, trimming, padding
   bitvector-prefix-length bitvector-suffix-length
   bitvector-prefix? bitvector-suffix?
   bitvector-pad bitvector-pad-right
   bitvector-trim bitvector-trim-right bitvector-trim-both

   ;; mutators
   bitvector-set!                       ;built-in
   bitvector-swap!
   bitvector-reverse!
   bitvector-copy!                      ;built-in
   bitvector-reverse-copy!

   ;; conversion
   bitvector->list/int bitvector->list/bool
   reverse-bitvector->list/int reverse-bitvector->list/bool
   list->bitvector                      ;built-in
   reverse-list->bitvector
   bitvector->vector/int bitvector->vector/bool
   reverse-bitvector->vector/int reverse-bitvector->vector/bool
   vector->bitvector
   reverse-vector->bitvector

   bitvector->string                    ;built-in
   string->bitvector                    ;built-in

   bitvector->integer integer->bitvector

   ;; generators
   make-bitvector/int-generator make-bitvector/bool-generator
   make-bitvector-accumulator

   ;; basic operations
   bitvector-not bitvector-not!
   bitvector-and bitvector-and!
   bitvector-ior bitvector-ior!
   bitvector-xor bitvector-xor!
   bitvector-eqv bitvector-eqv!

   bitvector-nand bitvector-nand!
   bitvector-nor bitvector-nor!
   bitvector-andc1 bitvector-andc1!
   bitvector-andc2 bitvector-andc2!
   bitvector-orc1 bitvector-orc1!
   bitvector-orc2 bitvector-orc2!

   ;; quasi-integer operations
   bitvector-logical-shift bitvector-count bitvector-count-run
   bitvector-if bitvector-first-bit

   ;; bit field operations
   bitvector-field-any? bitvector-field-every?
   bitvector-field-clear bitvector-field-clear!
   bitvector-field-set bitvector-field-set!
   bitvector-field-replace bitvector-field-replace!
   bitvector-field-replace-same bitvector-field-replace-same!
   bitvector-field-rotate
   bitvector-field-flip bitvector-field-flip!
   ))
(select-module srfi-178)

;; handling optional start/end parameters
(define-syntax with-range
  (syntax-rules ()
    [(_ (bvec start end) . body)        ;bvec may be an ordinary vector
     (let* ([len (size-of bvec)]
            [start (if (undefined? start) 0 start)]
            [end (if (undefined? end) len end)])
       (assume (exact-integer? start))
       (assume (exact-integer? end))
       (assume (<= start end len))
       . body)]))

(inline-stub
 (.include "gauche/bignum.h"))

;;; Constructors

;; make-bitvector, bitvector - built-in

(define (bitvector-unfold f len . seeds)
  (let1 v (make-bitvector len)
    (let loop ([k 0] [seeds seeds])
      (if (= k len)
        v
        (receive (b . seeds) (apply f k seeds)
          (bitvector-set! v k b)
          (loop (+ k 1) seeds))))))

(define (bitvector-unfold-right f len . seeds)
  (let1 v (make-bitvector len)
    (let loop ([k (- len 1)] [seeds seeds])
      (if (< k 0)
        v
        (receive (b . seeds) (apply f k seeds)
          (bitvector-set! v k b)
          (loop (- k 1) seeds))))))

;; bitvector-copy - built-in

(define (bitvector-reverse-copy bv :optional start end)
  (with-range (bv start end)
    ;; can be more efficient
    (rlet1 r (bitvector-copy bv start end)
      (bitvector-reverse! r))))

(define (bitvector-append . bvs) (bitvector-concatenate bvs))

(define (bitvector-concatenate bvs)
  (let1 len (fold (^[v l] (+ l (bitvector-length v))) 0 bvs)
    (rlet1 r (make-bitvector len)
      (let loop ([k 0] [bvs bvs])
        (unless (null? bvs)
          (bitvector-copy! r k (car bvs))
          (loop (+ k (bitvector-length (car bvs))) (cdr bvs)))))))

(define (bitvector-append-subbitvectors . args)
  (define (check xs len)
    (match xs
      [() len]
      [(bv s e . rest)
       (assume-type bv <bitvector>)
       (assume (and (exact-integer? s) (exact-integer? e)))
       (assume (<= s e))
       (check rest (+ len (- e s)))]
      [_ (error "number of arguments must be multiples of 3, but got" args)]))
 (let1 len (check args 0)
   (rlet1 r (make-bitvector len)
     (let loop ([k 0] [args args])
       (match args
         [(bv s e . srcs)
          (bitvector-copy! r k bv s e)
          (loop (+ k (- e s)) srcs)]
         [_ #f])))))

;;; Predicates

;; bitvector? - built-in

(define (bitvector-empty? bv)
  (assume-type bv <bitvector>)
  (zero? (bitvector-length bv)))

(define (bitvector=? . vs)             ;srfi-178
  ;; we can compare bitvectors using equal?.  just extra type checking.
  (or (null? vs)
      (if-let1 z (find (^x (not (bitvector? x))) vs)
        (error "Bitvector required, but got:" z)
        (let loop ([v (car vs)] [vs (cdr vs)])
          (or (null? vs)
              (and (equal? v (car vs))
                   (loop (car vs) (cdr vs))))))))

;;; Selectors

;; bitvector-ref/int, bitvector-ref/bool, bitvector-length - built-in

;;; Iteration

(define (bitvector-take bv n) (bitvector-copy bv 0 n))
(define (bitvector-take-right bv n)
  (let1 len (bitvector-length bv)
    (assume (<= n len))
    (bitvector-copy bv (- len n) len)))
(define (bitvector-drop bv n)
  (let1 len (bitvector-length bv)
    (assume (<= n len))
    (bitvector-copy bv n len)))
(define (bitvector-drop-right bv n)
  (let1 len (bitvector-length bv)
    (assume (<= n len))
    (bitvector-copy bv 0 (- len n))))

(define (bitvector-segment bv n)
  (let loop ([len (bitvector-length bv)]
             [k 0]
             [r '()])
    (if (>= (+ k n) len)
      (reverse (cons (bitvector-copy bv k) r))
      (loop len (+ k n) (cons (bitvector-copy bv k (+ k n)) r)))))

;; captures common pattern of iterators
(define-syntax bv-iterator
  (syntax-rules ()
    [(_ ref (arg ... bv bvs) len body1 bodyn)
     (case-lambda
       [(arg ... bv)                    ;short path
        (let1 len (bitvector-length bv)
          body1)]
       [(arg ...) (error "At least one bitvector is required")]
       [(arg ... . bvs)
        (let ([len (bitvector-length (car bvs))])
          (unless (every (^[v](= len (bitvector-length v))) (cdr bvs))
            (error "bitvectors must be of the same length" bvs))
          bodyn)])]))

(define-inline (*ref bvs ref k) (map (^v (ref v k)) bvs))

(define (%gen-fold ref)
  (bv-iterator ref (kons knil bv bvs) len
               (let loop ([k 0] [knil knil])
                 (if (= k len) knil (loop (+ k 1) (kons knil (ref bv k)))))
               (let loop ([k 0] [knil knil])
                 (if (= k len)
                   knil
                   (loop (+ k 1)
                         (apply kons knil (*ref bvs ref k)))))))

(define bitvector-fold/int  (%gen-fold bitvector-ref/int))
(define bitvector-fold/bool (%gen-fold bitvector-ref/bool))

(define (%gen-fold-right ref)
  (bv-iterator ref (kons knil bv bvs) len
               (let loop ([k (- len 1)] [knil knil])
                 (if (< k 0) knil (loop (- k 1) (kons knil (ref bv k)))))
               (let loop ([k (- len 1)] [knil knil])
                 (if (< k 0)
                   knil
                   (loop (- k 1)
                         (apply kons knil (*ref bvs ref k)))))))

(define bitvector-fold-right/int (%gen-fold-right bitvector-ref/int))
(define bitvector-fold-right/bool (%gen-fold-right bitvector-ref/bool))

(define (%gen-map ref)
  (bv-iterator ref (f bv bvs) len
               ;; we don't need to worry about restart-safety
               (rlet1 r (make-bitvector len)
                 (let loop ([k 0])
                   (unless (= k len)
                     (bitvector-set! r k (f (ref bv k)))
                     (loop (+ k 1)))))
               (rlet1 r (make-bitvector len)
                 (let loop ([k 0])
                   (unless (= k len)
                     (bitvector-set! r k (apply f (*ref bvs ref k)))
                     (loop (+ k 1)))))))

(define bitvector-map/int (%gen-map bitvector-ref/int))
(define bitvector-map/bool (%gen-map bitvector-ref/bool))

(define (%gen-map! ref)
  (bv-iterator ref (f bv bvs) len
               (let loop ([k 0])
                 (unless (= k len)
                   (bitvector-set! bv k (f (ref bv k)))
                   (loop (+ k 1))))
               (let loop ([k 0])
                 (unless (= k len)
                   (bitvector-set! (car bvs) k
                                   (apply f (*ref bvs ref k)))
                   (loop (+ k 1))))))

(define bitvector-map!/int (%gen-map! bitvector-ref/int))
(define bitvector-map!/bool (%gen-map! bitvector-ref/bool))

(define (%gen-map->list ref)
  (bv-iterator ref (f bv bvs) len
               (let loop ([k (- len 1)] [r '()])
                 (if (< k 0)
                   r
                   (loop (- k 1) (cons (f (ref bv k)) r))))
               (let loop ([k (- len 1)] [r '()])
                 (if (< k 0)
                   r
                   (loop (- k 1)
                         (cons (apply f (*ref bvs ref k)) r))))))

(define bitvector-map->list/int (%gen-map->list bitvector-ref/int))
(define bitvector-map->list/bool (%gen-map->list bitvector-ref/bool))

(define (%gen-for-each ref)
  (bv-iterator ref (f bv bvs) len
               (let loop ([k 0])
                 (unless (= k len)
                   (f (ref bv k))
                   (loop (+ k 1))))
               (let loop ([k 0])
                 (unless (= k len)
                   (apply f (*ref bvs ref k))
                   (loop (+ k 1))))))

(define bitvector-for-each/int (%gen-for-each bitvector-ref/int))
(define bitvector-for-each/bool (%gen-for-each bitvector-ref/bool))

;;; Prefix, suffix etc

(define-cproc bitvector-prefix-length (bv1::<bitvector> bv2::<bitvector>)
  ::<int>
  (let* ([len1::ScmSmallInt (SCM_BITVECTOR_SIZE bv1)]
         [len2::ScmSmallInt (SCM_BITVECTOR_SIZE bv2)]
         [len::ScmSmallInt (?: (< len1 len2) len1 len2)]
         [bits1::ScmBits* (SCM_BITVECTOR_BITS bv1)]
         [bits2::ScmBits* (SCM_BITVECTOR_BITS bv2)]
         [i::ScmSmallInt 0])
    (for (() (< i len) (post++ i))
      (unless (== (not (SCM_BITS_TEST bits1 i))
                  (not (SCM_BITS_TEST bits2 i)))
        (return i)))
    (return len)))

(define-cproc bitvector-suffix-length (bv1::<bitvector> bv2::<bitvector>)
  ::<int>
  (let* ([len1::ScmSmallInt (SCM_BITVECTOR_SIZE bv1)]
         [len2::ScmSmallInt (SCM_BITVECTOR_SIZE bv2)]
         [len::ScmSmallInt (?: (< len1 len2) len1 len2)]
         [bits1::ScmBits* (SCM_BITVECTOR_BITS bv1)]
         [bits2::ScmBits* (SCM_BITVECTOR_BITS bv2)]
         [i::ScmSmallInt 0])
    (for (() (< i len) (post++ i))
      (unless (== (not (SCM_BITS_TEST bits1 (- len1 i 1)))
                  (not (SCM_BITS_TEST bits2 (- len2 i 1))))
        (return i)))
    (return len)))

(define (bitvector-prefix? bv1 bv2)
  (= (bitvector-prefix-length bv1 bv2) (bitvector-length bv1)))
(define (bitvector-suffix? bv1 bv2)
  (= (bitvector-suffix-length bv1 bv2) (bitvector-length bv1)))

(define (bitvector-pad bit bv len)
  (if (>= (bitvector-length bv) len)
    bv
    (rlet1 r (make-bitvector len bit)
      (bitvector-copy! r (- len (bitvector-length bv)) bv))))

(define (bitvector-pad-right bit bv len)
  (if (>= (bitvector-length bv) len)
    bv
    (rlet1 r (make-bitvector len bit)
      (bitvector-copy! r 0 bv))))

(define (bitvector-trim bit bv)
  (let1 k (bitvector-first-bit (not (bit->boolean bit)) bv)
    (if (< k 0)
      (bitvector)
      (bitvector-drop bv k))))

(define (bitvector-trim-right bit bv)
  (let1 k (bitvector-last-bit (not (bit->boolean bit)) bv)
    (if (< k 0)
      (bitvector)
      (bitvector-take bv (+ k 1)))))

(define (bitvector-trim-both bit bv)
  (let1 j (bitvector-first-bit (not (bit->boolean bit)) bv)
    (if (< j 0)
      (bitvector)
      (let1 k (bitvector-last-bit (not (bit->boolean bit)) bv)
        (if (< k 0)
          (bitvector)
          (bitvector-copy bv j (+ k 1)))))))

;;; Mutators

;; bitvector-set!, bitvector-copy! - built-in

(define (bitvector-swap! bv i j)
  (let1 t (bitvector-ref/int bv i)
    (bitvector-set! bv i (bitvector-ref/int bv j))
    (bitvector-set! bv j t)))

(define (bitvector-reverse! bv :optional start end)
  (with-range (bv start end)
    (let loop ([s start] [e (- end 1)])
      (when (< s e)
        (bitvector-swap! bv s e)
        (loop (+ s 1) (- e 1))))
    bv))

(define (bitvector-reverse-copy! target tstart src :optional sstart send)
  (with-range (src sstart send)
    (assume (exact-integer? tstart))
    (let loop ([i (- send 1)] [j tstart])
      (when (<= sstart i)
        (bitvector-set! target j (bitvector-ref/int src i))
        (loop (- i 1) (+ j 1))))
    target))

;;; Conversion

(define (bitvector->list/int bv :optional start end)
  (with-range (bv start end)
    (let loop ([k (- end 1)] [r '()])
      (if (<= start k)
        (loop (- k 1) (cons (bitvector-ref/int bv k) r))
        r))))

(define (bitvector->list/bool bv :optional start end)
  (with-range (bv start end)
    (let loop ([k (- end 1)] [r '()])
      (if (<= start k)
        (loop (- k 1) (cons (bitvector-ref/bool bv k) r))
        r))))

(define (reverse-bitvector->list/int bv :optional start end)
  (with-range (bv start end)
    (let loop ([k start] [r '()])
      (if (< k end)
        (loop (+ k 1) (cons (bitvector-ref/int bv k) r))
        r))))

(define (reverse-bitvector->list/bool bv :optional start end)
  (with-range (bv start end)
    (let loop ([k start] [r '()])
      (if (< k end)
        (loop (+ k 1) (cons (bitvector-ref/bool bv k) r))
        r))))

;; list->bitvector - built-in

(define (reverse-list->bitvector lis)
  (let* ([len (length lis)]
         [v (make-bitvector len)])
    (let loop ([k (- len 1)] [lis lis])
      (when (<= 0 k)
        (bitvector-set! v k (car lis))
        (loop (- k 1) (cdr lis))))
    v))

(define (bitvector->vector/int bv :optional start end)
  (with-range (bv start end)
    (rlet1 v (make-vector (- end start))
      (let loop ([k start] [j 0])
        (when (< k end)
          (vector-set! v j (bitvector-ref/int bv k))
          (loop (+ k 1) (+ j 1)))))))

(define (bitvector->vector/bool bv :optional start end)
  (with-range (bv start end)
    (rlet1 v (make-vector (- end start))
      (let loop ([k start] [j 0])
        (when (< k end)
          (vector-set! v j (bitvector-ref/bool bv k))
          (loop (+ k 1) (+ j 1)))))))

(define (reverse-bitvector->vector/int  bv :optional start end)
  (with-range (bv start end)
    (rlet1 v (make-vector (- end start))
      (let loop ([k (- end 1)] [j 0])
        (when (<= start k)
          (vector-set! v j (bitvector-ref/int bv k))
          (loop (- k 1) (+ j 1)))))))

(define (reverse-bitvector->vector/bool  bv :optional start end)
  (with-range (bv start end)
    (rlet1 v (make-vector (- end start))
      (let loop ([k (- end 1)] [j 0])
        (when (<= start k)
          (vector-set! v j (bitvector-ref/bool bv k))
          (loop (- k 1) (+ j 1)))))))

(define (vector->bitvector vec :optional start end)
  (with-range (vec start end)
    (rlet1 bv (make-bitvector (- end start))
      (let loop ([k 0] [j start])
        (when (< j end)
          (bitvector-set! bv k (vector-ref vec j))
          (loop (+ k 1) (+ j 1)))))))

(define (reverse-vector->bitvector vec :optional start end)
  (with-range (vec start end)
    (rlet1 bv (make-bitvector (- end start))
      (let loop ([k 0] [j (- end 1)])
        (when (<= start j)
          (bitvector-set! bv k (vector-ref vec j))
          (loop (+ k 1) (- j 1)))))))

;; bitvector->string, string->bitvector - built-in

(define-cproc bitvector->integer (bv::<bitvector>)
  (if (== (SCM_BITVECTOR_SIZE bv) 0)
    (return (SCM_MAKE_INT 0))
    (return
     (Scm_NormalizeBignum
      (SCM_BIGNUM
       (Scm_MakeBignumFromUIArray
        1
        (SCM_BITVECTOR_BITS bv)
        (SCM_BITS_NUM_WORDS (SCM_BITVECTOR_SIZE bv))))))))

;; We could directly copy ScmBits* from a bignum but we don't have
;; an API to construct bitvector from ScmBits* yet.
(define (integer->bitvector n :optional (len (integer-length n)))
  (assume (and (exact-integer? n) (>= n 0)))
  (rlet1 bv (make-bitvector len)
    (let loop ([k 0])
      (when (< k len)
        (bitvector-set! bv k (logbit? k n))
        (loop (+ k 1))))))

;;; Generators

(define (make-bitvector/int-generator bv)
  (assume-type bv <bitvector>)
  (let ([len (bitvector-length bv)]
        [k 0])
    (^[] (if (< k len)
           (rlet1 b (bitvector-ref/int bv k)
             (inc! k))
           (eof-object)))))

(define (make-bitvector/bool-generator bv)
  (assume-type bv <bitvector>)
  (let ([len (bitvector-length bv)]
        [k 0])
    (^[] (if (< k len)
           (rlet1 b (bitvector-ref/bool bv k)
             (inc! k))
           (eof-object)))))

(define (make-bitvector-accumulator)
  (define vals '())
  (^[val] (if (eof-object? val)
            (reverse-list->bitvector vals)
            (if (memv val '(#t #f 0 1))
              (begin (push! vals val) (undefined))
              (error "value must be either #t, #f, 0, 1 or #<eof>, but got"
                     val)))))

;;; Basic operations

(inline-stub
 (define-cise-stmt %bv-op
   [(_ op v1 v2)
    `(begin
       (SCM_BITVECTOR_CHECK_MUTABLE ,v1)
       (unless (== (SCM_BITVECTOR_SIZE ,v1) (SCM_BITVECTOR_SIZE ,v2))
         (Scm_Error "Bitvector sizes don't match: %S vs %S" ,v1 ,v2))
       (Scm_BitsOperate (SCM_BITVECTOR_BITS ,v1) ,op
                        (SCM_BITVECTOR_BITS ,v1) (SCM_BITVECTOR_BITS ,v2)
                        0 (SCM_BITVECTOR_SIZE ,v1))
       (return (SCM_OBJ ,v1)))])
 )

(define (bitvector-not v) (bitvector-not! (bitvector-copy v)))
(define-cproc bitvector-not! (v::<bitvector>)
  (%bv-op SCM_BIT_NOT1 v v))

(define-syntax define-nary-op
  (syntax-rules ()
    [(_ name name! binary)
     (begin
       (define (name! v1 v2 . vs)
         (if (null? vs)
           (binary v1 v2)
           (and (binary v1 v2)
                (apply name! v1 vs))))   ;keep v1 as the accumulator
       (define (name v1 v2 . vs)
         (apply name! (bitvector-copy v1) v2 vs)))]))

(define-cproc %bitvector-and! (v1::<bitvector> v2::<bitvector>)
  (%bv-op SCM_BIT_AND v1 v2))
(define-cproc %bitvector-ior! (v1::<bitvector> v2::<bitvector>)
  (%bv-op SCM_BIT_IOR v1 v2))
(define-cproc %bitvector-xor! (v1::<bitvector> v2::<bitvector>)
  (%bv-op SCM_BIT_XOR v1 v2))
(define-cproc %bitvector-eqv! (v1::<bitvector> v2::<bitvector>)
  (%bv-op SCM_BIT_EQV v1 v2))

(define-nary-op bitvector-and bitvector-and! %bitvector-and!)
(define-nary-op bitvector-ior bitvector-ior! %bitvector-ior!)
(define-nary-op bitvector-xor bitvector-xor! %bitvector-xor!)
(define-nary-op bitvector-eqv bitvector-eqv! %bitvector-eqv!)

(define (bitvector-nand v1 v2) (bitvector-nand! (bitvector-copy v1) v2))
(define-cproc bitvector-nand! (v1::<bitvector> v2::<bitvector>)
  (%bv-op SCM_BIT_NAND v1 v2))
(define (bitvector-nor v1 v2) (bitvector-nor! (bitvector-copy v1) v2))
(define-cproc bitvector-nor! (v1::<bitvector> v2::<bitvector>)
  (%bv-op SCM_BIT_NOR v1 v2))
(define (bitvector-andc1 v1 v2) (bitvector-andc1! (bitvector-copy v1) v2))
(define-cproc bitvector-andc1! (v1::<bitvector> v2::<bitvector>)
  (%bv-op SCM_BIT_ANDC1 v1 v2))
(define (bitvector-andc2 v1 v2) (bitvector-andc2! (bitvector-copy v1) v2))
(define-cproc bitvector-andc2! (v1::<bitvector> v2::<bitvector>)
  (%bv-op SCM_BIT_ANDC2 v1 v2))
(define (bitvector-orc1 v1 v2) (bitvector-orc1! (bitvector-copy v1) v2))
(define-cproc bitvector-orc1! (v1::<bitvector> v2::<bitvector>)
  (%bv-op SCM_BIT_IORC1 v1 v2))
(define (bitvector-orc2 v1 v2) (bitvector-orc2! (bitvector-copy v1) v2))
(define-cproc bitvector-orc2! (v1::<bitvector> v2::<bitvector>)
  (%bv-op SCM_BIT_IORC2 v1 v2))

;;; Quasi-integer operations

(define (bitvector-logical-shift bv count bit)
  (let1 len (bitvector-length bv)
    (rlet1 r (make-bitvector len bit)
      (cond [(<= len count)]
            [(<= 0 count) (bitvector-copy! r 0 bv count)]
            [(< (- len) count)
             ;; negative count is right shift
             (bitvector-copy! r (- count) bv 0 (+ len count))]))))

(define-cproc bitvector-count (bit bv::<bitvector>) ::<int>
  (if (Scm_Bit2Int bit)
    (return (Scm_BitsCount1 (SCM_BITVECTOR_BITS bv) 0 (SCM_BITVECTOR_SIZE bv)))
    (return (Scm_BitsCount0 (SCM_BITVECTOR_BITS bv) 0 (SCM_BITVECTOR_SIZE bv)))))

(define-cproc bitvector-count-run (bit bv::<bitvector> i::<fixnum>) ::<int>
  (unless (and (<= 0 i) (<= i (SCM_BITVECTOR_SIZE bv)))
    (Scm_Error "bitvector index out of range: %ld" i))
  (let* ([b::int (Scm_Bit2Int bit)]
         [bits::ScmBits* (SCM_BITVECTOR_BITS bv)]
         [len::ScmSmallInt (SCM_BITVECTOR_SIZE bv)]
         [c::int 0])
    (for (() (< i len) (post++ i))
      (if (SCM_BITS_TEST bits i)
        (unless b (break))
        (when b (break)))
      (post++ c))
    (return c)))

(define-cproc bitvector-if (bv-if::<bitvector>
                            bv-then::<bitvector>
                            bv-else::<bitvector>)
  (let* ([len::ScmSmallInt (SCM_BITVECTOR_SIZE bv-if)]
         [nw::ScmSmallInt (SCM_BITS_NUM_WORDS len)]
         [bi::ScmBits* (SCM_BITVECTOR_BITS bv-if)]
         [bt::ScmBits* (SCM_BITVECTOR_BITS bv-then)]
         [be::ScmBits* (SCM_BITVECTOR_BITS bv-else)]
         [i::ScmSmallInt 0]
         [bv-result (Scm_MakeBitvector len SCM_FALSE)]
         [br::ScmBits* (SCM_BITVECTOR_BITS bv-result)])
    (unless (== (SCM_BITVECTOR_SIZE bv-then) len)
      (Scm_Error "lengths of bv-if and bv-then differ: %S vs %S"
                 bv-if bv-then))
    (unless (== (SCM_BITVECTOR_SIZE bv-else) len)
      (Scm_Error "lengths of bv-if and bv-else differ: %S vs %S"
                 bv-if bv-else))
    (for (() (< i nw) (post++ i))
      (set! (aref br i)
            (logior (logand (aref bi i) (aref bt i))
                    (logand (lognot (aref bi i)) (aref be i)))))
    (return bv-result)))

(define-cproc bitvector-first-bit (bit bv::<bitvector>) ::<int>
  (if (Scm_Bit2Int bit)
    (return (Scm_BitsLowest1 (SCM_BITVECTOR_BITS bv) 0 (SCM_BITVECTOR_SIZE bv)))
    (return (Scm_BitsLowest0 (SCM_BITVECTOR_BITS bv) 0 (SCM_BITVECTOR_SIZE bv)))))

;; NB: This is not in srfi-178, but useful in bitvector-trim-right
(define-cproc bitvector-last-bit (bit bv::<bitvector>) ::<int>
  (if (Scm_Bit2Int bit)
    (return (Scm_BitsHighest1 (SCM_BITVECTOR_BITS bv) 0 (SCM_BITVECTOR_SIZE bv)))
    (return (Scm_BitsHighest0 (SCM_BITVECTOR_BITS bv) 0 (SCM_BITVECTOR_SIZE bv)))))

;;; Bit field operations

(define-cproc bitvector-field-any? (bv::<bitvector>
                                    start::<fixnum>
                                    end::<fixnum>)
  ::<boolean>
  (let* ([len::ScmSmallInt (SCM_BITVECTOR_SIZE bv)]
         [bits::ScmBits* (SCM_BITVECTOR_BITS bv)]
         [i::ScmSmallInt 0])
    (SCM_CHECK_START_END start end len)
    (for ((set! i start) (< i end) (post++ i))
      (when (SCM_BITS_TEST bits i) (return TRUE)))
    (return FALSE)))

(define-cproc bitvector-field-every? (bv::<bitvector>
                                      start::<fixnum>
                                      end::<fixnum>)
  ::<boolean>
  (let* ([len::ScmSmallInt (SCM_BITVECTOR_SIZE bv)]
         [bits::ScmBits* (SCM_BITVECTOR_BITS bv)]
         [i::ScmSmallInt start])
    (SCM_CHECK_START_END start end len)
    (for (() (< i end) (post++ i))
      (unless (SCM_BITS_TEST bits i) (return FALSE)))
    (return TRUE)))

(define (bitvector-field-clear bv start end)
  (bitvector-field-clear! (bitvector-copy bv) start end))

(define-cproc bitvector-field-clear! (bv::<bitvector>
                                      start::<fixnum>
                                      end::<fixnum>)
  (let* ([len::ScmSmallInt (SCM_BITVECTOR_SIZE bv)]
         [bits::ScmBits* (SCM_BITVECTOR_BITS bv)]
         [i::ScmSmallInt start])
    (SCM_CHECK_START_END start end len)
    (for (() (< i end) (post++ i))
      (SCM_BITS_RESET bits i))
    (return (SCM_OBJ bv))))

(define (bitvector-field-set bv start end)
  (bitvector-field-set! (bitvector-copy bv) start end))

(define-cproc bitvector-field-set! (bv::<bitvector>
                                    start::<fixnum>
                                    end::<fixnum>)
  (let* ([len::ScmSmallInt (SCM_BITVECTOR_SIZE bv)]
         [bits::ScmBits* (SCM_BITVECTOR_BITS bv)]
         [i::ScmSmallInt start])
    (SCM_CHECK_START_END start end len)
    (for (() (< i end) (post++ i))
      (SCM_BITS_SET bits i))
    (return (SCM_OBJ bv))))

(define (bitvector-field-replace dest src start end)
  (bitvector-field-replace! (bitvector-copy dest) src start end))

(define (bitvector-field-replace! dest src start end)
  (bitvector-copy! dest start src 0 (- end start))
  dest)

(define (bitvector-field-replace-same dest src start end)
  (bitvector-field-replace-same! (bitvector-copy dest) src start end))

(define (bitvector-field-replace-same! dest src start end)
  (bitvector-copy! dest start src start end)
  dest)

(define (bitvector-field-rotate bv count start end)
  (assume (exact-integer? count))
  (assume (and (exact-integer? start) (<= 0 start (bitvector-length bv))))
  (assume (and (exact-integer? end)   (<= start end (bitvector-length bv))))
  (if (or (zero? count) (= start end))
    bv                                  ;trivial case
    (rlet1 r (bitvector-copy bv)
      (let* ([size (- end start)]
             [shift (modulo count size)])
        ;;
        ;; src   xxxxxxxxxaaaabbbbbbbbbbxxxxx
        ;;                ^   ^         ^
        ;;                s   s+S       e
        ;;
        ;; dst   xxxxxxxxxbbbbbbbbbbaaaaxxxxx
        ;;
        (bitvector-copy! r start bv (+ start shift) end)
        (bitvector-copy! r (- end shift) bv start (+ start shift))))))

(define (bitvector-field-flip bv start end)
  (bitvector-field-flip! (bitvector-copy bv) start end))

(define-cproc bitvector-field-flip! (bv::<bitvector>
                                     start::<fixnum> end::<fixnum>)
  (let* ([len::ScmSmallInt (SCM_BITVECTOR_SIZE bv)]
         [bits::ScmBits* (SCM_BITVECTOR_BITS bv)]
         [i::ScmSmallInt start])
    (SCM_CHECK_START_END start end len)
    (for (() (< i end) (post++ i))
      (if (SCM_BITS_TEST bits i)
        (SCM_BITS_RESET bits i)
        (SCM_BITS_SET bits i)))
    (return (SCM_OBJ bv))))
