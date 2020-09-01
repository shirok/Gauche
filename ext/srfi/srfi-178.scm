;;;
;;; srfi-178 - bitvectors
;;;
;;;   Copyright (c) 2020  Shiro Kawai  <shiro@acm.org>
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
  (export 
   ;; bit conversion
   bit->integer bit->boolean            ;built-in

   ;; constructors
   make-bitvector bitvector             ;built-in
   bitvector-unfold bitvector-unfold-right
   bitvector-copy                       ;built-in
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

(define (bitvector-reverse-copy bv :optional (s 0) e)
  (let1 e (if (undefined? e) (bitvector-length bv) e)
    ;; can be more efficient
    (rlet1 r (bitvector-copy bv s e)
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
      [(bv s e) 
       (assume-type bv <bitvector>)
       (assume (and (exact-integer? s) (exact-integer? e)))
       (assume (<= s e))
       (check (cdddr xs) (+ len (- e s)))]
      [_ (error "number of arguments must be multiples of 3, but got" args)])) 
 (receive (srcs len) (check args 0)
    (rlet1 r (make-bitvector len)
      (let loop ([k 0] [srcs srcs])
        (match srcs
          [((bv s e) . srcs)
           (bitvector-copy! r k bv s e)
           (loop (+ k (- e s)) (cdr srcs))]
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
      (reverse (cons bv r))
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
          (unless (every (^v (= len (bitvector-length v))) (cdr bvs))
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

(define bitvector-map>list/int (%gen-map->list bitvector-ref/int))
(define bitvector-map>list/bool (%gen-map->list bitvector-ref/bool))

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

(define (bitvector-prefix-length bv1 bv2) 'writeme)
(define (bitvector-suffix-length bv1 bv2) 'writeme)

(define (bitvector-prefix? bv1 bv2) 'writeme)
(define (bitvector-suffix? bv1 bv2) 'writeme)

(define (bitvector-pad bit bv len) 'writeme)
(define (bitvector-pad-right bit bv len) 'writeme)

(define (bitvector-trim bit bv) 'writeme)
(define (bitvector-trim-right bit bv) 'writeme)
(define (bitvector-trim-both bit bv) 'writeme)

;;; Mutators

;; bitvector-set!, bitvector-copy! - built-in

(define (bitvector-swap! bv i j)
  (let1 t (bitvector-ref/int bv i)
    (bitvector-set! bv i (bitvector-ref/int bv j))
    (bitvector-set! bv j t)))

(define (bitvector-reverse! bv :optional (s 0) e)
  (assume-type bv <bitvector>)
  (let ([e (if (undefined? e) (bitvector-length bv))])
    (assume (and (exact-integer? s) (exact-integer? e) (<= s e)))
    (let loop ([s s] [e e])
      (when (< s e)
        (bitvector-swap! bv s e)
        (loop (+ s 1) (- e 1))))))

(define (bitvector-reverse-copy! target tstart src :optional (sstart 0) send)
  (let1 send (if (undefined? send) (bitvector-length src) send)
    (assume (and (exact-integer? sstart) (exact-integer? send) (<= sstart send)))
    (assume (exact-integer? tstart))
    (let loop ([i (- send 1)] [j tstart])
      (when (<= sstart i)
        (bitvector-set! target j (bitvector-ref/int src i))
        (loop (- i 1) (+ j 1))))))

;;; Conversion


  

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



