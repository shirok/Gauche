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

;; Predicates

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

;; Basic operations

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



