;;;
;;; Array operations.  This is superset of SRFI-25.
;;;
;;;  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: array.scm,v 1.2 2002-06-28 09:56:32 shirok Exp $
;;;

;; Conceptually, an array is a backing storage and a procedure to
;; map n-dimensional indices to an index of the backing storage.

(define-module gauche.array
  (use srfi-1)
  (use srfi-11)
  (use gauche.uvector)
  (use gauche.collection)
  (use gauche.sequence)
  (use gauche.let-opt)
  (export <array-meta> <array>
          array? make-array shape array array-rank
          array-start array-end array-ref array-set!
          share-array
          array-shape array-length array-size array-equal?
          array-for-each-index shape-for-each-index
          )
  )
(select-module gauche.array)

(define-class <array-meta> (<class>)
  ((backing-storage-creator :init-keyword :backing-storage-creator
                            :getter backing-storage-creator-of)
   (backing-storage-getter  :init-keyword :backing-storage-getter
                            :getter backing-storage-getter-of)
   (backing-storage-setter  :init-keyword :backing-storage-setter
                            :getter backing-storage-setter-of))
  )

(define-class <array-base> ()
  ((start-vector    :init-keyword :start-vector :getter start-vector-of)
   (end-vector      :init-keyword :end-vector   :getter end-vector-of)
   (mapper          :init-keyword :mapper       :getter mapper-of)
   (getter          :getter getter-of)
   (setter          :getter setter-of)
   (backing-storage :init-keyword :backing-storage
                    :getter backing-storage-of))
  :metaclass <array-meta>)

(define-method initialize ((self <array-base>) initargs)
  (next-method)
  (let ((get   (backing-storage-getter-of (class-of self)))
        (set   (backing-storage-setter-of (class-of self)))
        (store (backing-storage-of self)))
    (set! (slot-ref self 'getter)
          (lambda (index) (get store index)))
    (set! (slot-ref self 'setter)
          (lambda (index value) (set store index value)))
    ))

(define-class <array> (<array-base>)
  ()
  :backing-storage-creator make-vector
  :backing-storage-getter vector-ref
  :backing-storage-setter vector-set!)

(define-method copy-object ((self <array-base>))
  (make (class-of self)
    :start-vector (start-vector-of self)
    :end-vector   (end-vector-of self)
    :mapper       (mapper-of self)
    :backing-storage (copy-object (backing-storage-of self))))

;; NB: these should be built-in; but here for now.
(define-method copy-object ((self <vector>))
  (vector-copy self))

;;-------------------------------------------------------------
;; Affine mapper
;;
;;  Given begin-vector Vb = #s32(b0 b1 ... bN)
;;        end-vector   Ve = #s32(e0 e1 ... eN)
;;        where b0 <= e0, ..., bN <= eN
;;  Returns a procedure,
;;    which calculates 1-dimentional offset off to the backing storage,
;;          from given index vector #s32(i0 i1 ... iN)
;;
;;  Pre-calculation:
;;    sizes          s0 = e0 - b0, s1 = e1 - b1 ...
;;    coefficients   c0   = s1 * s2 * ... * sN
;;                   c1   = s2 * ... * sN
;;                   cN-1 = sN
;;                   cN   = 1
;;
;;  Mapping
;;   off = c0*(i0-b0) + c1*(i1-b1) + .. + cN*(iN-bN)
;;
;;  

(define (zero-vector? vec)
  (not (s32vector-range-check vec 0 0)))

(define (generate-amap Vb Ve)
  (let* ((rank    (s32vector-length Vb))
         (indices (iota rank))
         (Vs      (s32vector-sub Ve Vb))
         (-Vb     (map-to <s32vector> - Vb))
         (vcl     (fold-right (lambda (sN l) (cons (* sN (car l)) l))
                              '(1)
                              (s32vector->list Vs)))
         (Vc      (coerce-to <s32vector> (cdr vcl)))
         )
    (lambda (Vi)
      (cond ((s32vector-range-check Ve Vi #f)
             => (lambda (i)
                  (errorf "index of dimension ~s is too big: ~s"
                          i (ref Vi i))))
            ((s32vector-range-check Vb #f Vi)
             => (lambda (i)
                  (errorf "index of dimension ~s is too small: ~s"
                          i (ref Vi i))))
            (else
             (s32vector-dot Vc (s32vector-add -Vb Vi)))))))

;;---------------------------------------------------------------
;; Shape
;;   ... is a special array that has a few constraints;
;;   that is, suppose S is a shape, then:
;;   (1) the shape of S is [0, d, 0, 2], where d is the rank
;;       of the array S represents.
;;   (2) all the elements of S are exact integers.
;;   (3) (array-ref S n 0) <= (array-ref S n 1) where 0 <= n <= d
;;

(define (shape . args)
  (let1 arglen (length args)
    (unless (even? arglen)
      (error "shape arguments not even" args))
    (let* ((rank (quotient arglen 2))
           (back (make-vector arglen)))
      ;; check vailidity of the shape
      (do ((l args (cddr l)))
          ((null? l))
        (unless (and (exact? (car l)) (integer? (car l)))
          (error "exact integer required for shape, but got" (car l)))
        (unless (and (exact? (cadr l)) (integer? (cadr l)))
          (error "exact integer required for shape, but got" (cadr l)))
        (unless (<= (car l) (cadr l))
          (error "beginning index ~s is larger than ending index ~s in shape argument: ~s" (car l) (cadr l) args)))
      ;; make array.
      (make <array>
        :start-vector (s32vector 0 0)
        :end-vector (s32vector rank 2)
        :mapper (lambda (Vi) (s32vector-dot (s32vector 2 1) Vi))
        :backing-storage (list->vector args))
      )))

(define (shape->start/end-vector shape)
  (let* ((rank (array-end shape 0))
         (cnt  (iota rank)))
    (values (map-to <s32vector> (lambda (i) (array-ref shape i 0)) cnt)
            (map-to <s32vector> (lambda (i) (array-ref shape i 1)) cnt))))

;;---------------------------------------------------------------
;; Make general array
;;

(define (make-array shape . maybe-init)
  (receive (Vb Ve) (shape->start/end-vector shape)
    (make <array>
      :start-vector Vb
      :end-vector Ve
      :mapper (generate-amap Vb Ve)
      :backing-storage (apply make-vector
                              (fold * 1 (s32vector-sub Ve Vb))
                              maybe-init))))

(define (array shape . inits)
  (let* ((a   (make-array shape))
         (bv  (backing-storage-of a))
         (len (vector-length bv)))
    (do ((i 0 (+ i 1))
         (inits inits (cdr inits)))
        ((= i len) a)
      (vector-set! bv i (car inits)))))

(define (array? obj)
  (is-a? obj <array-base>))

(define (array-rank array)
  (unless (array? array) (error "array required, but got" array))
  (s32vector-length (start-vector-of array)))

(define (array-start array k)
  (unless (array? array) (error "array required, but got" array))
  (s32vector-ref (start-vector-of array) k))

(define (array-end array k)
  (unless (array? array) (error "array required, but got" array))
  (s32vector-ref (end-vector-of array) k))

;;---------------------------------------------------------------
;; Array ref and set!
;;

(define-method array-ref ((array <array-base>) (index <integer>) . more-index)
  ((getter-of array)
   ((mapper-of array) (cons index more-index))))

(define-method array-ref ((array <array-base>) (index <vector>))
  ((getter-of array) ((mapper-of array) index)))

(define-method array-ref ((array <array-base>) (index <array>))
  (unless (= (array-rank index) 1)
    (error "index array must be rank 1" index))
  ;; This is slow, but we don't know if the index array using 
  ;; standard mapping.
  ((getter-of array)
   ((mapper-of array)
    (map (pa$ array-ref index)
         (iota (- (array-end index 0) (array-start index 0))
               (array-start index 0))))))

(define-method array-ref ((array <array-base>))
  ;; special case - zero dimensional array
  ((getter-of array) ((mapper-of array) '())))

(define-method array-set! ((array <array-base>) (index <integer>) . more-index)
  (receive (indices value)
      (split-at! (cons index more-index) (length more-index))
    ((setter-of array) ((mapper-of array) indices) (car value))))

(define-method array-set! ((array <array-base>) (index <vector>) value)
  ((setter-of array) ((mapper-of array) index) value))

(define-method array-set! ((array <array-base>) (index <array>) value)
  (unless (= (array-rank index) 1)
    (error "index array must be rank 1" index))
  ;; This is slow, but we don't know if the index array using 
  ;; standard mapping.
  ((setter-of array)
   ((mapper-of array)
    (map (pa$ array-ref index)
         (iota (- (array-end index 0) (array-start index 0))
               (array-start index 0))))
   value))

(define-method array-set! ((array <array-base>) value)
  ;; special case - zero dimensional array
  ((setter-of array) ((mapper-of array) '()) value))

;;---------------------------------------------------------------
;; Share array
;;

;; Given proc, calculates coefficients of affine mapper
;; Rank is the rank of the new array returned by share-array, which is
;; the same as the arity of proc.

(define (affine-proc->coeffs proc rank)
  (receive Cs (apply proc (make-list rank 0))
    (let ((cvecs (map (lambda (_) (make-s32vector rank)) Cs))
          (cnt   (iota rank)))
      (dotimes (i rank)
        (receive Ks (apply proc (map (lambda (j) (if (= j i) 1 0)) cnt))
          (for-each (lambda (v k c) (set! (ref v i) (- k c))) cvecs Ks Cs)))
      (values Cs cvecs))))

;; given calculated coefficients vectors and constants for affine mapper,
;; and the original mapping function, creates the new afiine mapper.
(define (generate-shared-map omap constants coeffs)
  (lambda (Vi)
    (omap (map (lambda (ci cvec)
                 (+ ci (s32vector-dot cvec Vi)))
               constants coeffs))))

(define (share-array array shape proc)
  (let*-values (((Vb Ve) (shape->start/end-vector shape))
                ((constants coeffs)
                 (affine-proc->coeffs proc (size-of Vb)))
                )
    (make <array>
      :start-vector Vb
      :end-vector   Ve
      :mapper (generate-shared-map (mapper-of array) constants coeffs)
      :backing-storage (backing-storage-of array))))

;;---------------------------------------------------------------
;; Array utilities
;;

(define (array-shape ar)
  (let1 r (array-rank ar)
    (apply array (shape 0 r 0 2)
           (append-map! (lambda (k)
                          (list (array-start ar k)
                                (array-end ar k)))
                        (iota r)))))

(define (array-length ar dim)
  (- (array-end ar dim) (array-start ar dim)))

(define (array-size ar)
  (reduce * 1 (map (cute array-length ar <>) (iota (array-rank ar)))))

(define (array-equal? a b)
  (let ((r   (array-rank a)))
    (and (= r (array-rank b))
         (every (lambda (dim)
                  (and (= (array-start a dim) (array-start b dim))
                       (= (array-end a dim) (array-end b dim))))
                (iota r))
         (call/cc
          (lambda (break)
            (array-for-each-index a
                                  (lambda (index)
                                    (unless (equal? (array-ref a index)
                                                    (array-ref b index))
                                      (break #f)))
                                  (make-vector r 0))
            #t)))))

;; repeat construct

(define (array-for-each-int proc rank Vb Ve ind)
  (define (vec-loop dim vi)
    (if (= dim rank)
        (proc vi)
        (let1 e (s32vector-ref Ve dim)
          (do ((k (s32vector-ref Vb dim) (+ k 1)))
              ((= k e))
            (vector-set! vi dim k)
            (vec-loop (+ dim 1) vi)))))

  (define (arr-loop dim ai)
    (if (= dim rank)
        (proc ai)
        (let1 e (s32vector-ref Ve dim)
          (do ((k (s32vector-ref Vb dim) (+ k 1)))
              ((= k e))
            (array-set! ai dim k)
            (arr-loop (+ dim 1) ai)))))
  
  (cond ((null? ind) (vec-loop 0 (make-vector rank)))
        ((vector? (car ind)) (vec-loop 0 (car ind)))
        ((array? (car ind)) (arr-loop 0 (car ind)))
        (else "bad index object (vector or array required)" (car ind))))

(define (array-for-each-index ar proc . o)
  (array-for-each-int proc
                      (array-rank ar)
                      (start-vector-of ar)
                      (end-vector-of ar)
                      o))

(define (shape-for-each-index sh proc . o)
  (let* ((rank (array-end sh 0))
         (ser  (iota rank)))
    (array-for-each-int proc
                        rank
                        (map-to <s32vector> (cut array-ref sh <> 0) ser)
                        (map-to <s32vector> (cut array-ref sh <> 1) ser)
                        o)))

(provide "gauche/array")
