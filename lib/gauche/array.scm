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
;;;  $Id: array.scm,v 1.2 2002-06-25 11:51:08 shirok Exp $
;;;

;; Conceptually, an array is a backing storage and a procedure to
;; map n-dimensional indices to an index of the backing storage.

(define-module gauche.array
  (use srfi-1)
  (use srfi-4)
  (use gauche.collection)
  (use gauche.sequence)
  (use gauche.let-opt)
  (export <array-meta> <array>
          array? make-array shape array array-rank
          array-start array-end array-ref array-set!
          share-array)
  )
(select-module gauche.array)

(define-class <array-meta> (<class>)
  ((backing-storage-creator :init-keyword :backing-storage-creator
                            :getter backing-storage-creator-of)
   (backing-storage-getter  :init-keyword :backing-storage-getter
                            :getter backing-storage-getter-of)
   (backing-storage-setter  :init-keyword :backing-storage-setter
                            :setter backing-storage-setter-of))
  )

(define-class <array-base> ()
  ((start-vector    :init-keyword :start-vector :getter start-vector-of)
   (end-vector      :init-keyword :end-vector   :getter end-vector-of)
   (mapper          :init-keyword :mapper       :getter mapper-of)
   (backing-storage :init-keyword :backing-storage
                    :getter backing-storage-of))
  :metaclass <array-meta>)

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
             (s32vector-dot Vc Vi))))))

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
  (let* ((shape-vec  (backing-storage-of shape))
         (shape-size (vector-length shape-vec)))
    (with-builder (<s32vector> add-vb! get-vb)
      (with-builder (<s32vector> add-ve! get-ve)
        (do ((i 0 (+ i 2)))
            ((= i shape-size)
             (values (get-vb) (get-ve)))
          (add-vb! (vector-ref shape-vec i))
          (add-ve! (vector-ref shape-vec (+ i 1))))))))

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

(define (array-ref array . indices)
  ((backing-storage-getter-of (class-of array))
   (backing-storage-of array)
   (apply (mapper-of array) indices)))

(provide "gauche/array")
