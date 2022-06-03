;;;
;;; Array operations.  This is superset of SRFI-25.
;;;
;;;  Copyright (c) 2002-2022  Shiro Kawai  <shiro@acm.org>
;;;  Copyright(C) 2004      by Alex Shinn  (foof@synthcode.com)
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

;; Conceptually, an array is a backing storage and a procedure to
;; map n-dimensional indices to an index of the backing storage.

(define-module gauche.array
  (use scheme.list)
  (use srfi-11)
  (use gauche.uvector)
  (use gauche.collection)
  (use gauche.sequence)
  (export <array-meta> <array>
          <u8array> <s8array> <u16array> <s16array> <u32array> <s32array>
          <u64array> <s64array> <f16array> <f32array> <f64array>
          array? make-array array-copy shape array array-rank
          array-start array-end array-ref array-set!
          share-array subarray array-equal?
          array-valid-index?  shape-valid-index?
          array-shape array-length array-size
          array-for-each-index shape-for-each
          array-for-each-index-by-dimension
          array-for-each array-every array-any
          tabulate-array array-retabulate!
          array-map array-map! array->vector array->list
          make-u8array make-s8array make-u16array make-s16array
          make-u32array make-s32array make-u64array make-s64array
          make-f16array make-f32array make-f64array
          u8array s8array u16array s16array u32array s32array
          u64array s64array f16array f32array f64array
          array-concatenate array-transpose array-rotate-90
          array-flip array-flip!
          identity-array array-inverse determinant determinant!
          array-mul array-expt
          array-div-left array-div-right
          array-add-elements array-add-elements!
          array-sub-elements array-sub-elements!
          array-negate-elements array-negate-elements!
          array-mul-elements array-mul-elements!
          array-div-elements array-div-elements!
          array-reciprocate-elements array-reciprocate-elements!
          pretty-print-array
          ))
(select-module gauche.array)

(autoload "gauche/matrix"
  array-concatenate array-transpose array-rotate-90 array-flip array-flip!
  identity-array array-inverse determinant determinant! array-mul array-expt
  array-div-left array-div-right
  array-add-elements array-add-elements!
  array-sub-elements array-sub-elements!
  array-negate-elements array-negate-elements!
  array-mul-elements array-mul-elements!
  array-div-elements array-div-elements!
  array-reciprocate-elements array-reciprocate-elements!
  pretty-print-array)

(define-class <array-meta> (<class>)
  ((backing-storage-creator :init-keyword :backing-storage-creator
                            :getter backing-storage-creator-of)
   (backing-storage-getter  :init-keyword :backing-storage-getter
                            :getter backing-storage-getter-of)
   (backing-storage-setter  :init-keyword :backing-storage-setter
                            :getter backing-storage-setter-of)
   (backing-storage-length  :init-keyword :backing-storage-length
                            :getter backing-storage-length-of))
  )

;; auto reader and writer for array-meta

(define-method initialize ((class <array-meta>) initargs)
  (next-method)
  (let ((name (class-name class)))
    (when name
      (define-method write-object ((self class) port)
        (format port "#,(~A ~S" name (array->list (array-shape self)))
        (array-for-each (cut format port " ~S" <>) self)
        (format port ")"))
      (define-reader-ctor name
        (^[sh . inits]
          (list-fill-array! (make-array-internal class (apply shape sh)) inits))))))


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
  (let ([get   (backing-storage-getter-of (class-of self))]
        [set   (backing-storage-setter-of (class-of self))]
        [store (backing-storage-of self)])
    (set! (slot-ref self 'getter) (^[index] (get store index)))
    (set! (slot-ref self 'setter) (^[index value] (set store index value)))))

(define-class <array> (<array-base>)
  ()
  :backing-storage-creator make-vector
  :backing-storage-getter vector-ref
  :backing-storage-setter vector-set!
  :backing-storage-length vector-length)

(define-class <u8array> (<array-base>)
  ()
  :backing-storage-creator make-u8vector
  :backing-storage-getter u8vector-ref
  :backing-storage-setter u8vector-set!
  :backing-storage-length u8vector-length)

(define-class <s8array> (<array-base>)
  ()
  :backing-storage-creator make-s8vector
  :backing-storage-getter s8vector-ref
  :backing-storage-setter s8vector-set!
  :backing-storage-length s8vector-length)

(define-class <u16array> (<array-base>)
  ()
  :backing-storage-creator make-u16vector
  :backing-storage-getter u16vector-ref
  :backing-storage-setter u16vector-set!
  :backing-storage-length u16vector-length)

(define-class <s16array> (<array-base>)
  ()
  :backing-storage-creator make-s16vector
  :backing-storage-getter s16vector-ref
  :backing-storage-setter s16vector-set!
  :backing-storage-length s16vector-length)

(define-class <u32array> (<array-base>)
  ()
  :backing-storage-creator make-u32vector
  :backing-storage-getter u32vector-ref
  :backing-storage-setter u32vector-set!
  :backing-storage-length u32vector-length)

(define-class <s32array> (<array-base>)
  ()
  :backing-storage-creator make-s32vector
  :backing-storage-getter s32vector-ref
  :backing-storage-setter s32vector-set!
  :backing-storage-length s32vector-length)

(define-class <u64array> (<array-base>)
  ()
  :backing-storage-creator make-u64vector
  :backing-storage-getter u64vector-ref
  :backing-storage-setter u64vector-set!
  :backing-storage-length u64vector-length)

(define-class <s64array> (<array-base>)
  ()
  :backing-storage-creator make-s64vector
  :backing-storage-getter s64vector-ref
  :backing-storage-setter s64vector-set!
  :backing-storage-length s64vector-length)

(define-class <f16array> (<array-base>)
  ()
  :backing-storage-creator make-f16vector
  :backing-storage-getter f16vector-ref
  :backing-storage-setter f16vector-set!
  :backing-storage-length f16vector-length)

(define-class <f32array> (<array-base>)
  ()
  :backing-storage-creator make-f32vector
  :backing-storage-getter f32vector-ref
  :backing-storage-setter f32vector-set!
  :backing-storage-length f32vector-length)

(define-class <f64array> (<array-base>)
  ()
  :backing-storage-creator make-f64vector
  :backing-storage-getter f64vector-ref
  :backing-storage-setter f64vector-set!
  :backing-storage-length f64vector-length)

;; internal
(define-inline (%xvector-copy v)
  (cond [(vector? v) (vector-copy v)]
        [(uvector? v) (uvector-copy v)]
        [else (error "Vector or uvector required, but got:" v)]))

(define (array-copy a)
  (assume-type a <array-base>)
  (make (class-of a)
    :start-vector (start-vector-of a)
    :end-vector   (end-vector-of a)
    :mapper       (mapper-of a)
    :backing-storage (%xvector-copy (backing-storage-of a))))

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
  (let* ([rank    (s32vector-length Vb)]
         [indices (iota rank)]
         [Vs      (s32vector-sub Ve Vb)]
         [-Vb     (map-to <s32vector> - Vb)]
         [Ve-1    (s32vector-sub Ve 1)]
         [vcl     (fold-right (^[sN l] (cons (* sN (car l)) l))
                              '(1)
                              (s32vector->list Vs))]
         [Vc      (coerce-to <s32vector> (cdr vcl))])
    (^[Vi]
      (cond [(s32vector-range-check Ve-1 Vi #f)
             => (^i (errorf "index of dimension ~s is too big: ~s"
                            i (ref Vi i)))]
            [(s32vector-range-check Vb #f Vi)
             => (^i (errorf "index of dimension ~s is too small: ~s"
                            i (ref Vi i)))]
            [else (s32vector-dot Vc (s32vector-add -Vb Vi))]))))

;; shape index tests

(define-method shape-valid-index? ((sh <array-base>) (ind <s32vector>))
  (receive (Vb Ve) (shape->start/end-vector sh)
    (and
     (= (s32vector-length ind) (s32vector-length Vb))
     (not (s32vector-range-check ind Vb (s32vector-sub Ve 1))))))

(define-method shape-valid-index? ((sh <array-base>) (ind <vector>))
  (shape-valid-index? sh (vector->s32vector ind)))
(define-method shape-valid-index? ((sh <array-base>) (ind <pair>))
  (shape-valid-index? sh (list->s32vector ind)))
(define-method shape-valid-index? ((sh <array-base>) (ind <array>))
  (shape-valid-index? sh (vector->s32vector (array->vector ind))))
(define-method shape-valid-index? ((sh <array-base>) (ind <integer>) . more-index)
  (shape-valid-index? sh (list->s32vector (cons ind more-index))))
(define-method shape-valid-index? ((sh <array-base>))
  ;; special case - zero dimensional array
  (null? (array->list sh)))

;; array index tests

(define (array-valid-index? ar . args)
  (apply shape-valid-index? (array-shape ar) args))

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
    (let* ([rank (quotient arglen 2)]
           [back (make-vector arglen)])
      ;; check vailidity of the shape
      (do ([l args (cddr l)])
          [(null? l)]
        (unless (and (exact? (car l)) (integer? (car l)))
          (error "exact integer required for shape, but got" (car l)))
        (unless (and (exact? (cadr l)) (integer? (cadr l)))
          (error "exact integer required for shape, but got" (cadr l)))
        (unless (<= (car l) (cadr l))
          (errorf "beginning index ~s is larger than ending index ~s in shape argument: ~s" (car l) (cadr l) args)))
      ;; make array.
      (make <array>
        :start-vector (s32vector 0 0)
        :end-vector (s32vector rank 2)
        :mapper (^[Vi] (s32vector-dot (s32vector 2 1) Vi))
        :backing-storage (list->vector args)))))

(define (shape->start/end-vector shape)
  (let* ([rank (array-end shape 0)]
         [cnt  (iota rank)])
    (values (map-to <s32vector> (^i (array-ref shape i 0)) cnt)
            (map-to <s32vector> (^i (array-ref shape i 1)) cnt))))

(define (start/end-vector->shape Vb Ve)
  (define (interleave a b)
    (cond [(null? a) b]
          [(null? b) a]
          [else (cons (car a) (interleave b (cdr a)))]))
  (apply shape (interleave (s32vector->list Vb) (s32vector->list Ve))))

;;---------------------------------------------------------------
;; Make general array
;;

(define (make-array-internal class shape . maybe-init)
  (receive (Vb Ve) (shape->start/end-vector shape)
    (make class
      :start-vector Vb
      :end-vector Ve
      :mapper (generate-amap Vb Ve)
      :backing-storage (apply (backing-storage-creator-of class)
                              (fold * 1 (s32vector-sub Ve Vb))
                              maybe-init))))

(define (list-fill-array! a inits)
  (let* ([bv  (backing-storage-of a)]
         [set (backing-storage-setter-of (class-of a))]
         [len ((backing-storage-length-of (class-of a)) bv)])
    (unless (= (length inits) len)
      (errorf "array element initializer doesn't match the shape ~s [~s]: ~s"
              (array->list (array-shape a)) len inits))
    (do ([i 0 (+ i 1)]
         [inits inits (cdr inits)])
        [(= i len) a]
      (set bv i (car inits)))))

(define-macro (define-array-ctors . pfxs)
  (define (build-ctor-def pfx)
    (define maker (symbol-append 'make- pfx 'array))
    (define ctor  (symbol-append pfx 'array))
    (define class (symbol-append '< pfx 'array>))
    `((define (,maker shape . opt)
        (apply make-array-internal ,class shape opt))
      (define (,ctor shape . inits)
        (list-fill-array! (,maker shape) inits))))
  `(begin ,@(append-map build-ctor-def pfxs)))

(define-array-ctors || u8 s8 u16 s16 u32 s32 u64 s64 f16 f32 f64)

(define (subarray ar sh)
  (receive (Vb Ve) (shape->start/end-vector sh)
    (let* ([rank (s32vector-length Vb)]
           [Vb2 (make-s32vector rank 0)]
           [Ve2 (s32vector-sub Ve Vb)]
           [new-shape (start/end-vector->shape Vb2 Ve2)]
           [res (make-array-internal (class-of ar) new-shape)])
      (array-retabulate! res new-shape
                         (^[ind] (array-ref ar (s32vector->vector
                                                (s32vector-add Vb ind))))
                         (make-vector rank))
      res)))

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
    (let ([cvecs (map (^_ (make-s32vector rank)) Cs)]
          [cnt   (iota rank)])
      (dotimes [i rank]
        (receive Ks (apply proc (map (^j (if (= j i) 1 0)) cnt))
          (for-each (^[v k c] (set! (ref v i) (- k c))) cvecs Ks Cs)))
      (values Cs cvecs))))

;; given calculated coefficients vectors and constants for affine mapper,
;; and the original mapping function, creates the new afiine mapper.
(define (generate-shared-map omap constants coeffs)
  (^[Vi] (omap (map (^[ci cvec] (+ ci (s32vector-dot cvec Vi)))
                    constants coeffs))))

(define (share-array array shape proc)
  (receive (Vb Ve) (shape->start/end-vector shape)
    (receive (constants coeffs) (affine-proc->coeffs proc (size-of Vb))
      (make (class-of array)
        :start-vector Vb
        :end-vector   Ve
        :mapper (generate-shared-map (mapper-of array) constants coeffs)
        :backing-storage (backing-storage-of array)))))

;;---------------------------------------------------------------
;; Array utilities
;;

(define (array-shape ar)
  (let1 r (array-rank ar)
    (apply array (shape 0 r 0 2)
           (append-map! (^k (list (array-start ar k) (array-end ar k)))
                        (iota r)))))

(define (array-length ar dim)
  (- (array-end ar dim) (array-start ar dim)))

(define (array-size ar)
  (reduce * 1 (map (cute array-length ar <>) (iota (array-rank ar)))))

(define (array-equal? a b :optional (eq equal?))
  (let1 r (array-rank a)
    (and (= r (array-rank b))
         (every (^[dim] (and (= (array-start a dim) (array-start b dim))
                             (= (array-end a dim) (array-end b dim))))
                (iota r))
         (let/cc break
           (array-for-each-index a
             (^[index] (unless (eq (array-ref a index)
                                   (array-ref b index))
                         (break #f)))
             (make-vector r))
           #t))))

(define-method object-equal? ((a <array-base>) (b <array-base>))
  (array-equal? a b equal?))

;; returns a proc that applies proc to indices that is given by a vector.

(define (array-index-applier rank)
  (case rank
    [(0) (^[proc vec] (proc))]
    [(1) (^[proc vec] (proc (vector-ref vec 0)))]
    [(2) (^[proc vec] (proc (vector-ref vec 0) (vector-ref vec 1)))]
    [(3) (^[proc vec]
           (proc (vector-ref vec 0) (vector-ref vec 1) (vector-ref vec 2)))]
    [else (^[proc vec] (apply proc (vector->list vec)))]))

(define (array-for-each proc ar)
  (for-each proc (backing-storage-of ar)))

(define (array-any pred ar)
  (let/cc found
    (for-each (^x (if (pred x) (found #t))) (backing-storage-of ar))
    #f))

(define (array-every pred ar)
  (let/cc found
    (for-each (^x (if (not (pred x)) (found #f))) (backing-storage-of ar))
    #t))

;; repeat construct

(define (array-for-each-int proc keep Vb Ve ind)

  (define i (if (pair? ind) (car ind) (make-vector (s32vector-length Vb))))
  (define applier #f)

  (define (list-loop dim k)
    (if (= dim (car k))
      (let ([e (s32vector-ref Ve dim)]
            [rest (cdr k)])
        (if (null? rest)
          (do ([k (s32vector-ref Vb dim) (+ k 1)])
              [(= k e)]
            (vector-set! i dim k)
            ;; use an applier
            (applier proc i))
          (do ([k (s32vector-ref Vb dim) (+ k 1)])
              [(= k e)]
            (vector-set! i dim k)
            (list-loop (+ dim 1) rest))))
      (list-loop (+ dim 1) k)))

  (define (helper-loop setter dimensions keep-ls)
    (let loop ([dim dimensions]
               [k keep-ls])
      (if (= dim (car k))
        ;; we loop over this dimension
        (let ([e (s32vector-ref Ve dim)]
              [rest (cdr k)])
          (if (null? rest)
            ;; inline last loop to avoid excess procedure calls
            (do ([k (s32vector-ref Vb dim) (+ k 1)])
                [(= k e)]
              (setter i dim k)
              (proc i))
            ;; set the index for this dimension and loop
            (do ([k (s32vector-ref Vb dim) (+ k 1)])
                [(= k e)]
              (setter i dim k)
              (loop (+ dim 1) rest))))
        ;; skip this dimension
        (loop (+ dim 1) k))))

  (unless (null? keep)
    (cond
      [(null? ind)
       (set! applier (array-index-applier (s32vector-length Vb)))
       (list-loop 0 keep)]
      [(vector? i)      (helper-loop vector-set! 0 keep)]
      [(array? i)       (helper-loop array-set! 0 keep)]
      [(s8vector? i)    (helper-loop s8vector-set! 0 keep)]
      [(s16vector? i)   (helper-loop s16vector-set! 0 keep)]
      [(s32vector? i)   (helper-loop s32vector-set! 0 keep)]
      [else "bad index object (vector or array required)" (car ind)])))

(define (array-for-each-index ar proc . o)
  (array-for-each-int
   proc
   (iota (array-rank ar))
   (start-vector-of ar)
   (end-vector-of ar)
   o))

(define (array-for-each-index-by-dimension ar keep proc . o)
  (array-for-each-int
   proc
   keep
   (start-vector-of ar)
   (end-vector-of ar)
   o))

(define (shape-for-each sh proc . o)
  (let* ([rank (array-end sh 0)]
         [ser  (iota rank)])
    (array-for-each-int
     proc
     ser
     (map-to <s32vector> (cut array-ref sh <> 0) ser)
     (map-to <s32vector> (cut array-ref sh <> 1) ser)
     o)))

(define (tabulate-array sh . args)
  (rlet1 res (make-array sh)
    (apply array-retabulate! res args)))

;; Mapping onto array.
;;   array-retabulate!
;;   array-map!
;;   array-map
;; These may take optional shape argument.  It is redundant, for the shape
;; has to match the target array's shape anyway.  In Jussi's reference
;; implementation, giving the shape allows some optimization.  In my
;; implementation it's not much of use.  I keep it just for compatibility
;; to Jussi's.

(define-method array-retabulate! ((ar <array-base>) (sh <array-base>) (proc <procedure>) . o)
  ;; need to check the shape sh matches the ar's shape.
  (apply array-retabulate! ar proc o))

(define-method array-retabulate! ((ar <array-base>) (proc <procedure>) . o)
  (let ([set (backing-storage-setter-of (class-of ar))]
        [store (backing-storage-of ar)]
        [mapper (mapper-of ar)])
    (cond [(null? o)
           (let1 applier (array-index-applier (array-rank ar))
             (array-for-each-index ar
               (^[ind] (set store (mapper ind) (applier proc ind)))
               (make-vector (array-rank ar))))]
          [(or (vector? (car o)) (array? (car o)))
           (array-for-each-index ar
             (^[ind] (set store (mapper ind) (proc ind)))
             (car o))]
          [else "bad index object (vector or array required)" (car o)])))

(define-method array-map! ((ar <array-base>) (sh <array-base>) (proc <procedure>) ar0 . more-arrays)
  ;; need to check the shape sh matches the ar's shape.
  (apply array-map! ar proc ar0 more-arrays))

(define-method array-map! ((ar <array-base>) (proc <procedure>) ar0)
  (let ([set (backing-storage-setter-of (class-of ar))]
        [store (backing-storage-of ar)]
        [mapper (mapper-of ar)]
        [get0 (backing-storage-getter-of (class-of ar0))]
        [store0 (backing-storage-of ar0)]
        [mapper0 (mapper-of ar0)])
    (array-for-each-index ar0
      (^[ind] (set store (mapper ind) (proc (get0 store0 (mapper0 ind)))))
      (make-vector (array-rank ar)))))

(define-method array-map! ((ar <array-base>) (proc <procedure>) ar0 ar1)
  (let ([set (backing-storage-setter-of (class-of ar))]
        [store (backing-storage-of ar)]
        [mapper (mapper-of ar)]
        [get0 (backing-storage-getter-of (class-of ar0))]
        [store0 (backing-storage-of ar0)]
        [mapper0 (mapper-of ar0)]
        [get1 (backing-storage-getter-of (class-of ar1))]
        [store1 (backing-storage-of ar1)]
        [mapper1 (mapper-of ar1)])
    (array-for-each-index ar0
      (^[ind] (set store (mapper ind) (proc (get0 store0 (mapper0 ind))
                                            (get1 store1 (mapper1 ind)))))
      (make-vector (array-rank ar)))))

(define-method array-map! ((ar <array-base>) (proc <procedure>) ar0 ar1 ar2 . more)
  (let1 arlist (list* ar0 ar1 ar2 more)
    (array-for-each-index ar
      (^[ind] (array-set! ar ind
                          (apply proc
                                 (map (cut array-ref <> ind) arlist))))
      (make-vector (array-rank ar)))))

(define-method array-map ((sh <array-base>) (proc <procedure>) ar0 . more)
  (apply array-map proc ar0 more))

(define-method array-map ((proc <procedure>) ar0 . more)
  (rlet1 target (make-array (array-shape ar0))
    (apply array-map! target proc ar0 more)))

(define (array->vector ar)
  (with-builder (<vector> add! get :size (array-size ar))
    (array-for-each-index ar
      (^[ind] (add! (array-ref ar ind)))
      (make-vector (array-rank ar)))
    (get)))

(define (array->list ar)
  (with-builder (<list> add! get)
    (array-for-each-index ar
      (^[ind] (add! (array-ref ar ind)))
      (make-vector (array-rank ar)))
    (get)))
