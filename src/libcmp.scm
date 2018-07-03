;;;
;;; libcmp.scm - compare and sort
;;;
;;;   Copyright (c) 2000-2018  Shiro Kawai  <shiro@acm.org>
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

(inline-stub
 (declcode (.include "gauche/class.h")))

;;;
;;; Comparator (a la srfi-114/128)
;;;

;; Our built-in comparator is compatible to *both* srfi-114 and srfi-128;
;; we automatically "fill-in" the difference (comparison-proc vs ordering-pred)
;; as needed.  The only incompatibility is the constructor---both srfi
;; uses the same name but the mearning of arguments differ.  We provide
;; srfi-114 constructor under different name (make-comparator/compare).
;;
;; Because of this on-demand filling, the internal accessors are a
;; bit complicated; it is justified by the simple api with low overhead
;; (we don't calculate until needed).
;;
;; Note: Some builtin comparator stuff are in libomega.scm, for they
;; depends on other builtin mechanisms.

(select-module gauche.internal)
(define (default-type-test _) #t)

(define-cproc %make-comparator (type-test equality-test
                                comparison-proc ; or order-proc
                                hash name 
                                any-type::<boolean> use-cmp::<boolean>
                                srfi-128::<boolean>)
  (let* ([flags::u_long (logior (?: srfi-128 SCM_COMPARATOR_SRFI_128 0)
                                (?: (SCM_EQ comparison-proc SCM_FALSE)
                                    SCM_COMPARATOR_NO_ORDER 0)
                                (?: (SCM_EQ hash SCM_FALSE)
                                    SCM_COMPARATOR_NO_HASH 0)
                                (?: any-type SCM_COMPARATOR_ANY_TYPE 0)
                                (?: use-cmp SCM_COMPARATOR_USE_COMPARISON 0))])
    (return
     (Scm_MakeComparator type-test equality-test comparison-proc hash
                         name flags))))

;; Argument checkers for consturctors.
;; We use <bottom> for applicability check except type-test, since
;; those procs are only required to handle objects that passes type-test.
(define (ensure-type-test type-test)
  (cond [(eq? type-test #t) default-type-test]
        [(applicable? type-test <top>) type-test]
        [else (error "make-comparator needs a one-argument procedure or #t as type-test, but got:" type-test)]))
(define (ensure-equality-test equality-test comparison-proc)
  (cond [(eq? equality-test #t)
         (if (applicable? comparison-proc <bottom> <bottom>)
           (^[a b] (= (comparison-proc a b) 0))
           (error "make-comparator needs a procedure as comparison-proc if equality-test is #t, but got:" comparison-proc))]
        [(applicable? equality-test <bottom> <bottom>) equality-test]
        [else (error "make-comparator needs a procedure or #t as equality-test, but got:" equality-test)]))
(define (ensure-hash-func hash)
  (cond [(or (eq? hash #f) (applicable? hash <bottom>)) hash]
        [else (error "make-comparator needs a procedure or #f as hash, but got:" hash)])) 

;; API - srfi-114 constructor
(define-in-module gauche (make-comparator/compare type-test equality-test
                                                  comparison-proc hash
                                                  :optional (name #f))
  (let1 type (ensure-type-test type-test)
    (%make-comparator type
                      (ensure-equality-test equality-test comparison-proc)
                      (if (or (eq? comparison-proc #f)
                              (applicable? comparison-proc <bottom> <bottom>))
                        comparison-proc
                        (error "make-comparator/compare needs a procedure \
                                or #f as comparison-proc, but got:"
                               comparison-proc))
                      (ensure-hash-func hash)
                      name
                      (eq? type default-type-test)
                      (eq? equality-test #t)
                      #f)))

;; API - srfi-128 constructor
(define-in-module gauche (make-comparator type-test equality-test
                                          ordering-pred hash
                                          :optional (name #f))
  (let1 type (ensure-type-test type-test)
    (%make-comparator type
                      (ensure-equality-test equality-test #f)
                      (if (or (eq? ordering-pred #f)
                              (applicable? ordering-pred <bottom> <bottom>))
                        ordering-pred
                        (error "make-comparator needs a procedure or #f as \
                                ordering-pred, but got:" ordering-pred))
                      (ensure-hash-func hash)
                      name
                      (eq? type default-type-test)
                      (eq? equality-test #t)
                      #t)))

(select-module gauche)
(define-cproc comparator? (obj) ::<boolean> SCM_COMPARATORP)
(define-cproc comparator-flavor (c::<comparator>) :constant
  (if (logand (-> c flags) SCM_COMPARATOR_SRFI_128)
    (return 'ordering)
    (return 'comparison)))

(define-cproc comparator-ordered? (c::<comparator>) ::<boolean>
  (return (not (logand (-> c flags) SCM_COMPARATOR_NO_ORDER))))
(define-cproc comparator-hashable? (c::<comparator>) ::<boolean>
  (return (not (logand (-> c flags) SCM_COMPARATOR_NO_HASH))))
(define-cproc comparator-type-test-predicate (c::<comparator>) :constant
  (return (-> c typeFn)))
(define-cproc comparator-equality-predicate (c::<comparator>) :constant
  (return (-> c eqFn)))

;; comparator-comparison-procedure
;; For SRFI-128 comparator, we auto-generate comparison procedure.
(define-cproc comparator-comparison-procedure (c::<comparator>) :constant
  Scm_ComparatorComparisonProcedure)

(inline-stub
 (define-cfn fallback-compare (argv::ScmObj* argc::int data::void*) :static
   (let* ([c::ScmComparator* (SCM_COMPARATOR data)]
          [a (aref argv 0)]
          [b (aref argv 1)])
     (unless (logand (-> c flags) SCM_COMPARATOR_SRFI_128) (goto err))
     (when (SCM_FALSEP (-> c orderFn)) (goto err))
     (let1/cps r (Scm_VMApply2 (-> c orderFn) a b)
       [c a b]
       (if (not (SCM_FALSEP r))
         (return (SCM_MAKE_INT -1))
         (let1/cps r (Scm_VMApply2 (-> (SCM_COMPARATOR c) eqFn) a b)
           []
           (if (not (SCM_FALSEP r))
             (return (SCM_MAKE_INT 0))
             (return (SCM_MAKE_INT 1))))))
     (label err)
     (Scm_Error "can't compare objects by %S: %S vs %S" (SCM_OBJ c) a b)
     (return SCM_UNDEFINED)))             ;dummy

 (define-cfn Scm_ComparatorComparisonProcedure (c::ScmComparator*)
   (let* ([cmp (-> c compareFn)])
     (when (SCM_FALSEP cmp)
       (set! cmp (Scm_MakeSubr fallback-compare (cast (void*) c) 2 0
                               '(fallback-compare a b)))
       (set! (-> c compareFn) cmp))
     (return cmp)))
 )

;; comparator-ordering-predicate
;; For SRFI-114 comparator, we auto-generate ordering predicate.
(define-cproc comparator-ordering-predicate (c::<comparator>) :constant
  Scm_ComparatorOrderingPredicate)
  
(inline-stub
 (define-cfn fallback-order (argv::ScmObj* argc::int data::void*) :static
   (let* ([c::ScmComparator* (SCM_COMPARATOR data)]
          [a (aref argv 0)]
          [b (aref argv 1)])
     (when (logand (-> c flags) SCM_COMPARATOR_SRFI_128) (goto err))
     (when (SCM_FALSEP (-> c compareFn)) (goto err))
     (let1/cps r (Scm_VMApply2 (-> c compareFn) a b)
       []
       (return (SCM_MAKE_BOOL (< (SCM_INT_VALUE r) 0))))
     (label err)
     (Scm_Error "can't order objects by %S: %S vs %S" (SCM_OBJ c) a b)
     (return SCM_UNDEFINED)))           ;dummy

 (define-cfn Scm_ComparatorOrderingPredicate (c::ScmComparator*)
   (let* ([ord (-> c orderFn)])
     (when (SCM_FALSEP ord)
       (set! ord (Scm_MakeSubr fallback-order (cast (void*) c) 2 0
                               '(fallback-order a b)))
       (set! (-> c orderFn) ord))
     (return ord)))
 )
 
;; 
(define-cproc comparator-hash-function (c::<comparator>) :constant
  Scm_ComparatorHashFunction)

(inline-stub
 (define-cfn fallback-hash (argv::ScmObj* argc::int data::void*) :static
   (let* ([c::ScmComparator* (SCM_COMPARATOR data)])
     (Scm_Error "%S doesn't have hash function" (SCM_OBJ c))
     (return SCM_UNDEFINED)))           ;dummy

 (define-cfn Scm_ComparatorHashFunction (c::ScmComparator*)
   (let* ([h (-> c hashFn)])
     (when (SCM_FALSEP h)
       (set! h (Scm_MakeSubr fallback-hash (cast (void*) c) 1 0
                             '(fallback-has obj)))
       (set! (-> c hashFn) h))
     (return h)))
 )

;;;
;;; Comparator comparison operators
;;;   They can be written more concisely in Scheme, but we don't want
;;;   them to be too slow compared to bare procedures (e.g. '=', '<' etc)
;;;   so we fiddle with C.
;;;

;;; =?
(inline-stub
 (define-cfn cmpr-eq (c::ScmComparator* a b more) :static
   (if (SCM_PAIRP more)
     (let1/cps r (Scm_VMApply2 (-> c eqFn) a b)
       [c::ScmComparator* b more]
       (if (SCM_FALSEP r)
         (return r)
         (return (cmpr-eq c b (SCM_CAR more) (SCM_CDR more)))))
     (return (Scm_VMApply2 (-> c eqFn) a b))))

 (define-cproc =? (c::<comparator> a b :rest more)
   (return (cmpr-eq c a b more))))

;;; <?
(inline-stub
 (define-cfn cmpr-lt-128 (c::ScmComparator* a b more) :static
   (if (SCM_PAIRP more)
     (let1/cps r (Scm_VMApply2 (-> c orderFn) a b)
       [c::ScmComparator* b more]
       (if (SCM_FALSEP r)
         (return r)
         (return (cmpr-lt-128 c b (SCM_CAR more) (SCM_CDR more)))))
     (return (Scm_VMApply2 (-> c orderFn) a b))))

 (define-cfn cmpr-lt-114 (c::ScmComparator* a b more) :static
   (if (SCM_PAIRP more)
     (let1/cps r (Scm_VMApply2 (-> c compareFn) a b)
       [c::ScmComparator* b more]
       (if (< (SCM_INT_VALUE r) 0)
         (return (cmpr-lt-114 c b (SCM_CAR more) (SCM_CDR more)))
         (return SCM_FALSE)))
     (let1/cps r (Scm_VMApply2 (-> c compareFn) a b)
       []
       (return (SCM_MAKE_BOOL (< (SCM_INT_VALUE r) 0))))))

 (define-cproc <? (c::<comparator> a b :rest more)
   (cond [(logand (-> c flags) SCM_COMPARATOR_SRFI_128)
          (when (SCM_FALSEP (-> c orderFn))
            (Scm_Error "%S doesn't have ordering predicate" (SCM_OBJ c)))
          (return (cmpr-lt-128 c a b more))]
         [else
          (when (SCM_FALSEP (-> c compareFn))
            (Scm_Error "%S doesn't have comparison procedure" (SCM_OBJ c)))
          (return (cmpr-lt-114 c a b more))])))

;;; <=?
(inline-stub
 (define-cfn cmpr-le-128 (c::ScmComparator* a b more) :static
   (if (SCM_PAIRP more)
     (let1/cps r (Scm_VMApply2 (-> c orderFn) a b)
       [c::ScmComparator* a b more]
       (if (SCM_FALSEP r)
         (let1/cps r (Scm_VMApply2 (-> c eqFn) a b)
           [c::ScmComparator* b more]
           (if (SCM_FALSEP r)
             (return r)
             (return (cmpr-le-128 c b (SCM_CAR more) (SCM_CDR more)))))
         (return (cmpr-le-128 c b (SCM_CAR more) (SCM_CDR more)))))
     (let1/cps r (Scm_VMApply2 (-> c orderFn) a b)
       [c::ScmComparator* a b]
       (if (SCM_FALSEP r)
         (return (Scm_VMApply2 (-> c eqFn) a b))
         (return SCM_TRUE)))))

 (define-cfn cmpr-le-114 (c::ScmComparator* a b more) :static
   (if (SCM_PAIRP more)
     (let1/cps r (Scm_VMApply2 (-> c compareFn) a b)
       [c::ScmComparator* b more]
       (if (<= (SCM_INT_VALUE r) 0)
         (return (cmpr-le-114 c b (SCM_CAR more) (SCM_CDR more)))
         (return SCM_FALSE)))
     (let1/cps r (Scm_VMApply2 (-> c compareFn) a b)
       []
       (return (SCM_MAKE_BOOL (<= (SCM_INT_VALUE r) 0))))))

 (define-cproc <=? (c::<comparator> a b :rest more)
   (cond [(logand (-> c flags) SCM_COMPARATOR_SRFI_128)
          (when (SCM_FALSEP (-> c orderFn))
            (Scm_Error "%S doesn't have ordering predicate" (SCM_OBJ c)))
          (return (cmpr-le-128 c a b more))]
         [else
          (when (SCM_FALSEP (-> c compareFn))
            (Scm_Error "%S doesn't have comparison procedure" (SCM_OBJ c)))
          (return (cmpr-le-114 c a b more))])))

;;; >?
(inline-stub
 (define-cfn cmpr-gt-128 (c::ScmComparator* a b more) :static
   (if (SCM_PAIRP more)
     (let1/cps r (Scm_VMApply2 (-> c orderFn) a b)
       [c::ScmComparator* a b more]
       (if (SCM_FALSEP r)
         (let1/cps r (Scm_VMApply2 (-> c eqFn) a b)
           [c::ScmComparator* b more]
           (if (SCM_FALSEP r)
             (return (cmpr-gt-128 c b (SCM_CAR more) (SCM_CDR more)))
             (return SCM_FALSE)))
         (return SCM_FALSE)))
     (let1/cps r (Scm_VMApply2 (-> c orderFn) a b)
       [c::ScmComparator* a b]
       (if (SCM_FALSEP r)
         (let1/cps r (Scm_VMApply2 (-> c eqFn) a b)
           []
           (return (SCM_MAKE_BOOL (SCM_FALSEP r))))
         (return SCM_FALSE)))))

 (define-cfn cmpr-gt-114 (c::ScmComparator* a b more) :static
   (if (SCM_PAIRP more)
     (let1/cps r (Scm_VMApply2 (-> c compareFn) a b)
       [c::ScmComparator* b more]
       (if (> (SCM_INT_VALUE r) 0)
         (return (cmpr-gt-114 c b (SCM_CAR more) (SCM_CDR more)))
         (return SCM_FALSE)))
     (let1/cps r (Scm_VMApply2 (-> c compareFn) a b)
       []
       (return (SCM_MAKE_BOOL (> (SCM_INT_VALUE r) 0))))))

 (define-cproc >? (c::<comparator> a b :rest more)
   (cond [(logand (-> c flags) SCM_COMPARATOR_SRFI_128)
          (when (SCM_FALSEP (-> c orderFn))
            (Scm_Error "%S doesn't have ordering predicate" (SCM_OBJ c)))
          (return (cmpr-gt-128 c a b more))]
         [else
          (when (SCM_FALSEP (-> c compareFn))
            (Scm_Error "%S doesn't have comparison procedure" (SCM_OBJ c)))
          (return (cmpr-gt-114 c a b more))])))

;;; >=?
(inline-stub
 (define-cfn cmpr-ge-128 (c::ScmComparator* a b more) :static
   (if (SCM_PAIRP more)
     (let1/cps r (Scm_VMApply2 (-> c orderFn) a b)
       [c::ScmComparator* b more]
       (if (SCM_FALSEP r)
         (return (cmpr-ge-128 c b (SCM_CAR more) (SCM_CDR more)))
         (return SCM_FALSE)))
     (let1/cps r (Scm_VMApply2 (-> c orderFn) a b)
       [c::ScmComparator* a b]
       (return (SCM_MAKE_BOOL (SCM_FALSEP r))))))

 (define-cfn cmpr-ge-114 (c::ScmComparator* a b more) :static
   (if (SCM_PAIRP more)
     (let1/cps r (Scm_VMApply2 (-> c compareFn) a b)
       [c::ScmComparator* b more]
       (if (>= (SCM_INT_VALUE r) 0)
         (return (cmpr-ge-114 c b (SCM_CAR more) (SCM_CDR more)))
         (return SCM_FALSE)))
     (let1/cps r (Scm_VMApply2 (-> c compareFn) a b)
       []
       (return (SCM_MAKE_BOOL (>= (SCM_INT_VALUE r) 0))))))

 (define-cproc >=? (c::<comparator> a b :rest more)
   (cond [(logand (-> c flags) SCM_COMPARATOR_SRFI_128)
          (when (SCM_FALSEP (-> c orderFn))
            (Scm_Error "%S doesn't have ordering predicate" (SCM_OBJ c)))
          (return (cmpr-ge-128 c a b more))]
         [else
          (when (SCM_FALSEP (-> c compareFn))
            (Scm_Error "%S doesn't have comparison procedure" (SCM_OBJ c)))
          (return (cmpr-ge-114 c a b more))])))

(select-module gauche.internal)
;; Used by (object-equal? <comparator> <comparator>), defined in libomega.scm
(define-cproc comparator-equality-use-comparison? (c::<comparator>) ::<boolean>
  (return (logand (-> c flags) SCM_COMPARATOR_USE_COMPARISON)))

;; Expose as a class
(select-module gauche)
(inline-stub
 (define-cclass <comparator>
   "ScmComparator*" "Scm_ComparatorClass"
   (c "SCM_CLASS_DEFAULT_CPL")
   ((name          :setter #f)
    (type-test     :c-name "typeFn" :setter #f)
    (equality-test :c-name "eqFn" :setter #f)
    (ordering      :c-name "orderFn" :setter #f)
    (comparison    :c-name "compareFn" :setter #f)
    (hash          :c-name "hashFn" :setter #f))
   (printer (let* ([c::ScmComparator* (SCM_COMPARATOR obj)])
              (if (SCM_FALSEP (-> c name))
                (Scm_Printf port "#<comparator %p>" c)
                (Scm_Printf port "#<comparator %S>" (-> c name)))))
   (comparer (begin
               (unless equalp
                 (Scm_Error "%S and %S can't be ordered" x y))
               (let* ([r (Scm_ApplyRec2 (SCM_OBJ (& Scm_GenericObjectEqualP))
                                        x y)])
                 (return (?: (SCM_FALSEP r) 1 0)))))
   ))

;; We implement these in C for performance.
;; TODO: We might be able to do shortcut in comparator-equal? by recognizing
;; the equality predicate to be eq? or eqv?.
(define-cproc comparator-test-type (c::<comparator> obj) :constant
  (if (logand (-> c flags) SCM_COMPARATOR_ANY_TYPE)
    (return SCM_TRUE)
    (return (Scm_VMApply1 (-> c typeFn) obj))))

(define-cproc comparator-check-type (c::<comparator> obj) :constant
  (if (logand (-> c flags) SCM_COMPARATOR_ANY_TYPE)
    (return SCM_TRUE)
    (let1/cps result (Scm_VMApply1 (-> c typeFn) obj)
      [c::ScmComparator* obj]
      (when (SCM_FALSEP result)
        (Scm_Error "Comparator %S cannot accept object %S" c obj))
      (return SCM_TRUE))))

;; TODO: We can avoid using Scm_ComparatorHashFunction and
;; Scm_ComparatorComparisonProcedure that may call Scm_ApplyRec.

(define-cproc comparator-hash (c::<comparator> x) :constant
  (if (logand (-> c flags) SCM_COMPARATOR_ANY_TYPE)
    (return (Scm_VMApply1 (Scm_ComparatorHashFunction c) x))
    (let1/cps result (Scm_VMApply1 (-> c typeFn) x)
      [c::ScmComparator* x]
      (when (SCM_FALSEP result)
        (Scm_Error "Comparator %S cannot accept object %S" c x))
      (return (Scm_VMApply1 (Scm_ComparatorHashFunction c) x)))))

(define-cproc comparator-compare (c::<comparator> a b) :constant
  (if (logand (-> c flags) SCM_COMPARATOR_ANY_TYPE)
    (return (Scm_VMApply2 (Scm_ComparatorComparisonProcedure c) a b))
    (let1/cps r (Scm_VMApply1 (-> c typeFn) a)
      [c::ScmComparator* a b]
      (when (SCM_FALSEP r)
        (Scm_Error "Comparator %S cannot accept object %S" c a))
      (let1/cps r (Scm_VMApply1 (-> c typeFn) b)
        [c::ScmComparator* a b]
        (when (SCM_FALSEP r)
          (Scm_Error "Comparator %S cannot accept object %S" c b))
        (return (Scm_VMApply2 (Scm_ComparatorComparisonProcedure c) a b))))))

;;;
;;; Generic comparison
;;;

(select-module gauche)
;; returns -1, 0 or 1
(define-cproc compare (x y) ::<fixnum> Scm_Compare)

;; eq-compare has two properties:
;;  Gives a total order to every Scheme object (within a single run of process)
;;  Returns 0 iff (eq? x y) => #t
(define-cproc eq-compare (x y) ::<fixnum>
  (if (SCM_EQ x y)
    (return 0)
    (return (?: (< (SCM_WORD x) (SCM_WORD y)) -1 1))))

;;;
;;; Sorting
;;;

;; The public API for sorting is in lib/gauche/sortutil.scm and
;; will be autoloaded.  We provide a C-implemented low-level routines.
(select-module gauche.internal)

(define-cproc %sort (seq)
  (cond [(SCM_VECTORP seq)
         (let* ([r (Scm_VectorCopy (SCM_VECTOR seq) 0 -1 SCM_UNDEFINED)])
           (Scm_SortArray (SCM_VECTOR_ELEMENTS r) (SCM_VECTOR_SIZE r) '#f)
           (return r))]
        [(>= (Scm_Length seq) 0) (return (Scm_SortList seq '#f))]
        [else (SCM_TYPE_ERROR seq "proper list or vector")
              (return SCM_UNDEFINED)]))

(define-cproc %sort! (seq)
  (cond [(SCM_VECTORP seq)
         (Scm_SortArray (SCM_VECTOR_ELEMENTS seq) (SCM_VECTOR_SIZE seq) '#f)
         (return seq)]
        [(>= (Scm_Length seq) 0) (return (Scm_SortListX seq '#f))]
        [else (SCM_TYPE_ERROR seq "proper list or vector")
              (return SCM_UNDEFINED)]))

