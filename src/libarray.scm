;;;
;;; libarray.scm - array basics
;;;
;;;   Copyright (c) 2025  Shiro Kawai  <shiro@acm.org>
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

;; Array operations are implemented in gauche.array (ext/uvector/array.scm
;; and ext/uvector/matrix.scm).  We want to dispatch array printers
;; from pprint, however, and want to delay loading gauche.array until
;; we see an array.  To do so, we need array core type to be defined
;; beforehand.

;; It would be plausible to provide basic array accessors (array-ref and
;; array-set!) in the core as well, for that could provide much better
;; performance.  Consider the current implementation transitional

;; NB: Wiring metaclass in C is cumbersome, so we define <array-base>
;; as a usual class.  The concrete classes (<array>, <u8array>, etc.)
;; must specify :metaclass <array-base>.

(select-module gauche.internal)

(inline-stub
 (.include "gauche/priv/arrayP.h")

 ;;
 ;; Array-base
 ;;

 ;; NB: We rely on the 'initialize' method of <array-base>, defined in array.scm,
 ;; to set up the slots properly.

 ;; start-vector, end-vector : s32vector of length N, where N is the rank
 ;; of the array.  K's dimension index starts from (~ start-vector k), inclusive,
 ;; and ends at (~ end-vector k), exclusive.
 ;;
 ;; coefficient-vector (s32vector) and offset (scalar) maps input index
 ;; vector Vi into the position of backing storage Pos as
 ;;
 ;;  Pos = Vc・(Vi - Vb) + Off
 ;;
 ;; Where Vc is the coefficient vector and Vb is the start vector.
 ;; Arrays created with make-*array maps all the elements in the backing
 ;; stroage contiguously (e.g. The range of Pos is [0..N-1] where N is the
 ;; number of total elements), and Off is always 0.
 ;; However, if an array is created by share-array, Pos can be incontiguous.
 ;;
 (define-ctype ScmArrayBase
   ::(.struct ScmArrayBaseRec
              (SCM_INSTANCE_HEADER::||
               start-vector
               end-vector
               coefficient-vector
               offset
               backing-storage)))

 (define-cclass <array-base> :base
   "ScmArrayBase*" "Scm_ArrayBaseClass"
   (c "SCM_CLASS_OBJECT_CPL")
   ((start-vector)
    (end-vector)
    (coefficient-vector)
    (offset)
    (backing-storage))
   (allocator (let* ([z::ScmArrayBase* (SCM_NEW_INSTANCE ScmArrayBase klass)])
                (set! (-> z start-vector) SCM_UNDEFINED)
                (set! (-> z end-vector) SCM_UNDEFINED)
                (set! (-> z coefficient-vector) SCM_UNDEFINED)
                (set! (-> z offset) (SCM_MAKE_INT 0))
                (set! (-> z backing-storage) SCM_UNDEFINED)
                (return (SCM_OBJ z)))))

 (declare-stub-type <array-base> "ScmArrayBase*")

 ;; If array's rank is smaller than this, array-ref and array-set! handles
 ;; arguments very efficiently.
 (.define ARRAY_FAST_RANK 10)
 (.define ARRAY_FAST_RANK1 (+ ARRAY_FAST_RANK 1))

 (define-cise-stmt ARRAY-CHECK
   ([_ array]
    `(unless (SCM_UVECTORP (-> ,array start-vector))
       (Scm_Error "Encountered an uninitialized array."))))

 (define-cfn Scm_ArrayRank (array::(const ScmArrayBase*)) ::ScmSmallInt
   (ARRAY-CHECK array)
   (return (SCM_UVECTOR_SIZE (-> array start-vector))))

 ;; If the # of index argument is greater than ARRAY_FAST_RANK, we need
 ;; to create a full array by adding eleemnts from ixlist.
 ;; For the time being, this is very inefficient since we allocate.
 ;;
 (define-cfn complete-ixs (ixs::ScmObj*
                           pixcount::int* ; out
                           ixlist)
   ::ScmObj* :static
   (if (SCM_NULLP ixlist)
     (return ixs)                       ; common case.  nothing to do.
     (let* ([len::ScmSmallInt (+ (* pixcount) (Scm_Length ixlist))]
            [ixs2::ScmObj* (SCM_NEW_ARRAY ScmObj len)]
            [i::int 0])
       (for (() (< i (* pixcount)) (post++ i))
         (set! (aref ixs2 i) (aref ixs i)))
       (for-each (lambda (v) (set! (aref ixs2 i) v)) ixlist)
       (set! (* pixcount) i)
       (return ixs2))))

 ;; Convert array indexes (ScmObj[]) to the position of the backing vector.
 ;; The caller should make sure that the size of IXS must be RANK.
 (define-cfn index-vector->pos (array::(const ScmArrayBase*)
                                rank::ScmSmallInt
                                ixs::ScmObj*)
   ::ScmSmallInt :static
   (let* ([Vb::int32_t* (SCM_S32VECTOR_ELEMENTS (-> array start-vector))]
          [Ve::int32_t* (SCM_S32VECTOR_ELEMENTS (-> array end-vector))]
          [Vc::int32_t* (SCM_S32VECTOR_ELEMENTS (-> array coefficient-vector))]
          [pos::ScmSmallInt (SCM_INT_VALUE (-> array offset))])
     (dotimes [i rank]
       (unless (SCM_INTP (aref ixs i))
         (Scm_Error "Bad array index: %S" (aref ixs i)))
       (let* ([ix::ScmSmallInt (SCM_INT_VALUE (aref ixs i))])
         (unless (and (>= ix (aref Vb i))
                      (< ix (aref Ve i)))
           (Scm_Error "Array index out of range: %d" ix))
         (set! pos (+ pos (* (- ix (aref Vb i)) (aref Vc i))))))
     (SCM_ASSERT (<= 0 pos))
     (return pos)))

 ;; This handles the case when array index is given as another array (iarray)
 ;; iarray must be rank 1 and the same size as arrray's rank.
 ;; We need a special handler, since iarray may have a nontrivial mapping
 ;; from index to the position of backing storage.
 (define-cfn index-array->pos (array::(const ScmArrayBase*)
                               rank::ScmSmallInt
                               iarray::(const ScmArrayBase*))
   ::ScmSmallInt :static
   (unless (SCM_VECTORP (-> iarray backing-storage))
     (Scm_Error "Only <array> can be used as an index object: %S" iarray))
   (let* ([iVb::int32_t* (SCM_S32VECTOR_ELEMENTS (-> iarray start-vector))]
          [iVc::int32_t* (SCM_S32VECTOR_ELEMENTS (-> iarray coefficient-vector))]
          [ioff::ScmSmallInt (SCM_INT_VALUE (-> iarray offset))]
          [ibs::ScmVector* (SCM_VECTOR (-> iarray backing-storage))]
          [ixs::(.array ScmObj (rank))])
     (dotimes [i rank]
       (let* ([ipos::ScmSmallInt (+ (* (aref iVc 0) (+ i (aref iVb 0))) ioff)])
         (SCM_ASSERT (and (<= 0 ipos)
                          (< ipos (SCM_VECTOR_SIZE ibs))))
         (set! (aref ixs i) (aref (SCM_VECTOR_ELEMENTS ibs) ipos))))
     (return (index-vector->pos array rank ixs))))

 ;; Errors in indexing
 (define-cfn bad-index (ixs::ScmObj* ixcount::int)
   ::void :static
   (Scm_Error "Bad array index: %S" (Scm_ArrayToList ixs ixcount)))

 (define-cfn index-mismatch (rank::ScmSmallInt ixs::ScmObj* ixcount::int)
   ::void :static
   (Scm_Error "Index %S does not match array's rank %ld"
              (Scm_ArrayToList ixs ixcount) rank))

 ;; array-ref and array-set! are variadic and polymorphic regarding
 ;; indexes.  This handles all the cases and returns the array of
 ;; ScmObj index.
 (define-cfn index-args->pos (array::(const ScmArrayBase*)
                              rank::ScmSmallInt
                              ixs::ScmObj*
                              ixcount::ScmSmallInt)
   ::ScmSmallInt :static
   (cond
    [(== ixcount 0)                     ; special case: 0-dim array
     (unless (== rank 0) (index-mismatch rank ixs ixcount))
     (return (SCM_INT_VALUE (-> array offset)))]
    [(SCM_VECTORP (aref ixs 0))
     (unless (== ixcount 1) (bad-index ixs ixcount))
     (unless (== rank (SCM_VECTOR_SIZE (aref ixs 0)))
       (index-mismatch rank ixs ixcount))
     (return (index-vector->pos array rank (SCM_VECTOR_ELEMENTS (aref ixs 0))))]
    [(SCM_ARRAY_BASE_P (aref ixs 0))
     (unless (== ixcount 1) (bad-index ixs ixcount))
     (let* ([iarr::(const ScmArrayBase*) (SCM_ARRAY_BASE (aref ixs 0))]
            [bv (-> iarr backing-storage)])
       (unless (SCM_VECTORP bv) (bad-index ixs ixcount))
       (unless (== (Scm_ArrayRank iarr) 1) (bad-index ixs ixcount))
       (unless (== (- (aref (SCM_S32VECTOR_ELEMENTS (-> iarr end-vector)) 0)
                      (aref (SCM_S32VECTOR_ELEMENTS (-> iarr start-vector)) 0))
                   rank)
         (index-mismatch rank ixs 1))
       (return (index-array->pos array rank iarr)))]
    [(SCM_INTP (aref ixs 0))
     (unless (== ixcount rank) (index-mismatch rank ixs ixcount))
     (return (index-vector->pos array rank ixs))]
    [else (bad-index ixs ixcount) (return 0)])) ;dummy
 )

;; Kludge: If we place (define-cproc array-rank ..) after (select-module gauche),
;; the procedure type info with <array-base> is recorded with the
;; #<module gauche>.  This causes procedure-type fail
;; (https://github.com/shirok/Gauche/issues/1192), since <array-base>
;; isn't visible from #<module gauche>.

(define-cproc array-rank (array::<array-base>) ::<fixnum>
  Scm_ArrayRank)

(define-in-module gauche array-rank array-rank)

(define-cproc array-ref (array::<array-base>
                         :optarray (ixs ixcount 10)
                         :rest ixlist)
  (ARRAY-CHECK array)
  (let* ([rank::ScmSmallInt (SCM_UVECTOR_SIZE (-> array start-vector))]
         [ixs2::ScmObj* (complete-ixs ixs (& ixcount) ixlist)]
         [pos::ScmSmallInt (index-args->pos array rank ixs2 ixcount)]
         [bv (-> array backing-storage)])
    (cond
     [(SCM_VECTORP bv)
      (SCM_ASSERT (< pos (SCM_VECTOR_SIZE bv)))
      (return (SCM_VECTOR_ELEMENT bv pos))]
     [(SCM_UVECTORP bv)
      (return (Scm_VMUVectorRef (SCM_UVECTOR bv) SCM_UVECTOR_GENERIC
                                pos SCM_UNBOUND))]
     [else (Scm_Panic "Invalid array")
           (return SCM_UNDEFINED)])))

(define-cproc array-set! (array::<array-base>
                         :optarray (ixs ixcount 11)
                         :rest ixlist)
  ::<void>
  (ARRAY-CHECK array)
  (unless (>= ixcount 1)
    (Scm_AssertionError (SCM_LIST1 (SCM_OBJ array))
                        "Wrong number of arguments for array-set!"))
  (let* ([rank::ScmSmallInt (SCM_UVECTOR_SIZE (-> array start-vector))]
         [ixs2::ScmObj* (complete-ixs ixs (& ixcount) ixlist)]
         [pos::ScmSmallInt (index-args->pos array rank ixs2 (- ixcount 1))]
         [val (aref ixs2 (- ixcount 1))]
         [bv (-> array backing-storage)])
      (cond
       [(SCM_VECTORP bv)
        (SCM_ASSERT (< pos (SCM_VECTOR_SIZE bv)))
        (set! (SCM_VECTOR_ELEMENT bv pos) val)]
       [(SCM_UVECTORP bv)
        (Scm_UVectorSet (SCM_UVECTOR bv) SCM_UVECTOR_GENERIC
                        pos val SCM_CLAMP_ERROR)]
       [else (Scm_Panic "Invalid array")])))

(define-in-module gauche array-ref array-ref)
(define-in-module gauche array-set! array-set!)
