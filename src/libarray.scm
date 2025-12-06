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
 (define-ctype ScmArrayBase
   ::(.struct ScmArrayBaseRec
              (SCM_INSTANCE_HEADER::||
               start-vector
               end-vector
               coefficient-vector
               mapper
               getter
               setter
               backing-storage)))

 (define-cclass <array-base> :base
   "ScmArrayBase*" "Scm_ArrayBaseClass"
   (c "SCM_CLASS_OBJECT_CPL")
   ((start-vector)
    (end-vector)
    (coefficient-vector)
    (mapper)
    (getter)
    (setter)
    (backing-storage))
   (allocator (let* ([z::ScmArrayBase* (SCM_NEW_INSTANCE ScmArrayBase klass)])
                (set! (-> z start-vector) SCM_UNDEFINED)
                (set! (-> z end-vector) SCM_UNDEFINED)
                (set! (-> z coefficient-vector) SCM_UNDEFINED)
                (set! (-> z mapper) SCM_UNDEFINED)
                (set! (-> z getter) SCM_UNDEFINED)
                (set! (-> z setter) SCM_UNDEFINED)
                (set! (-> z backing-storage) SCM_UNDEFINED)
                (return (SCM_OBJ z)))))

 (declare-stub-type <array-base> "ScmArrayBase*")

 (define-cise-stmt ARRAY-CHECK
   ([_ array]
    `(unless (SCM_UVECTORP (-> ,array start-vector))
       (Scm_Error "Encountered an uninitialized array."))))

 (define-cfn Scm_ArrayRank (array::(const ScmArrayBase*)) ::ScmSmallInt
   (ARRAY-CHECK array)
   (return (SCM_UVECTOR_SIZE (-> array start-vector))))

 ;; Convert array indexes (ScmObj[]) to the position of the backing vector.
 ;; The caller should make sure that the size of IXS must be RANK.
 (define-cfn map_array_index (array::(const ScmArrayBase*)
                              rank::ScmSmallInt
                              ixs::ScmObj*)
   ::ScmSmallInt :static :inline
   (let* ([Vb::int32_t* (SCM_S32VECTOR_ELEMENTS (-> array start-vector))]
          [Ve::int32_t* (SCM_S32VECTOR_ELEMENTS (-> array end-vector))]
          [Vc::int32_t* (SCM_S32VECTOR_ELEMENTS (-> array coefficient-vector))]
          [pos::ScmSmallInt 0])
     (dotimes [i rank]
       (fprintf stderr "kebekebe %d\n" i)
       (unless (SCM_INTP (aref ixs i))
         (Scm_Error "Bad array index: %S" (aref ixs i)))
       (let* ([ix::ScmSmallInt (SCM_INT_VALUE (aref ixs i))])
         (unless (and (>= ix (aref Vb i))
                      (< ix (aref Ve i)))
           (Scm_Error "Array index out of range: %d" ix))
         (set! pos (+ pos (* (- ix (aref Vb i)) (aref Vc i))))))
     (return pos)))

 (define-cfn bad_args (ixlist)
   ::void :static
   (Scm_Error "Bad array index: %S" ixlist))

 ;; array-ref and array-set! are variadic and polymorphic regarding
 ;; indexes.  This handles all the cases and returns the array of
 ;; ScmObj index.
 (define-cfn canonicalize_index_args (ixs::ScmObj*
                                      ixcount::ScmSmallInt
                                      ixlist)
   ::ScmObj* :static
   (cond
    [(== ixcount 0) (return ixs)]  ; special case: 0-dim array
    [(SCM_VECTORP (aref ixs 0))
     (unless (== ixcount 1) (bad_args ixlist))
     (return (SCM_VECTOR_ELEMENTS (aref ixs 0)))]
    [(SCM_ARRAY_BASE_P (aref ixs 0))
     (unless (== ixcount 1) (bad_args ixlist))
     (let* ([arr::(const ScmArrayBase*) (SCM_ARRAY_BASE (aref ixs 0))]
            [bv (-> arr backing-storage)])
       (unless (SCM_VECTORP bv) (bad_args ixlist))
       ;; TODO: check dimension and also reshaped case
       (return (SCM_VECTOR_ELEMENTS bv)))]
    [(SCM_INTP (aref ixs 0))
     (return ixs)]
    [else (bad_args ixlist)
          (return ixs)]))              ;DUMMY
 )

;; Kludge: If we place (define-cproc array-rank ..) after (select-module gauche),
;; the procedure type info with <array-base> is recorded with the
;; #<module gauche>.  This causes procedure-type fail
;; (https://github.com/shirok/Gauche/issues/1192), since <array-base>
;; isn't visible from #<module gauche>.

(define-cproc array-rank (array::<array-base>) ::<fixnum>
  Scm_ArrayRank)

(define-in-module gauche array-rank array-rank)

;; NB: Not quite ready.  DO NOT USE THIS YET.
(define-cproc %array-ref (array::<array-base>
                          :optarray (ixs ixcount 10)
                          :rest ixlist)
  (ARRAY-CHECK array)
  (let* ([rank::ScmSmallInt (SCM_UVECTOR_SIZE (-> array start-vector))]
         [ixsize::ScmSmallInt ixcount])
    (fprintf stderr "kwe kwe %ld %ld\n" rank ixsize)
    (unless (== rank ixsize)
      (Scm_Error "Array index %S doesn't match array rank of %S" ixlist array))
    (let* ([ixv::ScmObj* (canonicalize-index-args ixs ixcount ixlist)]
           [pos::ScmSmallInt (map-array-index array rank ixv)]
           [bv (-> array backing-storage)])
      (fprintf stderr ">>> pos=%ld\n" pos)
      (cond
       [(SCM_VECTORP bv)
        (return (SCM_VECTOR_ELEMENT bv pos))]
       [(SCM_UVECTORP bv)
        (return (Scm_VMUVectorRef (SCM_UVECTOR bv) SCM_UVECTOR_GENERIC
                                  pos SCM_UNBOUND))]
       [else (Scm_Panic "Invalid array")
             (return SCM_UNDEFINED)]))))
