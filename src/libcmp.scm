;;;
;;; libcmp.scm - compare and sort
;;;
;;;   Copyright (c) 2000-2014  Shiro Kawai  <shiro@acm.org>
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

;;;
;;; Comparator (a la srfi-114)
;;;

(select-module gauche)
(define-cproc make-comparator (type-test equality-test comparison-proc hash
                               :optional (name #f))
  Scm_MakeComparator)

(define-cproc comparator? (obj) ::<boolean> SCM_COMPARATORP)
(define-cproc comparator-comparison-procedure? (c::<comparator>) ::<boolean>
  (result (not (SCM_FALSEP (-> c compareFn)))))
(define-cproc comparator-hash-function? (c::<comparator>) ::<boolean>
  (result (not (SCM_FALSEP (-> c hashFn)))))

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
    (result 0)
    (result (?: (< (SCM_WORD x) (SCM_WORD y)) -1 1))))

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
           (result r))]
        [(>= (Scm_Length seq) 0) (result (Scm_SortList seq '#f))]
        [else (SCM_TYPE_ERROR seq "proper list or vector")
              (result SCM_UNDEFINED)]))

(define-cproc %sort! (seq)
  (cond [(SCM_VECTORP seq)
         (Scm_SortArray (SCM_VECTOR_ELEMENTS seq) (SCM_VECTOR_SIZE seq) '#f)
         (result seq)]
        [(>= (Scm_Length seq) 0) (result (Scm_SortListX seq '#f))]
        [else (SCM_TYPE_ERROR seq "proper list or vector")
              (result SCM_UNDEFINED)]))

