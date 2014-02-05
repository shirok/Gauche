;;;
;;; libmisc.scm - miscellaneous built-in procedures
;;;
;;;   Copyright (c) 2000-2013  Shiro Kawai  <shiro@acm.org>
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
 (declcode (.include <gauche/vminsn.h>)))

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

;;;
;;; Miscellaneous
;;;

(select-module gauche)

(define-cproc has-setter? (proc) ::<boolean> Scm_HasSetter)

(define-cproc identity (val) :constant (result val))   ;sometimes useful

(define-cproc undefined () (inliner CONSTU) (result SCM_UNDEFINED))
(define-cproc undefined? (obj) ::<boolean> :constant SCM_UNDEFINEDP)

(define-cproc warn (fmt::<string> :rest args) ::<void> Scm_FWarn)

;; Foreign pointer (may be in libsys.scm?)

(select-module gauche)

(define-cproc foreign-pointer-attributes (fp::<foreign-pointer>)
  Scm_ForeignPointerAttr)

(define-cproc foreign-pointer-attribute-get (fp::<foreign-pointer>
                                             key :optional fallback)
  Scm_ForeignPointerAttrGet)

(define-cproc foreign-pointer-attribute-set! (fp::<foreign-pointer> key value)
  Scm_ForeignPointerAttrSet)

; for backward compatibility - deprecated
(define foreign-pointer-attribute-set foreign-pointer-attribute-set!)

;;
;; Static configuration
;;

(select-module gauche.internal)
(define-cproc cond-features () Scm_GetFeatures)
