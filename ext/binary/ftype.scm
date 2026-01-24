;;;
;;; binary.ftype - foreign types and foreign objects
;;;
;;;   Copyright (c) 2011-2025  Shiro Kawai  <shiro@acm.org>
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

;; This module provides the means to define and manipulate
;; compound binary types.
;;
;; NB: This module has never been documented, and will likely to be
;; subsumed by FFI module.  Any code here is for experiment to explore
;; effective interface.

(define-module binary.ftype
  (use util.match)
  (extend gauche.typeutil)              ;access internal routines
  )
(select-module binary.ftype)

(inline-stub
 (.include "gauche/priv/typeP.h"))

;; native pointer type

(define-cproc %native-pointer-ref (type::<native-type>
                                   fp::<foreign-pointer>)
  (let* ([p::void* (Scm_ForeignPointerRef fp)]
         [c-ref::(.function (p::void*)::ScmObj *) (-> type c-ref)])
    (if (!= c-ref NULL)
      (return (c-ref p))
      (begin (Scm_Error "Cannot dereference foreign pinter: %S" fp)
             (return SCM_UNDEFINED))))) ;dummy

(define-cproc %native-pointer-set! (type::<native-type>
                                    fp::<foreign-pointer>
                                    val)
  ::<void>
  (let* ([p::void* (Scm_ForeignPointerRef fp)]
         [c-of-type::(.function (v::ScmObj)::int *) (-> type c-of-type)]
         [c-set::(.function (p::void* v::ScmObj)::void *) (-> type c-set)])
    (unless (c-of-type val)
      (Scm_Error "Invalid object to set to %S: %S" fp val))
    (if (!= c-set  NULL)
      (c-set p val)
      (Scm_Error "Cannot set foreign pinter: %S" fp))))
