;;;
;;; libmisc.scm - miscellaneous built-in procedures
;;;
;;;   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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
;;; Miscellaneous
;;;

(select-module gauche)

(define-cproc has-setter? (proc) ::<boolean> Scm_HasSetter)

(define-cproc identity (val) :constant (return val))   ;sometimes useful

(define-cproc undefined () (inliner CONSTU) (return SCM_UNDEFINED))
(define-cproc undefined? (obj) ::<boolean> :constant SCM_UNDEFINEDP)

(define (warn fmt . args)
  (unless (sys-getenv "GAUCHE_SUPPRESS_WARNING")
    (apply format (current-error-port) (string-append "WARNING: " fmt) args)
    (flush (current-error-port))))

;; srfi-111 box
;; NB: We have built-in support for boxes to use internally, but Scheme
;; interface is less frequently used, so we put these symbols in a separate
;; module.  It is a bit awkward to do so in the current genstub.
;; TODO: Better stub syntax for handling modules.
(select-module srfi-111)
(define-cproc box (v) (return (SCM_OBJ (Scm_MakeBox v))))
(define-cproc box? (v) ::<boolean> (return (SCM_BOXP v)))
(define-cproc unbox (b::<box>) (return (SCM_BOX_VALUE b)))
(define-cproc set-box! (b::<box> v) ::<void> (SCM_BOX_SET b v))
(export box box? unbox set-box!)

;; Foreign pointer (may be in libsys.scm?)

(select-module gauche)

(define-cproc foreign-pointer-invalid? (fp::<foreign-pointer>) ::<boolean>
  Scm_ForeignPointerInvalidP)

(define-cproc foreign-pointer-invalidate! (fp::<foreign-pointer>) ::<void>
  Scm_ForeignPointerInvalidate)

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
