;;;
;;; libbool.scm - builtin boolean/comparison procedures
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
 (declcode (.include <gauche/vminsn.h>))
 )

;;
;; Equivalence predicates
;;

(select-module scheme)
(define-cproc eqv? (obj1 obj2) ::<boolean> :fast-flonum :constant
  (inliner EQV) Scm_EqvP)
(define-cproc eq? (obj1 obj2)  ::<boolean> :fast-flonum :constant
  (inliner EQ) SCM_EQ)
(define-cproc equal? (obj1 obj2) ::<boolean> :fast-flonum Scm_EqualP)

;;
;; Booleans
;;

(select-module scheme)
(define-cproc not (obj) ::<boolean> :fast-flonum :constant
  (inliner NOT) SCM_FALSEP)
(define-cproc boolean? (obj) ::<boolean> :fast-flonum :constant SCM_BOOLP)

(select-module gauche)
;; a convenient coercer
(define-cproc boolean (obj) ::<boolean> :constant
  (result (not (SCM_FALSEP obj))))

;; R7RS
(define (boolean=? a b . args)
  (if-let1 z (find ($ not $ boolean? $) (list* a b args))
    (error "boolean value required, but got:" z))
  (if a
    (and b (every identity args))
    (and (not b) (every not args))))

;;
;; Generic comparison
;;

(select-module gauche)
(define-cproc compare (x y) ::<fixnum> Scm_Compare)



