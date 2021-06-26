;;;
;;; libbox.scm - Boxes
;;;
;;;   Copyright (c) 2000-2021  Shiro Kawai  <shiro@acm.org>
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

(select-module gauche)

;;;
;;;  srfi-111 & srfi-195 box
;;;

;; We use them in equal? implementation as well, so here we go...
(define-cproc box (:optarray (argv argc 1) :rest rest)
  (cond
   [(== argc 0) (return (SCM_OBJ (Scm_MakeMVBox 0 SCM_FALSE)))]
   [(SCM_NULLP rest) (return (SCM_OBJ (Scm_MakeBox (aref argv 0))))]
   [else (return (SCM_OBJ (Scm_ListToMVBox (Scm_Cons (aref argv 0) rest))))]))

(define-cproc box? (v) ::<boolean> (return (or (SCM_BOXP v) (SCM_MVBOXP v))))

(define-cproc unbox (b)
  (cond
   [(SCM_BOXP b) (return (SCM_BOX_VALUE b))]
   [(SCM_MVBOXP b) (return (Scm_ValuesFromArray (SCM_MVBOX_VALUES b)
                                                (SCM_MVBOX_SIZE b)))]
   [else (SCM_TYPE_ERROR b "<box> or <mv-box>") (return SCM_UNDEFINED)]))

(define-cproc set-box! (b :rest vs) ::<void>
  (cond
   [(SCM_BOXP b)
    (unless (and (SCM_PAIRP vs) (SCM_NULLP (SCM_CDR vs)))
      (Scm_Error "Wrong number of values to set to a single-value box %S: %S"
                 b vs))
    (SCM_BOX_SET b (SCM_CAR vs))]
   [(SCM_MVBOXP b)
    (let* ([argc::ScmSmallInt (Scm_Length vs)]
           [i::ScmSmallInt 0])
      (unless (== argc (SCM_MVBOX_SIZE b))
        (Scm_Error "Wrong number of values to set to a multi-value box %S: %S"
                   b vs))
      (for (() (< i argc) (begin (post++ i) (set! vs (SCM_CDR vs))))
        (set! (aref (SCM_MVBOX_VALUES b) i) (SCM_CAR vs))))]
   [else (SCM_TYPE_ERROR b "<box> or <mv-box>")]))

(define-cproc box-arity (b) ::<int>
  (cond
   [(SCM_BOXP b) (return 1)]
   [(SCM_MVBOXP b) (return (SCM_MVBOX_SIZE b))]
   [else (SCM_TYPE_ERROR b "<box> or <mv-box>")]))

(define-cproc unbox-value (b i::<fixnum>)
  (cond
   [(SCM_BOXP b)
    (unless (== i 0) (Scm_Error "index out of range for %S: %d" b i))
    (return (SCM_BOX_VALUE b))]
   [(SCM_MVBOXP b)
    (unless (and (<= 0 i)
                 (< i (SCM_MVBOX_SIZE b)))
      (Scm_Error "index out of range for %S: %d" b i))
    (return (aref (SCM_MVBOX_VALUES b) i))]
   [else (SCM_TYPE_ERROR b "<box> or <mv-box>")]))

(define-cproc set-box-value! (b i::<fixnum> val) ::<void>
  (cond
   [(SCM_BOXP b)
    (unless (== i 0) (Scm_Error "index out of range for %S: %d" b i))
    (SCM_BOX_SET b val)]
   [(SCM_MVBOXP b)
    (unless (and (<= 0 i)
                 (< i (SCM_MVBOX_SIZE b)))
      (Scm_Error "index out of range for %S: %d" b i))
    (set! (aref (SCM_MVBOX_VALUES b) i) val)]
   [else (SCM_TYPE_ERROR b "<box> or <mv-box>")]))
