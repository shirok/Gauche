;;;
;;; gauche.ffi.ffiaux - Auxiliary functions for FFI
;;;
;;;   Copyright (c) 2026  Shiro Kawai  <shiro@acm.org>
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

;; This is a submodule of gauche.ffi and not meant to be used directly.

(define-module gauche.ffi.ffiaux
  (export native-alloc
          native-free))
(select-module gauche.ffi.ffiaux)

;; Delay loading gauche.native-type to avoid built-time dependency
(autoload gauche.native-type c-aggregate-type? make-c-pointer-type)

(inline-stub
 (.include "gauche/priv/typeP.h")

 (declare-stub-type <native-handle> ScmNativeHandle*))

;;;
;;;  Raw memory
;;;

(define-cproc %native-alloc (size::<fixnum> type::<native-type>)
  (let* ([p::void* (malloc size)]
         [attrs (SCM_LIST1 (Scm_Cons 'malloc '#t))])
    (when (== p NULL)
      (Scm_Error "malloc failed (size %ld\n)" size))
    (return
     (Scm__MakeNativeHandle p
                            type
                            (-> type name)
                            p
                            (+ p size)
                            SCM_UNDEFINED
                            attrs
                            0))))

(define (native-alloc size-or-type)
  (assume-type size-or-type (</> <fixnum> <native-type>))
  (let ([realsize (typecase size-or-type
                    [<fixnum> size-or-type]
                    [<native-type> (~ size-or-type'size)])]
        [ptype (cond
                [(c-aggregate-type? size-or-type) size-or-type]
                [(is-a? size-or-type <native-type>)
                 (make-c-pointer-type size-or-type)]
                [else (make-c-pointer-type <void>)])])
    (%native-alloc realsize ptype)))

(define-cproc native-free (handle::<native-handle>) ::<void>
  (when (SCM_FALSEP (Scm_Assq 'malloc (-> handle attrs)))
    (Scm_Error "Attempt to free a handle which is not malloc'ed: %S"
               handle))
  (unless (SCM_FALSEP (Scm_Assq 'freed (-> handle attrs)))
    (Scm_Error "Attempt to free an already-freed handle: %S"
               handle))
  (set! (-> handle attrs)
        (Scm_Acons 'freed '#t (-> handle attrs)))
  (free (-> handle ptr)))
