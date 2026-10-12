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
          native-free

          <FILE*>
          port->FILE*
          close-FILE*))
(select-module gauche.ffi.ffiaux)

;; Delay loading gauche.native-type to avoid built-time dependency
(autoload gauche.native-type
          c-aggregate-type? make-c-pointer-type make-c-opaque-pointer-type
          native-type)

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

;;;
;;;  FILE*
;;;

;; Since Gauche implements its own buffered I/O integrated with
;; Scheme ports, we don't recommend using FILE*.  However, many
;; foreign functions provdies API taking it, so having this is
;; convenient to quickly adopt foreign libraries.

;; We represelnt FILE* as native pointer to an opaque type
(define-type <FILE*>
  (make-c-opaque-pointer-type 'FILE))

;; TRANSIENT: This is to obtain <FILE*> from C code.  We should have
;; a cise macro to generate this.
(inline-stub
 (define-cvar pfile-type::ScmNativeType* :static NULL)
 (define-cfn get-pfile-type () ::ScmNativeType* :static
   (when (== pfile-type NULL)
     (set! pfile-type
           (cast ScmNativeType* (Scm_GlobalVariableRef (Scm_CurrentModule)
                                                       (SCM_SYMBOL '<FILE*>)
                                                       0))))
   (return pfile-type)))

(define-cproc port->FILE* (port::<port> :key (share-fd?::<boolean> #f))
  (let* ([fd::int (Scm_PortFileNo port)]
         [fd2::int -1]
         [fp::FILE* NULL])
    (when (< fd 0)
      (Scm_Error "port->FILE* can only take a port associated to a file, \
                  but got: %S" port))
    ;; We dup fd so that close-FILE* won't close the port's fd.
    (when (SCM_OPORTP port) (Scm_Flush port))
    (set! fd2 (?: share-fd? fd (dup fd)))
    (when (< fd2 0)
      (Scm_SysError "dup failed for port %S" port))
    (set! fp (fdopen fd2 (?: (SCM_IPORTP port) "r" "w")))
    (when (== fp NULL)
      (when (!= fd fd2) (close fd2))
      (Scm_SysError "fdopen failed for port %S" port))
    (return (Scm_MakeNativeHandleSimple fp (SCM_OBJ (get-pfile-type))))))

(define-cproc %close-FILE* (handle::<native-handle>) ::<void>
  (let* ([fp::FILE* (Scm_NativeHandlePtr handle)])
    (when (== fp NULL)
      (Scm_Error "close-FILE* got a null pointer: %S" handle))
    (when (!= (fclose fp) 0)
      (Scm_SysError "fclsoe failed on %S" handle))))

(define (close-FILE* handle)
  (assume (of-type? handle <FILE*>))
  (%close-FILE* handle))
