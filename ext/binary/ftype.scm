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

;; native pointer type
;;
;; We might auto-generate those accessor/modifiers eventually.

(define-cproc %pointer-s8-ref (p::<foreign-pointer>)
  (let* ([ptr::int8_t* (SCM_FOREIGN_POINTER_REF (.type int8_t*) p)])
    (return (SCM_MAKE_INT (* ptr)))))

(define-cproc %pointer-s8-sset! (p::<foreign-pointer> v::<int8>) ::<void>
  (let* ([ptr::int8_t* (SCM_FOREIGN_POINTER_REF (.type int8_t*) p)])
    (set! (* ptr) v)))

(define-cproc %pointer-u8-ref (p::<foreign-pointer>)
  (let* ([ptr::uint8_t* (SCM_FOREIGN_POINTER_REF (.type uint8_t*) p)])
    (return (SCM_MAKE_INT (* ptr)))))

(define-cproc %pointer-u8-sset! (p::<foreign-pointer> v::<uint8>) ::<void>
  (let* ([ptr::uint8_t* (SCM_FOREIGN_POINTER_REF (.type uint8_t*) p)])
    (set! (* ptr) v)))

(define-cproc %pointer-s16-ref (p::<foreign-pointer>)
  (let* ([ptr::int16_t* (SCM_FOREIGN_POINTER_REF (.type int16_t*) p)])
    (return (SCM_MAKE_INT (* ptr)))))

(define-cproc %pointer-s16-sset! (p::<foreign-pointer> v::<int16>) ::<void>
  (let* ([ptr::int16_t* (SCM_FOREIGN_POINTER_REF (.type int16_t*) p)])
    (set! (* ptr) v)))

(define-cproc %pointer-u16-ref (p::<foreign-pointer>)
  (let* ([ptr::uint16_t* (SCM_FOREIGN_POINTER_REF (.type uint16_t*) p)])
    (return (SCM_MAKE_INT (* ptr)))))

(define-cproc %pointer-u16-sset! (p::<foreign-pointer> v::<uint16>) ::<void>
  (let* ([ptr::uint16_t* (SCM_FOREIGN_POINTER_REF (.type uint16_t*) p)])
    (set! (* ptr) v)))

(define-cproc %pointer-s32-ref (p::<foreign-pointer>)
  (let* ([ptr::int32_t* (SCM_FOREIGN_POINTER_REF (.type int32_t*) p)])
    (return (SCM_MAKE_INT (* ptr)))))

(define-cproc %pointer-s32-sset! (p::<foreign-pointer> v::<int32>) ::<void>
  (let* ([ptr::int32_t* (SCM_FOREIGN_POINTER_REF (.type int32_t*) p)])
    (set! (* ptr) v)))

(define-cproc %pointer-u32-ref (p::<foreign-pointer>)
  (let* ([ptr::uint32_t* (SCM_FOREIGN_POINTER_REF (.type uint32_t*) p)])
    (return (SCM_MAKE_INT (* ptr)))))

(define-cproc %pointer-u32-sset! (p::<foreign-pointer> v::<uint32>) ::<void>
  (let* ([ptr::uint32_t* (SCM_FOREIGN_POINTER_REF (.type uint32_t*) p)])
    (set! (* ptr) v)))

(define-cproc %pointer-s64-ref (p::<foreign-pointer>)
  (let* ([ptr::int64_t* (SCM_FOREIGN_POINTER_REF (.type int64_t*) p)])
    (return (SCM_MAKE_INT (* ptr)))))

(define-cproc %pointer-s64-sset! (p::<foreign-pointer> v::<int64>) ::<void>
  (let* ([ptr::int64_t* (SCM_FOREIGN_POINTER_REF (.type int64_t*) p)])
    (set! (* ptr) v)))

(define-cproc %pointer-u64-ref (p::<foreign-pointer>)
  (let* ([ptr::uint64_t* (SCM_FOREIGN_POINTER_REF (.type uint64_t*) p)])
    (return (SCM_MAKE_INT (* ptr)))))

(define-cproc %pointer-u64-sset! (p::<foreign-pointer> v::<uint64>) ::<void>
  (let* ([ptr::uint64_t* (SCM_FOREIGN_POINTER_REF (.type uint64_t*) p)])
    (set! (* ptr) v)))
