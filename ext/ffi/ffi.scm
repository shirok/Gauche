;;;
;;; Gauche-ffi - foreign function interface module
;;;
;;;   Copyright (c) 2025 Retropikzel <retropikzel@iki.fi>, All rights reserved.
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

(define-module gauche.ffi
  (export
    size-of-int8
    size-of-uint8
    size-of-int16
    size-of-uint16
    size-of-int32
    size-of-uint32
    size-of-int64
    size-of-uint64
    size-of-char
    size-of-unsigned-char
    size-of-short
    size-of-unsigned-short
    size-of-int
    size-of-unsigned-int
    size-of-long
    size-of-unsigned-long
    size-of-float
    size-of-double
    size-of-string
    size-of-pointer

    align-of-int8
    align-of-uint8
    align-of-int16
    align-of-uint16
    align-of-int32
    align-of-uint32
    align-of-int64
    align-of-uint64
    align-of-char
    align-of-unsigned-char
    align-of-short
    align-of-unsigned-short
    align-of-int
    align-of-unsigned-int
    align-of-long
    align-of-unsigned-long
    align-of-float
    align-of-double
    align-of-string
    align-of-pointer

    open-shared-library

    ))
(select-module gauche.ffi)

(inline-stub
 (.include "gauche-ffi.h")

 (define-cproc size-of-int8 () size_of_int8)
 (define-cproc size-of-uint8 () size_of_uint8)
 (define-cproc size-of-int16 () size_of_int16)
 (define-cproc size-of-uint16 () size_of_int16)
 (define-cproc size-of-int32 () size_of_int32)
 (define-cproc size-of-uint32 () size_of_int32)
 (define-cproc size-of-int64 () size_of_int64)
 (define-cproc size-of-uint64 () size_of_int64)
 (define-cproc size-of-char () size_of_char)
 (define-cproc size-of-unsigned-char () size_of_unsigned_char)
 (define-cproc size-of-short () size_of_short)
 (define-cproc size-of-unsigned-short () size_of_unsigned_short)
 (define-cproc size-of-int () size_of_int)
 (define-cproc size-of-unsigned-int () size_of_unsigned_int)
 (define-cproc size-of-long () size_of_long)
 (define-cproc size-of-unsigned-long () size_of_unsigned_long)
 (define-cproc size-of-float () size_of_float)
 (define-cproc size-of-double () size_of_double)
 (define-cproc size-of-string () size_of_string)
 (define-cproc size-of-pointer () size_of_pointer)

 (define-cproc align-of-int8 () align_of_int8)
 (define-cproc align-of-uint8 () align_of_uint8)
 (define-cproc align-of-int16 () align_of_int16)
 (define-cproc align-of-uint16 () align_of_int16)
 (define-cproc align-of-int32 () align_of_int32)
 (define-cproc align-of-uint32 () align_of_int32)
 (define-cproc align-of-int64 () align_of_int64)
 (define-cproc align-of-uint64 () align_of_int64)
 (define-cproc align-of-char () align_of_char)
 (define-cproc align-of-unsigned-char () align_of_unsigned_char)
 (define-cproc align-of-short () align_of_short)
 (define-cproc align-of-unsigned-short () align_of_unsigned_short)
 (define-cproc align-of-int () align_of_int)
 (define-cproc align-of-unsigned-int () align_of_unsigned_int)
 (define-cproc align-of-long () align_of_long)
 (define-cproc align-of-unsigned-long () align_of_unsigned_long)
 (define-cproc align-of-float () align_of_float)
 (define-cproc align-of-double () align_of_double)
 (define-cproc align-of-string () align_of_string)
 (define-cproc align-of-pointer () align_of_pointer)

 (define-cproc open-shared-library (path::<string>) open_shared_library)
 ;(define-cproc pointer-null () pointer_null)
 ;(define-cproc pointer-null? (pointer) is_pointer_null)
 ;(define-cproc pointer-allocate (size::<int>) pointer_allocate)
 ;(define-cproc pointer-address (object) pointer_address)
 ;(define-cproc pointer? (pointer) is_pointer)
 ;(define-cproc pointer-free (pointer) pointer_free)

 ;(define-cproc pointer-set-int8! (pointer offset::<int> value::<int8>) pointer_set_int8)
 ;(define-cproc pointer-set-uint8! (pointer offset::<int> value::<uint8>) pointer_set_uint8)
 ;(define-cproc pointer-set-int16! (pointer offset::<int> value::<int16>) pointer_set_int16)
 ;(define-cproc pointer-set-uint16! (pointer offset::<int> value::<int16>) pointer_set_uint16)
 ;(define-cproc pointer-set-int32! (pointer offset::<int> value::<int32>) pointer_set_int32)
 ;(define-cproc pointer-set-uint32! (pointer offset::<int> value::<int32>) pointer_set_uint32)
 ;(define-cproc pointer-set-int64! (pointer offset::<int> value::<int64>) pointer_set_int64)
 ;(define-cproc pointer-set-uint64! (pointer offset::<int> value::<int64>) pointer_set_uint64)
 ;(define-cproc pointer-set-char! (pointer offset::<int> value::<char>) pointer_set_char)
 ;(define-cproc pointer-set-unsigned-char! (pointer offset::<int> value::<char>) pointer_set_unsigned_char)
 ;(define-cproc pointer-set-short! (pointer offset::<int> value::<short>) pointer_set_short)
 ;(define-cproc pointer-set-unsigned-short! (pointer offset::<int> value::<short>) pointer_set_unsigned_short)
 ;(define-cproc pointer-set-int! (pointer offset::<int> value::<int>) pointer_set_int)
 ;(define-cproc pointer-set-unsigned-int! (pointer offset::<int> value::<int>) pointer_set_unsigned_int)
 ;(define-cproc pointer-set-long! (pointer offset::<int> value::<long>) pointer_set_long)
 ;(define-cproc pointer-set-unsigned-long! (pointer offset::<int> value::<long>) pointer_set_unsigned_long)
 ;(define-cproc pointer-set-float! (pointer offset::<int> value::<float>) pointer_set_float)
 ;(define-cproc pointer-set-double! (pointer offset::<int> value::<double>) pointer_set_double)
 ;(define-cproc pointer-set-pointer! (pointer offset::<int> value) pointer_set_pointer)

 ;(define-cproc pointer-get-int8 (pointer offset::<int>) pointer_get_int8)
 ;(define-cproc pointer-get-uint8 (pointer offset::<int>) pointer_get_uint8)
 ;(define-cproc pointer-get-int16 (pointer offset::<int>) pointer_get_int16)
 ;(define-cproc pointer-get-uint16 (pointer offset::<int>) pointer_get_uint16)
 ;(define-cproc pointer-get-int32 (pointer offset::<int>) pointer_get_int32)
 ;(define-cproc pointer-get-uint32 (pointer offset::<int>) pointer_get_uint32)
 ;(define-cproc pointer-get-int64 (pointer offset::<int>) pointer_get_int64)
 ;(define-cproc pointer-get-uint64 (pointer offset::<int>) pointer_get_uint64)
 ;(define-cproc pointer-get-char (pointer offset::<int>) pointer_get_char)
 ;(define-cproc pointer-get-unsigned-char (pointer offset::<int>) pointer_get_unsigned_char)
 ;(define-cproc pointer-get-short (pointer offset::<int>) pointer_get_short)
 ;(define-cproc pointer-get-unsigned-short (pointer offset::<int>) pointer_get_unsigned_short)
 ;(define-cproc pointer-get-int (pointer offset::<int>) pointer_get_int)
 ;(define-cproc pointer-get-unsigned-int (pointer offset::<int>) pointer_get_unsigned_int)
 ;(define-cproc pointer-get-long (pointer offset::<int>) pointer_get_long)
 ;(define-cproc pointer-get-unsigned-long (pointer offset::<int>) pointer_get_unsigned_long)
 ;(define-cproc pointer-get-float (pointer offset::<int>) pointer_get_float)
 ;(define-cproc pointer-get-double (pointer offset::<int>) pointer_get_double)
 ;(define-cproc pointer-get-pointer (pointer offset::<int>) pointer_get_pointer)

 ;(define-cproc dlerror () internal_dlerror)
 ;(define-cproc dlsym (shared-object c-name) internal_dlsym)
 ;(define-cproc internal-ffi-call (nargs rtype atypes fn rvalue avalues) internal_ffi_call)
 ;(define-cproc scheme-procedure-to-pointer (procedure) scheme_procedure_to_pointer)

 ;(define-cproc get-ffi-type-int8 () get_ffi_type_int8)
 ;(define-cproc get-ffi-type-uint8 () get_ffi_type_uint8)
 ;(define-cproc get-ffi-type-int16 () get_ffi_type_int16)
 ;(define-cproc get-ffi-type-uint16 () get_ffi_type_uint16)
 ;(define-cproc get-ffi-type-int32 () get_ffi_type_int32)
 ;(define-cproc get-ffi-type-uint32 () get_ffi_type_uint32)
 ;(define-cproc get-ffi-type-int64 () get_ffi_type_int64)
 ;(define-cproc get-ffi-type-uint64 () get_ffi_type_uint64)
 ;(define-cproc get-ffi-type-char () get_ffi_type_char)
 ;(define-cproc get-ffi-type-unsigned-char () get_ffi_type_unsigned_char)
 ;(define-cproc get-ffi-type-short () get_ffi_type_short)
 ;(define-cproc get-ffi-type-unsigned-short () get_ffi_type_unsigned_short)
 ;(define-cproc get-ffi-type-int () get_ffi_type_int)
 ;(define-cproc get-ffi-type-unsigned-int () get_ffi_type_unsigned_int)
 ;(define-cproc get-ffi-type-long () get_ffi_type_long)
 ;(define-cproc get-ffi-type-unsigned-long () get_ffi_type_unsigned_long)
 ;(define-cproc get-ffi-type-float () get_ffi_type_float)
 ;(define-cproc get-ffi-type-double () get_ffi_type_double)
 ;(define-cproc get-ffi-type-void() get_ffi_type_void)
 ;(define-cproc get-ffi-type-pointer () get_ffi_type_pointer)

 ;(define-cproc procedure-to-pointer (procedure) procedure_to_pointer)
 )
