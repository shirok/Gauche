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
  (export open-shared-library
          close-shared-library
          shared-object-suffix

          c-function
          make-c-function
          ;TODO address

          ;TODO c-callback
          ;TODO make-c-callback
          ;TODO free-c-callback

          pointer?
          ;TODO integer->pointer
          ;TODO pointer->integer
          ;TODO pointer->uinteger
          string->pointer
          pointer->string
          ;TODO pointer->bytevector
          ;TODO bytevector->pointer
          ;TODO deref
          ;TODO pointer-address
          allocate-pointer
          ;TODO c-malloc
          ;TODO c-free
          null-pointer
          null-pointer?
          empty-pointer
          pointer-ref-c-int8
          pointer-ref-c-uint8
          pointer-ref-c-int16
          pointer-ref-c-uint16
          pointer-ref-c-int32
          pointer-ref-c-uint32
          pointer-ref-c-int64
          pointer-ref-c-uint64
          pointer-ref-c-char
          pointer-ref-c-unsigned-char
          pointer-ref-c-short
          pointer-ref-c-unsigned-short
          pointer-ref-c-int
          pointer-ref-c-unsigned-int
          pointer-ref-c-long
          pointer-ref-c-unsigned-long
          ;TODO pointer-ref-c-long-long
          ;TODO pointer-ref-c-unsigned-long-long
          pointer-ref-c-float
          pointer-ref-c-double
          pointer-ref-c-pointer

          pointer-set-c-int8!
          pointer-set-c-uint8!
          pointer-set-c-int16!
          pointer-set-c-uint16!
          pointer-set-c-int32!
          pointer-set-c-uint32!
          pointer-set-c-int64!
          pointer-set-c-uint64!
          pointer-set-c-char!
          pointer-set-c-unsigned-char!
          pointer-set-c-short!
          pointer-set-c-unsigned-short!
          pointer-set-c-int!
          pointer-set-c-unsigned-int!
          pointer-set-c-long!
          pointer-set-c-unsigned-long!
          ;TODO pointer-set-c-long-long!
          ;TODO pointer-set-c-unsigned-long-long!
          pointer-set-c-float!
          pointer-set-c-double!
          pointer-set-c-pointer!

          size-of-type
          align-of-type))
(select-module gauche.ffi)

(inline-stub
 (.include "gauche-ffi.h")
 (initcode (Scm_Init_gauche_ffi))

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

 (define-cproc internal-open-shared-library (path::<string> version::<string> throw_error) internal_open_shared_library)
 (define-cproc close-shared-library (shared-library) close_shared_library)
 (define-cproc shared-object-suffix() shared_object_suffix)
 ;(define-cproc internal-address (pointer offset::<int>) address)
 (define-cproc pointer? (pointer) is_pointer)
 ;(define-cproc integer->pointer (integer::<int>) integer_to_pointer)
 ;(define-cproc pointer->integer (pointer) pointer_to_integer)
 ;(define-cproc pointer->uinteger (pointer) pointer_to_uinteger)
 (define-cproc string->pointer (str) string_to_pointer)
 (define-cproc pointer->string (pointer) pointer_to_string)
 (define-cproc allocate-pointer(size::<int>) allocate_pointer)
 (define-cproc empty-pointer () empty_pointer)
 (define-cproc null-pointer? (object) is_null_pointer)
 ;(define-cproc pointer-free (pointer) pointer_free)

 (define-cproc pointer-set-c-int8! (pointer offset::<int> value::<int8>) pointer_set_c_int8)
 (define-cproc pointer-set-c-uint8! (pointer offset::<int> value::<uint8>) pointer_set_c_uint8)
 (define-cproc pointer-set-c-int16! (pointer offset::<int> value::<int16>) pointer_set_c_int16)
 (define-cproc pointer-set-c-uint16! (pointer offset::<int> value::<int16>) pointer_set_c_uint16)
 (define-cproc pointer-set-c-int32! (pointer offset::<int> value::<int32>) pointer_set_c_int32)
 (define-cproc pointer-set-c-uint32! (pointer offset::<int> value::<int32>) pointer_set_c_uint32)
 (define-cproc pointer-set-c-int64! (pointer offset::<int> value::<int64>) pointer_set_c_int64)
 (define-cproc pointer-set-c-uint64! (pointer offset::<int> value::<int64>) pointer_set_c_uint64)
 (define-cproc pointer-set-c-char! (pointer offset::<int> value::<char>) pointer_set_c_char)
 (define-cproc pointer-set-c-unsigned-char! (pointer offset::<int> value::<char>) pointer_set_c_unsigned_char)
 (define-cproc pointer-set-c-short! (pointer offset::<int> value::<short>) pointer_set_c_short)
 (define-cproc pointer-set-c-unsigned-short! (pointer offset::<int> value::<short>) pointer_set_c_unsigned_short)
 (define-cproc pointer-set-c-int! (pointer offset::<int> value::<int>) pointer_set_c_int)
 (define-cproc pointer-set-c-unsigned-int! (pointer offset::<int> value::<int>) pointer_set_c_unsigned_int)
 (define-cproc pointer-set-c-long! (pointer offset::<int> value::<long>) pointer_set_c_long)
 (define-cproc pointer-set-c-unsigned-long! (pointer offset::<int> value::<long>) pointer_set_c_unsigned_long)
 (define-cproc pointer-set-c-float! (pointer offset::<int> value::<float>) pointer_set_c_float)
 (define-cproc pointer-set-c-double! (pointer offset::<int> value::<double>) pointer_set_c_double)
 (define-cproc pointer-set-c-pointer! (pointer offset::<int> value) pointer_set_c_pointer)

 (define-cproc pointer-ref-c-int8 (pointer offset::<int>) pointer_ref_c_int8)
 (define-cproc pointer-ref-c-uint8 (pointer offset::<int>) pointer_ref_c_uint8)
 (define-cproc pointer-ref-c-int16 (pointer offset::<int>) pointer_ref_c_int16)
 (define-cproc pointer-ref-c-uint16 (pointer offset::<int>) pointer_ref_c_uint16)
 (define-cproc pointer-ref-c-int32 (pointer offset::<int>) pointer_ref_c_int32)
 (define-cproc pointer-ref-c-uint32 (pointer offset::<int>) pointer_ref_c_uint32)
 (define-cproc pointer-ref-c-int64 (pointer offset::<int>) pointer_ref_c_int64)
 (define-cproc pointer-ref-c-uint64 (pointer offset::<int>) pointer_ref_c_uint64)
 (define-cproc pointer-ref-c-char (pointer offset::<int>) pointer_ref_c_char)
 (define-cproc pointer-ref-c-unsigned-char (pointer offset::<int>) pointer_ref_c_unsigned_char)
 (define-cproc pointer-ref-c-short (pointer offset::<int>) pointer_ref_c_short)
 (define-cproc pointer-ref-c-unsigned-short (pointer offset::<int>) pointer_ref_c_unsigned_short)
 (define-cproc pointer-ref-c-int (pointer offset::<int>) pointer_ref_c_int)
 (define-cproc pointer-ref-c-unsigned-int (pointer offset::<int>) pointer_ref_c_unsigned_int)
 (define-cproc pointer-ref-c-long (pointer offset::<int>) pointer_ref_c_long)
 (define-cproc pointer-ref-c-unsigned-long (pointer offset::<int>) pointer_ref_c_unsigned_long)
 (define-cproc pointer-ref-c-float (pointer offset::<int>) pointer_ref_c_float)
 (define-cproc pointer-ref-c-double (pointer offset::<int>) pointer_ref_c_double)
 (define-cproc pointer-ref-c-pointer (pointer offset::<int>) pointer_ref_c_pointer)

 ;(define-cproc dlerror () internal_dlerror)
 (define-cproc dlsym (shared-object c-name::<string>) internal_dlsym)
 (define-cproc internal-ffi-call (nargs rtype atypes fn rvalue avalues) internal_ffi_call)
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

(define size-of-type
  (lambda (type)
    (cond ((eq? type 'int8) (size-of-int8))
          ((eq? type 'uint8) (size-of-uint8))
          ((eq? type 'int16) (size-of-int16))
          ((eq? type 'uint16) (size-of-uint16))
          ((eq? type 'int32) (size-of-int32))
          ((eq? type 'uint32) (size-of-uint32))
          ((eq? type 'int64) (size-of-int64))
          ((eq? type 'uint64) (size-of-uint64))
          ((eq? type 'char) (size-of-char))
          ((eq? type 'unsigned-char) (size-of-char))
          ((eq? type 'short) (size-of-short))
          ((eq? type 'unsigned-short) (size-of-unsigned-short))
          ((eq? type 'int) (size-of-int))
          ((eq? type 'unsigned-int) (size-of-unsigned-int))
          ((eq? type 'long) (size-of-long))
          ((eq? type 'unsigned-long) (size-of-unsigned-long))
          ((eq? type 'float) (size-of-float))
          ((eq? type 'double) (size-of-double))
          ((eq? type 'pointer) (size-of-pointer))
          ((not (equal? type 'void)) (error "No such foreign type" type)))))

 (define (open-shared-library path . versions)
   (let ((shared-library-version ""))
     (for-each
       (lambda (version)
         (let ((version-string (if (number? version)
                                 (number->string version)
                                 version)))
           (write (string-append path
                                 "."
                                 (shared-object-suffix)
                                 "."
                                 version-string))
           (newline)
           (when (internal-open-shared-library path version-string #f)
             (set! shared-library-version version-string))))
       (if (null? versions) '() (car versions)))
       (internal-open-shared-library path shared-library-version #t)))

(define pointer-get
  (lambda (pointer type offset)
    (cond ((equal? type 'int8) (pointer-ref-c-int8 pointer offset))
          ((equal? type 'uint8) (pointer-ref-c-uint8 pointer offset))
          ((equal? type 'int16) (pointer-ref-c-int16 pointer offset))
          ((equal? type 'uint16) (pointer-ref-c-uint16 pointer offset))
          ((equal? type 'int32) (pointer-ref-c-int32 pointer offset))
          ((equal? type 'uint32) (pointer-ref-c-uint32 pointer offset))
          ((equal? type 'int64) (pointer-ref-c-int32 pointer offset))
          ((equal? type 'uint64) (pointer-ref-c-uint64 pointer offset))
          ((equal? type 'char) (integer->char (pointer-ref-c-char pointer offset)))
          ((equal? type 'unsigned-char) (integer->char (pointer-ref-c-uint8 pointer offset)))
          ((equal? type 'short) (pointer-ref-c-short pointer offset))
          ((equal? type 'unsigned-short) (pointer-ref-c-unsigned-short pointer offset))
          ((equal? type 'int) (pointer-ref-c-int pointer offset))
          ((equal? type 'unsigned-int) (pointer-ref-c-unsigned-int pointer offset))
          ((equal? type 'long) (pointer-ref-c-long pointer offset))
          ((equal? type 'unsigned-long) (pointer-ref-c-unsigned-long pointer offset))
          ((equal? type 'float) (pointer-ref-c-float pointer offset))
          ((equal? type 'double) (pointer-ref-c-double pointer offset))
          ((equal? type 'pointer) (pointer-ref-c-pointer pointer offset))
          ((equal? type 'void) (error "Can not get type void from pointer" type))
          (else (error "No such foreign type" type)))))

(define type->libffi-type-number
  (lambda (type)
    (cond ((equal? type 'int8) 1)
          ((equal? type 'uint8) 2)
          ((equal? type 'int16) 3)
          ((equal? type 'uint16) 4)
          ((equal? type 'int32) 5)
          ((equal? type 'uint32) 6)
          ((equal? type 'int64) 7)
          ((equal? type 'uint64) 8)
          ((equal? type 'char) 9)
          ((equal? type 'unsigned-char) 10)
          ((equal? type 'short) 11)
          ((equal? type 'unsigned-short) 12)
          ((equal? type 'int) 13)
          ((equal? type 'unsigned-int) 14)
          ((equal? type 'long) 15)
          ((equal? type 'unsigned-long) 16)
          ((equal? type 'float) 17)
          ((equal? type 'double) 18)
          ((equal? type 'void) 19)
          ((equal? type 'pointer) 20)
          ((equal? type 'pointer-address) 21)
          ((equal? type 'callback) 22)
          (else (error "Undefined type" type)))))

(define c-function
  (lambda (shared-object c-name return-type . argument-types)
    (make-c-function shared-object c-name return-type argument-types)))

(define make-c-function
  (lambda (shared-object return-type c-name argument-types)
    (let ((c-function (dlsym shared-object (symbol->string c-name))))
      (lambda arguments
        (let ((return-pointer (internal-ffi-call (length argument-types)
                                                 (type->libffi-type-number return-type)
                                                 (map type->libffi-type-number argument-types)
                                                 c-function
                                                 (size-of-type return-type)
                                                 arguments)))
          (if (equal? return-type 'void)
            (undefined)
            (pointer-get return-pointer return-type 0)))))))

(define null-pointer (empty-pointer))

(define size-of-type
  (lambda (type)
    (cond ((eq? type 'int8) (size-of-int8))
          ((eq? type 'uint8) (size-of-uint8))
          ((eq? type 'int16) (size-of-int16))
          ((eq? type 'uint16) (size-of-uint16))
          ((eq? type 'int32) (size-of-int32))
          ((eq? type 'uint32) (size-of-uint32))
          ((eq? type 'int64) (size-of-int64))
          ((eq? type 'uint64) (size-of-uint64))
          ((eq? type 'char) (size-of-char))
          ((eq? type 'unsigned-char) (size-of-char))
          ((eq? type 'short) (size-of-short))
          ((eq? type 'unsigned-short) (size-of-unsigned-short))
          ((eq? type 'int) (size-of-int))
          ((eq? type 'unsigned-int) (size-of-unsigned-int))
          ((eq? type 'long) (size-of-long))
          ((eq? type 'unsigned-long) (size-of-unsigned-long))
          ((eq? type 'float) (size-of-float))
          ((eq? type 'double) (size-of-double))
          ((eq? type 'pointer) (size-of-pointer))
          (else (error "Can not get size of type" type)))))

(define align-of-type
  (lambda (type)
    (cond ((eq? type 'int8) (align-of-int8))
          ((eq? type 'uint8) (align-of-uint8))
          ((eq? type 'int16) (align-of-int16))
          ((eq? type 'uint16) (align-of-uint16))
          ((eq? type 'int32) (align-of-int32))
          ((eq? type 'uint32) (align-of-uint32))
          ((eq? type 'int64) (align-of-int64))
          ((eq? type 'uint64) (align-of-uint64))
          ((eq? type 'char) (align-of-char))
          ((eq? type 'unsigned-char) (align-of-char))
          ((eq? type 'short) (align-of-short))
          ((eq? type 'unsigned-short) (align-of-unsigned-short))
          ((eq? type 'int) (align-of-int))
          ((eq? type 'unsigned-int) (align-of-unsigned-int))
          ((eq? type 'long) (align-of-long))
          ((eq? type 'unsigned-long) (align-of-unsigned-long))
          ((eq? type 'float) (align-of-float))
          ((eq? type 'double) (align-of-double))
          ((eq? type 'pointer) (align-of-pointer))
          (else (error "Can not get align of type" type)))))
