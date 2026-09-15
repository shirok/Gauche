;;;
;;; SRFI-281 Bytevector utilities
;;;

;; Most of srfi-281 APIs are already supported by core / other libraries.
;; This module just reexport it.

(define-module srfi.281
  (use gauche.uvector)
  (use scheme.bytevector)
  (use srfi.207)
  (export endianness?
          endianness        ; scheme.bytevector
          native-endianness ; scheme.bytevector
          make-bytevector   ; gauche.uvector
          bytevector=?      ; gauche.uvector
          bytevector<?      ; gauche.uvector
          bytevector<=?     ; gauche.uvector
          bytevector>?      ; gauche.uvector
          bytevector>=?     ; gauche.uvector
          bytevector-fill!  ; gauche.uvector

          bytevector->hex-string        ;srfi.207
          hex-string->bytevector        ;srfi.207
          bytevector->base64            ;srfi.207
          base64->bytevector            ;srfi.207
          )
  )
(select-module srfi.281)

;; TODO: Consolidate the allowed list of endianness with scheme.bytevector
(define (endianness? obj)
  (boolean (memq obj (big big-endian little little-endian
                          arm-little-endian))))
