;;;
;;; SRFI-281 Bytevector utilities
;;;

;; Most of srfi-281 APIs are already supported by core / other libraries.
;; This module just reexport it.

(define-module srfi.281
  (use gauche.uvector)
  (use gauche.unicode)
  (use scheme.bytevector)
  (use binary.io)
  (use srfi.207)
  (export endianness?
          endianness        ; scheme.bytevector
          native-endianness ; scheme.bytevector
          make-bytevector   ; gauche.uvector
          bytevector=?      ; gauche.uvector
          ;bytevector<?
          ;bytevector<=?
          ;bytevector>?
          ;bytevector>=?
          bytevector-fill!  ; gauche.uvector

          bytevector->hex-string        ;srfi.207
          hex-string->bytevector        ;srfi.207
          bytevector->base64            ;srfi.207
          base64->bytevector            ;srfi.207

          u8-list->bytevector           ;gauche.uvector
          bytevector->u8-list           ;gauche.uvector
          bytevector-u8-ref             ;gauche.uvector
          bytevector-u8-set!            ;gauche.uvector
          bytevector-s8-ref             ;gauche.uvector
          bytevector-s8-set!            ;gauche.uvector

          bytevector-uint-ref
          bytevector-uint-set!
          bytevector-sint-ref
          bytevector-sint-set!

          bytevector->uint-list         ;scheme.bytevector
          bytevector->sint-list         ;scheme.bytevector
          uint-list->bytevector         ;scheme.bytevector
          sint-list->bytevector         ;scheme.bytevector

          bytevector-u16-ref            ;scheme.bytevector
          bytevector-s16-ref            ;scheme.bytevector
          bytevector-u32-ref            ;scheme.bytevector
          bytevector-s32-ref            ;scheme.bytevector
          bytevector-u64-ref            ;scheme.bytevector
          bytevector-s64-ref            ;scheme.bytevector
          bytevector-u16-native-ref     ;scheme.bytevector
          bytevector-s16-native-ref     ;scheme.bytevector
          bytevector-u32-native-ref     ;scheme.bytevector
          bytevector-s32-native-ref     ;scheme.bytevector
          bytevector-u64-native-ref     ;scheme.bytevector
          bytevector-s64-native-ref     ;scheme.bytevector
          bytevector-u16-set!           ;scheme.bytevector
          bytevector-s16-set!           ;scheme.bytevector
          bytevector-u32-set!           ;scheme.bytevector
          bytevector-s32-set!           ;scheme.bytevector
          bytevector-u64-set!           ;scheme.bytevector
          bytevector-s64-set!           ;scheme.bytevector
          bytevector-u16-native-set!    ;scheme.bytevector
          bytevector-s16-native-set!    ;scheme.bytevector
          bytevector-u32-native-set!    ;scheme.bytevector
          bytevector-s32-native-set!    ;scheme.bytevector
          bytevector-u64-native-set!    ;scheme.bytevector
          bytevector-s64-native-set!    ;scheme.bytevector

          bytevector-binary32-ref
          bytevector-ieee-single-ref        ;scheme.bytevector
          bytevector-binary64-ref
          bytevector-ieee-double-ref        ;scheme.bytevector
          bytevector-binary32-native-ref
          bytevector-ieee-single-native-ref ;scheme.bytevector
          bytevector-binary64-native-ref
          bytevector-ieee-double-native-ref ;scheme.bytevector

          bytevector-binary32-set!
          bytevector-ieee-single-set!        ;scheme.bytevector
          bytevector-binary64-set!
          bytevector-ieee-double-set!        ;scheme.bytevector
          bytevector-binary32-native-set!
          bytevector-ieee-single-native-set! ;scheme.bytevector
          bytevector-binary64-native-set!
          bytevector-ieee-double-native-set! ;scheme.bytevector

          ;utf8->string
          ;utf16->string
          ;utf32->string
          string->utf8                  ; gauche.unicode
          (rename srfi-281:string->utf16 string->utf16)
          (rename srfi-281:string->utf32 string->utf32)
          )
  )
(select-module srfi.281)

;; TODO: Consolidate the allowed list of endianness with scheme.bytevector
(define (endianness? obj)
  (boolean (memq obj '(big big-endian little little-endian
                           arm-little-endian))))

(define (bytevector-uint-ref bv k endianness size)
  (get-uint size bv k endianness))
(define (bytevector-uint-set! bv k n endianness size)
  (put-uint! size bv k n endianness))
(define (bytevector-sint-ref bv k endianness size)
  (get-sint size bv k endianness))
(define (bytevector-sint-set! bv k n endianness size)
  (put-sint! size bv k n endianness))

(define bytevector-binary32-ref bytevector-ieee-single-ref)
(define bytevector-binary64-ref bytevector-ieee-double-ref)
(define bytevector-binary32-native-ref bytevector-ieee-single-native-ref)
(define bytevector-binary64-native-ref bytevector-ieee-double-native-ref)
(define bytevector-binary32-set! bytevector-ieee-single-set!)
(define bytevector-binary64-set! bytevector-ieee-double-set!)
(define bytevector-binary32-native-set! bytevector-ieee-single-native-set!)
(define bytevector-binary64-native-set! bytevector-ieee-double-native-set!)

;; Gauche's string->utf{16|32} takes add-bom? argument.
(define (srfi-281:string->utf16 str :optional (endian 'big-endian) start end)
  (string->utf16 str endian #f start end))
(define (srfi-281:string->utf32 str :optional (endian 'big-endian) start end)
  (string->utf32 str endian #f start end))
