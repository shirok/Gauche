;;;
;;; scheme.bytevector - R6RS-compatible bytevector library
;;;
;;;   Copyright (c) 2020-2022  Shiro Kawai  <shiro@acm.org>
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

(define-module scheme.bytevector
  (use util.match)
  (use gauche.uvector)
  (use gauche.unicode)
  (use binary.io)
  (export endianness native-endianness
          bytevector?                   ; gauche.uvector
          make-bytevector               ; gauche.uvector
          bytevector-length             ; gauche.uvector
          bytevector=?                  ; gauche.uvector
          bytevector-fill!              ; gauche.uvector
          (rename bytevector-copy!-r6 bytevector-copy!) ; gauche.uvector
          bytevector-copy               ; subset of gauche.uvector
          bytevector-u8-ref             ; gauche.uvector
          bytevector-s8-ref             ; gauche.uvector
          bytevector-u8-set!            ; gauche.uvector
          bytevector-s8-set!            ; gauche.uvector
          bytevector->u8-list           ; gauche.uvector
          u8-list->bytevector           ; gauche.uvector
          bytevector-uint-ref
          bytevector-sint-ref
          bytevector-uint-set!
          bytevector-sint-set!
          bytevector->uint-list
          bytevector->sint-list
          uint-list->bytevector
          sint-list->bytevector
          bytevector-u16-ref
          bytevector-s16-ref
          bytevector-u16-native-ref
          bytevector-s16-native-ref
          bytevector-u16-set!
          bytevector-s16-set!
          bytevector-u16-native-set!
          bytevector-s16-native-set!
          bytevector-u32-ref
          bytevector-s32-ref
          bytevector-u32-native-ref
          bytevector-s32-native-ref
          bytevector-u32-set!
          bytevector-s32-set!
          bytevector-u32-native-set!
          bytevector-s32-native-set!
          bytevector-u64-ref
          bytevector-s64-ref
          bytevector-u64-native-ref
          bytevector-s64-native-ref
          bytevector-u64-set!
          bytevector-s64-set!
          bytevector-u64-native-set!
          bytevector-s64-native-set!
          bytevector-ieee-single-ref
          bytevector-ieee-double-ref
          bytevector-ieee-single-native-ref
          bytevector-ieee-double-native-ref
          bytevector-ieee-single-set!
          bytevector-ieee-double-set!
          bytevector-ieee-single-native-set!
          bytevector-ieee-double-native-set!
          string->utf8                  ;gauche.unicode
          string->utf16                 ;gauche.unicode
          string->utf32                 ;gauche.unicode
          utf8->string                  ;gauche.unicode
          utf16->string                 ;gauche.unicode
          utf32->string                 ;gauche.unicode
          ))
(select-module scheme.bytevector)

(define-syntax endianness
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ e)
        (let1 ee (unwrap-syntax e)
          (unless (memq e
                        '(big big-endian little little-endian
                          arm-little-endian))
            (error "Unrecognized endianness symbol. Must be either one \
                    of big, big-endian, little, little-endian or \
                    arm-little-endian, but got:" ee))
          (quasirename r `',ee))]
       [_ (error "Malformed endianness: " f)]))))

(define (native-endianness) (native-endian))

(define (bytevector-uint-ref v pos endian size)
  (get-uint size v pos endian))
(define (bytevector-sint-ref v pos endian size)
  (get-sint size v pos endian))
(define (bytevector-uint-set! v pos val endian size)
  (put-uint! size v pos val endian))
(define (bytevector-sint-set! v pos val endian size)
  (put-sint! size v pos val endian))

(define (%v->list get v endian size)
  (assume-type <u8vector> v)
  (let1 len (u8vector-length v)
    (unless (zero? (mod len size))
      (errorf "bytevector length (~s) is not divisible by integer size (~s)"
              len size))
    (let loop ([pos 0] [r '()])
      (if (= pos len)
        (reverse r)
        (loop (+ pos size)
              (cons (get size v pos endian) r))))))

(define (bytevector->uint-list v endian size)
  (%v->list get-uint v endian size))
(define (bytevector->sint-list v endian size)
  (%v->list get-sint v endian size))

(define (%list->v put lis endian size)
  (rlet1 v (make-u8vector (* size (length list)))
    (do ([lis lis (cdr lis)]
         [pos 0   (+ pos size)])
        [(null? lis)]
      (put size v pos (car lis) endian))))

(define (uint-list->bytevector lis endian size)
  (%list->v put-uint! lis endian size))
(define (sint-list->bytevector lis endian size)
  (%list->v put-sint! lis endian size))

(define ne (native-endian))

(define (bytevector-u16-ref v k endian) (get-u16 v k endian))
(define (bytevector-s16-ref v k endian) (get-s16 v k endian))
(define (bytevector-u32-ref v k endian) (get-u32 v k endian))
(define (bytevector-s32-ref v k endian) (get-s32 v k endian))
(define (bytevector-u64-ref v k endian) (get-u64 v k endian))
(define (bytevector-s64-ref v k endian) (get-s64 v k endian))
(define (bytevector-u16-native-ref v k endian) (get-u16 v k ne))
(define (bytevector-s16-native-ref v k endian) (get-s16 v k ne))
(define (bytevector-u32-native-ref v k endian) (get-u32 v k ne))
(define (bytevector-s32-native-ref v k endian) (get-s32 v k ne))
(define (bytevector-u64-native-ref v k endian) (get-u64 v k ne))
(define (bytevector-s64-native-ref v k endian) (get-s64 v k ne))

(define (bytevector-u16-set! v k n endian) (put-u16! v k n endian))
(define (bytevector-s16-set! v k n endian) (put-s16! v k n endian))
(define (bytevector-u32-set! v k n endian) (put-u32! v k n endian))
(define (bytevector-s32-set! v k n endian) (put-s32! v k n endian))
(define (bytevector-u64-set! v k n endian) (put-u64! v k n endian))
(define (bytevector-s64-set! v k n endian) (put-s64! v k n endian))
(define (bytevector-u16-native-set! v k n) (put-u16! v k n ne))
(define (bytevector-s16-native-set! v k n) (put-s16! v k n ne))
(define (bytevector-u32-native-set! v k n) (put-u32! v k n ne))
(define (bytevector-s32-native-set! v k n) (put-s32! v k n ne))
(define (bytevector-u64-native-set! v k n) (put-u64! v k n ne))
(define (bytevector-s64-native-set! v k n) (put-s64! v k n ne))

(define (bytevector-ieee-single-ref v k endian) (get-f32 v k endian))
(define (bytevector-ieee-double-ref v k endian) (get-f64 v k endian))
(define (bytevector-ieee-single-native-ref v k) (get-f32 v k ne))
(define (bytevector-ieee-double-native-ref v k) (get-f64 v k ne))
(define (bytevector-ieee-single-set! v k x endian) (put-f32! v k x endian))
(define (bytevector-ieee-double-set! v k x endian) (put-f64! v k x endian))
(define (bytevector-ieee-single-native-set! v k x) (put-f32! v k x ne))
(define (bytevector-ieee-double-native-set! v k x) (put-f64! v k x ne))
