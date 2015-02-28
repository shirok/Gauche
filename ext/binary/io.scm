;;; binary.io -- serializing binary data

;;; Created:    <2003-01-20 23:25:06 foof>
;;; Time-stamp: <2003-01-29 20:53:45 foof>
;;; Author:     Alex Shinn <foof@synthcode.com>

;; It is computed that eleven thousand persons have at several times
;; suffered death, rather than submit to break their eggs at the smaller
;; end.  Many hundred large volumes have been published upon this
;; controversy: but the books of the Big-endians have been long
;; forbidden, and the whole party rendered incapable by law of holding
;; employments.
;;          -- from "Gulliver's Travels" by Jonathan Swift

;; This module was originally written by Alex Shinn, in pure Scheme.
;; Shiro Kawai rewrote primitive routines in C for the better performance,
;; renamed them for shorter names, and added uvector access routines.

(define-module binary.io
  (export read-uint read-u8 read-u16 read-u32 read-u64
          read-sint read-s8 read-s16 read-s32 read-s64
          read-ber-integer read-f16 read-f32 read-f64
          write-uint write-u8 write-u16 write-u32 write-u64
          write-sint write-s8 write-s16 write-s32 write-s64
          write-ber-integer write-f16 write-f32 write-f64
          get-u8 get-u16 get-u32 get-u64 get-s8 get-s16 get-s32 get-s64
          get-f16 get-f32 get-f64
          get-u16be get-u16le get-u32be get-u32le get-u64be get-u64le
          get-s16be get-s16le get-s32be get-s32le get-s64be get-s64le
          get-f16be get-f16le get-f32be get-f32le get-f64be get-f64le
          put-u8! put-u16! put-u32! put-u64! put-s8! put-s16! put-s32! put-s64!
          put-f16! put-f32! put-f64!
          put-u16be! put-u16le! put-u32be! put-u32le! put-u64be! put-u64le!
          put-s16be! put-s16le! put-s32be! put-s32le! put-s64be! put-s64le!
          put-f16be! put-f16le! put-f32be! put-f32le! put-f64be! put-f64le!

          ;; old names
          read-binary-uint
          read-binary-uint8 read-binary-uint16
          read-binary-uint32 read-binary-uint64
          read-binary-sint
          read-binary-sint8 read-binary-sint16
          read-binary-sint32 read-binary-sint64
          read-binary-short  read-binary-ushort
          read-binary-long   read-binary-ulong
          read-binary-float read-binary-double
          write-binary-uint
          write-binary-uint8 write-binary-uint16
          write-binary-uint32 write-binary-uint64
          write-binary-sint
          write-binary-sint8 write-binary-sint16
          write-binary-sint32 write-binary-sint64
          write-binary-short  write-binary-ushort
          write-binary-long   write-binary-ulong
          write-binary-float  write-binary-double
          ))
(select-module binary.io)

;;;
;;; config
;;;

(define-constant *bit-size* 2)   ;; hey, you never know :)
(define-constant *byte-size* 8)
(define-constant *byte-magnitude* (expt *bit-size* *byte-size*))
(define-constant *byte-mask* (- *byte-magnitude* 1))
(define-constant *byte-right-shift* (* -1 *byte-size*))

;;;
;;; basic reading
;;;

;; mind-numblingly slow, consider a uvector approach but it doesn't
;; handle endianess
(define (read-uint size :optional (port (current-input-port))
                                  (endian (default-endian)))
  (case size
    [(1) (read-u8 port endian)]
    [(2) (read-u16 port endian)]
    [(4) (read-u32 port endian)]
    [(8) (read-u64 port endian)]
    [else
     (let loop ([ls '()] [cnt 0])
       (if (= cnt size)
         (fold (^[a b] (+ a (* b *byte-magnitude*)))
               0
               (if (eq? endian 'big-endian) (reverse ls) ls))
         (let1 byte (read-byte port)
           (if (eof-object? byte)
             byte
             (loop (cons byte ls) (+ cnt 1))))))]
    ))

(define (lognot-small int bytes)
  (logand (lognot int) (- (expt *bit-size* (* *byte-size* bytes)) 1)))

(define (uint->sint int bytes)
  (let1 highbit (- (* *byte-size* bytes) 1)
    (if (logbit? highbit int)
      (* -1 (+ 1 (lognot-small int bytes)))
      int)))

(define (sint->uint int bytes)
  (if (< int 0)
    (+ 1 (lognot-small (abs int) bytes))
    int))

(define (read-sint size :optional (port (current-input-port))
                                  (endian (default-endian)))
  (case size
    [(1) (read-s8 port endian)]
    [(2) (read-s16 port endian)]
    [(4) (read-s32 port endian)]
    [(8) (read-s64 port endian)]
    [else (uint->sint (read-uint size port endian) size)]))

;;;
;;; basic writing
;;;

(define (write-uint size int :optional (port (current-output-port))
                                       (endian (default-endian)))
  (case size
    [(1) (write-u8 int port endian)]
    [(2) (write-u16 int port endian)]
    [(4) (write-u32 int port endian)]
    [(8) (write-u64 int port endian)]
    [else
     (let ([ls '()])
       ;; build a list of bytes
       (dotimes [i size]
         (push! ls (logand int *byte-mask*))
         (set! int (ash int *byte-right-shift*)))
       ;; reverse if big-endian
       (unless (eq? endian 'big-endian)
         (set! ls (reverse ls)))
       ;; write the list
       (for-each (cut write-byte <> port) ls))]))

(define (write-sint size int :optional (port (current-output-port))
                                       (endian (default-endian)))
  (case size
    [(1) (write-s8 int port endian)]
    [(2) (write-s16 int port endian)]
    [(4) (write-s32 int port endian)]
    [(8) (write-s64 int port endian)]
    [else (write-uint size (sint->uint int size) port endian)]))

;;;
;;; compatibility
;;;

;; These are used in binary.pack for the (unofficial) features to
;; read/write integers in "native" width of C system on the platform.
;; Should be removed soon (after binary.pack rewrite).  Do not use them.
(define read-binary-short  read-s16)
(define read-binary-ushort read-u16)
(define read-binary-long   read-s32)
(define read-binary-ulong  read-u32)

(define write-binary-short  write-s16)
(define write-binary-ushort write-u16)
(define write-binary-long   write-s32)
(define write-binary-ulong  write-u32)

;; Other compatibility names.  They have been official befor 0.8.10,
;; and used widely.  So we keep them for a while.
(define read-binary-uint  read-uint)
(define read-binary-sint  read-sint)
(define read-binary-uint8 read-u8)
(define read-binary-sint8 read-s8)
(define read-binary-uint16 read-u16)
(define read-binary-sint16 read-s16)
(define read-binary-uint32 read-u32)
(define read-binary-sint32 read-s32)
(define read-binary-uint64 read-u64)
(define read-binary-sint64 read-s64)
(define read-binary-float  read-f32)
(define read-binary-double read-f64)

(define write-binary-uint  write-uint)
(define write-binary-sint  write-sint)
(define write-binary-uint8 write-u8)
(define write-binary-sint8 write-s8)
(define write-binary-uint16 write-u16)
(define write-binary-sint16 write-s16)
(define write-binary-uint32 write-u32)
(define write-binary-sint32 write-s32)
(define write-binary-uint64 write-u64)
(define write-binary-sint64 write-s64)
(define write-binary-float  write-f32)
(define write-binary-double write-f64)

;;;
;;; bignum encodings -- Basic Encoding Rules (BER) from X.209
;;;

;; A BER compressed integer is an unsigned integer in base 128, most
;; significant digit first, where the high bit is set on all but the
;; final (least significant) byte.  Thus any size integer can be
;; encoded, but the encoding is efficient and small integers don't take
;; up any more space than they would in normal char/short/int encodings.

(define (read-ber-integer :optional (port (current-input-port)))
  (let1 first (read-byte port)
    (cond [(eof-object? first) first] ;; stop on eof
          [(< first 128) first]
          [else (let loop ([res (ash (logand first #b01111111) 7)]
                           [byte (read-u8 port)])
                  (if (< byte 128)
                    (+ res byte) ;; final byte
                    (loop (ash (+ res (logand byte #b01111111)) 7)
                          (read-u8 port))))])))

(define (write-ber-integer number :optional (port (current-output-port)))
  (let ([final (logand number #b01111111)]
        [start (ash number -7)])
    (unless (zero? start)
      (let loop ([n start])
        (cond [(< n 128) (write-u8 (logior n #b10000000) port)]
              [else (loop (ash n -7)) ;; write high bytes first
                    (write-u8 (logior (logand n #b01111111) #b10000000) port)])))
    (write-u8 final port)))

;;;
;;;  Low-level stuff
;;;

(inline-stub
 "#include \"binary.h\""
 
 (define-cproc read-u8
   (:optional (port::<input-port>? #f) (endian::<symbol>? #f))
   Scm_ReadBinaryU8)
 (define-cproc read-u16
   (:optional (port::<input-port>? #f) (endian::<symbol>? #f))
   Scm_ReadBinaryU16)
 (define-cproc read-u32
   (:optional (port::<input-port>? #f) (endian::<symbol>? #f))
   Scm_ReadBinaryU32)
 (define-cproc read-u64
   (:optional (port::<input-port>? #f) (endian::<symbol>? #f))
   Scm_ReadBinaryU64)

 (define-cproc read-s8
   (:optional (port::<input-port>? #f) (endian::<symbol>? #f))
   Scm_ReadBinaryS8)
 (define-cproc read-s16
   (:optional (port::<input-port>? #f) (endian::<symbol>? #f))
   Scm_ReadBinaryS16)
 (define-cproc read-s32
   (:optional (port::<input-port>? #f) (endian::<symbol>? #f))
   Scm_ReadBinaryS32)
 (define-cproc read-s64
   (:optional (port::<input-port>? #f) (endian::<symbol>? #f))
   Scm_ReadBinaryS64)

 (define-cproc write-u8
   (intval::<integer>
    :optional (port::<output-port>? #f) (endian::<symbol>? #f))
   ::<void> Scm_WriteBinaryU8)
 (define-cproc write-u16
   (intval::<integer>
    :optional (port::<output-port>? #f) (endian::<symbol>? #f))
   ::<void> Scm_WriteBinaryU16)
 (define-cproc write-u32
   (intval::<integer>
    :optional (port::<output-port>? #f) (endian::<symbol>? #f))
   ::<void> Scm_WriteBinaryU32)
 (define-cproc write-u64
   (intval::<integer>
    :optional (port::<output-port>? #f) (endian::<symbol>? #f))
   ::<void> Scm_WriteBinaryU64)
 (define-cproc write-s8
   (intval::<integer>
    :optional (port::<output-port>? #f) (endian::<symbol>? #f))
   ::<void> Scm_WriteBinaryS8)
 (define-cproc write-s16
   (intval::<integer>
    :optional (port::<output-port>? #f) (endian::<symbol>? #f))
   ::<void> Scm_WriteBinaryS16)
 (define-cproc write-s32
   (intval::<integer>
    :optional (port::<output-port>? #f) (endian::<symbol>? #f))
   ::<void> Scm_WriteBinaryS32)
 (define-cproc write-s64
   (intval::<integer>
    :optional (port::<output-port>? #f) (endian::<symbol>? #f))
   ::<void> Scm_WriteBinaryS64)

 (define-cproc read-f16
   (:optional (port::<input-port>? #f) (endian::<symbol>? #f))
   Scm_ReadBinaryF16)
 (define-cproc read-f32
   (:optional (port::<input-port>? #f) (endian::<symbol>? #f))
   Scm_ReadBinaryF32)
 (define-cproc read-f64
   (:optional (port::<input-port>? #f) (endian::<symbol>? #f))
   Scm_ReadBinaryF64)

 (define-cproc write-f16
   (val::<number>
    :optional (port::<output-port>? #f) (endian::<symbol>? #f))
   ::<void> Scm_WriteBinaryF16)
 (define-cproc write-f32
   (val::<number>
    :optional (port::<output-port>? #f) (endian::<symbol>? #f))
   ::<void> Scm_WriteBinaryF32)
 (define-cproc write-f64
   (val::<number>
    :optional (port::<output-port>? #f) (endian::<symbol>? #f))
   ::<void> Scm_WriteBinaryF64)

 (define-cproc get-u8
   (v::<uvector> off::<uint> :optional (endian::<symbol>? #f))
   Scm_GetBinaryU8)
 (define-cproc get-s8
   (v::<uvector> off::<uint> :optional (endian::<symbol>? #f))
   Scm_GetBinaryS8)
 (define-cproc get-u16
   (v::<uvector> off::<uint> :optional (endian::<symbol>? #f))
   Scm_GetBinaryU16)
 (define-cproc get-s16
   (v::<uvector> off::<uint> :optional (endian::<symbol>? #f))
   Scm_GetBinaryS16)
 (define-cproc get-u32
   (v::<uvector> off::<uint> :optional (endian::<symbol>? #f))
   Scm_GetBinaryU32)
 (define-cproc get-s32
   (v::<uvector> off::<uint> :optional (endian::<symbol>? #f))
   Scm_GetBinaryS32)
 (define-cproc get-u64
   (v::<uvector> off::<uint> :optional (endian::<symbol>? #f))
   Scm_GetBinaryU64)
 (define-cproc get-s64
   (v::<uvector> off::<uint> :optional (endian::<symbol>? #f))
   Scm_GetBinaryS64)
 (define-cproc get-f16
   (v::<uvector> off::<uint> :optional (endian::<symbol>? #f))
   Scm_GetBinaryF16)
 (define-cproc get-f32
   (v::<uvector> off::<uint> :optional (endian::<symbol>? #f))
   Scm_GetBinaryF32)
 (define-cproc get-f64
   (v::<uvector> off::<uint> :optional (endian::<symbol>? #f))
   Scm_GetBinaryF64)

 (define-cproc put-u8!
   (v::<uvector> off::<uint> val :optional (endian::<symbol>? #f))
   ::<void> Scm_PutBinaryU8)
 (define-cproc put-s8!
   (v::<uvector> off::<uint> val :optional (endian::<symbol>? #f))
   ::<void> Scm_PutBinaryS8)
 (define-cproc put-u16!
   (v::<uvector> off::<uint> val :optional (endian::<symbol>? #f))
   ::<void> Scm_PutBinaryU16)
 (define-cproc put-s16!
   (v::<uvector> off::<uint> val :optional (endian::<symbol>? #f))
   ::<void> Scm_PutBinaryS16)
 (define-cproc put-u32!
   (v::<uvector> off::<uint> val :optional (endian::<symbol>? #f))
   ::<void> Scm_PutBinaryU32)
 (define-cproc put-s32!
   (v::<uvector> off::<uint> val :optional (endian::<symbol>? #f))
   ::<void> Scm_PutBinaryS32)
 (define-cproc put-u64!
   (v::<uvector> off::<uint> val :optional (endian::<symbol>? #f))
   ::<void> Scm_PutBinaryU64)
 (define-cproc put-s64!
   (v::<uvector> off::<uint> val :optional (endian::<symbol>? #f))
   ::<void> Scm_PutBinaryS64)
 (define-cproc put-f16!
   (v::<uvector> off::<uint> val :optional (endian::<symbol>? #f))
   ::<void> Scm_PutBinaryF16)
 (define-cproc put-f32!
   (v::<uvector> off::<uint> val :optional (endian::<symbol>? #f))
   ::<void> Scm_PutBinaryF32)
 (define-cproc put-f64!
   (v::<uvector> off::<uint> val :optional (endian::<symbol>? #f))
   ::<void> Scm_PutBinaryF64)

 (define-cproc get-u16be (v::<uvector> off::<uint>)
   (return (Scm_GetBinaryU16 v off (SCM_SYMBOL SCM_SYM_BIG_ENDIAN))))
 (define-cproc get-u16le (v::<uvector> off::<uint>)
   (return (Scm_GetBinaryU16 v off (SCM_SYMBOL SCM_SYM_LITTLE_ENDIAN))))
 (define-cproc get-s16be (v::<uvector> off::<uint>)
   (return (Scm_GetBinaryS16 v off (SCM_SYMBOL SCM_SYM_BIG_ENDIAN))))
 (define-cproc get-s16le (v::<uvector> off::<uint>)
   (return (Scm_GetBinaryS16 v off (SCM_SYMBOL SCM_SYM_LITTLE_ENDIAN))))
 (define-cproc get-u32be (v::<uvector> off::<uint>)
   (return (Scm_GetBinaryU32 v off (SCM_SYMBOL SCM_SYM_BIG_ENDIAN))))
 (define-cproc get-u32le (v::<uvector> off::<uint>)
   (return (Scm_GetBinaryU32 v off (SCM_SYMBOL SCM_SYM_LITTLE_ENDIAN))))
 (define-cproc get-s32be (v::<uvector> off::<uint>)
   (return (Scm_GetBinaryS32 v off (SCM_SYMBOL SCM_SYM_BIG_ENDIAN))))
 (define-cproc get-s32le (v::<uvector> off::<uint>)
   (return (Scm_GetBinaryS32 v off (SCM_SYMBOL SCM_SYM_LITTLE_ENDIAN))))
 (define-cproc get-u64be (v::<uvector> off::<uint>)
   (return (Scm_GetBinaryU64 v off (SCM_SYMBOL SCM_SYM_BIG_ENDIAN))))
 (define-cproc get-u64le (v::<uvector> off::<uint>)
   (return (Scm_GetBinaryU64 v off (SCM_SYMBOL SCM_SYM_LITTLE_ENDIAN))))
 (define-cproc get-s64be (v::<uvector> off::<uint>)
   (return (Scm_GetBinaryS64 v off (SCM_SYMBOL SCM_SYM_BIG_ENDIAN))))
 (define-cproc get-s64le (v::<uvector> off::<uint>)
   (return (Scm_GetBinaryS64 v off (SCM_SYMBOL SCM_SYM_LITTLE_ENDIAN))))
 (define-cproc get-f16be (v::<uvector> off::<uint>)
   (return (Scm_GetBinaryF16 v off (SCM_SYMBOL SCM_SYM_BIG_ENDIAN))))
 (define-cproc get-f16le (v::<uvector> off::<uint>)
   (return (Scm_GetBinaryF16 v off (SCM_SYMBOL SCM_SYM_LITTLE_ENDIAN))))
 (define-cproc get-f32be (v::<uvector> off::<uint>)
   (return (Scm_GetBinaryF32 v off (SCM_SYMBOL SCM_SYM_BIG_ENDIAN))))
 (define-cproc get-f32le (v::<uvector> off::<uint>)
   (return (Scm_GetBinaryF32 v off (SCM_SYMBOL SCM_SYM_LITTLE_ENDIAN))))
 (define-cproc get-f64be (v::<uvector> off::<uint>)
   (return (Scm_GetBinaryF64 v off (SCM_SYMBOL SCM_SYM_BIG_ENDIAN))))
 (define-cproc get-f64le (v::<uvector> off::<uint>)
   (return (Scm_GetBinaryF64 v off (SCM_SYMBOL SCM_SYM_LITTLE_ENDIAN))))

 (define-cproc put-u16be! (v::<uvector> off::<uint> val) ::<void>
   (Scm_PutBinaryU16 v off val (SCM_SYMBOL SCM_SYM_BIG_ENDIAN)))
 (define-cproc put-u16le! (v::<uvector> off::<uint> val) ::<void>
   (Scm_PutBinaryU16 v off val (SCM_SYMBOL SCM_SYM_LITTLE_ENDIAN)))
 (define-cproc put-s16be! (v::<uvector> off::<uint> val) ::<void>
   (Scm_PutBinaryS16 v off val (SCM_SYMBOL SCM_SYM_BIG_ENDIAN)))
 (define-cproc put-s16le! (v::<uvector> off::<uint> val) ::<void>
   (Scm_PutBinaryS16 v off val (SCM_SYMBOL SCM_SYM_LITTLE_ENDIAN)))
 (define-cproc put-u32be! (v::<uvector> off::<uint> val) ::<void>
   (Scm_PutBinaryU32 v off val (SCM_SYMBOL SCM_SYM_BIG_ENDIAN)))
 (define-cproc put-u32le! (v::<uvector> off::<uint> val) ::<void>
   (Scm_PutBinaryU32 v off val (SCM_SYMBOL SCM_SYM_LITTLE_ENDIAN)))
 (define-cproc put-s32be! (v::<uvector> off::<uint> val ) ::<void>
   (Scm_PutBinaryS32 v off val (SCM_SYMBOL SCM_SYM_BIG_ENDIAN)))
 (define-cproc put-s32le! (v::<uvector> off::<uint> val ) ::<void>
   (Scm_PutBinaryS32 v off val (SCM_SYMBOL SCM_SYM_LITTLE_ENDIAN)))
 (define-cproc put-u64be! (v::<uvector> off::<uint> val ) ::<void>
   (Scm_PutBinaryU64 v off val (SCM_SYMBOL SCM_SYM_BIG_ENDIAN)))
 (define-cproc put-u64le! (v::<uvector> off::<uint> val ) ::<void>
   (Scm_PutBinaryU64 v off val (SCM_SYMBOL SCM_SYM_LITTLE_ENDIAN)))
 (define-cproc put-s64be! (v::<uvector> off::<uint> val ) ::<void>
   (Scm_PutBinaryS64 v off val (SCM_SYMBOL SCM_SYM_BIG_ENDIAN)))
 (define-cproc put-s64le! (v::<uvector> off::<uint> val ) ::<void>
   (Scm_PutBinaryS64 v off val (SCM_SYMBOL SCM_SYM_LITTLE_ENDIAN)))
 (define-cproc put-f16be! (v::<uvector> off::<uint> val) ::<void>
   (Scm_PutBinaryF16 v off val (SCM_SYMBOL SCM_SYM_BIG_ENDIAN)))
 (define-cproc put-f16le! (v::<uvector> off::<uint> val) ::<void>
   (Scm_PutBinaryF16 v off val (SCM_SYMBOL SCM_SYM_LITTLE_ENDIAN)))
 (define-cproc put-f32be! (v::<uvector> off::<uint> val) ::<void>
   (Scm_PutBinaryF32 v off val (SCM_SYMBOL SCM_SYM_BIG_ENDIAN)))
 (define-cproc put-f32le! (v::<uvector> off::<uint> val) ::<void>
   (Scm_PutBinaryF32 v off val (SCM_SYMBOL SCM_SYM_LITTLE_ENDIAN)))
 (define-cproc put-f64be! (v::<uvector> off::<uint> val) ::<void>
   (Scm_PutBinaryF64 v off val (SCM_SYMBOL SCM_SYM_BIG_ENDIAN)))
 (define-cproc put-f64le! (v::<uvector> off::<uint> val) ::<void>
   (Scm_PutBinaryF64 v off val (SCM_SYMBOL SCM_SYM_LITTLE_ENDIAN)))
 )

;;;
;;; Machine-dependent binary parameters
;;;

(inline-stub
 "struct { char b; short s; }    short_align;"
 "struct { char b; int s; }      int_align;"
 "struct { char b; long s; }     long_align;"
 "struct { char b; float s; }    float_align;"
 "struct { char b; double s; }   double_align;"
 "struct { char b; int8_t s; }   int8_align;"
 "struct { char b; int16_t s; }  int16_align;"
 "struct { char b; ScmInt32 s; } int32_align;"
 "struct { char b; ScmInt64 s; } int64_align;"

 "#ifdef HAVE_LONG_LONG"
 "typedef long long long_long_;"
 "struct { char b; long long s; } long_long_align;"
 "#else"
 "typedef long long_long_;"
 "struct { char b; long s; }      long_long_align;"
 "#endif"

 "#define alignof(str) ((intptr_t)&((str).s) - (intptr_t)&((str).b))"

 (define-cproc %primitive-type-info ()
   (return
    (Scm_List
     (SCM_LIST3 'char
                (SCM_MAKE_INT (sizeof (char)))
                (SCM_MAKE_INT 1))
     (SCM_LIST3 'short
                (SCM_MAKE_INT (sizeof (short)))
                (SCM_MAKE_INT (alignof short_align)))
     (SCM_LIST3 'int
                (SCM_MAKE_INT (sizeof (int)))
                (SCM_MAKE_INT (alignof int_align)))
     (SCM_LIST3 'long
                (SCM_MAKE_INT (sizeof (long)))
                (SCM_MAKE_INT (alignof long_align)))
     (SCM_LIST3 'longlong
                (SCM_MAKE_INT (sizeof (long_long_)))
                (SCM_MAKE_INT (alignof long_long_align)))
     (SCM_LIST3 'float
                (SCM_MAKE_INT (sizeof (float)))
                (SCM_MAKE_INT (alignof float_align)))
     (SCM_LIST3 'double
                (SCM_MAKE_INT (sizeof (double)))
                (SCM_MAKE_INT (alignof double_align)))
     (SCM_LIST3 'int8
                (SCM_MAKE_INT (sizeof (int8_t)))
                (SCM_MAKE_INT (alignof int8_align)))
     (SCM_LIST3 'int16
                (SCM_MAKE_INT (sizeof (int16_t)))
                (SCM_MAKE_INT (alignof int16_align)))
     (SCM_LIST3 'int32
                (SCM_MAKE_INT (sizeof (ScmInt32)))
                (SCM_MAKE_INT (alignof int32_align)))
     (SCM_LIST3 'int64
                (SCM_MAKE_INT (sizeof (ScmInt64)))
                (SCM_MAKE_INT (alignof int64_align)))
     NULL)))
 )
