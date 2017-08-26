;;;
;;; srfi-74 - Octet-addressed binary blocks
;;;

;; On Gauche, a blob is just an u8vector.

(define-module srfi-74
  (use gauche.uvector)
  (use gauche.generator)
  (use binary.io)
  (export endianness blob? make-blob blob-length
          blob-u8-ref blob-u8-set!
          blob-s8-ref blob-s8-set!
          blob-u16-ref blob-u16-set!
          blob-u16-native-ref blob-u16-native-set!
          blob-s16-ref blob-s16-set!
          blob-s16-native-ref blob-s16-native-set!
          blob-u32-ref blob-u32-set!
          blob-u32-native-ref blob-u32-native-set!
          blob-s32-ref blob-s32-set!
          blob-s32-native-ref blob-s32-native-set!
          blob-u64-ref blob-u64-set!
          blob-u64-native-ref blob-u64-native-set!
          blob-s64-ref blob-s64-set!
          blob-s64-native-ref blob-s64-native-set!
          blob-uint-ref blob-uint-set!
          blob-sint-ref blob-sint-set!
          blob=? blob-copy! blob-copy
          blob->u8-list u8-list->blob
          blob->uint-list uint-list->blob
          blob->sint-list sint-list->blob))
(select-module srfi-74)

(define-constant native (native-endian))

(define-syntax endianness
  (syntax-rules (big little native)
    [(_ big)    'big-endian]
    [(_ little) 'little-endian]
    [(_ native) native]
    [(_ x)      (syntax-error "Unknown endianness:" x)]))

(define blob? u8vector?)

(define (make-blob k) (make-u8vector k))

(define (blob-length blob) (u8vector-length blob))

(define (blob-u8-ref blob k) (get-u8 blob k))
(define (blob-u8-set! blob k v) (put-u8! blob k v))
(define (blob-s8-ref blob k) (get-s8 blob k))
(define (blob-s8-set! blob k v) (put-s8! blob k v))

(define (blob-u16-ref endi blob k)      (get-u16 blob k endi))
(define (blob-u16-native-ref blob k)    (get-u16 blob k native))
(define (blob-u16-set! endi blob k v)   (put-u16! blob k endi))
(define (blob-u16-native-set! blob k v) (put-u16! blob k native))

(define (blob-s16-ref endi blob k)      (get-s16 blob k endi))
(define (blob-s16-native-ref blob k)    (get-s16 blob k native))
(define (blob-s16-set! endi blob k v)   (put-s16! blob k endi))
(define (blob-s16-native-set! blob k v) (put-s16! blob k native))

(define (blob-u32-ref endi blob k)      (get-u32 blob k endi))
(define (blob-u32-native-ref blob k)    (get-u32 blob k native))
(define (blob-u32-set! endi blob k v)   (put-u32! blob k endi))
(define (blob-u32-native-set! blob k v) (put-u32! blob k native))

(define (blob-s32-ref endi blob k)      (get-s32 blob k endi))
(define (blob-s32-native-ref blob k)    (get-s32 blob k native))
(define (blob-s32-set! endi blob k v)   (put-s32! blob k endi))
(define (blob-s32-native-set! blob k v) (put-s32! blob k native))

(define (blob-u64-ref endi blob k)      (get-u64 blob k endi))
(define (blob-u64-native-ref blob k)    (get-u64 blob k native))
(define (blob-u64-set! endi blob k v)   (put-u64! blob k endi))
(define (blob-u64-native-set! blob k v) (put-u64! blob k native))

(define (blob-s64-ref endi blob k)      (get-s64 blob k endi))
(define (blob-s64-native-ref blob k)    (get-s64 blob k native))
(define (blob-s64-set! endi blob k v)   (put-s64! blob k endi))
(define (blob-s64-native-set! blob k v) (put-s64! blob k native))

(define (blob-uint-ref size endi blob k)
  (assume-type blob <u8vector>)
  (get-uint size blob k endi))

(define (blob-sint-ref size endi blob k)
  (assume-type blob <u8vector>)
  (get-sint size blob k endi))
  
(define (blob-uint-set! size endi blob k v)
  (assume-type blob <u8vector>)
  (put-uint! size blob k v endi))

(define (blob-sint-set! size endi blob k v)
  (assume-type blob <u8vector>)
  (put-sint! size blob k v endi))

(define (blob=? a b) (u8vector=? a b))

(define (blob-copy! src src-start target target-start n)
  (u8vector-copy! target target-start src src-start (+ src-start n)))

(define (blob-copy blob) (u8vector-copy blob))

(define blob->u8-list u8vector->list)
(define u8-list->blob list->u8vector)

(define (blob->uint-list size endi blob)
  (generator->list (gmap (cut blob-uint-ref size endi blob <>)
                         (grange 0 (blob-length blob) size))))
(define (blob->sint-list size endi blob)
  (generator->list (gmap (cut blob-sint-ref size endi blob <>)
                         (grange 0 (blob-length blob) size))))

(define (uint-list->blob size endi lis)
  (rlet1 v (make-u8vector (* size (length lis)))
    (do ([k 0 (+ k size)]
         [lis lis (cdr lis)])
        [(null? lis)]
      (blob-uint-set! size endi v k (car lis)))))
(define (sint-list->blob size endi lis)
  (rlet1 v (make-u8vector (* size (length lis)))
    (do ([k 0 (+ k size)]
         [lis lis (cdr lis)])
        [(null? lis)]
      (blob-sint-set! size endi v k (car lis)))))
