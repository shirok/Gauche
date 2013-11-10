;;
;; testing binary.io
;;

(use gauche.test)
(use gauche.uvector)
(use gauche.collection)
(use gauche.parameter)
(use srfi-1)

(test-start "binary")

;;----------------------------------------------------------
(test-section "binary.io")

(use binary.io)
(test-module 'binary.io)

;; prepare data file
(sys-unlink "test.o")

(define *u8*
  '(#x01 #x23 #x45 #x67 #x89 #xab #xcd #xef
    #x0f #x1e #x2d #x3c #x4b #x5a #x69 #x78))
(define *s8*
  '(#x01 #x23 #x45 #x67 #x-77 #x-55 #x-33 #x-11
    #x0f #x1e #x2d #x3c #x4b #x5a #x69 #x78))
(define *u16be* '(#x0123 #x4567 #x89ab #xcdef #x0f1e #x2d3c #x4b5a #x6978))
(define *u16le* '(#x2301 #x6745 #xab89 #xefcd #x1e0f #x3c2d #x5a4b #x7869))
(define *s16be* '(#x0123 #x4567 #x-7655 #x-3211 #x0f1e #x2d3c #x4b5a #x6978))
(define *s16le* '(#x2301 #x6745 #x-5477 #x-1033 #x1e0f #x3c2d #x5a4b #x7869))
(define *u32be* '(#x01234567 #x89abcdef #x0f1e2d3c #x4b5a6978))
(define *u32le* '(#x67452301 #xefcdab89 #x3c2d1e0f #x78695a4b))
(define *s32be* '(#x01234567 #x-76543211 #x0f1e2d3c #x4b5a6978))
(define *s32le* '(#x67452301 #x-10325477 #x3c2d1e0f #x78695a4b))
(define *u64be* '(#x0123456789abcdef #x0f1e2d3c4b5a6978))
(define *u64le* '(#xefcdab8967452301 #x78695a4b3c2d1e0f))
(define *s64be* '(#x0123456789abcdef #x0f1e2d3c4b5a6978))
(define *s64le* '(#x-1032547698badcff #x78695a4b3c2d1e0f))

(define *bytes-str*
  #*"\x01\x23\x45\x67\x89\xab\xcd\xef\x0f\x1e\x2d\x3c\x4b\x5a\x69\x78")

(with-output-to-file "test.o"
  (lambda ()
    (for-each write-byte *u8*)))

(define (test-read-binary label data proc . endian)
  (test* label data
         (call-with-input-file "test.o"
           (cut port->list (cut apply proc <> endian) <>))))

(test-read-binary "read-u8" *u8* read-u8)
(test-read-binary "read-s8" *s8* read-s8)

(test-read-binary "read-u16 be"
                  *u16be* read-u16 'big-endian)
(test-read-binary "read-u16 le"
                  *u16le* read-u16 'little-endian)
(test-read-binary "read-s16 be"
                  *s16be* read-s16 'big-endian)
(test-read-binary "read-s16 le"
                  *s16le* read-s16 'little-endian)

(test-read-binary "read-u32 be"
                  *u32be* read-u32 'big-endian)
(test-read-binary "read-u32 le"
                  *u32le* read-u32 'little-endian)
(test-read-binary "read-s32 be"
                  *s32be* read-s32 'big-endian)
(test-read-binary "read-s32 le"
                  *s32le* read-s32 'little-endian)

(test-read-binary "read-u64 be"
                  *u64be* read-u64 'big-endian)
(test-read-binary "read-u64 le"
                  *u64le* read-u64 'little-endian)
(test-read-binary "read-s64 be"
                  *s64be* read-s64 'big-endian)
(test-read-binary "read-s64 le"
                  *s64le* read-s64 'little-endian)

(parameterize ((default-endian 'big-endian))
  (test-read-binary "read-u64 (default - be)"
                    *u64be* read-u64))
(parameterize ((default-endian 'little-endian))
  (test-read-binary "read-u64 (default - le)"
                    *u64le* read-u64))

(define (test-write-binary label data proc . endian)
  (test* label *bytes-str*
         (string-complete->incomplete
          (call-with-output-string
            (lambda (p)
              (for-each (cut apply proc <> p endian) data))))))

(test-write-binary "write-u8" *u8* write-u8)
(test-write-binary "write-s8" *s8* write-s8)

(test-write-binary "write-u16 be"
                   *u16be* write-u16 'big-endian)
(test-write-binary "write-u16 le"
                   *u16le* write-u16 'little-endian)
(test-write-binary "write-s16 be"
                   *s16be* write-s16 'big-endian)
(test-write-binary "write-s16 le"
                   *s16le* write-s16 'little-endian)

(test-write-binary "write-u32 be"
                   *u32be* write-u32 'big-endian)
(test-write-binary "write-u32 le"
                   *u32le* write-u32 'little-endian)
(test-write-binary "write-s32 be"
                   *s32be* write-s32 'big-endian)
(test-write-binary "write-s32 le"
                   *s32le* write-s32 'little-endian)

(test-write-binary "write-u64 be"
                   *u64be* write-u64 'big-endian)
(test-write-binary "write-u64 le"
                   *u64le* write-u64 'little-endian)
(test-write-binary "write-s64 be"
                   *s64be* write-s64 'big-endian)
(test-write-binary "write-s64 le"
                   *s64le* write-s64 'little-endian)

(define (test-write-binary-range label proc min min-1 max max+1)
  (test* label '(#t #f #t #f)
         (map (lambda (val)
                (with-error-handler
                    (^e #f)
                  (lambda ()
                    (and (call-with-output-string (cut proc val <>))
                         #t))))
              (list min min-1 max max+1))))

(test-write-binary-range "write-u8" write-u8
                         0 -1 255 256)
(test-write-binary-range "write-s8" write-s8
                         -128 -129 127 128)
(test-write-binary-range "write-u16" write-u16
                         0 -1 #xffff #x10000)
(test-write-binary-range "write-s16" write-s16
                         #x-8000 #x-8001 #x7fff #x8000)
(test-write-binary-range "write-u32" write-u32
                         0 -1 #xffffffff #x100000000)
(test-write-binary-range "write-s32" write-s32
                         #x-80000000 #x-80000001 #x7fffffff #x80000000)
(test-write-binary-range "write-u64" write-u64
                         0 -1 #xffffffffffffffff #x10000000000000000)
(test-write-binary-range "write-s64" write-s64
                         #x-8000000000000000 #x-8000000000000001
                         #x7fffffffffffffff  #x8000000000000000)

(define (test-flonum-rw label reader writer bits)
  (test* label (string-complete->incomplete bits)
         (let ((val (with-input-from-string bits (cut reader))))
           (and (real? val)
                (string-complete->incomplete
                 (with-output-to-string (cut writer val)))))))

(define (test-flonum-rw-f16 bvec endian)
  (test-flonum-rw (format "read/write-f16 (~a)" endian)
                  (cut read-f16 #f endian)
                  (cut write-f16 <> #f endian)
                  (u8vector->string bvec)))

(define (test-flonum-rw-f32 bvec endian)
  (test-flonum-rw (format "read/write-f32 (~a)" endian)
                  (cut read-f32 #f endian)
                  (cut write-f32 <> #f endian)
                  (u8vector->string bvec)))

(define (test-flonum-rw-f64 bvec endian)
  (test-flonum-rw (format "read/write-f64 (~a)" endian)
                  (cut read-f64 #f endian)
                  (cut write-f64 <> #f endian)
                  (u8vector->string bvec)))

(define (test-flonum-rw-driver tester bytes-list)
  (for-each (lambda (bytes)
              (let ((be-vec (coerce-to <u8vector> bytes))
                    (le-vec (coerce-to <u8vector> (reverse bytes))))
                (tester be-vec 'big-endian)
                (tester le-vec 'little-endian)))
            bytes-list))

;; f16 - half float
(test-flonum-rw-driver
 test-flonum-rw-f16
 (append
  '((#x00 #x00)                         ; 0.0
    (#x04 #x00)                         ; normalized
    (#x04 #x01)
    (#x04 #x02)
    (#x3c #x00)                         ; 1.0
    (#x7b #xff)
    (#x7c #x00)                         ; +inf

    (#x80 #x00)                         ; -0.0
    (#x84 #x00)                         ; normalized
    (#xbc #x00)                         ; -1.0
    (#xfb #xff)
    (#xfc #x00)                         ; -inf

    (#x00 #x01)                         ; +denormalized
    (#x00 #x02)                         ;
    (#x00 #x03)                         ;
    (#x80 #x01)                         ; -denormalized
    (#x80 #x02)                         ;
    (#x80 #x03)                         ;
    )))

;; f32 - ieee single float
;; NB: Alpha chip doesn't support denormalized floats, so we exclude
;; them from the test.
(test-flonum-rw-driver
 test-flonum-rw-f32
 (append
  '((#x00 #x00 #x00 #x00)   ;; 0.0
    (#x00 #x80 #x00 #x00)   ;; normalized
    (#x00 #x80 #x00 #x01)
    (#x00 #x80 #x00 #x02)
    (#x3f #x80 #x00 #x00)   ;; 1.0
    (#x7f #x7f #xff #xff)
    (#x7f #x80 #x00 #x00)   ;; +inf

    (#x80 #x00 #x00 #x00)   ;;-0.0
    (#x80 #x80 #x00 #x00)   ;; normalized
    (#xbf #x80 #x00 #x00)   ;; -1.0
    (#xff #x7f #xff #xff)
    (#xff #x80 #x00 #x00)   ;; -inf
    )
  (if (#/alpha/ (gauche-architecture))
    '()
    '((#x00 #x00 #x00 #x01)   ;; +denormalized
      (#x00 #x00 #x00 #x02)
      (#x00 #x00 #x00 #x03)
      (#x80 #x00 #x00 #x01)   ;; -denormalized
      (#x80 #x00 #x00 #x02)
      (#x80 #x00 #x00 #x03)
      ))))

(test-flonum-rw-driver
 test-flonum-rw-f64
 '((#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)   ;; 0.0
   (#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x01)   ;; denormalized
   (#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x02)
   (#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x03)
   (#x00 #x10 #x00 #x00 #x00 #x00 #x00 #x00)   ;; normalized
   (#x00 #x10 #x00 #x00 #x00 #x00 #x00 #x01)
   (#x3f #xf0 #x00 #x00 #x00 #x00 #x00 #x00)   ;; 1.0
   (#x7f #xef #xff #xff #xff #xff #xff #xff)
   (#x7f #xf0 #x00 #x00 #x00 #x00 #x00 #x00)   ;; +inf

   (#x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00)   ;;-0.0
   (#x80 #x00 #x00 #x00 #x00 #x00 #x00 #x01)   ;; denormalized
   (#x80 #x00 #x00 #x00 #x00 #x00 #x00 #x02)
   (#x80 #x10 #x00 #x00 #x00 #x00 #x00 #x00)   ;; normalized
   (#xbf #xf0 #x00 #x00 #x00 #x00 #x00 #x00)   ;; -1.0
   (#xff #xef #xff #xff #xff #xff #xff #xff)
   (#xff #xf0 #x00 #x00 #x00 #x00 #x00 #x00)   ;; -inf
   ))

(test* "read-f16 (be)" 1.0
       (with-input-from-string #*"\x3c\x00"
         (cut read-f16 #f 'big-endian)))
(test* "read-f32 (be)" 1.0
       (with-input-from-string #*"\x3f\x80\x00\x00"
         (cut read-f32 #f 'big-endian)))
(test* "read-f64 (be)" 1.0
       (with-input-from-string #*"\x3f\xf0\x00\x00\x00\x00\x00\x00"
         (cut read-f64 #f 'big-endian)))
(test* "read-f16 (le)" 1.0
       (with-input-from-string #*"\x00\x3c"
         (cut read-f16 #f 'little-endian)))
(test* "read-f32 (le)" 1.0
       (with-input-from-string #*"\x00\x00\x80\x3f"
         (cut read-f32 #f 'little-endian)))
(test* "read-f64 (le)" 1.0
       (with-input-from-string #*"\x00\x00\x00\x00\x00\x00\xf0\x3f"
         (cut read-f64 #f 'little-endian)))
(test* "read-f16 (arm)" 1.0
       (with-input-from-string #*"\x00\x3c"
         (cut read-f16 #f 'arm-little-endian)))
(test* "read-f32 (arm)" 1.0
       (with-input-from-string #*"\x00\x00\x80\x3f"
         (cut read-f32 #f 'arm-little-endian)))
(test* "read-f64 (arm)" 1.0
       (with-input-from-string #*"\x00\x00\xf0\x3f\x00\x00\x00\x00"
         (cut read-f64 #f 'arm-little-endian)))


;; uvector reader

(test* "get-u8" '(1 2 254 255)
       (map (cut get-u8 '#u8(1 2 254 255) <>) (iota 4)))
(test* "get-u8" (test-error) (get-u8 '#u8(1) -1))
(test* "get-u8" (test-error) (get-u8 '#u8(1) 1))
(test* "get-s8" '(1 2 -2 -1)
       (map (cut get-s8 '#u8(1 2 254 255) <>) (iota 4)))
(test* "get-u16 be" '(#x0102 #x02fe #xfeff)
       (map (cut get-u16 '#u8(1 2 254 255) <> 'big-endian)
            '(0 1 2)))
(test* "get-u16 le" '(#x0201 #xfe02 #xfffe)
       (map (cut get-u16 '#u8(1 2 254 255) <> 'little-endian)
            '(0 1 2)))
(test* "get-u16 bound" (test-error)
       (get-u16 '#u8(1 2) -1))
(test* "get-u16 bound" (test-error)
       (get-u16 '#u8(1 2) 1))
(test* "get-s16 be" `(#x0102 #x02fe ,(- #xfeff #x10000))
       (map (cut get-s16 '#u8(1 2 254 255) <> 'big-endian)
            '(0 1 2)))
(test* "get-s16 le" `(#x0201 ,(- #xfe02 #x10000) ,(- #xfffe #x10000))
       (map (cut get-s16 '#u8(1 2 254 255) <> 'little-endian)
            '(0 1 2)))

(test* "get-u32 be"
       '(#x0102feff #x02feff03 #xfeff0304 #xff0304fc #x0304fcfd)
       (map (cut get-u32
                 '#u8(#x01 #x02 #xfe #xff #x03 #x04 #xfc #xfd)
                 <> 'big-endian)
            (iota 5)))
(test* "get-u32 le"
       '(#xfffe0201 #x03fffe02 #x0403fffe #xfc0403ff #xfdfc0403)
       (map (cut get-u32
                 '#u8(#x01 #x02 #xfe #xff #x03 #x04 #xfc #xfd)
                 <> 'little-endian)
            (iota 5)))
(test* "get-s32 be"
       `(#x0102feff #x02feff03 ,(- #xfeff0304 (expt 2 32))
                    ,(- #xff0304fc (expt 2 32)) #x0304fcfd)
       (map (cut get-s32
                 '#u8(#x01 #x02 #xfe #xff #x03 #x04 #xfc #xfd)
                 <> 'big-endian)
            (iota 5)))
(test* "get-s32 le"
       `(,(- #xfffe0201 (expt 2 32)) #x03fffe02 #x0403fffe
         ,(- #xfc0403ff (expt 2 32)) ,(- #xfdfc0403 (expt 2 32)))
       (map (cut get-s32
                 '#u8(#x01 #x02 #xfe #xff #x03 #x04 #xfc #xfd)
                 <> 'little-endian)
            (iota 5)))

(test* "get-u64 be"
       '(#x01ff02fe03fd04fc #xff02fe03fd04fc05)
       (map (cut get-u64
                 '#u8(#x01 #xff #x02 #xfe #x03 #xfd #x04 #xfc #x05)
                 <> 'big-endian)
            '(0 1)))
(test* "get-u64 le"
       '(#xfc04fd03fe02ff01 #x05fc04fd03fe02ff)
       (map (cut get-u64
                 '#u8(#x01 #xff #x02 #xfe #x03 #xfd #x04 #xfc #x05)
                 <> 'little-endian)
            '(0 1)))
(test* "get-s64 be"
       `(#x01ff02fe03fd04fc ,(- #xff02fe03fd04fc05 (expt 2 64)))
       (map (cut get-s64
                 '#u8(#x01 #xff #x02 #xfe #x03 #xfd #x04 #xfc #x05)
                 <> 'big-endian)
            '(0 1)))
(test* "get-s64 le"
       `(,(- #xfc04fd03fe02ff01 (expt 2 64)) #x05fc04fd03fe02ff)
       (map (cut get-s64
                 '#u8(#x01 #xff #x02 #xfe #x03 #xfd #x04 #xfc #x05)
                 <> 'little-endian)
            '(0 1)))

(test* "get-f16 be" '(1.0 -1.0)
       (map (cut get-f16
                 '#u8(#x3c #x00 #xbc #x00)
                 <> 'big-endian)
            '(0 2)))
(test* "get-f16 le" '(1.0 -1.0)
       (map (cut get-f16
                 '#u8(#x00 #x3c #x00 #xbc)
                 <> 'little-endian)
            '(0 2)))
(test* "get-f32 be" '(1.0 -1.0)
       (map (cut get-f32
                 '#u8(#x3f #x80 #x00 #x00 #xbf #x80 #x00 #x00)
                 <> 'big-endian)
            '(0 4)))
(test* "get-f32 le" '(1.0 -1.0)
       (map (cut get-f32
                 '#u8(#x00 #x00 #x80 #x3f #x00 #x00 #x80 #xbf)
                 <> 'little-endian)
            '(0 4)))
(test* "get-f64 be" '(1.0 -1.0)
       (map (cut get-f64
                 '#u8(#x3f #xf0 #x00 #x00 #x00 #x00 #x00 #x00
                      #xbf #xf0 #x00 #x00 #x00 #x00 #x00 #x00)
                 <> 'big-endian)
            '(0 8)))
(test* "get-f64 le" '(1.0 -1.0)
       (map (cut get-f64
                 '#u8(#x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x3f
                      #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #xbf)
                 <> 'little-endian)
            '(0 8)))
(test* "get-f64 arm" '(1.0 -1.0)
       (map (cut get-f64
                 '#u8(#x00 #x00 #xf0 #x3f #x00 #x00 #x00 #x00
                      #x00 #x00 #xf0 #xbf #x00 #x00 #x00 #x00)
                 <> 'arm-little-endian)
            '(0 8)))

(test* "put-u8!" '#u8(0 1 254 255)
       (let1 v (make-u8vector 4)
         (put-u8! v 3 255)
         (put-u8! v 2 254)
         (put-u8! v 1 1)
         (put-u8! v 0 0)
         v))
(test* "put-s8!" '#u8(0 1 254 255)
       (let1 v (make-u8vector 4)
         (put-s8! v 3 -1)
         (put-s8! v 2 -2)
         (put-s8! v 1 1)
         (put-s8! v 0 0)
         v))
(test* "put-u16! be" '#u8(1 2 0 254 255)
       (let1 v (make-u8vector 5 0)
         (put-u16! v 3 #xfeff 'big-endian)
         (put-u16! v 0 #x0102 'big-endian)
         v))
(test* "put-u16! le" '#u8(2 1 0 255 254)
       (let1 v (make-u8vector 5 0)
         (put-u16! v 3 #xfeff 'little-endian)
         (put-u16! v 0 #x0102 'little-endian)
         v))
(test* "put-s16! be" '#u8(1 2 0 254 255)
       (let1 v (make-u8vector 5 0)
         (put-s16! v 3 (- #xfeff #x10000) 'big-endian)
         (put-s16! v 0 #x0102 'big-endian)
         v))
(test* "put-s16! le" '#u8(2 1 0 255 254)
       (let1 v (make-u8vector 5 0)
         (put-s16! v 3 (- #xfeff #x10000) 'little-endian)
         (put-s16! v 0 #x0102 'little-endian)
         v))

(test* "put-u32! be" '#u8(1 2 3 4 0 252 253 254 255)
       (let1 v (make-u8vector 9 0)
         (put-u32! v 5 #xfcfdfeff 'big-endian)
         (put-u32! v 0 #x01020304 'big-endian)
         v))
(test* "put-u32! le" '#u8(4 3 2 1 0 255 254 253 252)
       (let1 v (make-u8vector 9 0)
         (put-u32! v 5 #xfcfdfeff 'little-endian)
         (put-u32! v 0 #x01020304 'little-endian)
         v))
(test* "put-s32! be" '#u8(1 2 3 4 0 252 253 254 255)
       (let1 v (make-u8vector 9 0)
         (put-s32! v 5 (- #xfcfdfeff (expt 2 32)) 'big-endian)
         (put-s32! v 0 #x01020304 'big-endian)
         v))
(test* "put-s32! le" '#u8(4 3 2 1 0 255 254 253 252)
       (let1 v (make-u8vector 9 0)
         (put-s32! v 5 (- #xfcfdfeff (expt 2 32)) 'little-endian)
         (put-s32! v 0 #x01020304 'little-endian)
         v))

(test* "put-u64! be"
       '#u8(1 2 3 4 5 6 7 8 0 248 249 250 251 252 253 254 255)
       (let1 v (make-u8vector 17 0)
         (put-u64! v 9 #xf8f9fafbfcfdfeff 'big-endian)
         (put-u64! v 0 #x0102030405060708 'big-endian)
         v))
(test* "put-u64! le"
       '#u8(8 7 6 5 4 3 2 1 0 255 254 253 252 251 250 249 248)
       (let1 v (make-u8vector 17 0)
         (put-u64! v 9 #xf8f9fafbfcfdfeff 'little-endian)
         (put-u64! v 0 #x0102030405060708 'little-endian)
         v))
(test* "put-s64! be"
       '#u8(1 2 3 4 5 6 7 8 0 248 249 250 251 252 253 254 255)
       (let1 v (make-u8vector 17 0)
         (put-s64! v 9 (- #xf8f9fafbfcfdfeff (expt 2 64)) 'big-endian)
         (put-s64! v 0 #x0102030405060708 'big-endian)
         v))
(test* "put-s64! le"
       '#u8(8 7 6 5 4 3 2 1 0 255 254 253 252 251 250 249 248)
       (let1 v (make-u8vector 17 0)
         (put-s64! v 9 (- #xf8f9fafbfcfdfeff (expt 2 64)) 'little-endian)
         (put-s64! v 0 #x0102030405060708 'little-endian)
         v))

(test* "put-f16! be" '#u8(#x3c #x00 0 #xbc #x00)
       (let1 v (make-u8vector 5 0)
         (put-f16! v 0 1.0 'big-endian)
         (put-f16! v 3 -1.0 'big-endian)
         v))
(test* "put-f16! le" '#u8(#x00 #x3c 0 #x00 #xbc)
       (let1 v (make-u8vector 5 0)
         (put-f16! v 0 1.0 'little-endian)
         (put-f16! v 3 -1.0 'little-endian)
         v))

(test* "put-f32! be"
       '#u8(#x3f #x80 #x00 #x00 0 #xbf #x80 #x00 #x00)
       (let1 v (make-u8vector 9 0)
         (put-f32! v 0 1.0 'big-endian)
         (put-f32! v 5 -1.0 'big-endian)
         v))
(test* "put-f32! le"
       '#u8(#x00 #x00 #x80 #x3f 0 #x00 #x00 #x80 #xbf)
       (let1 v (make-u8vector 9 0)
         (put-f32! v 0 1.0 'little-endian)
         (put-f32! v 5 -1.0 'little-endian)
         v))

(test* "put-f64! be"
       '#u8(#x3f #xf0 #x00 #x00 #x00 #x00 #x00 #x00 0
            #xbf #xf0 #x00 #x00 #x00 #x00 #x00 #x00)
       (let1 v (make-u8vector 17 0)
         (put-f64! v 0 1.0 'big-endian)
         (put-f64! v 9 -1.0 'big-endian)
         v))
(test* "put-f64! le"
       '#u8(#x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x3f 0
            #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #xbf)
       (let1 v (make-u8vector 17 0)
         (put-f64! v 0 1.0 'little-endian)
         (put-f64! v 9 -1.0 'little-endian)
         v))
(test* "put-f64! arm"
       '#u8(#x00 #x00 #xf0 #x3f #x00 #x00 #x00 #x00 0
            #x00 #x00 #xf0 #xbf #x00 #x00 #x00 #x00)
       (let1 v (make-u8vector 17 0)
         (put-f64! v 0 1.0 'arm-little-endian)
         (put-f64! v 9 -1.0 'arm-little-endian)
         v))

;;----------------------------------------------------------
(test-section "binary.ftype")

(use binary.ftype)
(test-module 'binary.ftype)

(define *fobject-storage*
  '#u8(#x80 #x01 #x02 #x03 #x04 #x05 #x06 #x07
       #x08 #x09 #x0a #x0b #x0c #x0d #x0e #x0f
       #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17
       #x18 #x19 #x1a #x1b #x1c #x1d #x1e #x1f
       #x20 #x21 #x22 #x23 #x24 #x25 #x26 #x27
       #x28 #x29 #x2a #x2b #x2c #x2d #x2e #x2f
       #x30 #x31 #x32 #x33 #x34 #x35 #x36 #x37
       #x38 #x39 #x3a #x3b #x3c #x3d #x3e #x3f
       ))

;; procedural interface
(let ([ft1 (make-fstruct-type 'ft1
                              `((a ,ftype:schar)
                                (b ,ftype:uchar)
                                (c ,ftype:short)
                                (d ,ftype:schar)
                                (e ,ftype:short)
                                (f ,ftype:schar)
                                (g ,ftype:int)
                                (h ,ftype:schar)
                                (i ,ftype:long))
                              #f #f)]
      [ft2 (make-fstruct-type 'ft2      ;fully packed
                              `((a ,ftype:schar)
                                (b ,ftype:uchar)
                                (c ,ftype:short)
                                (d ,ftype:schar)
                                (e ,ftype:short)
                                (f ,ftype:schar)
                                (g ,ftype:int)
                                (h ,ftype:schar)
                                (i ,ftype:long))
                              #f 0)])
  (define (read-all-slots ft endian :optional (off 0))
    (parameterize ([default-endian endian])
      (map (cut fobject-ref/uv ft <> *fobject-storage* off)
           '(a b c d e f g h i))))

  (test* "big endian, natural alignment"
         `(-128 1 #x0203 4 #x0607 8 #x0c0d0e0f #x10
                ,(case (ftype-size ftype:long)
                   [(4) #x14151617]
                   [(8) #x18191a1b1c1d1e1f]))
         (read-all-slots ft1 'big-endian))
  (test* "little endian, natural alignment"
         `(-128 1 #x0302 4 #x0706 8 #x0f0e0d0c #x10
                ,(case (ftype-size ftype:long)
                   [(4) #x17161514]
                   [(8) #x1f1e1d1c1b1a1918]))
         (read-all-slots ft1 'little-endian))
  (test* "with offset"
         `(8 9 #x0a0b 12 #x0e0f 16 #x14151617 24
             ,(case (ftype-size ftype:long)
                [(4) #x1c1d1e1f]
                [(8) #x2021222324252627]))
         (read-all-slots ft1 'big-endian 8))

  (test* "big endian, packed"
         `(-128 1 #x0203 4 #x0506 7 #x08090a0b #x0c
                ,(case (ftype-size ftype:long)
                   [(4) #x0d0e0f10]
                   [(8) #x0d0e0f1011121314]))
         (read-all-slots ft2 'big-endian))
  (test* "little endian, packed"
         `(-128 1 #x0302 4 #x0605 7 #x0b0a0908 #x0c
                ,(case (ftype-size ftype:long)
                   [(4) #x100f0e0d]
                   [(8) #x14131211100f0e0d]))
         (read-all-slots ft2 'little-endian))
  )

;; define-fstruct-type macro
(define-fstruct-type ft3 #t #t
  ((a ftype:schar)
   (b ftype:uchar)
   (c ftype:short)
   (d ftype:schar)
   (e ftype:short)
   (f ftype:schar)
   (g ftype:int)
   (h ftype:schar)
   (i ftype:long)))

(let ([fs #f])
  (test* "ft3 ctor and pred" #t
         (begin
           (set! fs (make-ft3 :a -1 :b 2 :c #x0304 :d 5
                              :e -1 :f 6 :g #x0708090a
                              :h 7 :i #x0b0c0d0e))
           (ft3? fs)))
  (test* "ft3 store"
         (case (default-endian)
           [(big-endian)
            (case (ftype-size ftype:long)
              [(4) '#u8(#xff 2 3 4 5 0 #xff #xff 6 0 0 0 7 8 9 10
                        7 0 0 0 11 12 13 14)]
              [(8) '#u8(#xff 2 3 4 5 0 #xff #xff 6 0 0 0 7 8 9 10
                        7 0 0 0 0 0 0 0 0 0 0 0 11 12 13 14)])]
           [(little-endian arm-little-endian)
            (case (ftype-size ftype:long)
              [(4) '#u8(#xff 2 4 3 5 0 #xff #xff 6 0 0 0 10 9 8 7
                        7 0 0 0 14 13 12 11)]
              [(8) '#u8(#xff 2 4 3 5 0 #xff #xff 6 0 0 0 10 9 8 7
                        7 0 0 0 0 0 0 0 14 13 12 11 0 0 0 0)])])
         (fobject-storage fs))
  (test* "ft3 init w/offset" (fobject-storage fs)
         (let1 v (make-u8vector (+ 5 (ftype-size ft3)))
           (init-ft3! v 5
                      :a -1 :b 2 :c #x0304 :d 5
                      :e -1 :f 6 :g #x0708090a
                      :h 7 :i #x0b0c0d0e)
           (uvector-alias <u8vector> v 5)))
  )

;;----------------------------------------------------------
(test-section "binary.pack")

(use binary.pack)
(test-module 'binary.pack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test helpers

(define (test-error title expected thunk)
  (test title #t
    (lambda ()
      (with-error-handler
          (lambda (err)
            (and (cond ((regexp? expected)
                        (rxmatch expected (slot-ref err 'message)))
                       ((procedure? expected)
                        (expected err))
                       (else
                        (equal? expected err)))
                 #t))
        thunk))))

(define (unpack-pack fmt ls)
  (unpack fmt :from-string (pack fmt ls :to-string? #t)))

(define (test-unpack-pack name fmt ls)
  (test name ls (cut unpack-pack fmt ls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start the perl tests

(test "ascii integer medley" #t
  (lambda ()
    (let ((fmt "c2 x5 C C x s d i l a6")
          (ls `(1 -100 127 128 32767 ,(/ 987.654321098 100.0) 12345 123456 "abcdef")))
      (equal? ls (unpack fmt :from-string (pack fmt ls :to-string? #t))))))

;; % is unpack only, so it probably doesn't belong

; (for-each
;  (lambda (x)
;    (test "counting bits" (caddr x)
;      (lambda () (unpack (car x) :from-string (cadr x)))))
;  '(("%32B*" "\001\002\004\010\020\040\100\200\377" 16)
;    ("%32b69" "\001\002\004\010\020\040\100\200\017" 12)
;    ("%32B69" "\001\002\004\010\020\040\100\200\017" 9)))

(test-unpack-pack "maxint" "I" '(#xFFFFFFFF))

(define x-ls `(5 130 256 560 32000 3097152 268435455 1073741844
                 ,(expt 2 33) 4503599627365785
                 23728385234614992549757750638446))

(define x (string-complete->incomplete (pack "w*" x-ls :to-string? #t)))

(define y (string-complete->incomplete
           (pack "H*" (list (string-append "0581028200843081fa0081bd8440ffffff7f8480808014A0808"
                                           "0800087ffffffffffdb19caefe8e1eeeea0c2e1e3e8ede1ee6e"))
                 :to-string? #t)))

(test "ber pack" x (lambda () y))

(for-each
 (lambda (a b) (test "ber unpack" a (lambda () b)))
 x-ls (unpack "w*" :from-string y))

(for-each
 (lambda (template)
   (test (string-append "unpack-pack lengths: " template) 2
     (lambda ()
       (length (unpack-pack (string-append template "*") '(12 34))))))
 '("c" "C" "i" "I" "s" "S" "l" "L" "n" "N" "v" "V" "f" "d" "q" "Q"))

;; test the ascii template types (A, a, Z)

(for-each
 (lambda (x)
   (test (format #f "ascii ~a template: ~a"
                 (if (eq? (car x) 'p) "pack" "unpack") (cadr x))
       (cadddr x)
     (lambda ()
       (if (equal? (car x) 'u)
         (car (unpack (cadr x) :from-string (caddr x)))
         (pack (cadr x) (list (caddr x)) :to-string? #t)
         ))))
 '((p "A*"  "foo\0bar\0 " "foo\0bar\0 ")
   (p "A11" "foo\0bar\0 " "foo\0bar\0   ")
   (u "A*"  "foo\0bar \0" "foo\0bar")
   (u "A8"  "foo\0bar \0" "foo\0bar")
   (p "a*"  "foo\0bar\0 " "foo\0bar\0 ")
   (p "a11" "foo\0bar\0 " "foo\0bar\0 \0\0")
   (u "a*"  "foo\0bar \0" "foo\0bar \0")
   (u "a8"  "foo\0bar \0" "foo\0bar ")
   (p "Z*"  "foo\0bar\0 " "foo\0bar\0 \0")
   (p "Z11" "foo\0bar\0 " "foo\0bar\0 \0\0")
   (p "Z3"  "foo"         "fo\0")
   (u "Z*"  "foo\0bar \0" "foo")
   (u "Z8"  "foo\0bar \0" "foo")
   ;; added more basic tests (cross-referenced with Perl)
   (p "A*"  "foo-bar"     "foo-bar")
   (p "A11" "foo-bar"     "foo-bar    ")
   (u "A*"  "foo-bar"     "foo-bar")
   (u "A8"  "foo-bar"     "foo-bar")
   (p "a*"  "foo-bar"     "foo-bar")
   (p "a11" "foo-bar"     "foo-bar\0\0\0\0")
   (u "a*"  "foo-bar"     "foo-bar")
   (u "a8"  "foo-bar"     "foo-bar\0")
   (p "Z*"  "foo-bar"     "foo-bar\0")
   (p "Z11" "foo-bar"     "foo-bar\0\0\0\0")
   (p "Z3"  "foo"         "fo\0")
   (u "Z*"  "foo"         "foo")
   (u "Z8"  "foo"         "foo")
   ;; added binary and hex tests (cross-referenced with Perl)
   (p "B8"  "01000001"    "A")
   (u "B8"  "A"           "01000001")
   (p "b8"  "10000010"    "A")
   (u "b8"  "A"           "10000010")
   (p "B*"  "0100001001000011"   "BC")
   (u "B*"  "BC"                 "0100001001000011")
   (p "b*"  "1100001001000010"   "CB")
   (u "b*"  "CB"                 "1100001001000010")
   (p "B*"  "01000010010000110"  "BC\0")
   (p "b*"  "11000010010000100"  "CB\0")
   (p "H2"  "41"          "A")
   (u "H2"  "A"           "41")
   (p "h2"  "14"          "A")
   (u "h2"  "A"           "14")
   (p "H*"  "78797a"      "xyz")
   (u "H*"  "xyz"         "78797a")
   (p "h*"  "8797a7"      "xyz")
   (u "h*"  "xyz"         "8797a7")
   ))

;; pack nvNV byteorders

(for-each
 (lambda (x)
   (test (string-append "pack " (car x) " byteorders") (cadr x)
     (lambda ()
       (string-complete->incomplete (pack (car x) (cddr x) :to-string? #t)))))
 '(("n" #*"\xde\xad" #xdead)
   ("v" #*"\xad\xde" #xdead)
   ("N" #*"\xde\xad\xbe\xef" #xdeadbeef)
   ("V" #*"\xef\xbe\xad\xde" #xdeadbeef)))

(for-each
 (lambda (op)
   (for-each
    (lambda (i)
      (test* (format "pack/unpack ~A ~D" op i) (list i)
        (unpack op :from-string (pack op (list i) :to-string? #t))))
    '(0 1 2 3 127 128 1023 1024 65534 65535)))
 '("n" "N" "v" "V"))

(for-each
 (lambda (op)
   (for-each
    (lambda (i)
      (test* (format "pack/unpack ~A ~D" op i) (list i)
        (unpack op :from-string (pack op (list i) :to-string? #t))))
    '(65536 16777215 16777216 4294967295)))
 '("N" "V"))

;; object tests

(test* "unpack o" '((1 (2 (3))))
  (unpack "o" :from-string "(1 (2 (3)))"))

(test* "pack o" "(1 (2 (3)))"
  (pack "o" '((1 (2 (3)))) :to-string? #t))

;; interesting, equivalent regexps aren't equal? even if they print the
;; same
;;(test* "unpack o" '((1 (2 (3))) "string" #/re?ge[x]p/)
;;  (unpack "o*" :from-string "(1 (2 (3)))\"string\"#/re?ge[x]p/"))

(test* "pack o" "(1 (2 (3)))\"string\"#/re?ge[x]p/"
  (pack "o*" '((1 (2 (3))) "string" #/re?ge[x]p/) :to-string? #t))

;; grouping tests

(test* "pack ()" "AB"
  (pack "(C C)" '(65 66) :to-string? #t))

(test* "unpack ()" '(65 66)
  (unpack "(C C)" :from-string "AB"))

(test* "pack <>" "AB"
  (pack "<C C>" '((65 66)) :to-string? #t))

(test* "unpack <>" '((65 66))
  (unpack "<C C>" :from-string "AB"))

(test* "pack <>" "\x02ABCD"
  (pack "C/<C C>" '((65 66) (67 68)) :to-string? #t))

(test* "unpack <>" '((65 66) (67 68))
  (unpack "C/<C C>" :from-string "\x02ABCD"))

(test* "pack <>" "\x01AB"
  (pack "C/<C C>" '((65 66)) :to-string? #t))

(test* "unpack <>" '((65 66))
  (unpack "C/<C C>" :from-string "\x01AB"))

(test* "pack <>" "ABCD"
  (pack "<C C> 2" '((65 66) (67 68)) :to-string? #t))

(test* "unpack <>" '((65 66) (67 68))
  (unpack "<C C> 2" :from-string "ABCD"))

;; / tests

(test-error "unpack / with no numeric type"
    #/unknown pack template/
    (cut unpack "/a*" :from-string "hello"))

(test* "unpack C/a*" '("yes")
  (unpack "C/a*" :from-string #*"\x03yes"))

(test* "pack C/a*" "\x03yes"
  (pack "C/a*" '("yes") :to-string? #t))

(test* "unpack C/A*" '("yes")
  (unpack "C/A*" :from-string #*"\x03yes"))

(test* "pack C/A*" "\x03yes"
  (pack "C/A*" '("yes") :to-string? #t))

(test* "unpack C/C*" '(65 66 67)
  (unpack "C/C*" :from-string "\x03ABC"))

(test* "pack C/C*" "\x03ABC"
  (pack "C/C*" '(65 66 67) :to-string? #t))

(test* "unpack w/C*" '(65 66 67)
  (unpack "w/C*" :from-string "\x03ABC"))

(test* "pack w/C*" "\x03ABC"
  (pack "w/C*" '(65 66 67) :to-string? #t))

;; not sure about supporting implicit type conversions
;;(test* "pack / medley" "\x01A\x01B003ok \x03yes\x04z\x00"
;;  (pack "C/B* C/H* a3/A C/a* C/Z" '("01000001" "42" "ok" "yes" "z") :to-string? #t))

(test* "unpack / medley" '("01000001" "42" "ok" "yes" "z")
  (unpack "C/B* C/H* a3/A C/a* C/Z" :from-string "\x01A\x01B003ok \x03yes\x04z\x00abc"))

;; @ tests

(test* "pack fixed @" "a\0\0\0z"
  (pack "a1@4a1" '("a" "z") :to-string? #t))

(test* "pack fixed @ again" "a\0\0\0z"
  (pack "a1@4a1" '("a" "z") :to-string? #t))

(test* "unpack fixed @" '("a" "z")
  (unpack "a1@4a1" :from-string "a\0\0\0z"))

(test* "pack variable @" "a\0\0\0z"
  (pack "a*@4a1" '("a" "z") :to-string? #t))

(test* "unpack fixed @" '("a" "z")
  (unpack "C/a @4 a1" :from-string "\x01a\0\0z"))

(test* "pack twice @" "a\0\0\0mn\0\0\0z"
  (pack "a1@4a1a1@9a1" '("a" "m" "n" "z") :to-string? #t))

(test* "unpack twice @" '("a" "m" "n" "z")
  (unpack "a1@4a1a1@9a1" :from-string "a\0\0\0mn\0\0\0z"))

;; These two fail in 0.9.1.  Reported by ryoakg.
(test* "V*; a bug of peeking 1"
       (unfold (pa$ <= 16) (^_`(,(* 16 _))) (pa$ + 1) 0 (^_'()))
       (map (.$ (cute unpack "V*" :from-string <>)
                u8vector->string
                (cute u8vector <> 0 0 0))
            (iota 16 0 16)))
(test* "V*: a bug of peeking 2"
       '(#x01010101 #x01010101 #x000000d0 #x01010101 #x01010101)
       (unpack "V*" :from-string
               #*"\x01\x01\x01\x01\
                  \x01\x01\x01\x01\
                  \xd0\x00\x00\x00\
                  \x01\x01\x01\x01\
                  \x01\x01\x01\x01"))

(test-end)
