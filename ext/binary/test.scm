;;
;; testing binary.io
;; $Id: test.scm,v 1.1 2004-01-28 09:34:55 shirok Exp $

(use gauche.test)
(use gauche.uvector)
(use gauche.collection)

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

(test-read-binary "read-binary-uint8" *u8* read-binary-uint8)
(test-read-binary "read-binary-sint8" *s8* read-binary-sint8)

(test-read-binary "read-binary-uint16 be"
                  *u16be* read-binary-uint16 'big-endian)
(test-read-binary "read-binary-uint16 le"
                  *u16le* read-binary-uint16 'little-endian)
(test-read-binary "read-binary-sint16 be"
                  *s16be* read-binary-sint16 'big-endian)
(test-read-binary "read-binary-sint16 le"
                  *s16le* read-binary-sint16 'little-endian)

(test-read-binary "read-binary-uint32 be"
                  *u32be* read-binary-uint32 'big-endian)
(test-read-binary "read-binary-uint32 le"
                  *u32le* read-binary-uint32 'little-endian)
(test-read-binary "read-binary-sint32 be"
                  *s32be* read-binary-sint32 'big-endian)
(test-read-binary "read-binary-sint32 le"
                  *s32le* read-binary-sint32 'little-endian)

(test-read-binary "read-binary-uint64 be"
                  *u64be* read-binary-uint64 'big-endian)
(test-read-binary "read-binary-uint64 le"
                  *u64le* read-binary-uint64 'little-endian)
(test-read-binary "read-binary-sint64 be"
                  *s64be* read-binary-sint64 'big-endian)
(test-read-binary "read-binary-sint64 le"
                  *s64le* read-binary-sint64 'little-endian)


(define (test-write-binary label data proc . endian)
  (test* label *bytes-str*
         (string-complete->incomplete
          (call-with-output-string
            (lambda (p)
              (for-each (cut apply proc <> p endian) data))))))

(test-write-binary "write-binary-uint8" *u8* write-binary-uint8)
(test-write-binary "write-binary-sint8" *s8* write-binary-sint8)

(test-write-binary "write-binary-uint16 be"
                   *u16be* write-binary-uint16 'big-endian)
(test-write-binary "write-binary-uint16 le"
                   *u16le* write-binary-uint16 'little-endian)
(test-write-binary "write-binary-sint16 be"
                   *s16be* write-binary-sint16 'big-endian)
(test-write-binary "write-binary-sint16 le"
                   *s16le* write-binary-sint16 'little-endian)

(test-write-binary "write-binary-uint32 be"
                   *u32be* write-binary-uint32 'big-endian)
(test-write-binary "write-binary-uint32 le"
                   *u32le* write-binary-uint32 'little-endian)
(test-write-binary "write-binary-sint32 be"
                   *s32be* write-binary-sint32 'big-endian)
(test-write-binary "write-binary-sint32 le"
                   *s32le* write-binary-sint32 'little-endian)

(test-write-binary "write-binary-uint64 be"
                   *u64be* write-binary-uint64 'big-endian)
(test-write-binary "write-binary-uint64 le"
                   *u64le* write-binary-uint64 'little-endian)
(test-write-binary "write-binary-sint64 be"
                   *s64be* write-binary-sint64 'big-endian)
(test-write-binary "write-binary-sint64 le"
                   *s64le* write-binary-sint64 'little-endian)

(define (test-write-binary-range label proc min min-1 max max+1)
  (test* label '(#t #f #t #f)
         (map (lambda (val)
                (with-error-handler
                    (lambda (e) #f)
                  (lambda ()
                    (and (call-with-output-string (cut proc val <>))
                         #t))))
              (list min min-1 max max+1))))

(test-write-binary-range "write-binary-uint8" write-binary-uint8
                         0 -1 255 256)
(test-write-binary-range "write-binary-sint8" write-binary-sint8
                         -128 -129 127 128)
(test-write-binary-range "write-binary-uint16" write-binary-uint16
                         0 -1 #xffff #x10000)
(test-write-binary-range "write-binary-sint16" write-binary-sint16
                         #x-8000 #x-8001 #x7fff #x8000)
(test-write-binary-range "write-binary-uint32" write-binary-uint32
                         0 -1 #xffffffff #x100000000)
(test-write-binary-range "write-binary-sint32" write-binary-sint32
                         #x-80000000 #x-80000001 #x7fffffff #x80000000)
(test-write-binary-range "write-binary-uint64" write-binary-uint64
                         0 -1 #xffffffffffffffff #x10000000000000000)
(test-write-binary-range "write-binary-sint64" write-binary-sint64
                         #x-8000000000000000 #x-8000000000000001
                         #x7fffffffffffffff  #x8000000000000000)

(define (test-flonum-rw label reader writer bits)
  (test* label (string-complete->incomplete bits)
         (let ((val (with-input-from-string bits (cut reader))))
           (and (real? val)
                (string-complete->incomplete
                 (with-output-to-string (cut writer val)))))))

(define (test-flonum-rw-f32 bvec endian)
  (test-flonum-rw (format "read/write-binary-float (~a)" endian)
                  (cut read-binary-float #f endian)
                  (cut write-binary-float <> #f endian)
                  (u8vector->string bvec)))

(define (test-flonum-rw-f64 bvec endian)
  (test-flonum-rw (format "read/write-binary-double (~a)" endian)
                  (cut read-binary-double #f endian)
                  (cut write-binary-double <> #f endian)
                  (u8vector->string bvec)))

(define (test-flonum-rw-driver tester bytes-list)
  (for-each (lambda (bytes)
              (let ((be-vec (coerce-to <u8vector> bytes))
                    (le-vec (coerce-to <u8vector> (reverse bytes))))
                (tester be-vec 'big-endian)
                (tester le-vec 'little-endian)))
            bytes-list))

;; f32 - ieee single float
(test-flonum-rw-driver
 test-flonum-rw-f32
 '((#x00 #x00 #x00 #x00)   ;; 0.0
   (#x00 #x00 #x00 #x01)   ;; denormalized
   (#x00 #x00 #x00 #x02)
   (#x00 #x00 #x00 #x03)
   (#x00 #x80 #x00 #x00)   ;; normalized
   (#x00 #x80 #x00 #x01)
   (#x00 #x80 #x00 #x02)
   (#x3f #x80 #x00 #x00)   ;; 1.0
   (#x7f #x7f #xff #xff)
   (#x7f #x80 #x00 #x00)   ;; +inf

   (#x80 #x00 #x00 #x00)   ;;-0.0
   (#x80 #x00 #x00 #x01)   ;; denormalized
   (#x80 #x00 #x00 #x02)
   (#x80 #x00 #x00 #x03)
   (#x80 #x00 #x00 #x00)   ;; normalized
   (#xbf #x80 #x00 #x00)   ;; -1.0
   (#xff #x7f #xff #xff)
   (#xff #x80 #x00 #x00)   ;; -inf
   ))

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

(test* "read-binary-float" 1.0
       (with-input-from-string #*"\x3f\x80\x00\x00"
                               (cut read-binary-float #f 'big-endian)))
(test* "read-binary-double" 1.0
       (with-input-from-string #*"\x3f\xf0\x00\x00\x00\x00\x00\x00"
                               (cut read-binary-double #f 'big-endian)))

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

(test-end)
