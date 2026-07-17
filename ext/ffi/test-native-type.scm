;;
;; testing gauche.native-type
;;

(use gauche.test)
(use gauche.uvector)

(test-start "native types")

(use gauche.native-type)
(test-module 'gauche.native-type)


;; Common data

(define *fobject-storage*
  '#u8(#x80 #x01 #x02 #x03 #x04 #x05 #x06 #x07
       #x08 #x09 #x0a #x0b #x0c #x0d #x0e #x0f
       #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17
       #x18 #x19 #x1a #x1b #x1c #x1d #x1e #x1f
       #x20 #x21 #x22 #x23 #x24 #x25 #x26 #x27
       #x28 #x29 #x2a #x2b #x2c #x2d #x2e #x2f
       #x30 #x31 #x32 #x33 #x34 #x35 #x36 #x37
       #x38 #x39 #x3a #x3b #x3c #x3d #x3e #x3f
       #xf0 #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 #xf7
       #xf8 #xf9 #xfa #xfb #xfc #xfd #xfe #xff
       ))

;;;---------------------------------------------------
(test-section "type equivalence")

;; Type equivalences
(let* ([int* (make-c-pointer-type <int>)]
       [int8* (make-c-pointer-type <int8>)]
       [int8a (make-c-array-type <int8> '(2 2))]
       [int8a2 (make-c-array-type <int8> '(2 2))]
       [int8a4 (make-c-array-type <int8> '(4))]
       [u8a2 (make-c-array-type <uint8> '(2 2))]
       [fn1 (make-c-function-type <int> `(,<int8> ,<uint16>))]
       [fn2 (make-c-function-type <int> `(,<int8> ,<uint16>))]
       [fn-var (make-c-function-type <int> `(,<int8> ,<uint16> ...))]
       [s1 (make-c-struct-type 's1 `((a ,<int8>) (b ,<uint16>)))]
       [s1b (make-c-struct-type 's1 `((a ,<int8>) (b ,<uint16>)))]
       [s2 (make-c-struct-type 's2 `((a ,<int8>) (b ,<uint16>)))]
       [u1 (make-c-union-type 'u1 `((a ,<int8>) (b ,<uint16>)))]
       [u1b (make-c-union-type 'u1 `((a ,<int8>) (b ,<uint16>)))]
       [u2 (make-c-union-type 'u2 `((a ,<int8>) (b ,<uint16>)))])
  ;; equivalent cases
  (test* "native type equal? pointer" #t
         (equal? (make-c-pointer-type <int>) (make-c-pointer-type <int>)))
  (test* "native type equal? array" #t
         (equal? int8a int8a2))
  (test* "native type equal? function" #t
         (equal? fn1 fn2))
  (test* "native type equal? struct" #t
         (equal? s1 s1b))
  (test* "native type equal? union" #t
         (equal? u1 u1b))

  ;; inequivalent cases
  (test* "native type equal? pointer mismatch" #f
         (equal? int* int8*))
  (test* "native type equal? array dims mismatch" #f
         (equal? int8a int8a4))
  (test* "native type equal? array element mismatch" #f
         (equal? int8a u8a2))
  (test* "native type equal? function varargs mismatch" #f
         (equal? fn1 fn-var))
  (test* "native type equal? struct tag mismatch" #f
         (equal? s1 s2))
  (test* "native type equal? union tag mismatch" #f
         (equal? u1 u2))

  ;; cross-type comparisons
  (test* "native type equal? struct vs pointer" #f
         (equal? s1 int*))
  (test* "native type equal? union vs array" #f
         (equal? u1 int8a))
  (test* "native type equal? function vs struct" #f
         (equal? fn1 s1)))


;;;---------------------------------------------------
(test-section "make-native-handle")

;; make-native-handle & basic check
(let* ([int8* (make-c-pointer-type <int8>)]
       [int8** (make-c-pointer-type int8*)]
       [s (native-type '(.struct (a::int8_t b::int8_t)))]
       [s* (make-c-pointer-type s)])
  (test* "make-native-handle int8*" #t
         (is-a? (make-native-handle int8* '#u8(0)) <native-handle>))
  (test* "make-native-handle s" #t
         (is-a? (make-native-handle s '#u8(0 0)) <native-handle>))
  (test* "make-native-handle s*" #t
         (is-a? (make-native-handle s* '#u8(0 0)) <native-handle>))
  (test* "make-native-handle int8** (error)"
         (test-error <error> #/type size \d too big/)
         (is-a? (make-native-handle int8** '#u8(0)) <native-handle>))
  (test* "make-native-handle s (error)"
         (test-error <error> #/type size 2 too big/)
         (is-a? (make-native-handle s '#u8(0)) <native-handle>))
  (test* "make-native-handle s* (error)"
         (test-error <error> #/type size 2 too big/)
         (is-a? (make-native-handle s* '#u8(0)) <native-handle>))
  )

(let* ([int16* (make-c-pointer-type <int16>)]
       [h (make-c-array-handle <int16> 8)])
  (test* "make-native-handle auto allocation" 16
         (and-let* ([s (native-handle-owner h)]
                    [ (u8vector? s) ])
           (u8vector-length s)))

  (test* "make-c-array-handle auto allocation (nonzero offset)"
         (test-error <error> #/Non-zero offset is not allowed/)
         (make-c-array-handle <int16> 8 #f 1))

  (test* "native-handle-belongs? h h" #t
         (native-handle-belongs? h h))
  (test* "native-handle-belongs? h+3 h" #t
         (let1 u (native-handle-owner h)
           (native-handle-belongs? (make-native-handle int16* u 3)
                                   h)))
  (test* "native-handle-belongs? h h+3" #t
         (let1 u (native-handle-owner h)
           (native-handle-belongs? h
                                   (make-native-handle int16* u 3))))
  (test* "native-handle-belongs? new h" #f
         (native-handle-belongs? (make-native-handle int16*)
                                 h))
  (test* "native-handle-belongs? NULL h" #f
         (native-handle-belongs? (null-pointer-handle int16*)
                                 h))
  (test* "native-handle-belongs? h NULL" #f
         (native-handle-belongs? h
                                 (null-pointer-handle int16*)))
  )

;;;---------------------------------------------------
(test-section "pointer dereference")

(let ([data (u8vector-copy *fobject-storage*)]
      [int8* (make-c-pointer-type <int8>)]
      [uint8* (make-c-pointer-type <uint8>)]
      [int16* (make-c-pointer-type <int16>)]
      [uint16* (make-c-pointer-type <uint16>)]
      [int32* (make-c-pointer-type <int32>)]
      [uint32* (make-c-pointer-type <uint32>)]
      [int64* (make-c-pointer-type <int64>)]
      [uint64* (make-c-pointer-type <uint64>)]
      [char* (make-c-pointer-type <c-char>)])
  (define (bc pos type) (make-native-handle type data pos))

  (test* "uint8* deref" '(#x80 #x09 #x3f)
         (list (native* (bc 0 uint8*))
               (native* (bc 9 uint8*))
               (native-aref (bc 56 uint8*) 7)))
  (test* "int8* deref" '(#x-80 #x09 #x3f)
         (list (native* (bc 0 int8*))
               (native* (bc 9 int8*))
               (native-aref (bc 56 int8*) 7)))

  (test* "uint16* deref" (case (native-endian)
                           [(big-endian) '(#x0809 #xfeff)]
                           [else         '(#x0908 #xfffe)])
         (list (native* (bc 8 uint16*))
               (native-aref (bc 72 uint16*) 3)))
  (test* "int16* deref" (case (native-endian)
                          [(big-endian) '(#x0809 #x-0101)]
                          [else         '(#x0908 #x-0002)])
         (list (native* (bc 8 int16*))
               (native-aref (bc 72 int16*) 3)))

  (test* "uint32* deref" (case (native-endian)
                           [(big-endian) '(#x10111213 #xf8f9fafb)]
                           [else         '(#x13121110 #xfbfaf9f8)])
         (list (native* (bc 16 uint32*))
               (native-aref (bc 64 uint32*) 2)))
  (test* "int32* deref" (case (native-endian)
                          [(big-endian) '(#x10111213 #x-07060505)]
                          [else         '(#x13121110 #x-04050608)])
         (list (native* (bc 16 int32*))
               (native-aref (bc 64 int32*) 2)))

  (test* "uint64* deref" (case (native-endian)
                           [(big-endian) '(#x2021222324252627
                                           #xf0f1f2f3f4f5f6f7)]
                           [else         '(#x2726252423222120
                                           #xf7f6f5f4f3f2f1f0)])
         (list (native* (bc 32 uint64*))
               (native-aref (bc 32 uint64*) 4)))
  (test* "int64* deref" (case (native-endian)
                          [(big-endian) '(#x2021222324252627
                                          #x-0f0e0d0c0b0a0909)]
                          [else         '(#x2726252423222120
                                          #x-08090a0b0c0d0e10)])
         (list (native* (bc 32 int64*))
               (native-aref (bc 32 int64*) 4)))

  (test* "char* deref" '(#\space #\!)
         (list (native* (bc 32 char*))
               (native-aref (bc 32 char*) 1)))

  (test* "uint8* modify" #xff
         (begin
           (set! (native-aref (bc 0 uint8*) 1) #xff)
           (native* (bc 1 uint8*))))
  (test* "int8* modify" -2
         (begin
           (set! (native-aref (bc 0 int8*) 1) -2)
           (native* (bc 1 int8*))))

  (test* "uint16* modify" #xabcd
         (begin
           (set! (native-aref (bc 0 uint16*) 1) #xabcd)
           (native* (bc 2 uint16*))))
  (test* "int16* modify" #x-1234
         (begin
           (set! (native-aref (bc 0 int16*) 1) #x-1234)
           (native* (bc 2 int16*))))

  (test* "uint32* modify" #x89abcdef
         (begin
           (set! (native-aref (bc 0 uint32*) 1) #x89abcdef)
           (native* (bc 4 uint32*))))
  (test* "int32* modify" #x-789abcde
         (begin
           (set! (native-aref (bc 0 int32*) 1) #x-789abcde)
           (native* (bc 4 int32*))))

  (test* "uint64* modify" #x0123456789abcdef
         (begin
           (set! (native-aref (bc 0 uint64*) 1) #x0123456789abcdef)
           (native* (bc 8 uint64*))))
  (test* "int64* modify" #x-0123456789abcdef
         (begin
           (set! (native-aref (bc 0 int64*) 1) #x-0123456789abcdef)
           (native* (bc 8 int64*))))

  (test* "native-char modify" #\Z
         (begin
           (set! (native* (bc 32 char*)) #\Z)
           (native-aref (bc 0 char*) 32)))
  )

(let* ([int*  (make-c-pointer-type <int>)]
       [int** (make-c-pointer-type int*)]
       [data (make-u8vector (+ (~ int*'size)
                               (~ <int>'size)))]
       [h1 (make-native-handle int** data)]
       [h2 (make-native-handle int* data (~ int*'size))])
  (test* "null pointer on stoage" #t
         (begin
           (set! (native* h1) (null-pointer-handle))
           (null-pointer-handle? (native* h1))))
  (test* "setting pointer on storage" #f
         (begin
           (set! (native* h1) h2)
           (null-pointer-handle? h1)))
  (test* "dereferencing pointer on storage" 123
         (begin
           (set! (native* h2) 123)
           (native* (native* h1))))
  )

;;;---------------------------------------------------
(test-section "pointer arithmetic")

(let ([h (make-native-handle (native-type 'char*) "abcde")])
  (test* "native-pointer+ (char*)" #\c
         (native* (native-pointer+ h 2)))
  (test* "native-pointer+ (char*)" (test-error <error> #/Offset out of range/)
         (native* (native-pointer+ h 10)))
  (test* "native-pointer+ (char*)" (test-error <error> #/Offset out of range/)
         (native* (native-pointer+ h -2)))
  (test* "native-pointer+ (char*)" #\b
         (native* (native-pointer+ (native-pointer+ h 4) -3)))
  )


(let ([h (make-native-handle (native-type 'void*) (make-u8vector 16))])
  (test* "native-pointer+ (void*)"
         '#u8(0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1)
         (begin
           (set! (native* (cast-handle 'int64_t* (native-pointer+ h 8)))
                 #x01010101_01010101)
           (native-handle-owner h))))

(let ([h (make-native-handle (native-type 'int32_t*) #s32(0 -10 -20 -30))])
  (test* "native-pointer+ (int32_t*)" -20
         (native* (native-pointer+ h 2)))
  (test* "native-pointer+ (int32_t)" (test-error <error> #/Offset out of range/)
         (native* (native-pointer+ h 5)))
  (test* "native-pointer+ (int32_t)" (test-error <error> #/Offset out of range/)
         (native* (native-pointer+ h -1)))
  (test* "native-pointer+ (int32_t)" -10
         (native* (native-pointer+ (native-pointer+ h 3) -2)))
  (test* "native-pointer+ (past-end pointer)" #t
         (native-handle-belongs? h (native-pointer+ h 4)))
  (test* "native-pointer+ (past-end pointer)" (test-error <error> #/Past-end pointer cannot/)
         (native* (native-pointer+ h 4)))
  )

(let ([h (make-c-array-handle <int32> '(4) #s32(10 20 30 40))])
  (test* "native-pointer+ (int[] auto-cast to int*)" #t
         (c-pointer-handle? (native-pointer+ h 0)))
  (test* "native-pointer+ (int[] auto-cast to int*) pointee" #t
         (equal? <int32>
                 (c-pointer-type-pointee
                  (native-handle-type (native-pointer+ h 0)))))
  (test* "native-pointer+ (int[] auto-cast to int*) deref" 30
         (native* (native-pointer+ h 2)))
  (test* "native-pointer+ (int[] auto-cast to int*) past-end" #t
         (native-handle-belongs? h (native-pointer+ h 4)))
  (test* "native-pointer+ (int[] auto-cast int*) past-end deref"
         (test-error <error> #/Past-end pointer cannot/)
         (native* (native-pointer+ h 4)))
  (test* "native-pointer+ (int[] auto-cast to int*) out of range"
         (test-error <error> #/Offset out of range/)
         (native* (native-pointer+ h 5)))
  )

(let ([h (make-c-array-handle <int32> '(3 2) #s32(10 20 30 40 50 60))])
  (test* "native-pointer+ (int[][2] auto-cast to (int[2])*)" #t
         (c-pointer-handle? (native-pointer+ h 0)))
  (test* "native-pointer+ (int[][2] auto-cast to (int[2])*) pointee" '(2)
         (c-array-type-dimensions
          (c-pointer-type-pointee
           (native-handle-type (native-pointer+ h 0)))))
  (test* "native-pointer+ (int[][2] auto-cast to (int[2])*) deref row 0" '(10 20)
         (let1 row (native* (native-pointer+ h 0))
           (list (native-aref row 0) (native-aref row 1))))
  (test* "native-pointer+ (int[][2] auto-cast to (int[2])*) deref row 1" '(30 40)
         (let1 row (native* (native-pointer+ h 1))
           (list (native-aref row 0) (native-aref row 1))))
  (test* "native-pointer+ (int[][2] auto-cast to (int[2])*) deref row 2" '(50 60)
         (let1 row (native* (native-pointer+ (native-pointer+ h 3) -1))
           (list (native-aref row 0) (native-aref row 1))))
  (test* "native-pointer+ (int[][2] auto-cast to (int[2])*) past-end" #t
         (native-handle-belongs? h (native-pointer+ h 3)))
  )

;;;---------------------------------------------------
(test-section "arrays")


(let ([data (u8vector-copy *fobject-storage*)]
      [int8a (make-c-array-type <int8> '(* 4 2))]
      [uint8a (make-c-array-type <uint8> '(2 4 2))]
      [int16a (make-c-array-type <int16> '(* 4 2))]
      [uint16a (make-c-array-type <uint16> '(2 4 2))]
      [int32a (make-c-array-type <int32> '(* 3 2))]
      [uint32a (make-c-array-type <uint32> '(2 3 2))]
      [int64a (make-c-array-type <int64> '(* 2))]
      [uint64a (make-c-array-type <uint64> '(2 2))])
  (define (bc pos type) (make-native-handle type data pos))
  (define (tsa type expect)
    (test* #"~|type| size&alignment" expect
           (list (~ type'size) (~ type'alignment))))

  (tsa int8a '(0 1))
  (tsa uint8a '(16 1))
  (tsa int16a '(0 2))
  (tsa uint16a '(32 2))
  (tsa int32a '(0 4))
  (tsa uint32a '(48 4))
  (tsa int64a '(0 8))
  (tsa uint64a '(32 8))

  (test* "uint8 array ref" '(#x80 #x01 #x02 #x03 #x04 #x05 #x06 #x07
                                  #x08 #x09)
         (list (native-aref (bc 0 uint8a) 0 0 0)
               (native-aref (bc 0 uint8a) 0 0 1)
               (native-aref (bc 0 uint8a) 0 1 0)
               (native-aref (bc 0 uint8a) 0 1 1)
               (native-aref (bc 0 uint8a) 0 2 0)
               (native-aref (bc 0 uint8a) 0 2 1)
               (native-aref (bc 0 uint8a) 0 3 0)
               (native-aref (bc 0 uint8a) 0 3 1)
               (native-aref (bc 0 uint8a) 1 0 0)
               (native-aref (bc 0 uint8a) 1 0 1)))

  (test* "int8 array ref" '(#x-80 #x01 #x02 #x03 #x04 #x05 #x06 #x07
                                  #x08 #x09 #x18)
         (list (native-aref (bc 0 int8a) 0 0 0)
               (native-aref (bc 0 int8a) 0 0 1)
               (native-aref (bc 0 int8a) 0 1 0)
               (native-aref (bc 0 int8a) 0 1 1)
               (native-aref (bc 0 int8a) 0 2 0)
               (native-aref (bc 0 int8a) 0 2 1)
               (native-aref (bc 0 int8a) 0 3 0)
               (native-aref (bc 0 int8a) 0 3 1)
               (native-aref (bc 0 int8a) 1 0 0)
               (native-aref (bc 0 int8a) 1 0 1)
               (native-aref (bc 0 int8a) 3 0 0)))

  (test* "uint16 array ref"
         (case (native-endian)
           [(big-endian) '(#x8001 #x0203 #x0405 #x0e0f
                           #x1011 #x1e1f)]
           [else         '(#x0180 #x0302 #x0504 #x0f0e
                           #x1110 #x1f1e)])
         (list (native-aref (bc 0 uint16a) 0 0 0)
               (native-aref (bc 0 uint16a) 0 0 1)
               (native-aref (bc 0 uint16a) 0 1 0)
               (native-aref (bc 0 uint16a) 0 3 1)
               (native-aref (bc 0 uint16a) 1 0 0)
               (native-aref (bc 0 uint16a) 1 3 1)))
  (test* "int16 array ref"
         (case (native-endian)
           [(big-endian) '(#x0203 #x0405 #x0e0f
                           #x1011 #x1e1f #xfeff)]
           [else         '(#x0302 #x0504 #x0f0e
                           #x1110 #x1f1e #x-0002)])
         (list (native-aref (bc 0 int16a) 0 0 1)
               (native-aref (bc 0 int16a) 0 1 0)
               (native-aref (bc 0 int16a) 0 3 1)
               (native-aref (bc 0 int16a) 1 0 0)
               (native-aref (bc 0 int16a) 1 3 1)
               (native-aref (bc 0 int16a) 4 3 1)))

  (test* "uint32 array ref"
         (case (native-endian)
           [(big-endian) '(#x80010203 #x04050607
                           #x10111213 #x2c2d2e2f)]
           [else         '(#x03020180 #x07060504
                           #x13121110 #x2f2e2d2c)])
         (list (native-aref (bc 0 uint32a) 0 0 0)
               (native-aref (bc 0 uint32a) 0 0 1)
               (native-aref (bc 0 uint32a) 0 2 0)
               (native-aref (bc 0 uint32a) 1 2 1)))
  (test* "int32 array ref"
         (case (native-endian)
           [(big-endian) '(#x80010203 #x04050607
                           #x10111213 #x-03020101)]
           [else         '(#x03020180 #x07060504
                           #x13121110 #x-00010204)])
         (list (native-aref (bc 0 int32a) 0 0 0)
               (native-aref (bc 0 int32a) 0 0 1)
               (native-aref (bc 0 int32a) 0 2 0)
               (native-aref (bc 0 int32a) 3 0 1)))

  (test* "uint64 array ref"
         (case (native-endian)
           [(big-endian) '(#x8001020304050607
                           #x18191a1b1c1d1e1f)]
           [else         '(#x0706050403020180
                           #x1f1e1d1c1b1a1918)])
         (list (native-aref (bc 0 uint64a) 0 0)
               (native-aref (bc 0 uint64a) 1 1)))
  (test* "int64 array ref"
         (case (native-endian)
           [(big-endian) '(#x8001020304050607
                           #x-0706050403020101)]
           [else         '(#x0706050403020180
                           #x-0001020304050608)])
         (list (native-aref (bc 0 int64a) 0 0)
               (native-aref (bc 0 int64a) 4 1)))

  (test* "uint8 array modify" #xff
         (begin
           (set! (native-aref (bc 0 uint8a) 0 0 1) #xff)
           (native-aref (bc 0 uint8a) 0 0 1)))
  (test* "int8 array modify" -2
         (begin
           (set! (native-aref (bc 0 int8a) 0 0 1) -2)
           (native-aref (bc 0 int8a) 0 0 1)))

  (test* "uint16 array modify" #xabcd
         (begin
           (set! (native-aref (bc 0 uint16a) 0 0 1) #xabcd)
           (native-aref (bc 0 uint16a) 0 0 1)))
  (test* "int16 array modify" #x-1234
         (begin
           (set! (native-aref (bc 0 int16a) 0 0 1) #x-1234)
           (native-aref (bc 0 int16a) 0 0 1)))

  (test* "uint32 array modify" #x89abcdef
         (begin
           (set! (native-aref (bc 0 uint32a) 0 2 0) #x89abcdef)
           (native-aref (bc 0 uint32a) 0 2 0)))
  (test* "int32 array modify" #x-789abcde
         (begin
           (set! (native-aref (bc 0 int32a) 0 2 0) #x-789abcde)
           (native-aref (bc 0 int32a) 0 2 0)))

  (test* "uint64 array modify" #x0123456789abcdef
         (begin
           (set! (native-aref (bc 0 uint64a) 0 0) #x0123456789abcdef)
           (native-aref (bc 0 uint64a) 0 0)))
  (test* "int64 array modify" #x-0123456789abcdef
         (begin
           (set! (native-aref (bc 0 int64a) 0 0) #x-0123456789abcdef)
           (native-aref (bc 0 int64a) 0 0)))

  ;; partial index
  (test* "int8 array partial dereference" #x17
         (let1 a (native-aref (bc 0 int8a) 2 3)
           (native-aref a 1)))
  )

(let ([data (case (native-endian)
              [(big-endian)
               (u8vector
                ;; floats (big-endian)
                #x3f #x80 #x00 #x00    ;  1.0f
                #xbf #x80 #x00 #x00    ; -1.0f
                #x40 #x60 #x00 #x00    ;  3.5f
                ;; doubles (big-endian)
                #x3f #xf0 #x00 #x00 #x00 #x00 #x00 #x00  ;  1.0
                #xbf #xf0 #x00 #x00 #x00 #x00 #x00 #x00  ; -1.0
                #x40 #x04 #x00 #x00 #x00 #x00 #x00 #x00  ;  2.5
                )]
              [(little-endian)
               (u8vector
                ;; floats (little-endian)
                #x00 #x00 #x80 #x3f    ;  1.0f
                #x00 #x00 #x80 #xbf    ; -1.0f
                #x00 #x00 #x60 #x40    ;  3.5f
                ;; doubles (little-endian)
                #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x3f  ;  1.0
                #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #xbf  ; -1.0
                #x00 #x00 #x00 #x00 #x00 #x00 #x04 #x40  ;  2.5
                )]
              [(arm-little-endian)
               (u8vector
                ;; floats (little-endian)
                #x00 #x00 #x80 #x3f    ;  1.0f
                #x00 #x00 #x80 #xbf    ; -1.0f
                #x00 #x00 #x60 #x40    ;  3.5f
                ;; doubles (little-endian)
                #x00 #x00 #xf0 #x3f #x00 #x00 #x00 #x00  ;  1.0
                #x00 #x00 #xf0 #xbf #x00 #x00 #x00 #x00  ; -1.0
                #x00 #x00 #x04 #x40 #x00 #x00 #x00 #x00  ;  2.5
                )])]
      [float* (make-c-pointer-type <float>)]
      [double* (make-c-pointer-type <double>)]
      [floata (make-c-array-type <float> '(3))]
      [doublea (make-c-array-type <double> '(3))])
  (define (bc pos type) (make-native-handle type data pos))
  (define (tsa type expect)
    (test* #"~|type| size&alignment" expect
           (list (~ type'size) (~ type'alignment))))

  (tsa floata '(12 4))
  (tsa doublea '(24 8))

  (test* "float* deref" '(1.0 -1.0 3.5)
         (list (native* (bc 0 float*))
               (native-aref (bc 0 float*) 1)
               (native-aref (bc 0 float*) 2)))
  (test* "double* deref" '(1.0 -1.0 2.5)
         (list (native* (bc 12 double*))
               (native-aref (bc 12 double*) 1)
               (native-aref (bc 12 double*) 2)))
  (test* "float array ref" '(1.0 -1.0 3.5)
         (list (native-aref (bc 0 floata) 0)
               (native-aref (bc 0 floata) 1)
               (native-aref (bc 0 floata) 2)))
  (test* "double array ref" '(1.0 -1.0 2.5)
         (list (native-aref (bc 12 doublea) 0)
               (native-aref (bc 12 doublea) 1)
               (native-aref (bc 12 doublea) 2)))

  (test* "float* modify" -2.0
         (begin
           (set! (native-aref (bc 0 float*) 1) -2.0)
           (native-aref (bc 0 float*) 1)))
  (test* "float array modify" -2.0
         (begin
           (set! (native-aref (bc 0 floata) 1) -2.0)
           (native-aref (bc 0 floata) 1)))

  (test* "double* modify" -4.5
         (begin
           (set! (native-aref (bc 12 double*) 1) -4.5)
           (native-aref (bc 12 double*) 1)))
  (test* "double array modify" -4.5
         (begin
           (set! (native-aref (bc 12 doublea) 1) -4.5)
           (native-aref (bc 12 doublea) 1)))
  )

;;;---------------------------------------------------
(test-section "structs and unions")

(let ([data (u8vector-copy *fobject-storage*)]
      [s1 (make-c-struct-type 's1
                              `((a ,<int8>)
                                (b ,<uint32>)
                                (c ,<uint16>)
                                (d ,<uint8>)))]
      [s2 (make-c-struct-type 's2
                              `((a ,<uint8>)
                                (b ,<uint64>)
                                (c ,<int16>)))]
      [s0 (make-c-struct-type 's0 '())])
  (define (bc pos type) (make-native-handle type data pos))
  (define (tsa type expect)
    (test* #"~|type| size&alignment" expect
           (list (~ type'size) (~ type'alignment))))
  (define native-type-offset*
    (with-module gauche.native-type native-type-offset))
  (define (offsets type fields)
    (map (cut native-type-offset* type <>) fields))

  (tsa s0 '(0 1))
  (tsa s1 '(12 4))
  (tsa s2 '(24 8))

  (test* "native struct offsets s1" '(0 4 8 10)
         (offsets s1 '(a b c d)))
  (test* "native struct offsets s2" '(0 8 16)
         (offsets s2 '(a b c)))

  (test* "native struct ref s1"
         (case (native-endian)
           [(big-endian) '(#x-80 #x04050607 #x0809 #x0a)]
           [else         '(#x-80 #x07060504 #x0908 #x0a)])
         (list (native. (bc 0 s1) 'a)
               (native. (bc 0 s1) 'b)
               (native. (bc 0 s1) 'c)
               (native. (bc 0 s1) 'd)))
  (test* "native struct ref s2"
         (case (native-endian)
           [(big-endian) '(#x80 #x08090a0b0c0d0e0f #x1011)]
           [else         '(#x80 #x0f0e0d0c0b0a0908 #x1110)])
         (list (native. (bc 0 s2) 'a)
               (native. (bc 0 s2) 'b)
               (native. (bc 0 s2) 'c)))

  (test* "native struct modify s1" '(#x-1 #x11223344 #xabcd #xfe)
         (begin
           (set! (native. (bc 0 s1) 'a) -1)
           (set! (native. (bc 0 s1) 'b) #x11223344)
           (set! (native. (bc 0 s1) 'c) #xabcd)
           (set! (native. (bc 0 s1) 'd) #xfe)
           (list (native. (bc 0 s1) 'a)
                 (native. (bc 0 s1) 'b)
                 (native. (bc 0 s1) 'c)
                 (native. (bc 0 s1) 'd))))

  (test* "native struct modify s2" '(#xaa #x0123456789abcdef #x-2)
         (begin
           (set! (native. (bc 0 s2) 'a) #xaa)
           (set! (native. (bc 0 s2) 'b) #x0123456789abcdef)
           (set! (native. (bc 0 s2) 'c) -2)
           (list (native. (bc 0 s2) 'a)
                 (native. (bc 0 s2) 'b)
                 (native. (bc 0 s2) 'c)))))

(let* ([data (u8vector-copy *fobject-storage*)]
       [u16x2 (make-c-array-type <uint16> '(2))]
       [s3 (make-c-struct-type 's3
                               `((arr ,u16x2)
                                 (b ,<uint8>)))])
  (define (bc pos type) (make-native-handle type data pos))
  (define (offsets type fields)
    (map (cut c-struct/union-type-field-offset type <>) fields))

  (test* "native struct array member size&alignment" '(6 2)
         (list (~ s3'size) (~ s3'alignment)))
  (test* "native struct array member offsets" '(0 4)
         (offsets s3 '(arr b)))

  (test* "native struct array member ref"
         (case (native-endian)
           [(big-endian) '(#x8001 #x0203 #x04)]
           [else         '(#x0180 #x0302 #x04)])
         (let1 arr (native. (bc 0 s3) 'arr)
           (list (native-aref arr 0)
                 (native-aref arr 1)
                 (native. (bc 0 s3) 'b))))

  (test* "native struct array member modify"
         '(#xabcd #xfe)
         (let1 arr (native. (bc 0 s3) 'arr)
           (set! (native-aref arr 1) #xabcd)
           (set! (native. (bc 0 s3) 'b) #xfe)
           (list (native-aref arr 1)
                 (native. (bc 0 s3) 'b)))))

;; c-union tests
(let ([data (u8vector-copy *fobject-storage*)]
      [u1 (make-c-union-type 'u1
                             `((a ,<int8>)
                               (b ,<uint32>)
                               (c ,<uint16>)))]
      [u2 (make-c-union-type 'u2
                             `((x ,<uint8>)
                               (y ,<uint64>)
                               (z ,<int16>)))]
      [u0 (make-c-union-type 'u0 '())])
  (define (bc pos type) (make-native-handle type data pos))
  (define (tsa type expect)
    (test* #"~|type| size&alignment" expect
           (list (~ type'size) (~ type'alignment))))
  (define native-type-offset*
    (with-module gauche.native-type native-type-offset))
  (define (offsets type fields)
    (map (cut native-type-offset* type <>) fields))

  ;; Union size is max of all field sizes, rounded up to alignment.
  ;; u1: fields are int8(1), uint32(4), uint16(2) => max size=4, alignment=4 => size=4
  ;; u2: fields are uint8(1), uint64(8), int16(2) => max size=8, alignment=8 => size=8
  (tsa u0 '(0 1))
  (tsa u1 '(4 4))
  (tsa u2 '(8 8))

  ;; All offsets in a union should be 0
  (test* "native union offsets u1" '(0 0 0)
         (offsets u1 '(a b c)))
  (test* "native union offsets u2" '(0 0 0)
         (offsets u2 '(x y z)))

  ;; Reading via different fields interprets the same bytes differently
  (test* "native union ref u1"
         (case (native-endian)
           [(big-endian) (list #x-80           ; a: int8 at offset 0
                               #x80010203      ; b: uint32 at offset 0
                               #x8001)]        ; c: uint16 at offset 0
           [else         (list #x-80           ; a: int8 at offset 0
                               #x03020180      ; b: uint32 at offset 0
                               #x0180)])       ; c: uint16 at offset 0
         (list (native. (bc 0 u1) 'a)
               (native. (bc 0 u1) 'b)
               (native. (bc 0 u1) 'c)))

  (test* "native union ref u2"
         (case (native-endian)
           [(big-endian) (list #x80                     ; x: uint8
                               #x8001020304050607       ; y: uint64
                               #x8001)]                 ; z: int16
           [else         (list #x80                     ; x: uint8
                               #x0706050403020180       ; y: uint64
                               #x0180)])                ; z: int16 (positive)
         (list (native. (bc 0 u2) 'x)
               (native. (bc 0 u2) 'y)
               (native. (bc 0 u2) 'z)))

  ;; Writing to one field and reading back via the same field
  (test* "native union modify via b, read b" #x11223344
         (begin
           (set! (native. (bc 0 u1) 'b) #x11223344)
           (native. (bc 0 u1) 'b)))

  ;; Writing to one field and reading via a different field
  ;; (all fields share offset 0, so writing b overwrites a and c's bytes too)
  (test* "native union modify via b, read a"
         (case (native-endian)
           [(big-endian) #x11]
           [else         #x44])
         (native. (bc 0 u1) 'a))

  (test* "native union modify via b, read c"
         (case (native-endian)
           [(big-endian) #x1122]
           [else         #x3344])
         (native. (bc 0 u1) 'c))

  (test* "native union modify u2" #x0123456789abcdef
         (begin
           (set! (native. (bc 0 u2) 'y) #x0123456789abcdef)
           (native. (bc 0 u2) 'y)))
  )

;; Distinguish c-struct and pointer to c-struct
(let* ([ts (make-c-struct-type 's1 `((a ,<c-char>) (b ,<int>)))]
       [tp (make-c-pointer-type ts)]
       [data (make-u8vector (~ ts'size))]
       [data-s (make-native-handle ts data)]
       [data-p (make-native-handle tp data)])
  (test* "native-ref via pointer" '(#\A 1234)
         (begin
           (set! (native. data-s 'a) #\A)
           (set! (native. data-s 'b) 1234)
           (list (native-> data-p 'a)
                 (native-> data-p 'b))))
  (test* "native-set! via pointer" '(#\B -5678)
         (begin
           (set! (native-> data-p 'a) #\B)
           (set! (native-> data-p 'b) -5678)
           (list (native. data-s 'a)
                 (native. data-s 'b))))
  )

;; Pointer in struct
(let* ([double* (make-c-pointer-type <double>)]
       [double** (make-c-pointer-type double*)]
       [s (make-c-struct-type #f `((a ,double*)))]
       [h (make-native-handle s)]
       [d (make-native-handle (make-c-pointer-type <double>))])
  (test* "c-pointer in a struct" #t
         (begin
           (set! (native. h 'a) (null-pointer-handle))
           (null-pointer-handle? (native. h 'a))))
  (test* "c-pointer in a struct dereference" 3.14
         (begin
           (set! (native* d) 3.14)
           (set! (native. h 'a) d)
           (native* (native. h 'a))))
  )

;; Nested struct
(let* ([inner (make-c-struct-type 'inner `((a ,<c-char>) (b ,<int8>)))]
       [outer (make-c-struct-type 'outer `((c ,inner)))]
       [h (make-native-handle outer)]
       [h2 (make-native-handle inner)])
  (set! (native. (native. h 'c) 'a) #\a)
  (set! (native. (native. h 'c) 'b) 99)
  (set! (native. h2 'a) #\b)
  (set! (native. h2 'b) 101)
  (test* "nested struct, setting substructure (before)" '(#\a 99)
         (let1 r (native. h 'c)
           (list (native. r 'a) (native. r 'b))))
  (test* "nested struct, setting substructure (after)" '(#\b 101)
         (begin
           (set! (native. h 'c) h2)
           (let1 r (native. h 'c)
             (list (native. r 'a) (native. r 'b)))))
  )

;; Nested struct, member address
(let* ([inner (make-c-struct-type 'inner `((a ,<c-char>) (b ,<int>)))]
       [inner* (make-c-pointer-type inner)]
       [outer (make-c-struct-type 'outer `((c ,inner) (d ,inner*)))]
       [h (make-native-handle outer)]
       [h2 (make-native-handle inner* (native-handle-owner h))])
  (test* "exteract inner struct" #t
         (c-struct-handle? (native. h 'c)))
  (test* "exteract inner struct pointer" #t
         (null-pointer-handle? (native. h 'd)))
  (test* "extract inner struct pointer set" '(#\@ 999)
         (begin
           (set! (native. h 'd) (native& h 'c))
           (set! (native. (native. h 'c) 'a) #\@)
           (set! (native. (native. h 'c) 'b) 999)
           (list (native-> (native. h 'd) 'a)
                 (native-> (native. h 'd) 'b))))
  (test* "dereferencing pointer to a struct" #t
         (c-struct-handle? (native* (native. h 'd))))
  )

;; Array of structs, element address
(let* ([point (make-c-struct-type 'point `((x ,<int>) (y ,<int>)))]
       [points (make-c-array-type point '(4))]
       [arr (make-native-handle points)])
  ;; Initialize: native-aref on an array of structs returns a struct handle.
  (dotimes [i 4]
    (let1 elt (native-aref arr i)
      (set! (native. elt 'x) (* i 10))
      (set! (native. elt 'y) (* i 100))))

  (test* "native& on array of structs returns c-pointer to element struct" #t
         (let1 p (native& arr 2)
           (and (c-pointer-handle? p)
                (equal? (~ (native-handle-type p) 'pointee-type) point))))

  (test* "native& on array of structs: deref then native. reads fields"
         '(20 200)
         (let1 s (native* (native& arr 2))
           (list (native. s 'x) (native. s 'y))))

  (test* "native& on array of structs: deref then native. writes back"
         '(-1 -2)
         (let1 s (native* (native& arr 1))
           (set! (native. s 'x) -1)
           (set! (native. s 'y) -2)
           (list (native. (native-aref arr 1) 'x)
                 (native. (native-aref arr 1) 'y)))))

;; Struct with an array field, member reference
(let* ([buf (make-c-array-type <uint16> '(4))]
       [frame (make-c-struct-type 'frame `((tag ,<int>) (data ,buf)))]
       [h (make-native-handle frame)])
  (set! (native. h 'tag) 42)
  (let1 a (native. h 'data)
    (set! (native-aref a 0) #x1111)
    (set! (native-aref a 1) #x2222)
    (set! (native-aref a 2) #x3333)
    (set! (native-aref a 3) #x4444))

  (test* "native& on struct array field returns c-pointer to array" #t
         (let1 p (native& h 'data)
           (and (c-pointer-handle? p)
                (equal? (~ (native-handle-type p) 'pointee-type) buf))))

  (test* "native& on struct array field: deref then native-aref reads"
         '(#x1111 #x2222 #x3333 #x4444)
         (let1 a (native* (native& h 'data))
           (list (native-aref a 0)
                 (native-aref a 1)
                 (native-aref a 2)
                 (native-aref a 3))))

  (test* "native& on struct array field: deref then native-aref writes back"
         '(#xaaaa #xbbbb)
         (let1 a (native* (native& h 'data))
           (set! (native-aref a 0) #xaaaa)
           (set! (native-aref a 3) #xbbbb)
           (list (native-aref (native. h 'data) 0)
                 (native-aref (native. h 'data) 3)))))

;; native tag registry
(parameterize ([current-native-tag-namespace
                (make-native-tag-namespace)])
  (let* ([t1 (make-c-struct-type 'foo `((x ,<int>)))]
         [t2 (make-c-union-type 'bar `((x ,<int>)))])
    (test* "tag->native-type" t1 (tag->native-type 'foo))
    (test* "tag->native-type" t2 (tag->native-type 'bar))

    (test* "dupe tags (different kind)" (test-error <error> #/already used/)
           (make-c-struct-type 'bar `((x ,<int>))))
    (test* "dupe tags (different fields)" (test-error <error> #/already used/)
           (make-c-union-type 'bar `((x ,<int8>))))
    (test* "dupe tags (equal allowed)" t2
           (make-c-union-type 'bar `((x ,<int>))))
    ))

;;;----------------------------------------------------------
(test-section "enums")

(define %implicit-enum-size (with-module gauche.typeutil implicit-enum-size))

;; predicate and accessors
(let ([color (make-c-enum-type 'Color #f '(red green blue))]
      [s (make-c-struct-type 's '())])
  (test* "c-enum-type? on enum" #t (c-enum-type? color))
  (test* "c-enum-type? on struct" #f (c-enum-type? s))
  (test* "c-enum-type? on native int" #f (c-enum-type? <int>))
  (test* "c-enum-type-tag" 'Color (c-enum-type-tag color))
  (test* "c-enum-type-enumerator-alist" '((red . 0) (green . 1) (blue . 2))
         (c-enum-type-enumerator-alist color))
  (test* "c-enum-type-tag rejects non-enum" (test-error <error>)
         (c-enum-type-tag <int>))
  (test* "enum maps to <integer>" #t (subtype? color <integer>))
  (test* "enum of-type?: in-range integer yes, out-of-range/symbol no"
         '(#t #f #f)
         (list (of-type? 2 color)        ; fits the unsigned underlying
               (of-type? -1 color)       ; negative, but underlying is unsigned
               (of-type? 'red color))))  ; not an integer

;; c-enum-value / c-enum-symbol: symbol <-> value lookup
(let ([color (make-c-enum-type 'Color #f '(red green blue))])
  (test* "c-enum-value" '(0 1 2)
         (map (cut c-enum-value color <>) '(red green blue)))
  (test* "c-enum-symbol" '(red green blue)
         (map (cut c-enum-symbol color <>) '(0 1 2)))
  (test* "c-enum-value invalid symbol"
         (test-error <error> #/Invalid enum symbol/)
         (c-enum-value color 'magenta))
  (test* "c-enum-symbol invalid value"
         (test-error <error> #/Invalid enum value/)
         (c-enum-symbol color 99))
  (test* "c-enum-value rejects non-enum" (test-error <error>)
         (c-enum-value <int> 'red))
  (test* "c-enum-symbol rejects non-enum" (test-error <error>)
         (c-enum-symbol <int> 0)))

;; with aliases (duplicate values), c-enum-symbol returns the first match
(let ([e (make-c-enum-type 'A #f '((x 0) (y 0) z))])
  (test* "c-enum-value of aliased symbols" '(0 0 1)
         (map (cut c-enum-value e <>) '(x y z)))
  (test* "c-enum-symbol on aliased value picks first key" 'x
         (c-enum-symbol e 0)))

;; equal? compares enum types structurally (tag, type-spec, enumerator-alist)
(test* "enum equal?: same tag/values" #t
       (equal? (make-c-enum-type 'C #f '(r g b))
               (make-c-enum-type 'C #f '(r g b))))
(test* "enum equal?: anonymous" #t
       (equal? (make-c-enum-type #f #f '(r g b))
               (make-c-enum-type #f #f '(r g b))))
(test* "enum equal?: same typespec" #t
       (equal? (make-c-enum-type 'C <int8> '(r g b))
               (make-c-enum-type 'C <int8> '(r g b))))
(test* "enum equal?: different tag" #f
       (equal? (make-c-enum-type 'C #f '(r g b))
               (make-c-enum-type 'D #f '(r g b))))
(test* "enum equal?: different values" #f
       (equal? (make-c-enum-type 'C #f '(r g b))
               (make-c-enum-type 'C #f '(r g (b 5)))))
(test* "enum equal?: typespec vs none" #f
       (equal? (make-c-enum-type 'C <int8> '(r g b))
               (make-c-enum-type 'C #f '(r g b))))
(test* "enum equal?: different typespec" #f
       (equal? (make-c-enum-type 'C <int8> '(r g b))
               (make-c-enum-type 'C <uint8> '(r g b))))
(test* "enum equal?: enum vs struct" #f
       (equal? (make-c-enum-type 'C #f '(r g b))
               (make-c-struct-type 'C '())))

;; flexible enumerator spec: bare symbol auto-increments, explicit value
;; resets the counter, duplicate values (aliases) are allowed.
(test* "enum auto-numbering / explicit / alias"
       '((a . 0) (b . 10) (c . 11) (d . 11) (e . 12))
       (c-enum-type-enumerator-alist
        (make-c-enum-type 'E #f '(a (b 10) c (d 11) e))))
(test* "enum first symbol without value is 0; negatives ok"
       '((lo . -2) (mid . -1) (hi . 0))
       (c-enum-type-enumerator-alist
        (make-c-enum-type 'N #f '((lo -2) mid hi))))

;; size / alignment / signedness derived from the value range
(define (enum-shape e)
  (list (~ e'size) (~ e'alignment) (~ e'unsigned?) (~ e'c-type-name)))
(test* "implicit small nonnegative -> unsigned"
       (list (%implicit-enum-size 8) (%implicit-enum-size 8) #t "enum A")
       (enum-shape (make-c-enum-type 'A #f '(x y z))))
(test* "implicit with a negative value -> signed"
       (list (%implicit-enum-size 8) (%implicit-enum-size 8) #f "enum B")
       (enum-shape (make-c-enum-type 'B #f '((p -1) q))))
(test* "implicit value needing 64 bits"
       (list (%implicit-enum-size 64) (%implicit-enum-size 64) #t "enum C")
       (enum-shape (make-c-enum-type 'C #f '((m 5000000000)))))

;; explicit typespec (C23 'enum tag : T')
(let ([e (make-c-enum-type 'T8 <int8> '(a (b -1) c))])
  (test* "typespec: size/alignment taken from typespec"
         (list (~ <int8>'size) (~ <int8>'alignment))
         (list (~ e'size) (~ e'alignment)))
  (test* "typespec: c-type-name" "enum T8 : int8_t" (~ e'c-type-name))
  (test* "typespec: type-spec slot" <int8> (~ e'type-spec))
  (test* "typespec: not unsigned" #f (~ e'unsigned?)))

;; anonymous enum: no tag, c-type-name falls back to the underlying int
(test* "anonymous enum: tag #f, c-type-name is the underlying int"
       '(#f #f)
       (let1 e (make-c-enum-type #f #f '((x 0) (y 300)))
         (list (c-enum-type-tag e)
               (and (#/enum/ (~ e'c-type-name)) #t))))

;; an enum is read/written through memory as its underlying integer
(let* ([data (u8vector-copy *fobject-storage*)]
       [e8  (make-c-enum-type 'E8 <int8> '(a))]
       [u8  (make-c-enum-type 'U8 <uint8> '(a))]
       [e8* (make-c-pointer-type e8)]
       [u8* (make-c-pointer-type u8)])
  (define (bc pos type) (make-native-handle type data pos))
  (test* "enum read (signed int8 underlying)"  #x-80 (native* (bc 0 e8*)))
  (test* "enum read (unsigned uint8 underlying)" #x80 (native* (bc 0 u8*)))
  (test* "enum write through memory" 42
         (begin (set! (native* (bc 0 e8*)) 42) (native* (bc 0 e8*)))))

;; error cases
(test* "enum duplicate id" (test-error <error> #/duplicate enumerator id/)
       (make-c-enum-type 'X #f '(a a)))
(test* "enum bad enumerator form" (test-error <error> #/bad c-enum enumerator/)
       (make-c-enum-type 'X #f '((a 1 2))))
(test* "enum value out of range for typespec" (test-error <error>)
       (make-c-enum-type 'X <int8> '((a 300))))
(test* "enum bad tag" (test-error <error> #/tag must be a symbol/)
       (make-c-enum-type "x" #f '(a)))

;;;----------------------------------------------------------
(test-section "native&")

;; native& on c-struct: extract pointer to a field
(let* ([s (make-c-struct-type 's `((a ,<int8>)
                                   (b ,<uint32>)
                                   (c ,<uint16>)
                                   (d ,<uint8>)))]
       [h (make-native-handle s)]
       [ph (native& h)])
  (dolist [h (list h ph)]
    (define nref (if (c-pointer-handle? h) native-> native.))

    (set! (nref h 'a) -1)
    (set! (nref h 'b) #x11223344)
    (set! (nref h 'c) #xabcd)
    (set! (nref h 'd) #xfe)

    (test* #"native& ~h returns c-pointer handle" #t
           (c-pointer-handle? (native& h 'a)))
    (test* #"native& ~h 'a" #t
           (equal? (~ (native-handle-type (native& h 'a)) 'pointee-type)
                   <int8>))
    (test* #"native& ~h 'b" #t
           (equal? (~ (native-handle-type (native& h 'b)) 'pointee-type)
                   <uint32>))
    (test* #"native& ~h 'c" #t
           (equal? (~ (native-handle-type (native& h 'c)) 'pointee-type)
                   <uint16>))

    (test* #"native& ~h deref reads field value"
           '(-1 #x11223344 #xabcd #xfe)
           (list (native* (native& h 'a))
                 (native* (native& h 'b))
                 (native* (native& h 'c))
                 (native* (native& h 'd))))

    (test* #"native& ~h write through pointer modifies field"
           '(127 #x55667788 #x1234 #x80)
           (begin
             (set! (native* (native& h 'a)) 127)
             (set! (native* (native& h 'b)) #x55667788)
             (set! (native* (native& h 'c)) #x1234)
             (set! (native* (native& h 'd)) #x80)
             (list (nref h 'a)
                   (nref h 'b)
                   (nref h 'c)
                   (nref h 'd))))

    (test* #"native& ~h error on non-symbol selector"
           (test-error <error>)
           (native& h 0))
    (test* #"native& ~h error on unknown field"
           (test-error <error>)
           (native& h 'zzz))))

;; native& on c-union: extract pointer to a field
(let* ([u (make-c-union-type 'u `((a ,<int8>)
                                  (b ,<uint32>)
                                  (c ,<uint16>)))]
       [h (make-native-handle u)])
  (test* "native& on union returns c-pointer handle" #t
         (c-pointer-handle? (native& h 'a)))
  (test* "native& on union field 'a pointee type" #t
         (equal? (~ (native-handle-type (native& h 'a)) 'pointee-type)
                 <int8>))
  (test* "native& on union field 'b pointee type" #t
         (equal? (~ (native-handle-type (native& h 'b)) 'pointee-type)
                 <uint32>))

  ;; All union pointers should point to the same storage
  (test* "native& on union: write via 'b then read via 'a and 'c"
         (case (native-endian)
           [(big-endian) (list #x11 #x1122)]
           [else         (list #x44 #x3344)])
         (begin
           (set! (native* (native& h 'b)) #x11223344)
           (list (native* (native& h 'a))
                 (native* (native& h 'c)))))

  (test* "native& on union: error on non-symbol selector"
         (test-error <error>)
         (native& h 1)))

;; native& on 1-D c-array: extract pointer to an element
(let* ([a (make-c-array-type <uint16> '(4))]
       [h (make-native-handle a)])
  (set! (native-aref h 0) #x1111)
  (set! (native-aref h 1) #x2222)
  (set! (native-aref h 2) #x3333)
  (set! (native-aref h 3) #x4444)

  (test* "native& on array returns c-pointer handle" #t
         (c-pointer-handle? (native& h 0)))
  (test* "native& on array pointee type" #t
         (equal? (~ (native-handle-type (native& h 0)) 'pointee-type)
                 <uint16>))

  (test* "native& on array: deref reads element value"
         '(#x1111 #x2222 #x3333 #x4444)
         (list (native* (native& h 0))
               (native* (native& h 1))
               (native* (native& h 2))
               (native* (native& h 3))))

  (test* "native& on array: list selector"
         '(#x1111 #x2222 #x3333 #x4444)
         (list (native* (native& h '(0)))
               (native* (native& h '(1)))
               (native* (native& h '(2)))
               (native* (native& h '(3)))))

  (test* "native& on array: write through pointer modifies element"
         '(#xaaaa #xbbbb #xcccc #xdddd)
         (begin
           (set! (native* (native& h 0)) #xaaaa)
           (set! (native* (native& h 1)) #xbbbb)
           (set! (native* (native& h 2)) #xcccc)
           (set! (native* (native& h 3)) #xdddd)
           (list (native-aref h 0)
                 (native-aref h 1)
                 (native-aref h 2)
                 (native-aref h 3))))

  ;; The pointer can be offset using native-aref
  (test* "native& on array: native-aref on returned pointer"
         '(#xaaaa #xcccc)
         (let1 p (native& h 0)
           (list (native-aref p 0)
                 (native-aref p 2))))

  (test* "native& on array: error on symbol selector"
         (test-error <error>)
         (native& h 'foo)))

;; native& on multi-dimensional c-array
(let* ([a (make-c-array-type <int32> '(2 3))]
       [h (make-native-handle a)])
  (set! (native-aref h 0 0)  10)
  (set! (native-aref h 0 1)  11)
  (set! (native-aref h 0 2)  12)
  (set! (native-aref h 1 0)  20)
  (set! (native-aref h 1 1)  21)
  (set! (native-aref h 1 2)  22)

  (test* "native& on 2D array: pointee type is element type" #t
         (equal? (~ (native-handle-type (native& h '(1 2))) 'pointee-type)
                 <int32>))

  (test* "native& on 2D array: deref each element"
         '(10 11 12 20 21 22)
         (list (native* (native& h '(0 0)))
               (native* (native& h '(0 1)))
               (native* (native& h '(0 2)))
               (native* (native& h '(1 0)))
               (native* (native& h '(1 1)))
               (native* (native& h '(1 2)))))

  (test* "native& on 2D array: write through pointer"
         99
         (begin
           (set! (native* (native& h '(1 1))) 99)
           (native-aref h 1 1))))

;; native& errors on inappropriate handle types
(let* ([int* (make-c-pointer-type <int>)]
       [ph (make-native-handle int*)])
  (test* "native& on c-pointer handle is an error"
         (test-error <error> #/native&/)
         (native& ph 0)))


;;;----------------------------------------------------------
(test-section "null pointers")

(test* "null pointer dereference" (test-error <error> #/NULL pointer/)
       (native* (null-pointer-handle (make-c-pointer-type <int>))))

(test* "null pointer constructor check" (test-error <error> #/Invalid type for/)
       (null-pointer-handle (native-type '(.struct (a::int b::int)))))

(test* "null pointer predicate" '(#t #t #f #f)
       (list
        (null-pointer-handle? (null-pointer-handle))
        (null-pointer-handle? (null-pointer-handle
                               (native-type '(.array char (16)))))
        (null-pointer-handle? (make-native-handle <void*>
                                                  (make-u8vector 16)))
        (null-pointer-handle? (make-native-handle
                               (native-type '(.array char (16)))
                               (make-u8vector 16)))))

;;;----------------------------------------------------------
(test-section "type size bounded flag")

;; Primitive types are always bounded
(test* "native-type bounded? int" #t (~ <int>'bounded?))
(test* "native-type bounded? int8" #t (~ <int8>'bounded?))
(test* "native-type bounded? double" #t (~ <double>'bounded?))

;; Arrays with all numeric dimensions are bounded
(let1 a (make-c-array-type <int8> '(5))
  (test* "native-type bounded? array(5)" #t (~ a'bounded?))
  (test* "native-type bounded? array(5) size" 5 (~ a'size)))

(let1 a (make-c-array-type <int8> '(3 4))
  (test* "native-type bounded? array(3 4)" #t (~ a'bounded?))
  (test* "native-type bounded? array(3 4) size" 12 (~ a'size)))

;; Array with * as first dimension is unbounded; size is 0 (minimum)
(let1 a (make-c-array-type <int8> '(*))
  (test* "native-type bounded? array(*)" #f (~ a'bounded?))
  (test* "native-type bounded? array(*) size" 0 (~ a'size)))

(let1 a (make-c-array-type <int8> '(* 4))
  (test* "native-type bounded? array(* 4)" #f (~ a'bounded?))
  (test* "native-type bounded? array(* 4) size" 0 (~ a'size)))

;; Struct with all bounded fields is bounded
(let1 s (make-c-struct-type 'bs `((a ,<int8>) (b ,<int8>)))
  (test* "native-type bounded? struct bounded" #t (~ s'bounded?)))

;; Struct with unbounded array at the end is unbounded;
;; minimum size covers only the bounded prefix
(let* ([tail (make-c-array-type <int8> '(*))]
       [s (make-c-struct-type 'ubs `((a ,<int8>) (b ,tail)))])
  (test* "native-type bounded? struct with unbounded tail" #f (~ s'bounded?))
  (test* "native-type bounded? struct with unbounded tail size" 1 (~ s'size)))

;; Struct with multi-element bounded prefix before unbounded array
(let* ([tail (make-c-array-type <int8> '(*))]
       [s (make-c-struct-type 'ubs2 `((n ,<int8>) (pad ,<int8>) (data ,tail)))])
  (test* "native-type bounded? struct unbounded size is prefix only" #f (~ s'bounded?))
  (test* "native-type bounded? struct unbounded prefix size" 2 (~ s'size)))

;; Struct cannot have unbounded field except at the end
(test* "native-type unbounded struct field not at end" (test-error)
       (let1 mid (make-c-array-type <int8> '(*))
         (make-c-struct-type 'bad `((a ,mid) (b ,<int8>)))))

;; Union with all bounded fields is bounded
(let1 u (make-c-union-type 'bu `((a ,<int8>) (b ,<int8>)))
  (test* "native-type bounded? union bounded" #t (~ u'bounded?)))

;; Union with any unbounded field is unbounded;
;; minimum size comes from the largest bounded field
(let* ([arr (make-c-array-type <int8> '(*))]
       [u (make-c-union-type 'ubu `((a ,<int8>) (b ,arr)))])
  (test* "native-type bounded? union with unbounded field" #f (~ u'bounded?))
  (test* "native-type bounded? union with unbounded field size" 1 (~ u'size)))

;; Union where all fields are unbounded has size 0
(let* ([arr (make-c-array-type <int8> '(*))]
       [u (make-c-union-type 'ubu2 `((a ,arr) (b ,arr)))])
  (test* "native-type bounded? union all unbounded" #f (~ u'bounded?))
  (test* "native-type bounded? union all unbounded size" 0 (~ u'size)))

;; Struct whose last field is an unbounded struct is itself unbounded
(let* ([inner-arr (make-c-array-type <int8> '(*))]
       [inner (make-c-struct-type 'inner `((x ,<int8>) (buf ,inner-arr)))]
       [outer (make-c-struct-type 'outer `((n ,<int8>) (rest ,inner)))])
  (test* "native-type bounded? nested unbounded struct" #f (~ outer'bounded?)))

;;;----------------------------------------------------------
(test-section "handle type predicates and equivalence")

(let* ([data  (u8vector-copy *fobject-storage*)]
       [int*  (make-c-pointer-type <int>)]
       [int8a (make-c-array-type <int8> '(4))]
       [s1    (make-c-struct-type 's1 `((a ,<int8>) (b ,<uint32>)))]
       [u1    (make-c-union-type  'u1 `((a ,<int8>) (b ,<uint32>)))]
       [fn1   (make-c-function-type <int> `(,<int>))]
       [ph    (make-native-handle int* data)]
       [ah    (make-native-handle int8a data)]
       [sh    (make-native-handle s1 data)]
       [uh    (make-native-handle u1 data)]
       [fh    (null-pointer-handle fn1)])

  ;; c-pointer-handle?
  (test* "c-pointer-handle? on pointer"    #t (c-pointer-handle? ph))
  (test* "c-pointer-handle? on array"      #f (c-pointer-handle? ah))
  (test* "c-pointer-handle? on struct"     #f (c-pointer-handle? sh))
  (test* "c-pointer-handle? on non-handle" #f (c-pointer-handle? 42))

  ;; c-array-handle?
  (test* "c-array-handle? on array"        #t (c-array-handle? ah))
  (test* "c-array-handle? on pointer"      #f (c-array-handle? ph))
  (test* "c-array-handle? on struct"       #f (c-array-handle? sh))

  ;; c-struct-handle?
  (test* "c-struct-handle? on struct"      #t (c-struct-handle? sh))
  (test* "c-struct-handle? on pointer"     #f (c-struct-handle? ph))
  (test* "c-struct-handle? on union"       #f (c-struct-handle? uh))

  ;; c-union-handle?
  (test* "c-union-handle? on union"        #t (c-union-handle? uh))
  (test* "c-union-handle? on struct"       #f (c-union-handle? sh))
  (test* "c-union-handle? on pointer"      #f (c-union-handle? ph))

  ;; c-function-handle?
  (test* "c-function-handle? on function"  #t (c-function-handle? fh))
  (test* "c-function-handle? on pointer"   #f (c-function-handle? ph))
  (test* "c-function-handle? on array"     #f (c-function-handle? ah))
  (test* "c-function-handle? on non-handle" #f (c-function-handle? "foo"))

  ;; c-aggregate-handle?
  (test* "c-aggregate-handle? on array"    #t (c-aggregate-handle? ah))
  (test* "c-aggregate-handle? on struct"   #t (c-aggregate-handle? sh))
  (test* "c-aggregate-handle? on union"    #t (c-aggregate-handle? uh))
  (test* "c-aggregate-handle? on pointer"  #f (c-aggregate-handle? ph))
  (test* "c-aggregate-handle? on function" #f (c-aggregate-handle? fh))

  ;; c-pointer-like-handle?
  (test* "c-pointer-like-handle? on pointer"  #t (c-pointer-like-handle? ph))
  (test* "c-pointer-like-handle? on array"    #t (c-pointer-like-handle? ah))
  (test* "c-pointer-like-handle? on function" #t (c-pointer-like-handle? fh))
  (test* "c-pointer-like-handle? on struct"   #f (c-pointer-like-handle? sh))
  (test* "c-pointer-like-handle? on union"    #f (c-pointer-like-handle? uh))

  ;; equal? on pointer handles: same underlying address => equal
  (test* "equal? pointer same address" #t
         (equal? ph (make-native-handle int* data)))
  ;; equal? on pointer handles: different storage => not equal
  (test* "equal? pointer different address" #f
         (equal? ph (make-native-handle int* (u8vector-copy data))))
  ;; equal? across handle kinds => always #f
  (test* "equal? pointer vs array"    #f (equal? ph ah))
  (test* "equal? pointer vs struct"   #f (equal? ph sh))
  (test* "equal? pointer vs function" #f (equal? ph fh))

  ;; equal? on function handles: both null => same address => equal
  (test* "equal? function same address" #t
         (equal? fh (null-pointer-handle fn1)))

  ;; equal? on array handles: same content => equal
  (test* "equal? array same content" #t
         (equal? ah (make-native-handle int8a (u8vector-copy data))))
  ;; equal? on array handles: different content => not equal
  (test* "equal? array different content" #f
         (equal? ah (make-native-handle
                     int8a (make-u8vector (u8vector-length data) 0))))

  ;; equal? on struct handles: same content => equal
  (test* "equal? struct same content" #t
         (equal? sh (make-native-handle s1 (u8vector-copy data))))
  ;; equal? on struct handles: different content => not equal
  (test* "equal? struct different content" #f
         (equal? sh (make-native-handle
                     s1 (make-u8vector (u8vector-length data) 0))))
  ;; equal? on union handles: same content => equal
  (test* "equal? union same content" #t
         (equal? uh (make-native-handle u1 (u8vector-copy data))))
  ;; equal? struct vs union => #f
  (test* "equal? struct vs union" #f (equal? sh uh))
  )

;; equal? on null pointer handles
(test* "equal? two null pointer handles" #t
       (equal? (null-pointer-handle) (null-pointer-handle)))
(test* "equal? null vs non-null pointer" #f
       (equal? (null-pointer-handle)
               (make-native-handle <void*> (make-u8vector 8))))

(let* ([int*  (make-c-pointer-type <int>)]
       [int8a (make-c-array-type <int8> '(4))]
       [s1    (make-c-struct-type 's1 `((a ,<int8>) (b ,<uint32>)))]
       [u1    (make-c-union-type  'u1 `((a ,<int8>) (b ,<uint32>)))]
       [fn1   (make-c-function-type <int> `(,<int>))])

  (test* "c-aggregate-type? array"    #t (c-aggregate-type? int8a))
  (test* "c-aggregate-type? struct"   #t (c-aggregate-type? s1))
  (test* "c-aggregate-type? union"    #t (c-aggregate-type? u1))
  (test* "c-aggregate-type? pointer"  #f (c-aggregate-type? int*))
  (test* "c-aggregate-type? function" #f (c-aggregate-type? fn1))
  (test* "c-aggregate-type? <int>"    #f (c-aggregate-type? <int>))

  (test* "c-pointer-like-type? pointer"  #t (c-pointer-like-type? int*))
  (test* "c-pointer-like-type? array"    #t (c-pointer-like-type? int8a))
  (test* "c-pointer-like-type? function" #t (c-pointer-like-type? fn1))
  (test* "c-pointer-like-type? struct"   #f (c-pointer-like-type? s1))
  (test* "c-pointer-like-type? union"    #f (c-pointer-like-type? u1))
  (test* "c-pointer-like-type? <int>"    #f (c-pointer-like-type? <int>)))

;;;----------------------------------------------------------
(test-section "casting")

(let* ([data   (u8vector-copy *fobject-storage*)]
       [int*   (make-c-pointer-type <int>)]
       [uint8* (make-c-pointer-type <uint8>)]
       [int8*  (make-c-pointer-type <int8>)]
       [int8a  (make-c-array-type <int8> '(4))]
       [s1     (make-c-struct-type 's1 `((a ,<int8>) (b ,<uint32>)))]
       [s1*    (make-c-pointer-type
                (make-c-struct-type 's1 `((a ,<int8>) (b ,<uint32>))))]
       [ph     (make-native-handle int* data)])

  ;; Basic cast: reinterpret int* as uint8*, same underlying address
  (test* "cast-handle int* to uint8* yields pointer handle" #t
         (c-pointer-handle? (cast-handle uint8* ph)))
  (test* "cast-handle int* to uint8* reads first byte" #x80
         (native* (cast-handle uint8* ph)))

  ;; Cast to int8* reads signed
  (test* "cast-handle int* to int8* reads signed first byte" #x-80
         (native* (cast-handle int8* ph)))

  ;; Cast preserves address (pointer compare)
  (test* "cast-handle preserves address" #t
         (c-pointer=? (cast-handle uint8* ph)
                      (cast-handle uint8* ph)))

  ;; Cast with byte offset: offset 1 reads second byte of data
  (test* "cast-handle with offset 1" #x01
         (native* (cast-handle uint8* ph 1)))

  ;; Cast with offset 8
  (test* "cast-handle with offset 8" #x08
         (native* (cast-handle uint8* ph 8)))

  ;; Cast pointer handle to array type
  (test* "cast-handle pointer to array type yields array handle" #t
         (c-array-handle? (cast-handle int8a ph)))

  ;; Cast array handle to pointer type
  (test* "cast-handle array to pointer type yields pointer handle" #t
         (let1 ah (make-native-handle int8a data)
           (c-pointer-handle? (cast-handle int8* ah))))

  ;; Cast array handle with offset then dereference
  (test* "cast-handle array to uint8* with offset 4" #x04
         (let1 ah (make-native-handle int8a data)
           (native* (cast-handle uint8* ah 4))))

  ;; Cast null pointer handle to different pointer type
  (test* "cast-handle null pointer to int8*" #t
         (null-pointer-handle? (cast-handle int8* (null-pointer-handle int*))))

  ;; Error: target type is not pointer-like (struct)
  (test* "cast-handle to struct type errors" (test-error <error> #/pointer-like/)
         (cast-handle s1 ph))

  ;; Error: source handle is not pointer-like (struct handle)
  (test* "cast-handle from struct handle errors" (test-error <error> #/pointer-like/)
         (let1 sh (make-native-handle s1 data)
           (cast-handle uint8* sh)))

  ;; Cast int* to pointer-to-struct, then dereference fields via native->
  (let1 psh (cast-handle s1* ph)
    (test* "cast-handle int* to s1* yields pointer handle" #t
           (c-pointer-handle? psh))
    (test* "cast-handle int* to s1*, deref field a" #x-80
           (native-> psh 'a))
    (test* "cast-handle int* to s1*, deref field b"
           (case (native-endian)
             [(big-endian) #x04050607]
             [else         #x07060504])
           (native-> psh 'b))
    ;; Modify through the cast pointer and read back via the original data
    (test* "cast-handle int* to s1*, set! field a"
           #x42
           (begin
             (set! (native-> psh 'a) #x42)
             (native* (cast-handle uint8* ph)))))

  ;; Aggregate-to-aggregate casts
  (let* ([s2     (make-c-struct-type 's2 `((x ,<int8>) (y ,<int8>)
                                           (z ,<int8>) (w ,<int8>)))]
         [u1     (make-c-union-type 'u1 `((i ,<int32>) (b ,<int8>)))]
         [ah     (make-native-handle int8a data)]
         [sh     (make-native-handle s1 data)])
    (test* "cast-handle array to struct (aggregate->aggregate)" #t
           (c-struct-handle? (cast-handle s2 ah)))
    (test* "cast-handle struct to union (aggregate->aggregate)" #t
           (c-union-handle? (cast-handle u1 sh)))
    (test* "cast-handle struct to array (aggregate->aggregate)" #t
           (c-array-handle? (cast-handle int8a sh))))

  ;; Region size check: dest type larger than region must error.
  (let* ([tiny   (make-u8vector 2 0)]
         [int8a8 (make-c-array-type <int8> '(8))]
         [tah    (make-native-handle (make-c-array-type <int8> '(2)) tiny)])
    (test* "cast-handle errors when region too small for dest aggregate"
           (test-error <error> #/does not fit/)
           (cast-handle int8a8 tah))
    ;; Casting to a c-pointer skips the region check.
    (test* "cast-handle to c-pointer skips region check" #t
           (c-pointer-handle? (cast-handle int8* tah)))))

;;;----------------------------------------------------------
(test-section "copy-handle-memory!")

;; Basic copy between handles of the same type.  Without size, the entire
;; struct is copied.
(let* ([s1      (make-c-struct-type 's1 `((a ,<int32>) (b ,<int32>)))]
       [src-h   (make-native-handle s1)]
       [dst-h   (make-native-handle s1)])
  (set! (native. src-h 'a) #x12345678)
  (set! (native. src-h 'b) #x55aa55aa)
  (copy-handle-memory! dst-h src-h)
  (test* "copy-handle-memory! whole struct: field a"
         #x12345678 (native. dst-h 'a))
  (test* "copy-handle-memory! whole struct: field b"
         #x55aa55aa (native. dst-h 'b))
  (test* "copy-handle-memory! produces memwise-equal result" 0
         (c-memwise-compare dst-h src-h)))

;; Nested struct: copy only the inner struct out of an outer struct.
;; Without an explicit size, the size is derived from the
;; inner struct's type.
(let* ([inner   (make-c-struct-type 'inner `((x ,<uint32>) (y ,<uint32>)))]
       [outer   (make-c-struct-type 'outer `((a ,<uint32>) (mid ,inner)
                                             (b ,<uint32>)))]
       [outer-h   (make-native-handle outer)])
  (set! (native. outer-h 'a) #x11111111)
  (let1 mid-h (native. outer-h 'mid)
    (set! (native. mid-h 'x) #xaabbccdd)
    (set! (native. mid-h 'y) #x77665544))
  (set! (native. outer-h 'b) #x22222222)

  (let* ([trailing 4]
         [dst-buf (make-u8vector (+ (~ inner'size) trailing) #xff)]
         [dst-h   (make-native-handle inner dst-buf)]
         [src-h   (native. outer-h 'mid)])
    (copy-handle-memory! dst-h src-h)
    (test* "copy-handle-memory! inner struct (no size): field x"
           #xaabbccdd (native. dst-h 'x))
    (test* "copy-handle-memory! inner struct (no size): field y"
           #x77665544 (native. dst-h 'y))
    (test* "copy-handle-memory! inner struct (no size): no overrun"
           (make-u8vector trailing #xff)
           (uvector-alias <u8vector> dst-buf (~ inner'size))))

  ;; Same copy with explicit size matching inner struct size: works.
  (let* ([dst-h   (make-native-handle inner)]
         [src-h   (native. outer-h 'mid)])
    (copy-handle-memory! dst-h src-h (~ inner'size))
    (test* "copy-handle-memory! inner struct (size = inner.size): field x"
           #xaabbccdd (native. dst-h 'x))
    (test* "copy-handle-memory! inner struct (size = inner.size): field y"
           #x77665544 (native. dst-h 'y)))

  ;; Size larger than destination is error
  (let* ([dst-h   (make-native-handle inner)]
         [src-h   (native. outer-h 'mid)])
    (test* "copy-handle-memory! errors when size > inner struct size"
           (test-error <error> #/out of range/)
           (copy-handle-memory! dst-h src-h (+ (~ inner'size) 1)))))

;; offset parameter: copy starting partway into the source.  With no size,
;; the derived size is src.type.size - offset.
(let* ([s1      (make-c-struct-type 's1 `((a ,<int32>) (b ,<int32>)))]
       [b-off   (c-struct/union-type-field-offset s1 'b)]
       [i32*    (make-c-pointer-type <int32>)]
       [src-h   (make-native-handle s1)]
       [dst-h   (make-native-handle i32*)])
  (set! (native. src-h 'a) #x12345678)
  (set! (native. src-h 'b) #x5a5a5a5a)
  (copy-handle-memory! dst-h src-h #f b-off)
  (test* "copy-handle-memory! with offset (no size): copies tail of source"
         #x5a5a5a5a (native* dst-h)))

;; Argument validation.
(let* ([s1   (make-c-struct-type 's1 `((a ,<int32>)))]
       [h    (make-native-handle s1)])
  (test* "copy-handle-memory! errors on negative size"
         (test-error <error> #/size must be a nonnegative/)
         (copy-handle-memory! h h -1))
  (test* "copy-handle-memory! errors on negative offset"
         (test-error <error> #/offset must be a nonnegative/)
         (copy-handle-memory! h h #f -1)))

;; NULL handle errors.
(let* ([s1    (make-c-struct-type 's1 `((a ,<int32>)))]
       [s1*   (make-c-pointer-type s1)]
       [h     (make-native-handle s1)]
       [nullh (null-pointer-handle s1*)])
  (test* "copy-handle-memory! errors on NULL src"
         (test-error <error> #/NULL/)
         (copy-handle-memory! h nullh))
  (test* "copy-handle-memory! errors on NULL dst"
         (test-error <error> #/NULL/)
         (copy-handle-memory! nullh h)))

;;;----------------------------------------------------------
(test-section "pointer comparison")

(define (sign x) (cond [(< x 0) -1] [(> x 0) 1] [else 0]))

(let* ([data  (make-u8vector 32 0)]
       [int*  (make-c-pointer-type <int>)]
       [int8a (make-c-array-type <int8> '(4))]
       [fn1   (make-c-function-type <int> `(,<int>))]
       [p0    (make-native-handle int* data 0)]   ; offset 0
       [p0b   (make-native-handle int* data 0)]   ; same address as p0
       [p8    (make-native-handle int* data 8)]   ; offset 8, known to be > p0
       [a0    (make-native-handle int8a data 0)]
       [a8    (make-native-handle int8a data 8)]
       [fnull (null-pointer-handle fn1)])

  ;; c-pointer-compare
  (test* "c-pointer-compare same address"   0  (c-pointer-compare p0 p0b))
  (test* "c-pointer-compare lower < higher" -1 (c-pointer-compare p0 p8))
  (test* "c-pointer-compare higher > lower"  1 (c-pointer-compare p8 p0))
  ;; array handles are also valid
  (test* "c-pointer-compare array same address" 0  (c-pointer-compare a0 (make-native-handle int8a data 0)))
  (test* "c-pointer-compare array lower < higher" -1 (c-pointer-compare a0 a8))
  ;; null function pointer
  (test* "c-pointer-compare null function handles" 0
         (c-pointer-compare fnull (null-pointer-handle fn1)))
  ;; error on non-pointer-like handle
  (let ([sh (make-native-handle (make-c-struct-type 'ps `((x ,<int>))) data)])
    (test* "c-pointer-compare error on struct" (test-error)
           (c-pointer-compare sh sh)))

  ;; c-pointer=?
  (test* "c-pointer=? same address"      #t (c-pointer=? p0 p0b))
  (test* "c-pointer=? different address" #f (c-pointer=? p0 p8))

  ;; c-pointer<? / c-pointer<=? / c-pointer>? / c-pointer>=?
  (test* "c-pointer<? lower < higher"   #t (c-pointer<? p0 p8))
  (test* "c-pointer<? higher < lower"   #f (c-pointer<? p8 p0))
  (test* "c-pointer<? same"             #f (c-pointer<? p0 p0b))
  (test* "c-pointer<=? lower <= higher" #t (c-pointer<=? p0 p8))
  (test* "c-pointer<=? same"            #t (c-pointer<=? p0 p0b))
  (test* "c-pointer<=? higher <= lower" #f (c-pointer<=? p8 p0))
  (test* "c-pointer>? higher > lower"   #t (c-pointer>? p8 p0))
  (test* "c-pointer>? lower > higher"   #f (c-pointer>? p0 p8))
  (test* "c-pointer>? same"             #f (c-pointer>? p0 p0b))
  (test* "c-pointer>=? higher >= lower" #t (c-pointer>=? p8 p0))
  (test* "c-pointer>=? same"            #t (c-pointer>=? p0 p0b))
  (test* "c-pointer>=? lower >= higher" #f (c-pointer>=? p0 p8)))

;;;----------------------------------------------------------
(test-section "memwise comparison")

(let* ([s1   (make-c-struct-type 's1 `((a ,<int8>) (b ,<uint32>)))]
       [u1   (make-c-union-type  'u1 `((a ,<int8>) (b ,<uint32>)))]
       [int8a (make-c-array-type <int8> '(5))]
       [same  (make-u8vector 8 42)]
       [other (make-u8vector 8  0)]
       [sa  (make-native-handle s1  same)]
       [sa2 (make-native-handle s1 (u8vector-copy same))]
       [sb  (make-native-handle s1 other)]
       [ua  (make-native-handle u1  same)]
       [aa  (make-native-handle int8a (make-u8vector 5 7))]
       [aa2 (make-native-handle int8a (make-u8vector 5 7))]
       [ab  (make-native-handle int8a (make-u8vector 5 0))])

  (test* "c-memwise-compare struct same content"      0  (c-memwise-compare sa sa2))
  (test* "c-memwise-compare struct different content" -1 (sign (c-memwise-compare sb sa)))
  (test* "c-memwise-compare struct antisymmetric"
         (- (sign (c-memwise-compare sa sb)))
         (sign (c-memwise-compare sb sa)))
  (test* "c-memwise-compare union same content"  0 (c-memwise-compare ua (make-native-handle u1 (u8vector-copy same))))
  (test* "c-memwise-compare array same content"  0 (c-memwise-compare aa aa2))
  (test* "c-memwise-compare array different content" -1 (sign (c-memwise-compare ab aa)))
  ;; error on pointer handle
  (let ([ph (make-native-handle (make-c-pointer-type <int>) same)])
    (test* "c-memwise-compare error on pointer" (test-error)
           (c-memwise-compare ph ph))))

;;;----------------------------------------------------------
(test-section "endian specified types")

;; Type binding: all exported endian variants must be bound and be native types.
(for-each (^[t]
            (test* #"~|t| is a native-type" #t (is-a? t <native-type>)))
          (list <int16-le> <int16-be> <uint16-le> <uint16-be>
                <int32-le> <int32-be> <uint32-le> <uint32-be>
                <int64-le> <int64-be> <uint64-le> <uint64-be>
                <float-le> <float-be> <double-le> <double-be>))

;; Read tests.
;; *fobject-storage* has a predictable byte sequence; we compute the expected
;; BE/LE value from that byte sequence directly, so results are platform-independent.
;;
;;   offset  8: #x08 #x09 #x0a #x0b ...
;;   offset 16: #x10 #x11 #x12 #x13 ...
;;   offset 32: #x20 #x21 #x22 #x23 #x24 #x25 #x26 #x27 ...
;;   offset 64: #xf0 #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 #xf7
;;   offset 72: #xf8 #xf9 ...
(let ([data      (u8vector-copy *fobject-storage*)]
      [int16be*  (make-c-pointer-type <int16-be>)]
      [int16le*  (make-c-pointer-type <int16-le>)]
      [uint16be* (make-c-pointer-type <uint16-be>)]
      [uint16le* (make-c-pointer-type <uint16-le>)]
      [int32be*  (make-c-pointer-type <int32-be>)]
      [int32le*  (make-c-pointer-type <int32-le>)]
      [uint32be* (make-c-pointer-type <uint32-be>)]
      [uint32le* (make-c-pointer-type <uint32-le>)]
      [int64be*  (make-c-pointer-type <int64-be>)]
      [int64le*  (make-c-pointer-type <int64-le>)]
      [uint64be* (make-c-pointer-type <uint64-be>)]
      [uint64le* (make-c-pointer-type <uint64-le>)])
  (define (bc pos type) (make-native-handle type data pos))

  ;; 16-bit dereference at offset 8 (bytes #x08 #x09)
  (test* "int16be deref"  #x0809 (native* (bc 8 int16be*)))
  (test* "int16le deref"  #x0908 (native* (bc 8 int16le*)))
  (test* "uint16be deref" #x0809 (native* (bc 8 uint16be*)))
  (test* "uint16le deref" #x0908 (native* (bc 8 uint16le*)))

  ;; 16-bit at offset 72 (bytes #xf8 #xf9) — signed variants go negative
  ;; int16be: -(0x10000 - 0xf8f9) = -0x0707
  ;; int16le: -(0x10000 - 0xf9f8) = -0x0608
  (test* "int16be deref negative"  #x-0707 (native* (bc 72 int16be*)))
  (test* "int16le deref negative"  #x-0608 (native* (bc 72 int16le*)))
  (test* "uint16be deref unsigned" #xf8f9  (native* (bc 72 uint16be*)))
  (test* "uint16le deref unsigned" #xf9f8  (native* (bc 72 uint16le*)))

  ;; 16-bit array-ref: element 1 from offset 8 lands on bytes #x0a #x0b (offset 10)
  (test* "int16be aref[1]"  #x0a0b (native-aref (bc 8 int16be*) 1))
  (test* "int16le aref[1]"  #x0b0a (native-aref (bc 8 int16le*) 1))
  (test* "uint16be aref[1]" #x0a0b (native-aref (bc 8 uint16be*) 1))
  (test* "uint16le aref[1]" #x0b0a (native-aref (bc 8 uint16le*) 1))

  ;; 32-bit at offset 16 (bytes #x10 #x11 #x12 #x13) — positive
  (test* "int32be deref"  #x10111213 (native* (bc 16 int32be*)))
  (test* "int32le deref"  #x13121110 (native* (bc 16 int32le*)))
  (test* "uint32be deref" #x10111213 (native* (bc 16 uint32be*)))
  (test* "uint32le deref" #x13121110 (native* (bc 16 uint32le*)))

  ;; 32-bit at offset 64 (bytes #xf0 #xf1 #xf2 #xf3) — signed variants go negative
  ;; int32be: -(0x100000000 - 0xf0f1f2f3) = -0x0f0e0d0d
  ;; int32le: -(0x100000000 - 0xf3f2f1f0) = -0x0c0d0e10
  (test* "int32be deref negative"  #x-0f0e0d0d (native* (bc 64 int32be*)))
  (test* "int32le deref negative"  #x-0c0d0e10 (native* (bc 64 int32le*)))
  (test* "uint32be deref unsigned" #xf0f1f2f3  (native* (bc 64 uint32be*)))
  (test* "uint32le deref unsigned" #xf3f2f1f0  (native* (bc 64 uint32le*)))

  ;; 32-bit array-ref: element 1 from offset 16 lands on bytes #x14..#x17 (offset 20)
  (test* "int32be aref[1]" #x14151617 (native-aref (bc 16 int32be*) 1))
  (test* "int32le aref[1]" #x17161514 (native-aref (bc 16 int32le*) 1))

  ;; 64-bit at offset 32 (bytes #x20..#x27) — positive
  (test* "uint64be deref" #x2021222324252627 (native* (bc 32 uint64be*)))
  (test* "uint64le deref" #x2726252423222120 (native* (bc 32 uint64le*)))

  ;; 64-bit at offset 64 (bytes #xf0..#xf7) — signed variants go negative
  ;; int64be: -(0x10000000000000000 - 0xf0f1f2f3f4f5f6f7) = -0x0f0e0d0c0b0a0909
  ;; int64le: -(0x10000000000000000 - 0xf7f6f5f4f3f2f1f0) = -0x08090a0b0c0d0e10
  (test* "int64be deref negative"  #x-0f0e0d0c0b0a0909 (native* (bc 64 int64be*)))
  (test* "int64le deref negative"  #x-08090a0b0c0d0e10 (native* (bc 64 int64le*)))
  (test* "uint64be deref unsigned" #xf0f1f2f3f4f5f6f7 (native* (bc 64 uint64be*)))
  (test* "uint64le deref unsigned" #xf7f6f5f4f3f2f1f0 (native* (bc 64 uint64le*)))

  ;; 64-bit array-ref: element 1 from offset 32 lands on bytes #x28..#x2f (offset 40)
  (test* "uint64be aref[1]" #x28292a2b2c2d2e2f (native-aref (bc 32 uint64be*) 1))
  (test* "uint64le aref[1]" #x2f2e2d2c2b2a2928 (native-aref (bc 32 uint64le*) 1)))

;; Write tests: write through one endian pointer and read back through the opposite.
;; The byte-swapped read-back value confirms the bytes were stored in the right order.
(let ([data      (make-u8vector 16 0)]
      [int16be*  (make-c-pointer-type <int16-be>)]
      [int16le*  (make-c-pointer-type <int16-le>)]
      [uint16be* (make-c-pointer-type <uint16-be>)]
      [uint16le* (make-c-pointer-type <uint16-le>)]
      [int32be*  (make-c-pointer-type <int32-be>)]
      [int32le*  (make-c-pointer-type <int32-le>)]
      [int64be*  (make-c-pointer-type <int64-be>)]
      [int64le*  (make-c-pointer-type <int64-le>)])
  (define (bc pos type) (make-native-handle type data pos))

  ;; 16-bit BE round-trip, then cross-endian read
  (test* "int16be write/read round-trip" #x1234
         (begin (set! (native* (bc 0 int16be*)) #x1234)
                (native* (bc 0 int16be*))))
  (test* "int16be write, int16le read byte-swapped" #x3412
         (native* (bc 0 int16le*)))

  ;; 16-bit LE round-trip, then cross-endian read
  (test* "uint16le write/read round-trip" #xcd12
         (begin (set! (native* (bc 0 uint16le*)) #xcd12)
                (native* (bc 0 uint16le*))))
  (test* "uint16le write, uint16be read byte-swapped" #x12cd
         (native* (bc 0 uint16be*)))

  ;; 32-bit BE round-trip, then cross-endian read
  (test* "int32be write/read round-trip" #x12345678
         (begin (set! (native* (bc 0 int32be*)) #x12345678)
                (native* (bc 0 int32be*))))
  (test* "int32be write, int32le read byte-swapped" #x78563412
         (native* (bc 0 int32le*)))

  ;; 64-bit BE round-trip, then cross-endian read
  (test* "int64be write/read round-trip" #x0102030405060708
         (begin (set! (native* (bc 0 int64be*)) #x0102030405060708)
                (native* (bc 0 int64be*))))
  (test* "int64be write, int64le read byte-swapped" #x0807060504030201
         (native* (bc 0 int64le*))))

;; Float/double round-trip tests.
;; Use values exactly representable in the target type so round-trip is exact.
(let ([data      (make-u8vector 16 0)]
      [floatbe*  (make-c-pointer-type <float-be>)]
      [floatle*  (make-c-pointer-type <float-le>)]
      [doublebe* (make-c-pointer-type <double-be>)]
      [doublele* (make-c-pointer-type <double-le>)])
  (define (bc pos type) (make-native-handle type data pos))

  (test* "floatbe write/read round-trip" 1.5
         (begin (set! (native* (bc 0 floatbe*)) 1.5)
                (native* (bc 0 floatbe*))))
  (test* "floatle write/read round-trip" -2.5
         (begin (set! (native* (bc 0 floatle*)) -2.5)
                (native* (bc 0 floatle*))))
  ;; Writing as BE stores different bytes than reading as LE interprets
  (test* "floatbe and floatle interpret bytes differently" #f
         (begin (set! (native* (bc 0 floatbe*)) 1.5)
                (= (native* (bc 0 floatbe*))
                   (native* (bc 0 floatle*)))))

  (test* "doublebe write/read round-trip" 1.5
         (begin (set! (native* (bc 0 doublebe*)) 1.5)
                (native* (bc 0 doublebe*))))
  (test* "doublele write/read round-trip" -2.5
         (begin (set! (native* (bc 0 doublele*)) -2.5)
                (native* (bc 0 doublele*))))
  (test* "doublebe and doublele interpret bytes differently" #f
         (begin (set! (native* (bc 0 doublebe*)) 1.5)
                (= (native* (bc 0 doublebe*))
                   (native* (bc 0 doublele*))))))

;;;----------------------------------------------------------
(test-section "type signature parser")

;; Primitive types via C-style names

(test* "native-type int" <int> (native-type 'int))
(test* "native-type uint" <uint> (native-type 'u_int))
(test* "native-type short" <short> (native-type 'short))
(test* "native-type u_short" <ushort> (native-type 'u_short))
(test* "native-type long" <long> (native-type 'long))
(test* "native-type u_long" <ulong> (native-type 'u_long))
(test* "native-type float" <float> (native-type 'float))
(test* "native-type double" <double> (native-type 'double))
(test* "native-type void" <void> (native-type 'void))
(test* "native-type char" <c-char> (native-type 'char))
(test* "native-type int8_t" <int8> (native-type 'int8_t))
(test* "native-type uint8_t" <uint8> (native-type 'uint8_t))
(test* "native-type int16_t" <int16> (native-type 'int16_t))
(test* "native-type uint16_t" <uint16> (native-type 'uint16_t))
(test* "native-type int32_t" <int32> (native-type 'int32_t))
(test* "native-type uint32_t" <uint32> (native-type 'uint32_t))
(test* "native-type int64_t" <int64> (native-type 'int64_t))
(test* "native-type uint64_t" <uint64> (native-type 'uint64_t))
(test* "native-type size_t" <size_t> (native-type 'size_t))
(test* "native-type ssize_t" <ssize_t> (native-type 'ssize_t))
(test* "native-type ptrdiff_t" <ptrdiff_t> (native-type 'ptrdiff_t))

;; Pass-through of existing native type instances
(test* "native-type pass-through" <int> (native-type <int>))
(test* "native-type pass-through pointer" #t
       (let1 p (make-c-pointer-type <int>)
         (equal? p (native-type p))))

(test* "native-type (const int)" <int>
       (native-type '(const int)))

;; Signed/unsigned qualifiers on integral types
;; NB: signed/unsigned char maps to <int8>/<uint8>, not <c-char>.
(test* "native-type signed char" <int8> (native-type '(signed char)))
(test* "native-type unsigned char" <uint8> (native-type '(unsigned char)))
(test* "native-type signed short" <short> (native-type '(signed short)))
(test* "native-type unsigned short" <ushort> (native-type '(unsigned short)))
(test* "native-type signed int" <int> (native-type '(signed int)))
(test* "native-type unsigned int" <uint> (native-type '(unsigned int)))
(test* "native-type signed long" <long> (native-type '(signed long)))
(test* "native-type unsigned long" <ulong> (native-type '(unsigned long)))
(test* "native-type signed int8_t" <int8> (native-type '(signed int8_t)))
(test* "native-type unsigned int8_t" <uint8> (native-type '(unsigned int8_t)))
(test* "native-type signed int16_t" <int16> (native-type '(signed int16_t)))
(test* "native-type unsigned int16_t" <uint16> (native-type '(unsigned int16_t)))
(test* "native-type signed int32_t" <int32> (native-type '(signed int32_t)))
(test* "native-type unsigned int32_t" <uint32> (native-type '(unsigned int32_t)))
(test* "native-type signed int64_t" <int64> (native-type '(signed int64_t)))
(test* "native-type unsigned int64_t" <uint64> (native-type '(unsigned int64_t)))
;; Signed/unsigned pushes through pointer types down to the pointee.
(test* "native-type signed int *"
       (make-c-pointer-type <int>)
       (native-type '(signed int *)))
(test* "native-type unsigned int *"
       (make-c-pointer-type <uint>)
       (native-type '(unsigned int *)))
(test* "native-type unsigned char**"
       (make-c-pointer-type (make-c-pointer-type <uint8>))
       (native-type '(unsigned char**)))
;; Mixed with const
(test* "native-type unsigned const int*"
       (make-c-pointer-type <uint>)
       (native-type '(unsigned const int*)))
(test* "native-type const unsigned int*"
       (make-c-pointer-type <uint>)
       (native-type '(const unsigned int*)))

;; signed/unsigned on a signless type is an error.
(test* "native-type signed float error"
       (test-error <error> #/signed can't be attached to/)
       (native-type '(signed float)))
(test* "native-type unsigned double error"
       (test-error <error> #/unsigned can't be attached to/)
       (native-type '(unsigned double)))
(test* "native-type signed void error"
       (test-error <error> #/signed can't be attached to/)
       (native-type '(signed void)))
(test* "native-type unsigned c-string error"
       (test-error <error> #/unsigned can't be attached to/)
       (native-type '(unsigned c-string)))

;; Pointer types
(test* "native-type int*" #t
       (equal? (native-type 'int*) (make-c-pointer-type <int>)))
(test* "native-type double*" #t
       (equal? (native-type 'double*) (make-c-pointer-type <double>)))
(test* "native-type char*" #t
       (equal? (native-type 'char*) (make-c-pointer-type <c-char>)))
(test* "native-type void*" #t
       ;; test it with eq? to ensure the signleton
       (eq? (native-type 'void*) (make-c-pointer-type <void>)))

;; Double pointer
(test* "native-type int**" #t
       (equal? (native-type 'int**)
               (make-c-pointer-type (make-c-pointer-type <int>))))

;; Triple pointer
(test* "native-type char***" #t
       (equal? (native-type 'char***)
               (make-c-pointer-type
                (make-c-pointer-type
                 (make-c-pointer-type <c-char>)))))

;; Alternative forms
(test* "native-type (char*)" #t
       (equal? (native-type '(char*)) (make-c-pointer-type <c-char>)))
(test* "native-type (char *)" #t
       (equal? (native-type '(char *)) (make-c-pointer-type <c-char>)))
(test* "native-type (const char*)" #t
       (equal? (native-type '(const char*)) (make-c-pointer-type <c-char>)))
(test* "native-type (char const*)" #t
       (equal? (native-type '(char const*)) (make-c-pointer-type <c-char>)))
(test* "native-type int **" #t
       (equal? (native-type '(int **))
               (make-c-pointer-type (make-c-pointer-type <int>))))
(test* "native-type int* *" #t
       (equal? (native-type '(int* *))
               (make-c-pointer-type (make-c-pointer-type <int>))))

;; Pointer type is a <c-pointer>
(test* "native-type int* is <c-pointer>" #t
       (is-a? (native-type 'int*) <c-pointer>))
(test* "native-type int* pointee-type" #t
       (eq? (c-pointer-type-pointee (native-type 'int*)) <int>))

;; Inserted native-type instance in pointer syntax
(let1 foo (native-type '(.struct (a::int b::int)))
  (test* "native-type (,foo *)" #t
         (equal? (native-type `(,foo *)) (make-c-pointer-type foo)))
  (test* "native-type (const ,foo *)" #t
         (equal? (native-type `(const ,foo *)) (make-c-pointer-type foo)))
  (test* "native-type (,foo **)" #t
         (equal? (native-type `(,foo **))
                 (make-c-pointer-type (make-c-pointer-type foo))))
  (test* "native-type (,foo * *)" #t
         (equal? (native-type `(,foo * *))
                 (make-c-pointer-type (make-c-pointer-type foo))))
  (test* "native-type (,foo const*)" #t
         (equal? (native-type `(,foo const*)) (make-c-pointer-type foo)))
  (test* "native-type (,foo)" #t
         (eq? (native-type `(,foo)) foo))
  (test* "native-type (,<int> *)" #t
         (equal? (native-type `(,<int> *)) (make-c-pointer-type <int>)))
  (test* "native-type rejects non-modifier symbol with instance"
         (test-error)
         (native-type `(,foo int)))
  (test* "native-type rejects two instances"
         (test-error)
         (native-type `(,foo ,foo *))))

;; Special treatment of c-string
(test* "native-type c-string" #t
       (eq? (native-type 'c-string) <c-string>))
(test* "native-type c-string*" #t
       (equal? (native-type 'c-string*) (make-c-pointer-type <c-string>)))

;; Array types
(test* "native-type (.array int (3))" #t
       (equal? (native-type '(.array int (3)))
               (make-c-array-type <int> '(3))))

(test* "native-type (.array char (8))" #t
       (equal? (native-type '(.array char (8)))
               (make-c-array-type <c-char> '(8))))

;; Multi-dimensional array
(test* "native-type (.array int (2 3))" #t
       (equal? (native-type '(.array int (2 3)))
               (make-c-array-type <int> '(2 3))))

(test* "native-type (.array uint8_t (4 4 4))" #t
       (equal? (native-type '(.array uint8_t (4 4 4)))
               (make-c-array-type <uint8> '(4 4 4))))

;; Array type properties
(let1 a (native-type '(.array int (5)))
  (test* "native-type array element-type" #t
         (eq? (~ a'element-type) <int>))
  (test* "native-type array dimensions" '(5)
         (c-array-type-dimensions a))
  (test* "native-type array is <c-array>" #t
         (c-array-type? a)))

;; Array with unsized first dimension
(test* "native-type (.array int (* 3))" #t
       (equal? (native-type '(.array int (* 3)))
               (make-c-array-type <int> '(* 3))))

;; Struct/union

(test* "native-type (.struct bar (x::int y::float))" #t
       (equal? (native-type '(.struct bar (x::int y::float)))
               (make-c-struct-type 'bar `((x ,<int>) (y ,<float>)))))
(test* "native-type (.struct foo (a:: int b ::double))" #t
       (equal? (native-type '(.struct foo (a:: int b ::double)))
               (make-c-struct-type 'foo `((a ,<int>) (b ,<double>)))))

;; Struct type properties
(let1 s (native-type '(.struct pt (x::int y::int)))
  (test* "native-type struct is <c-struct>" #t
         (c-struct-type? s))
  (test* "native-type struct tag" 'pt
         (c-struct/union-type-tag s)))

;; Struct with nested array field
(test* "native-type struct with array field" #t
       (equal? (native-type '(.struct foo (a::int
                                           b::(.array char (8)))))
               (make-c-struct-type
                'foo
                `((a ,<int>)
                  (b ,(make-c-array-type <c-char> '(8)))))))

;; Struct with pointer field
(test* "native-type struct with pointer field" #t
       (let1 s (native-type '(.struct node (val::int next::int*)))
         (and (c-struct-type? s)
              (eq? (c-struct/union-type-tag s) 'node)
              (equal? (c-struct/union-type-field-type s 'next)
                      (make-c-pointer-type <int>)))))

;; Struct equivalence: two identical signatures produce equal types
(test* "native-type struct equivalence" #t
       (equal? (native-type '(.struct s1 (a::int b::double)))
               (native-type '(.struct s1 (a::int b::double)))))

;; Inserted native-type instance as a field type
(let1 foo (native-type '(.struct (a::int b::int)))
  (test* "native-type (.struct bar (x:: ,foo))" #t
         (equal? (native-type `(.struct bar (x:: ,foo)))
                 (make-c-struct-type 'bar `((x ,foo)))))
  (test* "native-type (.struct (x:: ,foo)) -- anonymous" #t
         (equal? (c-struct/union-type-field-type
                  (native-type `(.struct (x:: ,foo))) 'x)
                 foo))
  (test* "native-type mixed concrete and inserted field types" #t
         (let1 s (native-type `(.struct s (a::int b:: ,foo c::double)))
           (and (eq? (c-struct/union-type-field-type s 'a) <int>)
                (eq? (c-struct/union-type-field-type s 'b) foo)
                (eq? (c-struct/union-type-field-type s 'c) <double>))))
  (test* "native-type field type wrapped in pointer syntax" #t
         (equal? (c-struct/union-type-field-type
                  (native-type `(.struct (p::(,foo *)))) 'p)
                 (make-c-pointer-type foo))))

;; Struct inequivalence: different tags
(test* "native-type struct different tags" #f
       (equal? (native-type '(.struct s1 (a::int)))
               (native-type '(.struct s2 (a::int)))))

;; Unions
(test* "native-type (.union u2 (x::int y::double))" #t
       (equal? (native-type '(.union u2 (x::int y::double)))
               (make-c-union-type 'u2 `((x ,<int>) (y ,<double>)))))
(test* "native-type (.union u1 (a :: int  b :: float))" #t
       (equal? (native-type '(.union u1 (a :: int b :: float)))
               (make-c-union-type 'u1 `((a ,<int>) (b ,<float>)))))

;; Inserted native-type instance as a union field type
(let1 foo (native-type '(.struct (a::int b::int)))
  (test* "native-type (.union u (x:: ,foo y::int))" #t
         (let1 u (native-type `(.union u (x:: ,foo y::int)))
           (and (eq? (c-struct/union-type-field-type u 'x) foo)
                (eq? (c-struct/union-type-field-type u 'y) <int>))))
  (test* "native-type (.union (x:: ,foo y::int)) -- anonymous" #t
         (let1 u (native-type `(.union (x:: ,foo y::int)))
           (and (c-union-type? u)
                (eq? (c-struct/union-type-field-type u 'x) foo)))))

;; Union type properties
(let1 u (native-type '(.union val (i::int f::float)))
  (test* "native-type union is <c-union>" #t
         (c-union-type? u))
  (test* "native-type union tag" 'val
         (c-struct/union-type-tag u)))

;; All union field offsets should be 0
(let1 u (native-type '(.union uu (a::int b::double c::int8_t)))
  (define native-type-offset*
    (with-module gauche.native-type native-type-offset))
  (test* "native-type union offsets all zero" '(0 0 0)
         (map (cut native-type-offset* u <>) '(a b c))))

;; Enums
(let1 e (native-type '(.enum color (red green blue)))
  (test* "native-type .enum is <c-enum>" #t (c-enum-type? e))
  (test* "native-type .enum tag" 'color (c-enum-type-tag e))
  (test* "native-type .enum alist" '((red . 0) (green . 1) (blue . 2))
         (c-enum-type-enumerator-alist e)))
(test* "native-type (.enum (a b c)) -- anonymous" '(#t #f)
       (let1 e (native-type '(.enum (a b c)))
         (list (c-enum-type? e) (c-enum-type-tag e))))
(test* "native-type (.enum flag : uint8_t (a (b 4) c))"
       (list 'flag <uint8> '((a . 0) (b . 4) (c . 5)))
       (let1 e (native-type '(.enum flag : uint8_t (a (b 4) c)))
         (list (c-enum-type-tag e) (~ e'type-spec)
               (c-enum-type-enumerator-alist e))))
(test* "native-type (.enum : int8_t ((lo -2) mid hi)) -- anonymous w/ typespec"
       (list #f <int8> '((lo . -2) (mid . -1) (hi . 0)))
       (let1 e (native-type '(.enum : int8_t ((lo -2) mid hi)))
         (list (c-enum-type-tag e) (~ e'type-spec)
               (c-enum-type-enumerator-alist e))))
(test* "native-type bad .enum signature"
       (test-error <error> #/Invalid .enum signature/)
       (native-type '(.enum a b (c))))

;; Function types

(test* "native-type (.function (int int) double)" #t
       (equal? (native-type '(.function (int int) double))
               (make-c-function-type <double> `(,<int> ,<int>))))

(test* "native-type (.function (int char*) void)" #t
       (equal? (native-type '(.function (int char*) void))
               (make-c-function-type
                <void>
                `(,<int> ,(make-c-pointer-type <c-char>)))))

;; Varargs function
(test* "native-type (.function (int ...) int)" #t
       (equal? (native-type '(.function (int ...) int))
               (make-c-function-type <int> `(,<int> ...))))

;; Function type properties
(let1 f (native-type '(.function (int double) float))
  (test* "native-type function is <c-function>" #t
         (c-function-type? f))
  (test* "native-type function return-type" #t
         (eq? (c-function-type-return-type f) <float>))
  (test* "native-type function arg-types" #t
         (equal? (c-function-type-argument-types f) `(,<int> ,<double>))))

;; No-arg function
(test* "native-type (.function () int)" #t
       (equal? (native-type '(.function () int))
               (make-c-function-type <int> '())))

;; Nested compound types

;; Array of pointers (via nested native-type call in element type)
(test* "native-type (.array int* (4))" #t
       (equal? (native-type '(.array int* (4)))
               (make-c-array-type (make-c-pointer-type <int>) '(4))))

;; Struct containing struct (via nested (.struct ...) in field type)
(test* "native-type nested struct" #t
       (let1 outer (native-type '(.struct outer
                                   (pos::(.struct point (x::int y::int))
                                    val::double)))
         (and-let* ([ (c-struct-type? outer) ]
                    [t (c-struct/union-type-field-type outer 'pos)]
                    [ (c-struct-type? t) ])
           (eq? (c-struct/union-type-tag t) 'point))))

;; Struct containing array
(test* "native-type struct with 2d array" #t
       (let1 s (native-type '(.struct matrix (data::(.array double (3 3))
                                              name::int)))
         (and-let* ([ (c-struct-type? s) ]
                    [t (c-struct/union-type-field-type s 'data)]
                    [ (c-array-type? t) ])
           (equal? (c-array-type-dimensions t) '(3 3)))))

;; Integration with existing make-* constructors
;; Verify that types produced by native-type work correctly with
;; the existing native-ref/native-set! infrastructure.

(let* ([data (u8vector-copy *fobject-storage*)]
       [s1 (native-type '(.struct s1 (a::int8_t
                                      b::uint32_t
                                      c::uint16_t
                                      d::uint8_t)))]
       [s1-manual (make-c-struct-type 's1
                    `((a ,<int8>) (b ,<uint32>) (c ,<uint16>) (d ,<uint8>)))])
  (define (bc pos type) (make-native-handle type data pos))

  (test* "native-type struct equals manual struct" #t
         (equal? s1 s1-manual))

  ;; Size and alignment should match
  (test* "native-type struct size matches" #t
         (= (~ s1'size) (~ s1-manual'size)))
  (test* "native-type struct alignment matches" #t
         (= (~ s1'alignment) (~ s1-manual'alignment)))

  ;; Can actually dereference using the native-type constructed type
  (test* "native-type struct ref works"
         (native. (bc 0 s1-manual) 'a)
         (native. (bc 0 s1) 'a))
  (test* "native-type struct ref field b"
         (native. (bc 0 s1-manual) 'b)
         (native. (bc 0 s1) 'b)))

;; native-type pointer used with native-ref
(let* ([data (u8vector-copy *fobject-storage*)]
       [int32* (native-type 'int32_t*)]
       [int32*-manual (make-c-pointer-type <int32>)])
  (define (bc pos type) (make-native-handle type data pos))

  (test* "native-type pointer equals manual" #t
         (equal? int32* int32*-manual))
  (test* "native-type pointer deref works"
         (native* (bc 0 int32*-manual))
         (native* (bc 0 int32*))))

;; native-type array used with native-ref
(let* ([data (u8vector-copy *fobject-storage*)]
       [u8a (native-type '(.array uint8_t (4 4)))]
       [u8a-manual (make-c-array-type <uint8> '(4 4))])
  (define (bc pos type) (make-native-handle type data pos))

  (test* "native-type array equals manual" #t
         (equal? u8a u8a-manual))
  (test* "native-type array ref works"
         (native-aref (bc 0 u8a-manual) 0 0)
         (native-aref (bc 0 u8a) 0 0))
  (test* "native-type array ref (1 2)"
         (native-aref (bc 0 u8a-manual) 1 2)
         (native-aref (bc 0 u8a) 1 2)))

(test* "native-type unknown symbol" (test-error)
       (native-type 'nonexistent_type_xyz))

(test* "native-type invalid signature" (test-error)
       (native-type 42))

(test* "native-type invalid field spec"
       (test-error <error> #/missing type for field/)
       (native-type '(.struct bad (invalid_no_separator))))

;;;----------------------------------------------------------
(test-section "type signature constructor")

;; Primitive types
(test* "signature int" 'int (native-type->signature <int>))
(test* "signature uint" 'u_int (native-type->signature <uint>))
(test* "signature short" 'short (native-type->signature <short>))
(test* "signature u_short" 'u_short (native-type->signature <ushort>))
(test* "signature long" 'long (native-type->signature <long>))
(test* "signature u_long" 'u_long (native-type->signature <ulong>))
(test* "signature float" 'float (native-type->signature <float>))
(test* "signature double" 'double (native-type->signature <double>))
(test* "signature void" 'void (native-type->signature <void>))
(test* "signature char" 'char (native-type->signature <c-char>))
(test* "signature int8_t" 'int8_t (native-type->signature <int8>))
(test* "signature uint8_t" 'uint8_t (native-type->signature <uint8>))
(test* "signature int16_t" 'int16_t (native-type->signature <int16>))
(test* "signature uint16_t" 'uint16_t (native-type->signature <uint16>))
(test* "signature int32_t" 'int32_t (native-type->signature <int32>))
(test* "signature uint32_t" 'uint32_t (native-type->signature <uint32>))
(test* "signature int64_t" 'int64_t (native-type->signature <int64>))
(test* "signature uint64_t" 'uint64_t (native-type->signature <uint64>))
(test* "signature size_t" 'size_t (native-type->signature <size_t>))
(test* "signature ssize_t" 'ssize_t (native-type->signature <ssize_t>))
(test* "signature ptrdiff_t" 'ptrdiff_t (native-type->signature <ptrdiff_t>))
(test* "signature c-string" 'c-string (native-type->signature <c-string>))

;; Endian-specific types
(test* "signature int16_be" 'int16_be (native-type->signature <int16-be>))
(test* "signature int16_le" 'int16_le (native-type->signature <int16-le>))
(test* "signature uint16_be" 'uint16_be (native-type->signature <uint16-be>))
(test* "signature uint16_le" 'uint16_le (native-type->signature <uint16-le>))
(test* "signature int32_be" 'int32_be (native-type->signature <int32-be>))
(test* "signature int32_le" 'int32_le (native-type->signature <int32-le>))
(test* "signature uint32_be" 'uint32_be (native-type->signature <uint32-be>))
(test* "signature uint32_le" 'uint32_le (native-type->signature <uint32-le>))
(test* "signature int64_be" 'int64_be (native-type->signature <int64-be>))
(test* "signature int64_le" 'int64_le (native-type->signature <int64-le>))
(test* "signature uint64_be" 'uint64_be (native-type->signature <uint64-be>))
(test* "signature uint64_le" 'uint64_le (native-type->signature <uint64-le>))
(test* "signature float_be" 'float_be (native-type->signature <float-be>))
(test* "signature float_le" 'float_le (native-type->signature <float-le>))
(test* "signature double_be" 'double_be (native-type->signature <double-be>))
(test* "signature double_le" 'double_le (native-type->signature <double-le>))

;; Pointer types
(test* "signature int*" 'int*
       (native-type->signature (make-c-pointer-type <int>)))
(test* "signature char**" 'char**
       (native-type->signature (make-c-pointer-type
                               (make-c-pointer-type <c-char>))))
(test* "signature void*" 'void*
       (native-type->signature (make-c-pointer-type <void>)))
(test* "signature c-string*" 'c-string*
       (native-type->signature (make-c-pointer-type <c-string>)))
(test* "signature double***" 'double***
       (native-type->signature (make-c-pointer-type
                                (make-c-pointer-type
                                 (make-c-pointer-type <double>)))))

;; Array types
(test* "signature array int (3)" '(.array int (3))
       (native-type->signature (make-c-array-type <int> '(3))))
(test* "signature array 2d" '(.array uint8_t (4 4))
       (native-type->signature (make-c-array-type <uint8> '(4 4))))
(test* "signature array unsized" '(.array int (* 3))
       (native-type->signature (make-c-array-type <int> '(* 3))))
(test* "signature array of pointers" '(.array int* (4))
       (native-type->signature (make-c-array-type
                               (make-c-pointer-type <int>) '(4))))

;; Struct types
(test* "signature struct" '(.struct foo (a::int b::double))
       (native-type->signature
        (make-c-struct-type 'foo `((a ,<int>) (b ,<double>)))))
(test* "signature struct (anonymous)" '(.struct (a::int b::double))
       (native-type->signature
        (make-c-struct-type #f `((a ,<int>) (b ,<double>)))))
(test* "signature struct with array field"
       '(.struct bar (x::int y:: (.array char (8))))
       (native-type->signature
        (make-c-struct-type 'bar
          `((x ,<int>) (y ,(make-c-array-type <c-char> '(8)))))))
(test* "signature struct with pointer field"
       '(.struct node (val::int next::int*))
       (native-type->signature
        (make-c-struct-type 'node
          `((val ,<int>) (next ,(make-c-pointer-type <int>))))))

;; Union types
(test* "signature union" '(.union u1 (x::int y::float))
       (native-type->signature
        (make-c-union-type 'u1 `((x ,<int>) (y ,<float>)))))
(test* "signature union (anonymous)" '(.union (x::int y::float))
       (native-type->signature
        (make-c-union-type #f `((x ,<int>) (y ,<float>)))))

;; Enum types
(test* "signature enum" '(.enum color (red green blue))
       (native-type->signature
        (make-c-enum-type 'color #f '(red green blue))))
(test* "signature enum (anonymous)" '(.enum (red green blue))
       (native-type->signature
        (make-c-enum-type #f #f '(red green blue))))
(test* "signature enum with typespec" '(.enum flag : uint8_t (a (b 4) c))
       (native-type->signature
        (make-c-enum-type 'flag <uint8> '(a (b 4) c))))
(test* "signature enum (anonymous) with typespec" '(.enum : int8_t (a b c))
       (native-type->signature
        (make-c-enum-type #f <int8> '(a b c))))
(test* "signature enum bare/explicit minimal form"
       '(.enum E (a (b 10) c (d 11) e))
       (native-type->signature
        (make-c-enum-type 'E #f '(a (b 10) c (d 11) e))))

;; Function types
(test* "signature function" '(.function (int int) double)
       (native-type->signature
        (make-c-function-type <double> `(,<int> ,<int>))))
(test* "signature function with pointer arg"
       '(.function (int char*) void)
       (native-type->signature
        (make-c-function-type <void>
          `(,<int> ,(make-c-pointer-type <c-char>)))))
(test* "signature function varargs" '(.function (int ...) int)
       (native-type->signature
        (make-c-function-type <int> `(,<int> ...))))
(test* "signature function no args" '(.function () void)
       (native-type->signature
        (make-c-function-type <void> '())))

;; Round-trip
(define (round-trip sig)
  (equal? (native-type sig)
          (native-type (native-type->signature (native-type sig)))))
(test* "round-trip int" #t (round-trip 'int))
(test* "round-trip char**" #t (round-trip 'char**))
(test* "round-trip c-string*" #t (round-trip 'c-string*))
(test* "round-trip array" #t (round-trip '(.array int (2 3))))
(test* "round-trip struct" #t
       (round-trip '(.struct s (a::int b::double c::uint8_t))))
(test* "round-trip union" #t
       (round-trip '(.union u (x::int y::float))))
(test* "round-trip function" #t
       (round-trip '(.function (int char*) void)))
(test* "round-trip varargs function" #t
       (round-trip '(.function (int ...) int)))
(test* "round-trip nested struct" #t
       (round-trip '(.struct outer
                      (pos::(.struct point (x::int y::int))
                       val::double))))
(test* "round-trip int16_be" #t (round-trip 'int16_be))
(test* "round-trip uint32_le" #t (round-trip 'uint32_le))
(test* "round-trip int64_be" #t (round-trip 'int64_be))
(test* "round-trip float_le" #t (round-trip 'float_le))
(test* "round-trip double_be" #t (round-trip 'double_be))

;; Endian types in aggregate types
(test* "signature struct with endian fields"
       '(.struct pkt (len::uint16_be data::uint32_be))
       (native-type->signature
        (make-c-struct-type 'pkt `((len ,<uint16-be>) (data ,<uint32-be>)))))
(test* "signature array of int32_le"
       '(.array int32_le (4))
       (native-type->signature (make-c-array-type <int32-le> '(4))))
(test* "round-trip struct with endian fields" #t
       (round-trip '(.struct pkt (len::uint16_be data::uint32_be))))
(test* "round-trip array of int32_le" #t
       (round-trip '(.array int32_le (4))))

;; Enums: check signature-level stability, i.e. native-type->signature emits
;; the canonical (minimal) form that reproduces itself.  (This is stronger
;; than the structural round-trip above, which only checks type equality.)
(define (sig-round-trip sig)
  (equal? (native-type->signature (native-type sig)) sig))
(test* "round-trip enum" #t
       (sig-round-trip '(.enum color (red green blue))))
(test* "round-trip enum anonymous" #t
       (sig-round-trip '(.enum (a b c))))
(test* "round-trip enum with typespec" #t
       (sig-round-trip '(.enum flag : uint8_t (a (b 4) c))))
(test* "round-trip enum anonymous with typespec" #t
       (sig-round-trip '(.enum : int8_t ((lo -2) mid hi))))

(test-end)
