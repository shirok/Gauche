;;
;; testing gauche.ctype
;;

(use gauche.test)
(use gauche.uvector)

(test-start "ffi")

;;----------------------------------------------------------
(test-section "gauche.ctype")

(use gauche.ctype)
(test-module 'gauche.ctype)

;; Type equivalences
(let* ([int* (make-c-pointer-type <c-int>)]
       [int8* (make-c-pointer-type <c-int8>)]
       [int8a (make-c-array-type <c-int8> '(2 2))]
       [int8a2 (make-c-array-type <c-int8> '(2 2))]
       [int8a4 (make-c-array-type <c-int8> '(4))]
       [u8a2 (make-c-array-type <c-uint8> '(2 2))]
       [fn1 (make-c-function-type <c-int> `(,<c-int8> ,<c-uint16>))]
       [fn2 (make-c-function-type <c-int> `(,<c-int8> ,<c-uint16>))]
       [fn-var (make-c-function-type <c-int> `(,<c-int8> ,<c-uint16> ...))]
       [s1 (make-c-struct-type 's1 `((a ,<c-int8>) (b ,<c-uint16>)))]
       [s1b (make-c-struct-type 's1 `((a ,<c-int8>) (b ,<c-uint16>)))]
       [s2 (make-c-struct-type 's2 `((a ,<c-int8>) (b ,<c-uint16>)))]
       [u1 (make-c-union-type 'u1 `((a ,<c-int8>) (b ,<c-uint16>)))]
       [u1b (make-c-union-type 'u1 `((a ,<c-int8>) (b ,<c-uint16>)))]
       [u2 (make-c-union-type 'u2 `((a ,<c-int8>) (b ,<c-uint16>)))])
  ;; equivalent cases
  (test* "native type equal? pointer" #t
         (equal? (make-c-pointer-type <c-int>) (make-c-pointer-type <c-int>)))
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

(let ([data (u8vector-copy *fobject-storage*)]
      [int8* (make-c-pointer-type <c-int8>)]
      [uint8* (make-c-pointer-type <c-uint8>)]
      [int16* (make-c-pointer-type <c-int16>)]
      [uint16* (make-c-pointer-type <c-uint16>)]
      [int32* (make-c-pointer-type <c-int32>)]
      [uint32* (make-c-pointer-type <c-uint32>)]
      [int64* (make-c-pointer-type <c-int64>)]
      [uint64* (make-c-pointer-type <c-uint64>)]
      [char* (make-c-pointer-type <c-char>)])
  (define (bc pos type) (uvector->native-handle data type pos))

  (test* "uint8* deref" '(#x80 #x09 #x3f)
         (list (native-ref (bc 0 uint8*) 0)
               (native-ref (bc 9 uint8*) 0)
               (native-ref (bc 56 uint8*) 7)))
  (test* "int8* deref" '(#x-80 #x09 #x3f)
         (list (native-ref (bc 0 int8*) 0)
               (native-ref (bc 9 int8*) 0)
               (native-ref (bc 56 int8*) 7)))

  (test* "uint16* deref" (case (native-endian)
                           [(big-endian) '(#x0809 #xfeff)]
                           [else         '(#x0908 #xfffe)])
         (list (native-ref (bc 8 uint16*) 0)
               (native-ref (bc 72 uint16*) 3)))
  (test* "int16* deref" (case (native-endian)
                          [(big-endian) '(#x0809 #x-0101)]
                          [else         '(#x0908 #x-0002)])
         (list (native-ref (bc 8 int16*) 0)
               (native-ref (bc 72 int16*) 3)))

  (test* "uint32* deref" (case (native-endian)
                           [(big-endian) '(#x10111213 #xf8f9fafb)]
                           [else         '(#x13121110 #xfbfaf9f8)])
         (list (native-ref (bc 16 uint32*) 0)
               (native-ref (bc 64 uint32*) 2)))
  (test* "int32* deref" (case (native-endian)
                          [(big-endian) '(#x10111213 #x-07060505)]
                          [else         '(#x13121110 #x-04050608)])
         (list (native-ref (bc 16 int32*) 0)
               (native-ref (bc 64 int32*) 2)))

  (test* "uint64* deref" (case (native-endian)
                           [(big-endian) '(#x2021222324252627
                                           #xf0f1f2f3f4f5f6f7)]
                           [else         '(#x2726252423222120
                                           #xf7f6f5f4f3f2f1f0)])
         (list (native-ref (bc 32 uint64*) 0)
               (native-ref (bc 32 uint64*) 4)))
  (test* "int64* deref" (case (native-endian)
                          [(big-endian) '(#x2021222324252627
                                          #x-0f0e0d0c0b0a0909)]
                          [else         '(#x2726252423222120
                                          #x-08090a0b0c0d0e10)])
         (list (native-ref (bc 32 int64*) 0)
               (native-ref (bc 32 int64*) 4)))

  (test* "char* deref" '(#\space #\!)
         (list (native-ref (bc 32 char*) 0)
               (native-ref (bc 32 char*) 1)))

  (test* "uint8* modify" #xff
         (begin
           (native-set! (bc 0 uint8*) 1 #xff)
           (native-ref (bc 1 uint8*) 0)))
  (test* "int8* modify" -2
         (begin
           (native-set! (bc 0 int8*) 1 -2)
           (native-ref (bc 1 int8*) 0)))

  (test* "uint16* modify" #xabcd
         (begin
           (native-set! (bc 0 uint16*) 1 #xabcd)
           (native-ref (bc 2 uint16*) 0)))
  (test* "int16* modify" #x-1234
         (begin
           (native-set! (bc 0 int16*) 1 #x-1234)
           (native-ref (bc 2 int16*) 0)))

  (test* "uint32* modify" #x89abcdef
         (begin
           (native-set! (bc 0 uint32*) 1 #x89abcdef)
           (native-ref (bc 4 uint32*) 0)))
  (test* "int32* modify" #x-789abcde
         (begin
           (native-set! (bc 0 int32*) 1 #x-789abcde)
           (native-ref (bc 4 int32*) 0)))

  (test* "uint64* modify" #x0123456789abcdef
         (begin
           (native-set! (bc 0 uint64*) 1 #x0123456789abcdef)
           (native-ref (bc 8 uint64*) 0)))
  (test* "int64* modify" #x-0123456789abcdef
         (begin
           (native-set! (bc 0 int64*) 1 #x-0123456789abcdef)
           (native-ref (bc 8 int64*) 0)))

  (test* "native-char modify" #\Z
         (begin
           (native-set! (bc 32 char*) 0 #\Z)
           (native-ref (bc 0 char*) 32)))
  )

(let ([data (u8vector-copy *fobject-storage*)]
      [int8a (make-c-array-type <c-int8> '(* 4 2))]
      [uint8a (make-c-array-type <c-uint8> '(2 4 2))]
      [int16a (make-c-array-type <c-int16> '(* 4 2))]
      [uint16a (make-c-array-type <c-uint16> '(2 4 2))]
      [int32a (make-c-array-type <c-int32> '(* 3 2))]
      [uint32a (make-c-array-type <c-uint32> '(2 3 2))]
      [int64a (make-c-array-type <c-int64> '(* 2))]
      [uint64a (make-c-array-type <c-uint64> '(2 2))])
  (define (bc pos type) (uvector->native-handle data type pos))
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
         (list (native-ref (bc 0 uint8a) '(0 0 0))
               (native-ref (bc 0 uint8a) '(0 0 1))
               (native-ref (bc 0 uint8a) '(0 1 0))
               (native-ref (bc 0 uint8a) '(0 1 1))
               (native-ref (bc 0 uint8a) '(0 2 0))
               (native-ref (bc 0 uint8a) '(0 2 1))
               (native-ref (bc 0 uint8a) '(0 3 0))
               (native-ref (bc 0 uint8a) '(0 3 1))
               (native-ref (bc 0 uint8a) '(1 0 0))
               (native-ref (bc 0 uint8a) '(1 0 1))))

  (test* "int8 array ref" '(#x-80 #x01 #x02 #x03 #x04 #x05 #x06 #x07
                                  #x08 #x09 #x18)
         (list (native-ref (bc 0 int8a) '(0 0 0))
               (native-ref (bc 0 int8a) '(0 0 1))
               (native-ref (bc 0 int8a) '(0 1 0))
               (native-ref (bc 0 int8a) '(0 1 1))
               (native-ref (bc 0 int8a) '(0 2 0))
               (native-ref (bc 0 int8a) '(0 2 1))
               (native-ref (bc 0 int8a) '(0 3 0))
               (native-ref (bc 0 int8a) '(0 3 1))
               (native-ref (bc 0 int8a) '(1 0 0))
               (native-ref (bc 0 int8a) '(1 0 1))
               (native-ref (bc 0 int8a) '(3 0 0))))

  (test* "uint16 array ref"
         (case (native-endian)
           [(big-endian) '(#x8001 #x0203 #x0405 #x0e0f
                           #x1011 #x1e1f)]
           [else         '(#x0180 #x0302 #x0504 #x0f0e
                           #x1110 #x1f1e)])
         (list (native-ref (bc 0 uint16a) '(0 0 0))
               (native-ref (bc 0 uint16a) '(0 0 1))
               (native-ref (bc 0 uint16a) '(0 1 0))
               (native-ref (bc 0 uint16a) '(0 3 1))
               (native-ref (bc 0 uint16a) '(1 0 0))
               (native-ref (bc 0 uint16a) '(1 3 1))))
  (test* "int16 array ref"
         (case (native-endian)
           [(big-endian) '(#x0203 #x0405 #x0e0f
                           #x1011 #x1e1f #xfeff)]
           [else         '(#x0302 #x0504 #x0f0e
                           #x1110 #x1f1e #x-0002)])
         (list (native-ref (bc 0 int16a) '(0 0 1))
               (native-ref (bc 0 int16a) '(0 1 0))
               (native-ref (bc 0 int16a) '(0 3 1))
               (native-ref (bc 0 int16a) '(1 0 0))
               (native-ref (bc 0 int16a) '(1 3 1))
               (native-ref (bc 0 int16a) '(4 3 1))))

  (test* "uint32 array ref"
         (case (native-endian)
           [(big-endian) '(#x80010203 #x04050607
                           #x10111213 #x2c2d2e2f)]
           [else         '(#x03020180 #x07060504
                           #x13121110 #x2f2e2d2c)])
         (list (native-ref (bc 0 uint32a) '(0 0 0))
               (native-ref (bc 0 uint32a) '(0 0 1))
               (native-ref (bc 0 uint32a) '(0 2 0))
               (native-ref (bc 0 uint32a) '(1 2 1))))
  (test* "int32 array ref"
         (case (native-endian)
           [(big-endian) '(#x80010203 #x04050607
                           #x10111213 #x-03020101)]
           [else         '(#x03020180 #x07060504
                           #x13121110 #x-00010204)])
         (list (native-ref (bc 0 int32a) '(0 0 0))
               (native-ref (bc 0 int32a) '(0 0 1))
               (native-ref (bc 0 int32a) '(0 2 0))
               (native-ref (bc 0 int32a) '(3 0 1))))

  (test* "uint64 array ref"
         (case (native-endian)
           [(big-endian) '(#x8001020304050607
                           #x18191a1b1c1d1e1f)]
           [else         '(#x0706050403020180
                           #x1f1e1d1c1b1a1918)])
         (list (native-ref (bc 0 uint64a) '(0 0))
               (native-ref (bc 0 uint64a) '(1 1))))
  (test* "int64 array ref"
         (case (native-endian)
           [(big-endian) '(#x8001020304050607
                           #x-0706050403020101)]
           [else         '(#x0706050403020180
                           #x-0001020304050608)])
         (list (native-ref (bc 0 int64a) '(0 0))
               (native-ref (bc 0 int64a) '(4 1))))

  (test* "uint8 array modify" #xff
         (begin
           (native-set! (bc 0 uint8a) '(0 0 1) #xff)
           (native-ref (bc 0 uint8a) '(0 0 1))))
  (test* "int8 array modify" -2
         (begin
           (native-set! (bc 0 int8a) '(0 0 1) -2)
           (native-ref (bc 0 int8a) '(0 0 1))))

  (test* "uint16 array modify" #xabcd
         (begin
           (native-set! (bc 0 uint16a) '(0 0 1) #xabcd)
           (native-ref (bc 0 uint16a) '(0 0 1))))
  (test* "int16 array modify" #x-1234
         (begin
           (native-set! (bc 0 int16a) '(0 0 1) #x-1234)
           (native-ref (bc 0 int16a) '(0 0 1))))

  (test* "uint32 array modify" #x89abcdef
         (begin
           (native-set! (bc 0 uint32a) '(0 2 0) #x89abcdef)
           (native-ref (bc 0 uint32a) '(0 2 0))))
  (test* "int32 array modify" #x-789abcde
         (begin
           (native-set! (bc 0 int32a) '(0 2 0) #x-789abcde)
           (native-ref (bc 0 int32a) '(0 2 0))))

  (test* "uint64 array modify" #x0123456789abcdef
         (begin
           (native-set! (bc 0 uint64a) '(0 0) #x0123456789abcdef)
           (native-ref (bc 0 uint64a) '(0 0))))
  (test* "int64 array modify" #x-0123456789abcdef
         (begin
           (native-set! (bc 0 int64a) '(0 0) #x-0123456789abcdef)
           (native-ref (bc 0 int64a) '(0 0))))

  ;; partial index
  (test* "int8 array partial dereference" #x17
         (let1 a (native-ref (bc 0 int8a) '(2 3))
           (native-ref a '(1))))
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
      [float* (make-c-pointer-type <c-float>)]
      [double* (make-c-pointer-type <c-double>)]
      [floata (make-c-array-type <c-float> '(3))]
      [doublea (make-c-array-type <c-double> '(3))])
  (define (bc pos type) (uvector->native-handle data type pos))
  (define (tsa type expect)
    (test* #"~|type| size&alignment" expect
           (list (~ type'size) (~ type'alignment))))

  (tsa floata '(12 4))
  (tsa doublea '(24 8))

  (test* "float* deref" '(1.0 -1.0 3.5)
         (list (native-ref (bc 0 float*) 0)
               (native-ref (bc 0 float*) 1)
               (native-ref (bc 0 float*) 2)))
  (test* "double* deref" '(1.0 -1.0 2.5)
         (list (native-ref (bc 12 double*) 0)
               (native-ref (bc 12 double*) 1)
               (native-ref (bc 12 double*) 2)))
  (test* "float array ref" '(1.0 -1.0 3.5)
         (list (native-ref (bc 0 floata) '(0))
               (native-ref (bc 0 floata) '(1))
               (native-ref (bc 0 floata) '(2))))
  (test* "double array ref" '(1.0 -1.0 2.5)
         (list (native-ref (bc 12 doublea) '(0))
               (native-ref (bc 12 doublea) '(1))
               (native-ref (bc 12 doublea) '(2))))

  (test* "float* modify" -2.0
         (begin
           (native-set! (bc 0 float*) 1 -2.0)
           (native-ref (bc 0 float*) 1)))
  (test* "float array modify" -2.0
         (begin
           (native-set! (bc 0 floata) '(1) -2.0)
           (native-ref (bc 0 floata) '(1))))

  (test* "double* modify" -4.5
         (begin
           (native-set! (bc 12 double*) 1 -4.5)
           (native-ref (bc 12 double*) 1)))
  (test* "double array modify" -4.5
         (begin
           (native-set! (bc 12 doublea) '(1) -4.5)
           (native-ref (bc 12 doublea) '(1))))
  )

(let ([data (u8vector-copy *fobject-storage*)]
      [s1 (make-c-struct-type 's1
                                   `((a ,<c-int8>)
                                     (b ,<c-uint32>)
                                     (c ,<c-uint16>)
                                     (d ,<c-uint8>)))]
      [s2 (make-c-struct-type 's2
                                   `((a ,<c-uint8>)
                                     (b ,<c-uint64>)
                                     (c ,<c-int16>)))]
      [s0 (make-c-struct-type 's0 '())])
  (define (bc pos type) (uvector->native-handle data type pos))
  (define (tsa type expect)
    (test* #"~|type| size&alignment" expect
           (list (~ type'size) (~ type'alignment))))
  (define native-type-offset*
    (with-module gauche.ctype native-type-offset))
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
         (list (native-ref (bc 0 s1) 'a)
               (native-ref (bc 0 s1) 'b)
               (native-ref (bc 0 s1) 'c)
               (native-ref (bc 0 s1) 'd)))
  (test* "native struct ref s2"
         (case (native-endian)
           [(big-endian) '(#x80 #x08090a0b0c0d0e0f #x1011)]
           [else         '(#x80 #x0f0e0d0c0b0a0908 #x1110)])
         (list (native-ref (bc 0 s2) 'a)
               (native-ref (bc 0 s2) 'b)
               (native-ref (bc 0 s2) 'c)))

  (test* "native struct modify s1" '(#x-1 #x11223344 #xabcd #xfe)
         (begin
           (native-set! (bc 0 s1) 'a -1)
           (native-set! (bc 0 s1) 'b #x11223344)
           (native-set! (bc 0 s1) 'c #xabcd)
           (native-set! (bc 0 s1) 'd #xfe)
           (list (native-ref (bc 0 s1) 'a)
                 (native-ref (bc 0 s1) 'b)
                 (native-ref (bc 0 s1) 'c)
                 (native-ref (bc 0 s1) 'd))))

  (test* "native struct modify s2" '(#xaa #x0123456789abcdef #x-2)
         (begin
           (native-set! (bc 0 s2) 'a #xaa)
           (native-set! (bc 0 s2) 'b #x0123456789abcdef)
           (native-set! (bc 0 s2) 'c -2)
           (list (native-ref (bc 0 s2) 'a)
                 (native-ref (bc 0 s2) 'b)
                 (native-ref (bc 0 s2) 'c)))))

(let* ([data (u8vector-copy *fobject-storage*)]
       [u16x2 (make-c-array-type <c-uint16> '(2))]
       [s3 (make-c-struct-type 's3
                                    `((arr ,u16x2)
                                      (b ,<c-uint8>)))])
  (define (bc pos type) (uvector->native-handle data type pos))
  (define native-type-offset*
    (with-module gauche.ctype native-type-offset))
  (define (offsets type fields)
    (map (cut native-type-offset* type <>) fields))

  (test* "native struct array member size&alignment" '(6 2)
         (list (~ s3'size) (~ s3'alignment)))
  (test* "native struct array member offsets" '(0 4)
         (offsets s3 '(arr b)))

  (test* "native struct array member ref"
         (case (native-endian)
           [(big-endian) '(#x8001 #x0203 #x04)]
           [else         '(#x0180 #x0302 #x04)])
         (let1 arr (native-ref (bc 0 s3) 'arr)
           (list (native-ref arr '(0))
                 (native-ref arr '(1))
                 (native-ref (bc 0 s3) 'b))))

  (test* "native struct array member modify"
         '(#xabcd #xfe)
         (let1 arr (native-ref (bc 0 s3) 'arr)
           (native-set! arr '(1) #xabcd)
           (native-set! (bc 0 s3) 'b #xfe)
           (list (native-ref arr '(1))
                 (native-ref (bc 0 s3) 'b)))))

;; c-union tests
(let ([data (u8vector-copy *fobject-storage*)]
      [u1 (make-c-union-type 'u1
                                  `((a ,<c-int8>)
                                    (b ,<c-uint32>)
                                    (c ,<c-uint16>)))]
      [u2 (make-c-union-type 'u2
                                  `((x ,<c-uint8>)
                                    (y ,<c-uint64>)
                                    (z ,<c-int16>)))]
      [u0 (make-c-union-type 'u0 '())])
  (define (bc pos type) (uvector->native-handle data type pos))
  (define (tsa type expect)
    (test* #"~|type| size&alignment" expect
           (list (~ type'size) (~ type'alignment))))
  (define native-type-offset*
    (with-module gauche.ctype native-type-offset))
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
         (list (native-ref (bc 0 u1) 'a)
               (native-ref (bc 0 u1) 'b)
               (native-ref (bc 0 u1) 'c)))

  (test* "native union ref u2"
         (case (native-endian)
           [(big-endian) (list #x80                     ; x: uint8
                               #x8001020304050607       ; y: uint64
                               #x8001)]                 ; z: int16
           [else         (list #x80                     ; x: uint8
                               #x0706050403020180       ; y: uint64
                               #x0180)])                ; z: int16 (positive)
         (list (native-ref (bc 0 u2) 'x)
               (native-ref (bc 0 u2) 'y)
               (native-ref (bc 0 u2) 'z)))

  ;; Writing to one field and reading back via the same field
  (test* "native union modify via b, read b" #x11223344
         (begin
           (native-set! (bc 0 u1) 'b #x11223344)
           (native-ref (bc 0 u1) 'b)))

  ;; Writing to one field and reading via a different field
  ;; (all fields share offset 0, so writing b overwrites a and c's bytes too)
  (test* "native union modify via b, read a"
         (case (native-endian)
           [(big-endian) #x11]
           [else         #x44])
         (native-ref (bc 0 u1) 'a))

  (test* "native union modify via b, read c"
         (case (native-endian)
           [(big-endian) #x1122]
           [else         #x3344])
         (native-ref (bc 0 u1) 'c))

  (test* "native union modify u2" #x0123456789abcdef
         (begin
           (native-set! (bc 0 u2) 'y #x0123456789abcdef)
           (native-ref (bc 0 u2) 'y)))
  )

;;;
;;; native-type: type signature parser
;;;
(test-section "type signature parser")

;; Primitive types via C-style names

(test* "native-type int" <c-int> (native-type 'int))
(test* "native-type uint" <c-uint> (native-type 'u_int))
(test* "native-type short" <c-short> (native-type 'short))
(test* "native-type u_short" <c-ushort> (native-type 'u_short))
(test* "native-type long" <c-long> (native-type 'long))
(test* "native-type u_long" <c-ulong> (native-type 'u_long))
(test* "native-type float" <c-float> (native-type 'float))
(test* "native-type double" <c-double> (native-type 'double))
(test* "native-type void" <void> (native-type 'void))
(test* "native-type char" <c-char> (native-type 'char))
(test* "native-type int8_t" <c-int8> (native-type 'int8_t))
(test* "native-type uint8_t" <c-uint8> (native-type 'uint8_t))
(test* "native-type int16_t" <c-int16> (native-type 'int16_t))
(test* "native-type uint16_t" <c-uint16> (native-type 'uint16_t))
(test* "native-type int32_t" <c-int32> (native-type 'int32_t))
(test* "native-type uint32_t" <c-uint32> (native-type 'uint32_t))
(test* "native-type int64_t" <c-int64> (native-type 'int64_t))
(test* "native-type uint64_t" <c-uint64> (native-type 'uint64_t))
(test* "native-type size_t" <c-size_t> (native-type 'size_t))
(test* "native-type ssize_t" <c-ssize_t> (native-type 'ssize_t))
(test* "native-type ptrdiff_t" <c-ptrdiff_t> (native-type 'ptrdiff_t))

;; Pass-through of existing native type instances
(test* "native-type pass-through" <c-int> (native-type <c-int>))
(test* "native-type pass-through pointer" #t
       (let1 p (make-c-pointer-type <c-int>)
         (equal? p (native-type p))))

(test* "native-type (const int)" <c-int>
       (native-type '(const int)))

;; Pointer types
(test* "native-type int*" #t
       (equal? (native-type 'int*) (make-c-pointer-type <c-int>)))
(test* "native-type double*" #t
       (equal? (native-type 'double*) (make-c-pointer-type <c-double>)))
(test* "native-type void*" #t
       (equal? (native-type 'void*) (make-c-pointer-type <void>)))
(test* "native-type char*" #t
       (equal? (native-type 'char*) (make-c-pointer-type <c-char>)))

;; Double pointer
(test* "native-type int**" #t
       (equal? (native-type 'int**)
               (make-c-pointer-type (make-c-pointer-type <c-int>))))

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
               (make-c-pointer-type (make-c-pointer-type <c-int>))))
(test* "native-type int* *" #t
       (equal? (native-type '(int* *))
               (make-c-pointer-type (make-c-pointer-type <c-int>))))

;; Pointer type is a <c-pointer>
(test* "native-type int* is <c-pointer>" #t
       (is-a? (native-type 'int*) <c-pointer>))
(test* "native-type int* pointee-type" #t
       (eq? (~ (native-type 'int*) 'pointee-type) <c-int>))

;; Special treatment of c-string
(test* "native-type c-string" #t
       (eq? (native-type 'c-string) <c-string>))
(test* "native-type c-string*" #t
       (equal? (native-type 'c-string*) (make-c-pointer-type <c-string>)))

;; Array types
(test* "native-type (.array int (3))" #t
       (equal? (native-type '(.array int (3)))
               (make-c-array-type <c-int> '(3))))

(test* "native-type (.array char (8))" #t
       (equal? (native-type '(.array char (8)))
               (make-c-array-type <c-char> '(8))))

;; Multi-dimensional array
(test* "native-type (.array int (2 3))" #t
       (equal? (native-type '(.array int (2 3)))
               (make-c-array-type <c-int> '(2 3))))

(test* "native-type (.array uint8_t (4 4 4))" #t
       (equal? (native-type '(.array uint8_t (4 4 4)))
               (make-c-array-type <c-uint8> '(4 4 4))))

;; Array type properties
(let1 a (native-type '(.array int (5)))
  (test* "native-type array element-type" #t
         (eq? (~ a'element-type) <c-int>))
  (test* "native-type array dimensions" '(5)
         (~ a'dimensions))
  (test* "native-type array is <c-array>" #t
         (is-a? a <c-array>)))

;; Array with unsized first dimension
(test* "native-type (.array int (* 3))" #t
       (equal? (native-type '(.array int (* 3)))
               (make-c-array-type <c-int> '(* 3))))

;; Struct/union

(test* "native-type (.struct bar (x::int y::float))" #t
       (equal? (native-type '(.struct bar (x::int y::float)))
               (make-c-struct-type 'bar `((x ,<c-int>) (y ,<c-float>)))))
(test* "native-type (.struct foo (a:: int b ::double))" #t
       (equal? (native-type '(.struct foo (a:: int b ::double)))
               (make-c-struct-type 'foo `((a ,<c-int>) (b ,<c-double>)))))

;; Struct type properties
(let1 s (native-type '(.struct pt (x::int y::int)))
  (test* "native-type struct is <c-struct>" #t
         (is-a? s <c-struct>))
  (test* "native-type struct tag" 'pt
         (~ s'tag)))

;; Struct with nested array field
(test* "native-type struct with array field" #t
       (equal? (native-type '(.struct foo (a::int
                                           b::(.array char (8)))))
               (make-c-struct-type
                'foo
                `((a ,<c-int>)
                  (b ,(make-c-array-type <c-char> '(8)))))))

;; Struct with pointer field
(test* "native-type struct with pointer field" #t
       (let1 s (native-type '(.struct node (val::int next::int*)))
         (and (is-a? s <c-struct>)
              (eq? (~ s'tag) 'node)
              (equal? (cadr (assq 'next (~ s'fields)))
                      (make-c-pointer-type <c-int>)))))

;; Struct equivalence: two identical signatures produce equal types
(test* "native-type struct equivalence" #t
       (equal? (native-type '(.struct s1 (a::int b::double)))
               (native-type '(.struct s1 (a::int b::double)))))

;; Struct inequivalence: different tags
(test* "native-type struct different tags" #f
       (equal? (native-type '(.struct s1 (a::int)))
               (native-type '(.struct s2 (a::int)))))

;; Unions
(test* "native-type (.union u2 (x::int y::double))" #t
       (equal? (native-type '(.union u2 (x::int y::double)))
               (make-c-union-type 'u2 `((x ,<c-int>) (y ,<c-double>)))))
(test* "native-type (.union u1 (a :: int  b :: float))" #t
       (equal? (native-type '(.union u1 (a :: int b :: float)))
               (make-c-union-type 'u1 `((a ,<c-int>) (b ,<c-float>)))))

;; Union type properties
(let1 u (native-type '(.union val (i::int f::float)))
  (test* "native-type union is <c-union>" #t
         (is-a? u <c-union>))
  (test* "native-type union tag" 'val
         (~ u'tag)))

;; All union field offsets should be 0
(let1 u (native-type '(.union uu (a::int b::double c::int8_t)))
  (define native-type-offset*
    (with-module gauche.ctype native-type-offset))
  (test* "native-type union offsets all zero" '(0 0 0)
         (map (cut native-type-offset* u <>) '(a b c))))

;; Function types

(test* "native-type (.function (int int) double)" #t
       (equal? (native-type '(.function (int int) double))
               (make-c-function-type <c-double> `(,<c-int> ,<c-int>))))

(test* "native-type (.function (int char*) void)" #t
       (equal? (native-type '(.function (int char*) void))
               (make-c-function-type
                <void>
                `(,<c-int> ,(make-c-pointer-type <c-char>)))))

;; Varargs function
(test* "native-type (.function (int ...) int)" #t
       (equal? (native-type '(.function (int ...) int))
               (make-c-function-type <c-int> `(,<c-int> ...))))

;; Function type properties
(let1 f (native-type '(.function (int double) float))
  (test* "native-type function is <c-function>" #t
         (is-a? f <c-function>))
  (test* "native-type function return-type" #t
         (eq? (~ f'return-type) <c-float>))
  (test* "native-type function arg-types" #t
         (equal? (~ f'arg-types) `(,<c-int> ,<c-double>))))

;; No-arg function
(test* "native-type (.function () int)" #t
       (equal? (native-type '(.function () int))
               (make-c-function-type <c-int> '())))

;; Nested compound types

;; Array of pointers (via nested native-type call in element type)
(test* "native-type (.array int* (4))" #t
       (equal? (native-type '(.array int* (4)))
               (make-c-array-type (make-c-pointer-type <c-int>) '(4))))

;; Struct containing struct (via nested (.struct ...) in field type)
(test* "native-type nested struct" #t
       (let1 outer (native-type '(.struct outer
                                   (pos::(.struct point (x::int y::int))
                                    val::double)))
         (and (is-a? outer <c-struct>)
              (is-a? (cadr (assq 'pos (~ outer'fields))) <c-struct>)
              (eq? (~ (cadr (assq 'pos (~ outer'fields))) 'tag) 'point))))

;; Struct containing array
(test* "native-type struct with 2d array" #t
       (let1 s (native-type '(.struct matrix (data::(.array double (3 3))
                                              name::int)))
         (and (is-a? s <c-struct>)
              (is-a? (cadr (assq 'data (~ s'fields))) <c-array>)
              (equal? (~ (cadr (assq 'data (~ s'fields))) 'dimensions)
                      '(3 3)))))

;; Integration with existing make-* constructors
;; Verify that types produced by native-type work correctly with
;; the existing native-ref/native-set! infrastructure.

(let* ([data (u8vector-copy *fobject-storage*)]
       [s1 (native-type '(.struct s1 (a::int8_t
                                      b::uint32_t
                                      c::uint16_t
                                      d::uint8_t)))]
       [s1-manual (make-c-struct-type 's1
                    `((a ,<c-int8>) (b ,<c-uint32>) (c ,<c-uint16>) (d ,<c-uint8>)))])
  (define (bc pos type) (uvector->native-handle data type pos))

  (test* "native-type struct equals manual struct" #t
         (equal? s1 s1-manual))

  ;; Size and alignment should match
  (test* "native-type struct size matches" #t
         (= (~ s1'size) (~ s1-manual'size)))
  (test* "native-type struct alignment matches" #t
         (= (~ s1'alignment) (~ s1-manual'alignment)))

  ;; Can actually dereference using the native-type constructed type
  (test* "native-type struct ref works"
         (native-ref (bc 0 s1-manual) 'a)
         (native-ref (bc 0 s1) 'a))
  (test* "native-type struct ref field b"
         (native-ref (bc 0 s1-manual) 'b)
         (native-ref (bc 0 s1) 'b)))

;; native-type pointer used with native-ref
(let* ([data (u8vector-copy *fobject-storage*)]
       [int32* (native-type 'int32_t*)]
       [int32*-manual (make-c-pointer-type <c-int32>)])
  (define (bc pos type) (uvector->native-handle data type pos))

  (test* "native-type pointer equals manual" #t
         (equal? int32* int32*-manual))
  (test* "native-type pointer deref works"
         (native-ref (bc 0 int32*-manual) 0)
         (native-ref (bc 0 int32*) 0)))

;; native-type array used with native-ref
(let* ([data (u8vector-copy *fobject-storage*)]
       [u8a (native-type '(.array uint8_t (4 4)))]
       [u8a-manual (make-c-array-type <c-uint8> '(4 4))])
  (define (bc pos type) (uvector->native-handle data type pos))

  (test* "native-type array equals manual" #t
         (equal? u8a u8a-manual))
  (test* "native-type array ref works"
         (native-ref (bc 0 u8a-manual) '(0 0))
         (native-ref (bc 0 u8a) '(0 0)))
  (test* "native-type array ref (1 2)"
         (native-ref (bc 0 u8a-manual) '(1 2))
         (native-ref (bc 0 u8a) '(1 2))))

(test* "native-type unknown symbol" (test-error)
       (native-type 'nonexistent_type_xyz))

(test* "native-type invalid signature" (test-error)
       (native-type 42))

(test* "native-type invalid field spec"
       (test-error <error> #/missing type for field/)
       (native-type '(.struct bad (invalid_no_separator))))

(test-end)
