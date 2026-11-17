;;
;; testing gauche.ffi
;;

(use gauche.test)
(use gauche.uvector)

(test-start "FFI")

(use gauche.ffi)
(test-module 'gauche.ffi)

;;;---------------------------------------------------
(test-section "with-ffi")

(define-module ffi-test-sandbox
  (use gauche.test)
  (use gauche.ffi)
  (use gauche.native-type)
  (use gauche.uvector)

  (define-type foo (native-type
                    '(.struct foo (c::char i::int s::short
                                           l::long f::float d::double))))
  (define-type foo* (make-c-pointer-type foo))

  (define-type bar (native-type `(.array int32_t (4))))

  (define-syntax do-test-f
    (syntax-rules ::: ()
      [(_ opts)
       (let-syntax ([t (syntax-rules ()
                         [(_ expect expr)
                          (test* #"f ~'opts ~'expr" expect expr)])])
         (test* #"with-ffi f ~'opts" 'ok
                (begin
                  (eval
                   `(with-ffi (dlopen "./f") opts
                      ;; Primitive interface
                      (define-c-function F-c () 'char)
                      (define-c-function F-i () 'int)
                      (define-c-function F-ul () 'u_long)
                      (define-c-function F-ull () 'ulonglong)
                      (define-c-function F-f () 'float)
                      (define-c-function F-d () 'double)
                      (define-c-function F-v () 'void)
                      (define-c-function Fi-i '(int) 'int)
                      (define-c-function Ff-f '(float) 'float)

                      (define-c-function Fi_i8 '(int) 'int8_t)
                      (define-c-function Fi_i16 '(int) 'int16_t)
                      (define-c-function Fi_i32 '(int) 'int32_t)
                      (define-c-function Fi_i64 '(int) 'int64_t)
                      (define-c-function Fu_u8 '(u_int) 'uint8_t)
                      (define-c-function Fu_u16 '(u_int) 'uint16_t)
                      (define-c-function Fu_u32 '(u_int) 'uint32_t)
                      (define-c-function Fu_u64 '(u_int) 'uint64_t)
                      (define-c-function Fll_ll '(longlong) 'longlong)

                      (define-c-function Ffff-f '(float float float) 'float)
                      (define-c-function Fd-d '(double) 'double)
                      (define-c-function Fddd-d '(double double double) 'double)
                      (define-c-function Fifd-d'(int float double) 'double)
                      (define-c-function Fifd-f'(int float double) 'float)
                      (define-c-function Fidf-d '(int double float) 'double)
                      (define-c-function Fidf-f '(int double float) 'float)
                      ;; mixing normal form
                      (define (Fiii) (Fi-i (F-i)))

                      ;; using native-type instance
                      (define-c-function Gd-d `(,<double>) <double>)

                      ;; varargs
                      (define-c-function Fivar '(int ...) 'int)
                      (define-c-function Fdvar '(int ...) 'double)
                      (define-c-function Fidfvar '(int ...) 'double)

                      ;; spill tests
                      (define-c-function Fiiiiiii-i '(int int int int int int int) 'int)
                      (define-c-function Fddddddddd-d '(double double double double double double double double double) 'double)
                      (define-c-function Fiiiiiiidddd-d '(int int int int int int int double double double double) 'double)

                      ;; c-string
                      (define-c-function Fs-i '(c-string) 'int)
                      (define-c-function Fi-s '(int) 'c-string)

                      ;; null pointer passing
                      (define-c-function Fpnull-i '(void*) 'int)

                      ;; struct pointer passing
                      (define-c-function F-pstruct-c-pstruct `(,foo* char) foo*)
                      (define-c-function F-pstruct-s-pstruct `(,foo* short) foo*)
                      (define-c-function F-pstruct-i-pstruct `(,foo* int) foo*)
                      (define-c-function F-pstruct-l-pstruct `(,foo* long) foo*)
                      (define-c-function F-pstruct-f-pstruct `(,foo* float) foo*)
                      (define-c-function F-pstruct-d-pstruct `(,foo* double) foo*)

                      ;; array
                      (define-c-function Fia-i `(int ,bar) 'int)

                      ;; '%' prefix
                      (define-c-function %say-ok '() <c-string>)
                      )
                   (current-module))
                  'ok))

         (t #\x09 (F-c))
         (t 42 (F-i))
         (t (- (expt 2 (* 8 (~ <ulong>'size))) 1) (F-ul))
         (t (- (expt 2 (* 8 (~ <ulonglong>'size))) 1) (F-ull))
         (t 1.25 (F-f))
         (t 3.14 (F-d))
         (t (undefined) (F-v))
         ;(test* "F_o" 'foo (F-o))
         (t 101 (Fi-i 100))
         (t -99 (Fi-i -100))
         (t 0.125 (Ff-f 0.25))

         (t 23 (Fi_i8 23))
         (t -45 (Fi_i8 -45))
         (t 2345 (Fi_i16 2345))
         (t -4567 (Fi_i16 -4567))
         (t 234567 (Fi_i32 234567))
         (t -456789 (Fi_i32 -456789))
         (t 234562345 (Fi_i64 234562345))
         (t -256784567 (Fi_i64 -256784567))
         (t 234 (Fu_u8 234))
         (t 40000 (Fu_u16 40000))
         (t #x7fff_ffff (Fu_u32 #x7FFF_FFFF))
         (t #x7fff_ffff (Fu_u64 #x7FFF_FFFF))
         (let* ([llmax (- (expt 2 (- (* 8 (~ <longlong>'size)) 1)) 1)]
                [llmin (- (- llmax) 1)])
           (t llmax (Fll_ll llmax))
           (t llmin (Fll_ll llmin)))

         ;(test* "Foo_o" '(a . b) (Foo-o 'a 'b))
         (t 0.875 (Ffff-f 0.5 0.25 0.125))
         (t 1.2 (Fd-d 0.6))
         (t 3.5 (Fddd-d 0.5 1.0 2.0))
         (t -24.0 (Fifd-d 32 -0.5 -0.25))
         (t 24.0 (Fifd-f 32 -0.5 -0.25))
         (t -8.0 (Fidf-d 32 -0.5 -0.25))
         (t 8.0 (Fidf-f 32 -0.5 -0.25))
         (t 2.4 (Gd-d 0.6))
         (t 43 (Fiii))

         (t 45 (Fivar 9 1 2 3 4 5 6 7 8 9))
         (t 4.5 (Fdvar 9 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9))

         ;; mixed varargs: cnt pairs of (int n, double x), returns sum of n*x
         ;; 1*2.0 + 2*3.0 + 3*4.0 = 20.0
         (t 20.0 (Fidfvar 3 1 2.0 2 3.0 3 4.0))

         ;; spill tests
         ;; 7 int args: a + 2b + 3c + ... + 7g with (1..7) => 140
         (t 140 (Fiiiiiii-i 1 2 3 4 5 6 7))
         ;; 9 double args: a + 2b + ... + 9i with (1.0..9.0) => 285.0
         (t 285.0 (Fddddddddd-d 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0))
         ;; 7 ints + 4 doubles, sum all: 28 + 10 = 38.0
         (t 38.0 (Fiiiiiiidddd-d 1 2 3 4 5 6 7 1.0 2.0 3.0 4.0))

         ;; c-string passing and returning
         (t 5 (Fs-i "hello"))
         (t "two" (Fi-s 2))

         ;; null pointer passing
         (t 1 (Fpnull-i (null-pointer-handle)))
         (t 1 (Fpnull-i (null-pointer-handle (native-type 'int*))))
         (t 0 (let1 data (make-u8vector (~ foo'size))
                (Fpnull-i (make-native-handle foo* data))))

         ;; array pasing
         (t 0 (Fia-i 0 (null-pointer-handle (native-type 'int*))))
         (t 6 (Fia-i 3 (make-native-handle bar '#s32(1 2 3 4))))
         (t 10 (Fia-i 4 (make-native-handle bar '#s32(1 2 3 4))))

         ;; '%' prefix
         (t "ok" (%say-ok))

         (let* ([p (make-native-handle foo*)])
           (set! (native. p 'c) #\a)
           (t #\b (let ([r (F-pstruct-c-pstruct p #\b)])
                    (native. r 'c)))
           (set! (native. p 's) 12345)
           (t -23456 (let ([r (F-pstruct-s-pstruct p -23456)])
                       (native. r 's)))
           (set! (native. p 'i) 123456789)
           (t -987654321 (let ([r (F-pstruct-i-pstruct p -987654321)])
                           (native. r 'i)))
           (cond
            [(>= (~ <long>'size) 8)
             (set! (native. p 'l) 123456789012)
             (t -987654321098
                (let ([r (F-pstruct-l-pstruct p -987654321098)])
                  (native. r 'l)))]
            [else
             (set! (native. p 'l) 123456789)
             (t -987654321
                (let ([r (F-pstruct-l-pstruct p -987654321)])
                  (native. r 'l)))])
           (set! (native. p 'f) 0.5)
           (t -0.25
              (let ([r (F-pstruct-f-pstruct p -0.25)])
                (native. r 'f)))
           (set! (native. p 'd) 0.5)
           (t -0.25
              (let ([r (F-pstruct-d-pstruct p -0.25)])
                (native. r 'd))))
         )]))

  (define-syntax do-test-g
    (syntax-rules ::: ()
      [(_ opts)
       (let-syntax ([t (syntax-rules ()
                         [(_ expect expr)
                          (test* #"g ~'opts ~'expr" expect expr)])])
         (test* #"with-ffi g ~'opts" 'ok
                (begin
                  (eval
                   `(with-ffi (dlopen "./g") opts
                      (define-c-function F-o '() <top>)
                      (define-c-function Foo-o `(,<top> ,<top>) <top>)
                      (define-c-function Foooooooooo-o
                        '(,<top> ,<top> ,<top> ,<top> ,<top>
                                 ,<top> ,<top> ,<top> ,<top> ,<top>)
                        <top>)
                      (define-c-function Fcb `(,<top>) <top>)
                      (define-c-function Fcb-spill9
                        '(,<top> ,<top> ,<top> ,<top> ,<top>
                                 ,<top> ,<top> ,<top> ,<top> ,<top>)
                        <top>)
                      (define-c-function Fcb-spill10
                        '(,<top> ,<top> ,<top> ,<top> ,<top>
                                 ,<top> ,<top> ,<top> ,<top> ,<top> ,<top>)
                        <top>)
                      )
                   (current-module))
                  'ok))

         (t 'foo (F-o))
         (t '(a . b) (Foo-o 'a 'b))
         (t '(9 8 7 6 5 4 3 2 1 0)
            (Foooooooooo-o 0 1 2 3 4 5 6 7 8 9))
         (t 11 (Fcb (^[x] (+ x 10))))
         (t '(8 7 6 5 4 3 2 1 0)
            (Fcb-spill9 0 1 2 3 4 5 6 7 8 reverse))
         (t '(9 8 7 6 5 4 3 2 1 0)
            (Fcb-spill10 0 1 2 3 4 5 6 7 8 9 reverse))

         (test* #"recursion via FFI ~'opts" 99
                (let ([k 0])
                  (define (f n)
                    (if (= k 10)
                      99
                      (begin (inc! k) (Fcb f))))
                  (Fcb f)))

         (test* #"error and codepad memory management ~'opts" #t
                  (let1 proc (lambda (_) (error "wow"))
                    (dotimes [2000]
                      (guard (e [else #t])
                        (Fcb proc)))
                    #t))
         )]))

  ;; define-c-callback (currently only supported in :stub)
  (define-syntax do-test-cb
    (syntax-rules ::: ()
      [(_ opts)
       (let-syntax ([t (syntax-rules ()
                         [(_ expect expr)
                          (test* #"cb ~'opts ~'expr" expect expr)])])
         (test* #"with-ffi cb ~'opts" 'ok
                (begin
                  (eval
                   `(with-ffi (dlopen "./f") opts
                      (define-c-function Fcb2-i '(void* int int) 'int)
                      (define-c-function Fcb2-d '(void* double double) 'double)
                      (define-c-function Fcb-v-count '(void* int) 'int)
                      (define-c-function Fcb-pi-i '(void* int* int) 'int)

                      (define-c-callback cb-add ((x 'int) (y 'int)) 'int
                        (+ x y))
                      (define-c-callback cb-mul-d ((x 'double) (y 'double)) 'double
                        (* x y))
                      (define-c-callback cb-noop () 'void
                        (set! cb-noop-counter (+ cb-noop-counter 1)))
                      (define-c-callback cb-pderef ((p 'int*) (i 'int)) 'int
                        (native-aref p i))
                      (define-c-callback cb-bad ((x 'int) (y 'int)) 'int
                        (error "callback-failure")))
                   (current-module))
                  'ok))

         (test* #"with-ffi cb - passing ScmObj as void*" 'ok
                (begin
                  (eval
                   `(with-ffi (dlopen "./g") opts
                      (define-c-function Fopaque_data
                        `((.function (ScmObj) ScmObj)
                          ScmObj)
                        <top>)
                      (define-c-callback cb-opaque ((obj 'ScmObj)) 'ScmObj
                        (list obj obj)))
                   (current-module))
                  'ok))

         (t 7  (Fcb2-i cb-add 3 4))
         (t 12.5 (Fcb2-d cb-mul-d 2.5 5.0))
         (t 5  (begin (set! cb-noop-counter 0)
                      (Fcb-v-count cb-noop 5)))
         (t 5  cb-noop-counter)

         ;; pointer arg test: pass int array, callback dereferences with
         ;; native-aref
         (let* ([uv (u32vector 10 20 30 40 50)]
                [p (make-native-handle (native-type 'int*) uv)])
           (t 30 (Fcb-pi-i cb-pderef p 2))
           (t 50 (Fcb-pi-i cb-pderef p 4)))

         ;; <name> is bound to a c-function native handle
         (t #t (c-function-handle? cb-add))

         ;; Passing ScmObj through opaque pointer
         (t '(can can) (Fopaque_data cb-opaque 'can))

         ;; exception propagates from callback through C and back
         (test* #"cb ~'opts exception propagation" 'caught
                (guard (e [else 'caught])
                  (Fcb2-i cb-bad 1 2)))
         )]))

  (define cb-noop-counter 0)

  (parameterize ([default-ffi-subsystem :stub])
    (do-test-f ())
    (do-test-g ())
    (do-test-cb ()))
  (when (ffi-subsystem-available? :native)
    (do-test-f (:subsystem :native))
    (do-test-g (:subsystem :native))
    (do-test-cb (:subsystem :native)))
  )

;;;----------------------------------------------------------
(test-section "define-c-constant")

;; The same set of tests is run under each subsystem, in its own module so
;; that the constant bindings don't interfere.
(define-macro (define-const-test-module modname subsystem)
  `(define-module ,modname
     (use gauche.test)
     (use gauche.ffi)
     (use gauche.native-type)
     (use file.util)

     ;; Directory holding c/ffi-const.h.  Derived from the test file
     ;; location so that it works with out-of-tree builds as well.
     (define c-dir (build-path (sys-dirname (current-load-path)) "c"))

     (define (tname name) (format "~a (~s)" name ,subsystem))

     (parameterize ([default-ffi-subsystem ,subsystem])
       ;; Constants from the system headers; no include path needed.
       (eval '(with-ffi #f (:c-headers ("limits.h" "stdio.h"))
                (define-c-constant CHAR-BIT)
                (define-c-constant SEEK-END 'int)
                (define-c-constant EOF 'int))
             (current-module))
       ;; Constants from our own header, reached via :c-include-paths.
       (eval `(with-ffi #f (:c-headers ("ffi-const.h")
                            :c-include-paths (,c-dir))
                (define-c-constant FFI-TEST-MAX-VALUE)
                (define-c-constant FFI-TEST-NEG)
                (define-c-constant FFI-TEST-GREETING 'c-string)
                (define-c-constant FFI-TEST-BLUE 'int)
                (define-c-constant FFI-TEST-DOUBLE 'double)
                (define-c-constant FFI-TEST-NEG-DOUBLE 'double)
                (define-c-constant FFI-TEST-FLOAT 'float)
                (define-c-constant FFI-TEST-TENTH-D 'double)
                (define-c-constant FFI-TEST-TENTH-F 'float)
                (define-c-constant FFI-TEST-INT-AS-DOUBLE 'double)
                (define-c-constant FFI-TEST-GREEN
                  (make-c-enum-type 'ffi_test_color <int16> '()))
                (define-c-constant FFI-TEST-CHAR <c-char>)
                (define-c-constant FFI-TEST-TRUE <c-bool>))
             (current-module))
       ;; Flonum constants from the system header.
       (eval '(with-ffi #f (:c-headers ("float.h"))
                (define-c-constant DBL-EPSILON 'double)
                (define-c-constant FLT-EPSILON 'float)
                (define-c-constant DBL-MAX <double>))
             (current-module))
       (eval '(with-ffi #f (:c-headers ("stdio.h"))
                (define-c-constant SEEK-SET <c-bool>))
             (current-module)))

     (test* (tname "define-c-constant, type omitted") 8 CHAR-BIT)
     (test* (tname "define-c-constant, name translation") 2 SEEK-END)
     (test* (tname "define-c-constant, negative value") -1 EOF)
     (test* (tname "define-c-constant, user header") 65535 FFI-TEST-MAX-VALUE)
     (test* (tname "define-c-constant, parenthesized macro") -3 FFI-TEST-NEG)
     (test* (tname "define-c-constant, c-string") "hello, ffi"
            FFI-TEST-GREETING)
     (test* (tname "define-c-constant, enum member") 2 FFI-TEST-BLUE)
     (test* (tname "define-c-constant, double") 2.5 FFI-TEST-DOUBLE)
     (test* (tname "define-c-constant, negative double") -0.125
            FFI-TEST-NEG-DOUBLE)
     (test* (tname "define-c-constant, float") 0.75 FFI-TEST-FLOAT)
     (test* (tname "define-c-constant, float is a flonum") #t
            (flonum? FFI-TEST-FLOAT))
     (test* (tname "define-c-constant, double keeps double precision") 0.1
            FFI-TEST-TENTH-D)
     (test* (tname "define-c-constant, float rounds to float precision")
            (let1 v (make-f32vector 1 0.1) (f32vector-ref v 0))
            FFI-TEST-TENTH-F)
     (test* (tname "define-c-constant, integer macro as double") 3.0
            FFI-TEST-INT-AS-DOUBLE)
     (test* (tname "define-c-constant, integer macro as double is a flonum")
            #t (flonum? FFI-TEST-INT-AS-DOUBLE))
     (test* (tname "define-c-constant, enum type") 1 FFI-TEST-GREEN)
     (test* (tname "define-c-constant, c-bool") '(#t #f)
            (list FFI-TEST-TRUE SEEK-SET))
     (test* (tname "define-c-constant, c-char") #\A FFI-TEST-CHAR)
     (test* (tname "define-c-constant, DBL_EPSILON") (flonum-epsilon)
            DBL-EPSILON)
     (test* (tname "define-c-constant, FLT_EPSILON") (expt 2.0 -23)
            FLT-EPSILON)
     (test* (tname "define-c-constant, DBL_MAX") (greatest-positive-flonum)
            DBL-MAX)

     (test* (tname "define-c-constant binds a constant") (test-error <error>)
            (eval '(set! EOF 0) (current-module)))
     (test* (tname "define-c-constant binding is constant") #t
            ((with-module gauche.internal gloc-const?)
             ((with-module gauche.internal find-binding)
              (current-module) 'EOF #f)))
     (test* (tname "define-c-constant, later-compiled reference") -1
            ((eval '(lambda () EOF) (current-module))))

     (test* (tname "define-c-constant rejects pointer type")
            (test-error <error> #/pointer, array and function types/)
            (let1 m (make-module #f)
              (parameterize ([default-ffi-subsystem ,subsystem])
                (eval '(use gauche.ffi) m)
                (eval '(with-ffi #f (:c-headers ("stdio.h"))
                         (define-c-constant EOF 'int*))
                      m))))

     (test* (tname "define-c-constant outside with-ffi") (test-error <error>)
            (eval '(define-c-constant EOF) (current-module)))
     ))

(define-const-test-module ffi-const-stub-sandbox :stub)
(when (ffi-subsystem-available? :native)
  (eval '(define-const-test-module ffi-const-native-sandbox :native)
        (current-module)))

;; Tests specific to the native subsystem.
(when (ffi-subsystem-available? :native)
  (define-module ffi-const-native-error-sandbox
    (use gauche.test)
    (use gauche.ffi)
    (use gauche.native-type)
    (use file.util)

    (define c-dir (build-path (sys-dirname (current-load-path)) "c"))

    ;; The constants are available to the body forms at load time.
    ;; (The stub subsystem evaluates the body before the constants.)
    (let1 m (make-module #f)
      (eval '(use gauche.ffi) m)
      (eval '(with-ffi #f (:subsystem :native :c-headers ("stdio.h"))
               (define-c-constant EOF 'int)
               (define eof-in-body (list EOF)))
            m)
      (test* "define-c-constant (native), used in body" '(-1)
             (eval 'eof-in-body m)))

    (define-syntax native-const-error
      (syntax-rules ()
        [(_ name rx headers ccst ...)
         (test* name (test-error <error> rx)
                (eval `(with-ffi #f (:subsystem :native
                                     :c-headers headers
                                     :c-include-paths (,c-dir))
                         ccst ...)
                      (current-module)))]))

    (native-const-error "define-c-constant (native), fixnum overflow"
                        #/LONG-MAX \(C name: LONG_MAX\) doesn't fit in a fixnum/
                        ("limits.h")
                        (define-c-constant LONG-MAX))
    (native-const-error "define-c-constant (native), non-string as c-string"
                        #/FFI-TEST-MAX-VALUE .* is not a string constant/
                        ("ffi-const.h")
                        (define-c-constant FFI-TEST-MAX-VALUE 'c-string))
    (native-const-error "define-c-constant (native), string as integer"
                        #/FFI-TEST-GREETING .* is a string constant/
                        ("ffi-const.h")
                        (define-c-constant FFI-TEST-GREETING 'int))
    (native-const-error "define-c-constant (native), undefined name"
                        #/NO-SUCH-CONST \(C name: NO_SUCH_CONST\) is not found in the headers/
                        ("stdio.h")
                        (define-c-constant NO-SUCH-CONST))
    (native-const-error "define-c-constant (native), function name"
                        #/fopen \(C name: fopen\) is not a constant/
                        ("stdio.h")
                        (define-c-constant fopen))
    (native-const-error "define-c-constant (native), variable name"
                        #/\(C name: stdin\)( is not a constant|: Unevaluatable initializer)/
                        ("stdio.h")
                        (define-c-constant stdin))
    (native-const-error "define-c-constant (native), missing header"
                        #/Can't process the headers given by :c-headers/
                        ("stdio.h" "no-such-header-for-ffi-test.h")
                        (define-c-constant EOF 'int))
    (native-const-error "define-c-constant (native), multiple failures"
                        #/NO-SUCH-1 .*\n.*NO-SUCH-2/
                        ("stdio.h")
                        (define-c-constant NO-SUCH-1)
                        (define-c-constant NO-SUCH-2))
    ))

;;;----------------------------------------------------------
(test-section "define-c-enum")

;; The same set of tests is run under each subsystem, in its own module so
;; that the bindings don't interfere.
(define-macro (define-enum-test-module modname subsystem)
  `(define-module ,modname
     (use gauche.test)
     (use gauche.ffi)
     (use gauche.native-type)
     (use file.util)

     (define c-dir (build-path (sys-dirname (current-load-path)) "c"))

     (define (tname name) (format "~a (~s)" name ,subsystem))

     ;; Evaluate with-ffi form in a fresh module, under the subsystem.
     (define (eval-ffi form)
       (let1 m (make-module #f)
         (parameterize ([default-ffi-subsystem ,subsystem])
           (eval '(use gauche.ffi) m)
           (eval '(use gauche.native-type) m)
           (eval form m))))

     (parameterize ([default-ffi-subsystem ,subsystem])
       (eval `(with-ffi #f (:c-headers ("ffi-const.h")
                            :c-include-paths (,c-dir))
                ;; tag derived from the name
                (define-c-enum ffi-test-color
                  (FFI-TEST-RED FFI-TEST-GREEN FFI-TEST-BLUE))
                ;; tag given explicitly
                (define-c-enum (flags ffi_test_flags)
                  (FFI-TEST-F-A FFI-TEST-F-B FFI-TEST-F-WIDE))
                ;; anonymous, with an explicit base type
                (define-c-enum (anon #f) (FFI-TEST-ANON-X FFI-TEST-ANON-Y)
                  'uint8_t)
                ;; signed base type
                (define-c-enum (signed-e ffi_test_signed)
                  (FFI-TEST-S-LO FFI-TEST-S-HI) 'int16_t))
             (current-module)))

     (test* (tname "define-c-enum binds enumerators") '(0 1 2)
            (list FFI-TEST-RED FFI-TEST-GREEN FFI-TEST-BLUE))
     (test* (tname "define-c-enum enumerators are constants")
            (test-error <error>)
            (eval '(set! FFI-TEST-RED 3) (current-module)))

     (test* (tname "define-c-enum binds the enum set") #t
            (c-enum-type? ffi-test-color))
     (test* (tname "define-c-enum derives the tag from the name")
            'ffi_test_color
            (c-enum-type-tag ffi-test-color))
     (test* (tname "define-c-enum name->value") 1
            (c-enum-value ffi-test-color 'FFI-TEST-GREEN))
     (test* (tname "define-c-enum value->name") 'FFI-TEST-BLUE
            (c-enum-symbol ffi-test-color 2))
     (test* (tname "define-c-enum name->value, unknown") (test-error <error>)
            (c-enum-value ffi-test-color 'FFI-TEST-MAUVE))
     (test* (tname "define-c-enum value->name, unknown") (test-error <error>)
            (c-enum-symbol ffi-test-color 99))
     (test* (tname "define-c-enum enumerator alist")
            '((FFI-TEST-RED . 0) (FFI-TEST-GREEN . 1) (FFI-TEST-BLUE . 2))
            (c-enum-type-enumerator-alist ffi-test-color))

     ;; A value beyond int; the reified value must still be exact.
     (test* (tname "define-c-enum explicit tag") 'ffi_test_flags
            (c-enum-type-tag flags))
     (test* (tname "define-c-enum wide enumerator") #x80000000
            FFI-TEST-F-WIDE)
     (test* (tname "define-c-enum wide enumerator, via the set") #x80000000
            (c-enum-value flags 'FFI-TEST-F-WIDE))

     ;; Base type fixes size and alignment; no tag at all.
     (test* (tname "define-c-enum anonymous") #f (c-enum-type-tag anon))
     (test* (tname "define-c-enum base type") '(1 1 10 20)
            (list (~ anon'size) (~ anon'alignment)
                  FFI-TEST-ANON-X FFI-TEST-ANON-Y))

     ;; With a base type the boxer is chosen at code generation time from
     ;; its signedness; without one, at runtime from the enumerator itself.
     ;; Both must reify the value exactly.
     (test* (tname "define-c-enum signed base type") '(2 -5 5)
            (list (~ signed-e'size) FFI-TEST-S-LO FFI-TEST-S-HI))
     (test* (tname "define-c-enum signed base type, via the set") -5
            (c-enum-value signed-e 'FFI-TEST-S-LO))

     ;; The enum type is usable as an FFI argument type: it is passed as its
     ;; base type, so the generated stub compiles without naming the tag.
     (parameterize ([default-ffi-subsystem ,subsystem])
       (eval `(with-ffi (dlopen "./f") ()
                (define-c-function Fi-i (list ffi-test-color) 'int))
             (current-module)))
     (test* (tname "c-enum as an FFI argument type") 3 (Fi-i FFI-TEST-BLUE))

     ;; A define-c-enum, and a define-c-function and a define-c-constant
     ;; that use it, in one with-ffi form.  The enum-set name has to be
     ;; bound before the cdef instances are built, so define-c-enum binds it
     ;; to a <c-enum-type> that carries the declared representation but no
     ;; enumerators yet, and the enumerators are filled in later.
     (test* (tname "c-enum declared and used in one with-ffi form")
            '(1 2 FFI-TEST-BLUE ffi_test_color 1)
            (eval-ffi
             `(begin
                (with-ffi (dlopen "./f") (:c-headers ("ffi-const.h")
                                          :c-include-paths (,c-dir))
                  (define-c-enum (color ffi_test_color)
                    (FFI-TEST-RED FFI-TEST-BLUE))
                  (define-c-function Fi-i (list color) color)
                  (define-c-constant FFI-TEST-GREEN color))
                (list (Fi-i FFI-TEST-RED)
                      (c-enum-value color 'FFI-TEST-BLUE)
                      (c-enum-symbol color 2)
                      (c-enum-type-tag color)
                      FFI-TEST-GREEN))))

     ;; The error names the enum and the enumerator at fault: the values
     ;; come from the C compiler or the headers, so what the user got wrong
     ;; is the base type.
     (test* (tname "define-c-enum rejects a base type that can't hold the values")
            (test-error <error>
                        #/enumerator FFI-TEST-F-WIDE of the enum narrow .* <uint8>/)
            (eval-ffi `(with-ffi #f (:c-headers ("ffi-const.h")
                                     :c-include-paths (,c-dir))
                         (define-c-enum (narrow ffi_test_flags)
                           (FFI-TEST-F-A FFI-TEST-F-WIDE) 'uint8_t))))

     ;; The representation is settled when the enum is declared, before the
     ;; values are known, so an enum the default can't hold has to say so.
     (test* (tname "define-c-enum rejects negative enumerators without a base type")
            (test-error <error> #/enumerator FFI-TEST-S-LO of the enum bare/)
            (eval-ffi `(with-ffi #f (:c-headers ("ffi-const.h")
                                     :c-include-paths (,c-dir))
                         (define-c-enum (bare ffi_test_signed)
                           (FFI-TEST-S-LO FFI-TEST-S-HI)))))

     (test* (tname "define-c-enum, malformed name and tag") (test-error <error>)
            (eval-ffi '(with-ffi #f ()
                         (define-c-enum (a b c) (X)))))
     (test* (tname "define-c-enum, malformed enumerator") (test-error <error>)
            (eval-ffi '(with-ffi #f ()
                         (define-c-enum foo ("X")))))

     (test* (tname "define-c-enum outside with-ffi") (test-error <error>)
            (eval '(define-c-enum foo (X)) (current-module)))
     ))

(define-enum-test-module ffi-enum-stub-sandbox :stub)
(when (ffi-subsystem-available? :native)
  (eval '(define-enum-test-module ffi-enum-native-sandbox :native)
        (current-module)))

;; Tests specific to the native subsystem.
(when (ffi-subsystem-available? :native)
  (define-module ffi-enum-native-error-sandbox
    (use gauche.test)
    (use gauche.ffi)
    (use gauche.native-type)
    (use file.util)

    (define c-dir (build-path (sys-dirname (current-load-path)) "c"))

    ;; The enumerators are available to the body forms at load time.
    (let1 m (make-module #f)
      (eval '(use gauche.ffi) m)
      (eval `(with-ffi #f (:subsystem :native
                           :c-headers ("ffi-const.h")
                           :c-include-paths (,c-dir))
               (define-c-enum ffi-test-color (FFI-TEST-BLUE))
               (define blue-in-body (list FFI-TEST-BLUE)))
            m)
      (test* "define-c-enum (native), used in body" '(2)
             (eval 'blue-in-body m)))

    ;; Without other definitions, the headers are still grokked.
    (let1 m (make-module #f)
      (eval '(use gauche.ffi) m)
      (eval `(with-ffi #f (:subsystem :native
                           :c-headers ("ffi-const.h")
                           :c-include-paths (,c-dir))
               (define-c-enum (an #f) (FFI-TEST-ANON-Y FFI-TEST-ANON-X)))
            m)
      (test* "define-c-enum (native), anonymous enum located by enumerator"
             '(20 10 ((FFI-TEST-ANON-Y . 20) (FFI-TEST-ANON-X . 10)))
             (eval '(list FFI-TEST-ANON-Y FFI-TEST-ANON-X
                          ((with-module gauche.native-type
                             c-enum-type-enumerator-alist) an))
                   m)))

    (define-syntax native-enum-error
      (syntax-rules ()
        [(_ name rx headers cdef ...)
         (test* name (test-error <error> rx)
                (let1 m (make-module #f)
                  (eval '(use gauche.ffi) m)
                  (eval `(with-ffi #f (:subsystem :native
                                       :c-headers headers
                                       :c-include-paths (,c-dir))
                           cdef ...)
                        m)))]))

    (native-enum-error "define-c-enum (native), tag not found"
                       #/define-c-enum ffi-test-colour: enum ffi_test_colour is not found/
                       ("ffi-const.h")
                       (define-c-enum ffi-test-colour (FFI-TEST-RED)))
    (native-enum-error "define-c-enum (native), tag of a struct"
                       #/enum foo is not found .* used for a struct/
                       ("f.h")
                       (define-c-enum foo (X)))
    (native-enum-error "define-c-enum (native), enumerator of another enum"
                       #/FFI-TEST-RED \(C name: FFI_TEST_RED\) is not an enumerator of enum ffi_test_signed/
                       ("ffi-const.h")
                       (define-c-enum (e ffi_test_signed)
                         (FFI-TEST-S-LO FFI-TEST-RED) 'int8_t))
    (native-enum-error "define-c-enum (native), macro as an enumerator"
                       #/FFI-TEST-MAX-VALUE \(C name: FFI_TEST_MAX_VALUE\) is not found as an enumerator/
                       ("ffi-const.h")
                       (define-c-enum ffi-test-color (FFI-TEST-RED FFI-TEST-MAX-VALUE)))
    (native-enum-error "define-c-enum (native), anonymous, not an enumerator"
                       #/NO-SUCH \(C name: NO_SUCH\) is not an enumerator/
                       ("ffi-const.h")
                       (define-c-enum (e #f) (NO-SUCH)))
    (native-enum-error "define-c-enum (native), anonymous, no enumerators"
                       #/anonymous enum needs at least one enumerator/
                       ("ffi-const.h")
                       (define-c-enum (e #f) ()))
    (native-enum-error "define-c-enum (native), missing header"
                       #/define-c-enum: Can't process the headers/
                       ("no-such-header-for-ffi-test.h")
                       (define-c-enum ffi-test-color (FFI-TEST-RED)))
    (native-enum-error "define-c-enum (native), errors with constants"
                       #/with-ffi: 3 errors:\n  define-c-constant: NO-SUCH-1 .*\n  define-c-enum ffi-test-color: NO-SUCH-2 .*\n  define-c-enum ffi-test-color: NO-SUCH-3/
                       ("ffi-const.h")
                       (define-c-constant NO-SUCH-1)
                       (define-c-enum ffi-test-color
                         (FFI-TEST-RED NO-SUCH-2 NO-SUCH-3)))
    ))

;;;----------------------------------------------------------
(test-section "foreign-function-info")

(with-module ffi-test-sandbox

  (define-module ffi-info-sandbox
    (use gauche.test)
    (use gauche.ffi)
    (use gauche.native-type)

    (define (check-info proc dlobj-rx subsystem argtypes rettype)
      (let1 info (foreign-function-info proc)
        (test* #"foreign-function-info ~subsystem returns list" #t
               (list? info))
        (test* #"foreign-function-info ~subsystem :subsystem" subsystem
               (get-keyword :subsystem info #f))
        (test* #"foreign-function-info ~subsystem :dlobj" #t
               (boolean (dlobj-rx (get-keyword :dlobj info #f))))
        (test* #"foreign-function-info ~subsystem :argtypes" argtypes
               (get-keyword :argtypes info #f))
        (test* #"foreign-function-info ~subsystem :rettype" rettype
               (get-keyword :rettype info #f))))

    (parameterize ([default-ffi-subsystem :stub])
      (eval
       `(with-ffi (dlopen "./f") ()
          (define-c-function Fi-i '(int) 'int))
       (current-module))
      (check-info Fi-i #/^\.\/f/ :stub '(int) 'int))

    (when (ffi-subsystem-available? :native)
      (eval
       `(with-ffi (dlopen "./f") (:subsystem :native)
          (define-c-function Fi-i '(int) 'int))
       (current-module))
      (check-info Fi-i #/^\.\/f/ :native '(int) 'int))

    (test* "foreign-function-info on non-ffi proc returns #f" #f
           (foreign-function-info car))
    )
  )

(test-end)
