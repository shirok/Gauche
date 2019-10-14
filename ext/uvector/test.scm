;;
;; test for srfi-4 module
;;

(use gauche.test)
(use gauche.parameter)
(use srfi-1)

(test-start "uniform vector and array")
(use gauche.uvector)
(test-module 'gauche.uvector)

(define-macro (expand-uvec tags body)
  (define (subst tag str)
    (regexp-replace-all #/@/ str (symbol->string tag)))
  (define (expand tag body)
    (cond [(symbol? body) (string->symbol (subst tag (symbol->string body)))]
          [(string? body) (subst tag body)]
          [(pair? body) (cons (expand tag (car body))
                              (expand tag (cdr body)))]
          [else body]))
  (if (list? tags)
    `(begin ,@(map (cut expand <> body) tags))
    (expand tags body)))

;;-------------------------------------------------------------------
(test-section "reader syntax")

(test* "#s8()" #t (s8vector? '#s8(0 1 2 3 4)))
(test* "#s8()" #f (s8vector? '#(0 1 2 3 4)))
(test* "#u8()" #t (u8vector? '#u8(0 1 2 3 4)))
(test* "#u8()" #f (u8vector? '#(0 1 2 3 4)))
(test* "#s16()" #t (s16vector? '#s16(0 1 2 3 4)))
(test* "#s16()" #f (s16vector? '#(0 1 2 3 4)))
(test* "#u16()" #t (u16vector? '#u16(0 1 2 3 4)))
(test* "#u16()" #f (u16vector? '#(0 1 2 3 4)))
(test* "#s32()" #t (s32vector? '#s32(0 1 2 3 4)))
(test* "#s32()" #f (s32vector? '#(0 1 2 3 4)))
(test* "#u32()" #t (u32vector? '#u32(0 1 2 3 4)))
(test* "#u32()" #f (u32vector? '#(0 1 2 3 4)))
(test* "#s64()" #t (s64vector? '#s64(0 1 2 3 4)))
(test* "#s64()" #f (s64vector? '#(0 1 2 3 4)))
(test* "#u64()" #t (u64vector? '#u64(0 1 2 3 4)))
(test* "#u64()" #f (u64vector? '#(0 1 2 3 4)))
(test* "#f16()" #t (f16vector? '#f16(0.0 1.0 2.0 3.0 4.0)))
(test* "#f16()" #f (f16vector? '#(0.0 1.0 2.0 3.0 4.0)))
(test* "#f32()" #t (f32vector? '#f32(0.0 1.0 2.0 3.0 4.0)))
(test* "#f32()" #f (f32vector? '#(0.0 1.0 2.0 3.0 4.0)))
(test* "#f64()" #t (f64vector? '#f64(0.0 1.0 2.0 3.0 4.0)))
(test* "#f64()" #f (f64vector? '#(0.0 1.0 2.0 3.0 4.0)))
(test* "#c32()" #t (c32vector? '#c32(1+i 1-i 2+2i 2-2i)))
(test* "#c32()" #f (c32vector? '#(1+i 1-i 2+2i 2-2i)))
(test* "#c64()" #t (c64vector? '#c64(1+i 1-i 2+2i 2-2i)))
(test* "#c64()" #f (c64vector? '#(1+i 1-i 2+2i 2-2i)))
(test* "#c128()" #t (c128vector? '#c128(1+i 1-i 2+2i 2-2i)))
(test* "#c128()" #f (c128vector? '#(1+i 1-i 2+2i 2-2i)))

;;-------------------------------------------------------------------
(test-section "writer syntax")

(test* "#s8()" "#s8(0 1 2 3 4)"
       (with-output-to-string
         (^[] (write (apply s8vector (iota 5))))))
(test* "#u8()" "#u8(0 1 2 3 4)"
       (with-output-to-string
         (^[] (write (apply u8vector (iota 5))))))
(test* "#s16()" "#s16(0 1 2 3 4)"
       (with-output-to-string
         (^[] (write (apply s16vector (iota 5))))))
(test* "#u16()" "#u16(0 1 2 3 4)"
       (with-output-to-string
         (^[] (write (apply u16vector (iota 5))))))
(test* "#s32()" "#s32(0 1 2 3 4)"
       (with-output-to-string
         (^[] (write (apply s32vector (iota 5))))))
(test* "#u32()" "#u32(0 1 2 3 4)"
       (with-output-to-string
         (^[] (write (apply u32vector (iota 5))))))
(test* "#s64()" "#s64(0 1 2 3 4)"
       (with-output-to-string
         (^[] (write (apply s64vector (iota 5))))))
(test* "#u64()" "#u64(0 1 2 3 4)"
       (with-output-to-string
         (^[] (write (apply u64vector (iota 5))))))
(test* "#f16()" "#f16()"
       (with-output-to-string
         (^[] (write (f16vector)))))
(test* "#f32()" "#f32()"
       (with-output-to-string
         (^[] (write (f32vector)))))
(test* "#f64()" "#f64()"
       (with-output-to-string
         (^[] (write (f64vector)))))
(test* "#c32()" "#c32()"
       (with-output-to-string
         (^[] (write (c32vector)))))
(test* "#c64()" "#c64()"
       (with-output-to-string
         (^[] (write (c64vector)))))
(test* "#c128()" "#c128()"
       (with-output-to-string
         (^[] (write (c128vector)))))

;;-------------------------------------------------------------------
(test-section "constructors")

(expand-uvec
 (u8 s8 u16 s16 u32 s32 u64 s64 f16 f32 f64 c32 c64 c128)
 (define (uvmaketest-@ inits)
   ;; zero length
   (test* "make @ 0"
          (make-uvector <@vector> 0)
          (make-@vector 0))
   ;; some content
   (dolist [init inits]
     (test* (format "make @ 10 ~a" init)
            (make-uvector <@vector> 10 init)
            (make-@vector 10 init)))))

(uvmaketest-s8 '(0 10 -4))
(uvmaketest-u8 '(0 4 255))
(uvmaketest-s16 '(0 -32768 32767))
(uvmaketest-u16 '(0 65535))
(uvmaketest-s32 '(0))
(uvmaketest-u32 '(0))
(uvmaketest-s64 '(0))
(uvmaketest-u64 '(0))
(uvmaketest-f16 '(0 1.0 -1.0)) 
(uvmaketest-f32 '(0 1.0 -1.0)) 
(uvmaketest-f64 '(0 1.0 -1.0)) 
(uvmaketest-c32 '(0 1.0+i -1.0-i)) 
(uvmaketest-c64 '(0 1.0+i -1.0-i)) 
(uvmaketest-c128 '(0 1.0+i -1.0-i)) 

;;-------------------------------------------------------------------
(test-section "ref and set")

(expand-uvec
 (u8 s8 u16 s16 u32 s32 u64 s64 f16 f32 f64 c32 c64 c128)
 (define (uvrefset-test-@ numlist expvec)
   (test* "@vector-ref|set!" #t
          (let ([vec (make-@vector (length numlist))]
                [seq (iota (length numlist))])
            (for-each (^[n i] (@vector-set! vec i n)) numlist seq)
            (and (equal? expvec vec)
                 (equal? numlist (map (^i (@vector-ref vec i)) seq))
                 (equal? numlist (map (^i (uvector-ref vec i)) seq))
                 (begin 
                   (uvector-set! vec 0 (@vector-ref expvec 1))
                   (equal? (@vector-ref expvec 1) (@vector-ref vec 0)))
                 (begin
                   (set! (uvector-ref vec 1) (@vector-ref expvec 2))
                   (equal? (@vector-ref expvec 2) (@vector-ref vec 1))))))))

(uvrefset-test-s8 '(0 -1 1 -128 127)
                  '#s8(0 -1 1 -128 127))
(uvrefset-test-u8 '(0 1 2 3 255)
                  '#u8(0 1 2 3 255))
(uvrefset-test-s16 '(0 -1 1 -32768 32767)
                   '#s16(0 -1 1 -32768 32767))
(uvrefset-test-u16 '(0 1 2 3 65535)
                   '#u16(0 1 2 3 65535))
(uvrefset-test-s32 '(0 -1 1 #x-80000000 #x7fffffff)
                   '#s32(0 -1 1 #x-80000000 #x7fffffff))
(uvrefset-test-u32 '(0 1 2 #xffffffff)
                   '#u32(0 1 2 #xffffffff))
(uvrefset-test-s64 '(0 -1 1 #x-8000000000000000 #x7fffffffffffffff)
                   '#s64(0 -1 1 #x-8000000000000000 #x7fffffffffffffff))
(uvrefset-test-u64 '(0 1 2 #xffffffffffffffff)
                   '#u64(0 1 2 #xffffffffffffffff))
(uvrefset-test-f16 '(0.0 -1.0 1.0)
                   '#f16(0.0 -1.0 1.0))
(uvrefset-test-f32 '(0.0 -1.0 1.0)
                   '#f32(0.0 -1.0 1.0))
(uvrefset-test-f64 '(0.0 -1.0 1.0)
                   '#f64(0.0 -1.0 1.0))
(uvrefset-test-c32 '(0.0 -1-i 1+i)
                   '#c32(0.0 -1-i 1+i))
(uvrefset-test-c64 '(0.0 -1-i 1+i)
                   '#c64(0.0 -1-i 1+i))
(uvrefset-test-c128 '(0.0 -1-i 1+i)
                    '#c128(0.0 -1-i 1+i))

(expand-uvec
 (u8 s8 u16 s16 u32 s32 u64 s64)
 (define (uvset-clamp-test-@ expect value)
   (test* "@vector-set! clamp" expect
          (let1 v (make-@vector 1)
            (list (with-error-handler (^e 'error)
                    (^[] (@vector-set! v 0 value) (@vector-ref v 0)))
                  (with-error-handler (^e 'error)
                    (^[] (@vector-set! v 0 value 'low) (@vector-ref v 0)))
                  (with-error-handler (^e 'error)
                    (^[] (@vector-set! v 0 value 'high) (@vector-ref v 0)))
                  (with-error-handler (^e 'error)
                    (^[] (@vector-set! v 0 value 'both) (@vector-ref v 0))))))))

(uvset-clamp-test-s8 '(error -128 error -128) -129)
(uvset-clamp-test-s8 '(error error 127 127) 128)
(uvset-clamp-test-u8 '(error 0 error 0) -1)
(uvset-clamp-test-u8 '(error error 255 255) 256)

(uvset-clamp-test-s16 '(error -32768 error -32768) -32769)
(uvset-clamp-test-s16 '(error error 32767 32767) 32768)
(uvset-clamp-test-u16 '(error 0 error 0) -1)
(uvset-clamp-test-u16 '(error error 65535 65535) 65536)

(uvset-clamp-test-s32 '(error -2147483648 error -2147483648) -2147483649)
(uvset-clamp-test-s32 '(error error 2147483647 2147483647) 2147483648)
(uvset-clamp-test-u32 '(error 0 error 0) -1)
(uvset-clamp-test-u32 '(error error 4294967295 4294967295) 4294967296)


(uvset-clamp-test-s64 '(error -9223372036854775808 error -9223372036854775808)
                      -9223372036854775809)
(uvset-clamp-test-s64 '(error error 9223372036854775807 9223372036854775807)
                      9223372036854775808)
(uvset-clamp-test-u64 '(error 0 error 0) -1)
(uvset-clamp-test-u64 '(error error 18446744073709551615 18446744073709551615)
                      18446744073709551616)

;;-------------------------------------------------------------------
(test-section "conversions")

(expand-uvec
 (u8 s8 u16 s16 u32 s32 u64 s64 f16 f32 f64 c32 c64 c128)
 (define (uvconv-test-@ uvec nums)
   (test* "@vector conversion" #t
          (let* ([lis (@vector->list uvec)]
                 [uv2 (list->@vector lis)]
                 [vec (@vector->vector  uvec)]
                 [uv3 (vector->@vector  vec)])
            (and (equal? lis nums)
                 (equal? (uvector->list uvec) nums)
                 (equal? uv2 uvec)
                 (equal? vec (list->vector nums))
                 (equal? (uvector->vector uvec) (list->vector nums))
                 (equal? uv3 uvec))))))

(uvconv-test-s8 '#s8(0 -1 1 -128 127) '(0 -1 1 -128 127))
(uvconv-test-u8 '#u8(0 1 254 255) '(0 1 254 255))
(uvconv-test-s16 '#s16(0 -1 1 -32768 32767) '(0 -1 1 -32768 32767))
(uvconv-test-u16 '#u16(0 1 65534 65535) '(0 1 65534 65535))
(uvconv-test-s32 '#s32(0 -1 1 #x-80000000 #x7fffffff)
                 '(0 -1 1 #x-80000000 #x7fffffff))
(uvconv-test-u32 '#u32(0 1 #xfffffffe #xffffffff)
                 '(0 1 #xfffffffe #xffffffff))
(uvconv-test-s64 '#s64(0 -1 1 #x-8000000000000000 #x7fffffffffffffff)
                 '(0 -1 1 #x-8000000000000000 #x7fffffffffffffff))
(uvconv-test-u64 '#u64(0 1 #xffffffffffffffff)
                 '(0 1 #xffffffffffffffff))
(uvconv-test-f16 '#f16(0.0 -1.0 1.0) '(0.0 -1.0 1.0))
(uvconv-test-f32 '#f32(0.0 -1.0 1.0) '(0.0 -1.0 1.0))
(uvconv-test-f64 '#f64(0.0 -1.0 1.0) '(0.0 -1.0 1.0))
(uvconv-test-c32 '#c32(0.0 -1.0+i 1.0-i) '(0.0 -1.0+i 1.0-i))
(uvconv-test-c64 '#c64(0.0 -1.0+i 1.0-i) '(0.0 -1.0+i 1.0-i))
(uvconv-test-c128 '#c128(0.0 -1.0+i 1.0-i) '(0.0 -1.0+i 1.0-i))

;;-------------------------------------------------------------------
(test-section "comparison")

(define (uvcomp-tester list-> samples)
  (dolist [x samples]
    (dolist [y samples]
      (test* #"equal? ~(list-> x) ~(list-> y)"
             (and (= (length x) (length y))
                  (every = x y))
             (equal? (list-> x) (list-> y))))))

(for-each (cut uvcomp-tester <> '(() (0) (1) (0 0) (0 1)))
          (list list->s8vector list->u8vector
                list->s16vector list->u16vector
                list->s32vector list->u32vector
                list->s64vector list->u64vector))

(for-each (cut uvcomp-tester <> '(() (0) (1) (0 0) (0 1)
                                  (+inf.0) (-inf.0) (+nan.0)
                                  (0 +inf.0) (0 -inf.0) (0 +nan.0)))
          (list list->f16vector
                list->f32vector
                list->f64vector))

;; srfi-66 procedures
;;  Internally we just call builtin equal? and compare, so here we just
;;  check it can be callable.
(test* "u8vector=?" #t (u8vector=? '#u8(1 2 3) '#u8(1 2 3)))
(test* "u8vector=?" (test-error) (u8vector=? '#u8(1 2 3) '#s8(1 2 3)))
(test* "u8vector-compare" -1 (u8vector-compare '#u8(2 2) '#u8(1 2 3)))
(test* "u8vector-compare" (test-error) (u8vector-compare '#s8(2 2) '#u8(1 2 3)))

;;-------------------------------------------------------------------
(test-section "copying and filling")

(expand-uvec 
 (u8 s8 u16 s16 u32 s32 u64 s64 f16 f32 f64 c32 c64 c128)
 (define (uvcopy-test-@ uvec filler)
   (test* "@vector-copy|fill!" #t
          (let* ([c0 (list->@vector (@vector->list uvec))]
                 [c1 (@vector-copy uvec)]
                 [c2 (uvector-copy uvec)]
                 [cr (@vector-reverse-copy uvec)]
                 [len (@vector-length uvec)]
                 [cb (make-@vector (* len  2) filler)])
            (and (equal? c1 uvec)
                 (equal? c2 uvec)
                 (equal? (@vector->list cr)
                         (reverse (@vector->list uvec)))
                 (begin (@vector-fill! c1 filler)
                        (and (equal? c0 uvec)
                             (every (^n (= n filler))  (@vector->list c1))
                             (begin (@vector-copy! c1 uvec)
                                    (equal? c1 c0))))
                 (begin (@vector-reverse-copy! cb 1 cr 1)
                        (and (equal? (@vector-ref cb 0) filler)
                             (equal? (@vector-copy cb 1 len)
                                     (@vector-copy uvec 0 (- len 1)))
                             (equal? (@vector-ref cb len) filler))))))))


(uvcopy-test-s8 '#s8(0 -1 1 -128 127) -128)
(uvcopy-test-u8 '#u8(0 1 255) 255)
(uvcopy-test-s16 '#s16(0 -1 1 -32768 32767) -32768)
(uvcopy-test-u16 '#u16(0 1 65535) 32768)
(uvcopy-test-s32 '#s32(0 -1 1 #x-80000000 #x7fffffff) #x7fffffff)
(uvcopy-test-u32 '#u32(0 1 #xffffffff) #x80000000)
(uvcopy-test-s64 '#s64(0 -1 1 #x-8000000000000000 #x7fffffffffffffff)
                 #x7fffffffffffffff)
(uvcopy-test-u64 '#u64(0 1 #xffffffffffffffff) #x8000000000000000)
(uvcopy-test-f16 '#f16(0 -1.0 1.0) 1.0)
(uvcopy-test-f32 '#f32(0 -1.0 1.0) 1.0)
(uvcopy-test-f64 '#f64(0 -1.0 1.0) 1.0)
(uvcopy-test-c32 '#c32(0+i -1.0-i 1.0) 1.0)
(uvcopy-test-c64 '#c64(0+i -1.0-i 1.0) 1.0)
(uvcopy-test-c128 '#c128(0+i -1.0-i 1.0) 1.0)

(expand-uvec 
 (u8 s8 u16 s16 u32 s32 u64 s64 f16 f32 f64 c32 c64 c128)
 (test* "uvcopy-startend @vector" 
        (list (@vector 1 2 3) (@vector 1 2) (@vector 0 9 9 3))
        (let* ([v (@vector 0 1 2 3)]
               [v1 (@vector-copy v 1)]
               [v2 (@vector-copy v 1 3)])
          (@vector-fill! v 9 1 3)
          (list v1 v2 v))))

(expand-uvec
 (u8 s8 u16 s16 u32 s32 u64 s64 f16 f32 f64 c32 c64 c128)
 (begin
   (test* #"@vector-copy! newapi /tstart" (@vector 0 7 8 9)
          (@vector-copy! (@vector 0 1 2 3) 1 (@vector 7 8 9 10)))
   (test* #"@vector-copy! newapi /tstart(over)" (@vector 0 1 2 3)
          (@vector-copy! (@vector 0 1 2 3) 4 (@vector 7 8 9 10)))
   (test* #"@vector-copy! newapi /tstart,sstart" (@vector 0 9 10 3)
          (@vector-copy! (@vector 0 1 2 3) 1 (@vector 7 8 9 10) 2))
   (test* #"@vector-copy! newapi /tstart,sstart,send" (@vector 0 1 2 9)
          (@vector-copy! (@vector 0 1 2 3) 3 (@vector 7 8 9 10) 2 3))))

(test* "uvector-copy! (generic)" '#u16(0 0 1 2 65535 0)
       (rlet1 v (make-u16vector 6 0)
         (uvector-copy! v 2 '#s16(1 2 -1))))
(test* "uvector-copy! (generic, start)" '#u16(0 0 1 2 65535 0)
       (rlet1 v (make-u16vector 6 0)
         (uvector-copy! v 2 '#s16(0 1 2 -1) 1)))
(test* "uvector-copy! (generic, start+end)" '#u16(0 0 1 2 65535 0)
       (rlet1 v (make-u16vector 6 0)
         (uvector-copy! v 2 '#s16(0 1 2 -1 -2) 1 4)))
(test* "uvector-copy! (generic, different size)" '#u16(0 0 257 0)
       (rlet1 v (make-u16vector 4 0)
         (uvector-copy! v 2 '#u8(1 1))))

(expand-uvec
 (u8 s8 u16 s16 u32 s32 u64 s64 f16 f32 f64 c32 c64 c128)
 (begin
  (test* #"@vector-multi-copy! generic" (@vector 0 1 2 3 0 1 2 3 0 1 2)
         (rlet1 dst (make-@vector 11 0)
           (@vector-multi-copy! dst 1 4 (@vector 1 2 3))))
  (test* #"@vector-multi-copy! ssize" (@vector 0 1 2 3 0 0 4 5 6 0 0 7)
         (rlet1 dst (make-@vector 12 0)
           (@vector-multi-copy! dst 1 5 (@vector 1 2 3 4 5 6 7 8 9) 0 3)))
  (test* #"@vector-multi-copy! sstart, ssize" (@vector 0 5 6 7 0 0 8 9 0 0 0 0)
         (rlet1 dst (make-@vector 12 0)
           (@vector-multi-copy! dst 1 5 (@vector 1 2 3 4 5 6 7 8 9) 4 3)))
  (test* #"@vector-multi-copy! ssize, sstride" (@vector 1 2 3 0 2 3 4 0 3 4 5 0)
         (rlet1 dst (make-@vector 12 0)
           (@vector-multi-copy! dst 0 4 (@vector 1 2 3 4 5 6 7 8 9) 0 3 1)))
  (test* #"@vector-multi-copy! count" (@vector 1 2 3 0 2 3 4 0 0 0 0 0)
         (rlet1 dst (make-@vector 12 0)
           (@vector-multi-copy! dst 0 4 (@vector 1 2 3 4 5 6 7 8 9) 0 3 1 2)))
  (test* #"@vector-multi-copy! single item" (@vector 1 0 1 0 1 0 1 0 1 0)
         (rlet1 dst (make-@vector 10 0)
           (@vector-multi-copy! dst 0 2 (@vector 1))))))

(expand-uvec
 (u8 s8 u16 s16 u32 s32 u64 s64 f16 f32 f64 c32 c64 c128)
 (begin
  (test* #"@vector-append base" (@vector) (@vector-append))
  (test* #"@vector-append unit" (@vector 1 2 3 )
         (@vector-append (@vector 1 2 3)))
  (test* #"@vector-append" (@vector 1 2 3 4 5 6 7 8)
         (@vector-append (@vector 1 2 3)
                         (@vector) (@vector 4 5) (@vector 6 7 8)))))

;;-------------------------------------------------------------------
(test-section "swapping bytes")

(test* "swapb s16"
       '#s16(#x0123 #x4567 #x7654 #x3210)
       (s16vector-swap-bytes '#s16(#x2301 #x6745 #x5476 #x1032)))
(test* "swapb u16"
       '#u16(#x0123 #x4567 #x7654 #x3210)
       (u16vector-swap-bytes '#u16(#x2301 #x6745 #x5476 #x1032)))
(test* "swapb s32"
       '#s32(#x01234567 #x76543210)
       (s32vector-swap-bytes '#s32(#x67452301 #x10325476)))
(test* "swapb u32"
       '#u32(#x01234567 #x76543210)
       (u32vector-swap-bytes '#u32(#x67452301 #x10325476)))
(test* "swapb s64"
       '#s64(#x0123456789abcdef #x-123456789abcdf0)
       (s64vector-swap-bytes
        '#s64(#x-1032547698badcff #x1032547698badcfe)))
(test* "swapb u64"
       '#u64(#x0123456789abcdef #xfedcba9876543210)
       (u64vector-swap-bytes
        '#u64(#xefcdab8967452301 #x1032547698badcfe)))

;;-------------------------------------------------------------------
(test-section "collection interface")

(use gauche.collection)
(use gauche.sequence)

(define (num-equal? l1 l2)
  (cond [(pair? l1)
         (if (pair? l2)
             (and (num-equal? (car l1) (car l2))
                  (num-equal? (cdr l1) (cdr l2)))
             #f)]
        [(vector? l1)
         (if (vector? l2)
             (with-iterator (l1 end1 next1)
               (with-iterator (l2 end2 next2)
                 (let loop ()
                   (if (end1)
                       (if (end2) #t #f)
                       (and (num-equal? (next1) (next2))
                            (loop)))))))]
        [(number? l1)
         (if (number? l2)
             (= l1 l2)
             #f)]
        [else (equal? l1 l2)]))

(expand-uvec
 (u8 s8 u16 s16 u32 s32 u64 s64 f16 f32 f64 c32 c64 c128)
 (test* "@vector collection interface" #t
        (let1 vec (@vector 1 2 3 4)
          (and (=      (fold + 0 vec) 10)
               (num-equal? (map identity vec) '(1 2 3 4))
               (=      (find (^e (= e 2)) vec) 2)
               (equal? (coerce-to <@vector> '(1 2 3 4)) vec)
               (=      (ref vec 2) 3)
               (num-equal? (begin (set! (ref vec 1) 0)
                                  (coerce-to <list> vec))
                           '(1 0 3 4))
               (num-equal? (coerce-to <vector> (subseq vec 1 3)) '#(0 3))))))

;;-------------------------------------------------------------------
(test-section "arithmetic operations")

;; there are too many combinations to write down by hand.
;;
;; for each operation in add, sub, mul
;;   for each testdata such that:
;;     - vector vs vector normal
;;     - vector vs vector underflow
;;     - vector vs vector overflow
;;     - vector vs smallint normal
;;     - vector vs smallint underflow
;;     - vector vs smallint overflow
;;     - vector vs bignum normal
;;     - vector vs bignum underflow
;;     - vector vs bignum overflow
;;

(define *bounds*
  `((s8   ,(- (expt 2  7)) ,(- (expt 2  7) 1))
    (u8   0                ,(- (expt 2  8) 1))
    (s16  ,(- (expt 2 15)) ,(- (expt 2 15) 1))
    (u16  0                ,(- (expt 2 16) 1))
    (s32  ,(- (expt 2 31)) ,(- (expt 2 31) 1))
    (u32  0                ,(- (expt 2 32) 1))
    (s64  ,(- (expt 2 63)) ,(- (expt 2 63) 1))
    (u64  0                ,(- (expt 2 64) 1))
    ))

(define (tag->min tag) (cadr  (assq tag *bounds*)))
(define (tag->max tag) (caddr (assq tag *bounds*)))

(define (arith-test tag min max make add sub mul)
  (define v0 (make 0 1 2 3))
  (define v1 (make 4 5 6 7))
  (define v2 (make (- max 2) (- max 2) (- max 2) (- max 2)))
  (define v3 (make (+ min 2) (+ min 2) (+ min 2) (+ min 2)))
  (define v0+v1 (make 4 6 8 10))
  (define v1-v0 (make 4 4 4 4))
  (define v0*v1 (make 0 5 12 21))
  (define v0*2  (make 0 2 4 6))
  (define v0+v2 (make (- max 2) (- max 1) max max))
  (define v0*v2 (make 0 (- max 2) max max))
  (define v0-min-1 (make min min (+ min 1) (+ min 2)))
  (define v3-v0    (make (+ min 2) (+ min 1) min min))
  (define v0*v3 (make 0 (+ min 2) min min))
  (define vmin (make min min min min))
  (define vmax (make max max max max))
  (define big32  #xffffffff)
  (define big64  #xffffffffffffffff)

  (define (gen-tester op v0 v1)
    (define (safe-test clamp-flag)
      (with-error-handler (^e 'error)
        (^[] (op v0 v1 clamp-flag))))
    (^[] (list (safe-test #f)
               (safe-test 'high)
               (safe-test 'low)
               (safe-test 'both))))

  (define (result-normal v) (list v v v v))
  (define (result-hi-ok  v) (list 'error v 'error v))
  (define (result-lo-ok  v) (list 'error 'error v v))

  ;; Add
  (test (format #f "~avector-add (v+v)" tag) (result-normal v0+v1)
        (gen-tester add v0 v1))
  (test (format #f "~avector-add (v+v)" tag) (result-hi-ok v0+v2)
        (gen-tester add v0 v2))
  (test (format #f "~avector-add (v+l)" tag) (result-normal v0+v1)
        (gen-tester add v0 (coerce-to <list> v1)))
  (test (format #f "~avector-add (v+vv)" tag) (result-normal v0+v1)
        (gen-tester add v0 (coerce-to <vector> v1)))
  (test (format #f "~avector-add (v+s)" tag) (result-normal v1)
        (gen-tester add v0 4))
  (test (format #f "~avector-add (v+s)" tag) (result-hi-ok v0+v2)
        (gen-tester add v0 (- max 2)))
  (test (format #f "~avector-add (v+s)" tag) (result-lo-ok v0-min-1)
        (gen-tester add v0 (- min 1)))
  (test (format #f "~avector-add (v+b)" tag)
        (case tag
          [(s64 u64)
           (result-normal (make big32 (+ big32 1) (+ big32 2) (+ big32 3)))]
          [else (result-hi-ok vmax)])
        (gen-tester add v0 big32))
  (test (format #f "~avector-add (v+b)" tag)
        (case tag
          [(s64)
           (result-normal (make (- big32) (- 1 big32) (- 2 big32) (- 3 big32)))]
          [else (result-lo-ok vmin)])
        (gen-tester add v0 (- big32)))
  (test (format #f "~avector-add (v+b)" tag) (result-hi-ok vmax)
        (gen-tester add v0 big64))
  (test (format #f "~avector-add (v+b)" tag) (result-lo-ok vmin)
        (gen-tester add v0 (- big64)))

  ;; Sub
  (test (format #f "~avector-sub (v-v)" tag) (result-normal v1-v0)
        (gen-tester sub v1 v0))
  (test (format #f "~avector-sub (v-v)" tag) (result-lo-ok  v3-v0)
        (gen-tester sub v3 v0))
  (test (format #f "~avector-sub (v-l)" tag) (result-normal v1-v0)
        (gen-tester sub v1 (coerce-to <list> v0)))
  (test (format #f "~avector-sub (v-vv)" tag) (result-normal v1-v0)
        (gen-tester sub v1 (coerce-to <vector> v0)))
  (test (format #f "~avector-sub (v-s)" tag) (result-normal v0)
        (gen-tester sub v1 4))
  (test (format #f "~avector-sub (v-s)" tag) (result-lo-ok v0-min-1)
        (gen-tester sub v0 (- (- min 1))))
  (test (format #f "~avector-sub (v-b)" tag)
        (case tag
          [(s64) (result-normal (make (- big32) (- 1 big32) (- 2 big32) (- 3 big32)))]
          [else  (result-lo-ok vmin)])
        (gen-tester sub v0 big32))
  (test (format #f "~avector-sub (v-b)" tag)
        (case tag
          [(s64 u64)
           (result-normal (make big32 (+ big32 1) (+ big32 2) (+ big32 3)))]
          [else (result-hi-ok vmax)])
        (gen-tester sub v0 (- big32)))
  (test (format #f "~avector-sub (v-b)" tag) (result-lo-ok vmin)
        (gen-tester sub v0 big64))
  (test (format #f "~avector-sub (v-b)" tag) (result-hi-ok vmax)
        (gen-tester sub v0 (- big64)))

  ;; Mul
  (test (format #f "~avector-mul (v*v)" tag) (result-normal v0*v1)
        (gen-tester mul v0 v1))
  (test (format #f "~avector-mul (v*v)" tag) (result-hi-ok v0*v2)
        (gen-tester mul v0 v2))
  (test (format #f "~avector-mul (v*l)" tag) (result-normal v0*v1)
        (gen-tester mul v0 (coerce-to <list> v1)))
  (test (format #f "~avector-mul (v*vv)" tag) (result-normal v0*v1)
        (gen-tester mul v0 (coerce-to <vector> v1)))
  (unless (memq tag '(u8 u16 u32 u64))
    (test (format #f "~avector-mul (v*v)" tag) (result-lo-ok v0*v3)
          (gen-tester mul v0 v3)))
  (test (format #f "~avector-mul (v*s)" tag) (result-normal v0*2)
        (gen-tester mul v0 2))
  (test (format #f "~avector-mul (v*s)" tag)
        (if (memq tag '(s8 s16 s32 s64))
          (result-normal (make 0 -2 -4 -6))
          (result-lo-ok  vmin))
        (gen-tester mul v0 -2))
  (test (format #f "~avector-mul (v*s)" tag)
        (result-hi-ok (make 0 (- max 1) max max))
        (gen-tester mul v0 (- max 1)))
  (test (format #f "~avector-mul (v*s)" tag)
        (case tag
          [(s8 s16 s32 s64) (result-lo-ok (make 0 (- (- max 1)) min min))]
          [else (result-lo-ok  vmin)])
        (gen-tester mul v0 (- (- max 1))))
  (test (format #f "~avector-mul (v*b)" tag)
        (case tag
          [(s64 u64) (result-normal (make 0 big32 (* big32 2) (* big32 3)))]
          [else (result-hi-ok  (make 0 max max max))])
        (gen-tester mul v0 big32))
  (test (format #f "~avector-mul (v*b)" tag)
        (case tag
          [(s64)
           (result-normal (make 0 (- big32) (- (* big32 2))  (- (* big32 3))))]
          [else (result-lo-ok  (make 0 min min min))])
        (gen-tester mul v0 (- big32)))
  )

(expand-uvec
 (u8 s8 u16 s16 u32 s32 u64 s64)
 (arith-test '@ (tag->min '@) (tag->max '@)
             @vector @vector-add @vector-sub @vector-mul))

;; flonum vectors; no clamping, so it's a bit simple
(expand-uvec
 (f16 f32 f64 c32 c64 c128)
 (begin
   (test* "@vector-add (v+v)"
          (@vector 4.0 6.0 8.0 10.0)
          (@vector-add (@vector 0.0 1.0 2.0 3.0) (@vector 4.0 5.0 6.0 7.0)))
   (test* "@vector-add (v+s)"
          (@vector 4.0 5.0 6.0 7.0)
          (@vector-add (@vector 0.0 1.0 2.0 3.0) 4.0))
   (test* "@vector-sub (v-v)"
          (@vector -4.0 -4.0 -4.0 -4.0)
          (@vector-sub (@vector 0.0 1.0 2.0 3.0) (@vector 4.0 5.0 6.0 7.0)))
   (test* "@vector-sub (v-s)"
          (@vector -4.0 -3.0 -2.0 -1.0)
          (@vector-sub (@vector 0.0 1.0 2.0 3.0) 4.0))
   (test* "@vector-mul (v*v)"
          (@vector 0.0 5.0 12.0 21.0)
          (@vector-mul (@vector 0.0 1.0 2.0 3.0) (@vector 4.0 5.0 6.0 7.0)))
   (test* "@vector-mul (v*s)"
          (@vector 0.0 5.0 10.0 15.0)
          (@vector-mul (@vector 0.0 1.0 2.0 3.0) 5.0))
   (test* "@vector-div (v/v)"
          (@vector 0.0 0.5 0.5 0.375)
          (@vector-div (@vector 0.0 1.0 2.0 3.0) (@vector 1.0 2.0 4.0 8.0)))
   (test* "@vector-div (v/v)"
          (@vector 0.0 0.5 1.0 1.5)
          (@vector-div (@vector 0.0 1.0 2.0 3.0) 2.0))
   ))

;;-------------------------------------------------------------------
(test-section "bitwise operations")

(define (bit-test tag v0 v1 s0 s1 ->list list-> and ior xor)
  (define (tests opname op logop)
    (test* (format #f "~svector-~s ~s ~s" tag opname v0 v1)
           (list-> (map logop (->list v0) (->list v1)))
           (op v0 v1))
    (test* (format #f "~svector-~s ~s ~s" tag opname v0 s0)
           (list-> (map (pa$ logop s0) (->list v0)))
           (op v0 s0))
    (test* (format #f "~svector-~s ~s ~s" tag opname v0 s1)
           (list-> (map (pa$ logop s1) (->list v0)))
           (op v0 s1)))
  (tests 'and and logand)
  (tests 'ior ior logior)
  (tests 'xor xor logxor))

(expand-uvec
 (u8 s8 u16 s16 u32 s32 u64 s64)
 (define (bit-test-@ v0 v1 s0 s1)
   (bit-test '@ v0 v1 s0 s1 
                @vector->list
                list->@vector
                @vector-and
                @vector-ior
                @vector-xor)))

(bit-test-s8 #s8(#x0f #x70 #x-0f #x-70)
             #s8(#x55 #x2a #x-55 #x-2a)
             #x55
             #x-55)
(bit-test-u8 #u8(#x0f #x70 #xf0 #xcc)
             #u8(#x55 #xaa #x5a #xa5)
             #x55
             #xaa)
(bit-test-s16 #s16(#x0fff #x7070 #x-0fff #x-7070)
              #s16(#x3c3c #x-43c3 #x43c3 #x-3c3c)
              #x55aa
              #x-55aa)
(bit-test-u16 #u16(#x0fff #x7070 #xff00 #xc0c0)
              #u16(#x3c3c #xc3c3 #x55aa #xaa55)
              #x55aa
              #x9696)
(bit-test-s32 #s32(#x0fffffff #x70707070 #x-0fffffff #x-70707070)
              #s32(#x3c3c3c3c #x-43c3c3c3 #x43c3c3c3 #x-3c3c3c3c)
              #x55aa55aa
              #x-55aa55aa)
(bit-test-u32 #u32(#x0fffffff #x70707070 #xff00ff00 #xc0c0c0c0)
              #u32(#x3c3c3c3c #xc3c3c3c3 #x55aa55aa #xaa55aa55)
              #x55aa55aa
              #x96966969)
(bit-test-s64 #s64(#x0fffffffffffffff #x7070707007070707
                  #x-0fffffffffffffff #x-7070707007070707)
              #s64(#x3c3c3c3cc3c3c3c3 #x-43c3c3c33c3c3c3c
                   #x43c3c3c3c3c3c3c3 #x-3c3c3c3c3c3c3c3c)
              #x55aa55aa55aa55aa
              #x-55aa55aa55aa55aa)
(bit-test-u64 #u64(#x0fffffffffffffff #x70707070f0f0f0f0
                   #xff00ff00ff00ff00 #xc0c0c0c003030303)
              #u64(#x3c3c3c3c3c3c3c3c #xc3c3c3c3c3c3c3c3
                   #x55aa55aa55aa55aa #xaa55aa55aa55aa55)
              #x55aa55aa5a5a5a5a
              #x9696696988778877)

;;-------------------------------------------------------------------
(test-section "dot product")

(define (dotprod-test tag v0 v1 dot)
  (let1 result (fold (^[e0 e1 sum] (+ sum (* e0 e1)))
                     0
                     (coerce-to <list> v0)
                     (coerce-to <list> v1))
    (test* (format #f "~svector-dot(~s, ~s)" tag v0 v1)
           result
           (dot v0 v1))
    (test* (format #f "~svector-dot(~s, ~s)" tag v0 (coerce-to <list> v1))
           result
           (dot v0 (coerce-to <list> v1)))
    (test* (format #f "~svector-dot(~s, ~s)" tag v0 (coerce-to <vector> v1))
           result
           (dot v0 (coerce-to <vector> v1)))
    ))

(expand-uvec
 (u8 s8 u16 s16 u32 s32 u64 s64 f16 f32 f64)
 (define (dotprod-test-@ v0 v1)
   (dotprod-test '@ v0 v1 @vector-dot)))

(dotprod-test-s8 #s8() #s8())
(dotprod-test-s8 #s8(0 1 2 3) #s8(4 5 6 7))
(dotprod-test-s8 #s8(0 -1 2 -3) #s8(-4 5 -6 7))
(dotprod-test-s8 #s8(127 127 127 127 127) #s8(127 127 127 127 127))
(dotprod-test-s8
 #s8(-128 -128 -128 -128 -128)
 #s8(127 127 127 127 127))
(dotprod-test-u8 #u8(0 1 2 3) #u8(4 5 6 7))
(dotprod-test-u8 #u8(255 255 255 255 255) #u8(255 255 255 255 255))

(dotprod-test-s16 #s16(0 1 2 3) #s16(4 5 6 7))
(dotprod-test-s16 #s16(0 -1 2 -3) #s16(-4 5 -6 7))
(dotprod-test-s16 #s16(16384 16384 16384 16384 16384)
                  #s16(16384 16384 16384 16384 16384))
(dotprod-test-s16 #s16(16384 -16384 16384 -16384 16384)
                  #s16(16384 -16384 16384 -16384 16384))
(dotprod-test-s16 #s16(32767 32767 32767 32767 32767)
                  #s16(32767 32767 32767 32767 32767))
(dotprod-test-s16 #s16(32767 1 2 3 4)
                  #s16(32767 1 2 3 4))
(dotprod-test-s16 #s16(1 2 3 4 32767)
                  #s16(1 2 3 4 32767))
(dotprod-test-s16 #s16(32767 -32767 32767 -32767 32767)
                  #s16(32767 32767 32767 32767 32767))
(dotprod-test-s16 #s16(-32768 -32768 -32768 -32768 -32768)
                  #s16(32767 32767 32767 32767 32767))
(dotprod-test-u16 #u16(0 1 2 3) #u16(4 5 6 7))
(dotprod-test-u16 #u16(16384 16384 16384 16384 16384)
                  #u16(16384 16384 16384 16384 16384))
(dotprod-test-u16 #u16(65535 65535 65535 65535 65535)
                  #u16(65535 65535 65535 65535 65535))
(dotprod-test-u16 #u16(32767 1 2 3 4)
                  #u16(32767 1 2 3 4))
(dotprod-test-u16 #u16(1 2 3 4 32767)
                  #u16(1 2 3 4 32767))

(dotprod-test-s32 #s32(0 1 2 3) #s32(4 5 6 7))
(dotprod-test-s32 #s32(0 -1 2 -3) #s32(-4 5 -6 7))
(dotprod-test-s32 #s32(16384 16384 16384 16384 16384)
                  #s32(16384 16384 16384 16384 16384))
(dotprod-test-s32 #s32(16384 -16384 16384 -16384 16384)
                  #s32(16384 -16384 16384 -16384 16384))
(dotprod-test-s32 #s32(32767 32767 32767 32767 32767)
                  #s32(32767 32767 32767 32767 32767))
(dotprod-test-s32 #s32(214748367 214748367 214748367 214748367 214748367)
                  #s32(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-s32 #s32(214748367 1 2 3 4)
                  #s32(214748367 1 2 3 4))
(dotprod-test-s32 #s32(1 2 3 4 214748367)
                  #s32(1 2 3 4 214748367))
(dotprod-test-s32 #s32(32767 -32767 32767 -32767 32767)
                  #s32(32767 32767 32767 32767 32767))
(dotprod-test-s32 #s32(214748367 -214748367 214748367 -214748367 214748367)
                  #s32(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-s32 #s32(-214748368 -214748368 -214748368 -214748368 -214748368)
                  #s32(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-u32 #u32(0 1 2 3) #u32(4 5 6 7))
(dotprod-test-u32 #u32(16384 16384 16384 16384 16384)
                  #u32(16384 16384 16384 16384 16384))
(dotprod-test-u32 #u32(4294967295 4294967295 4294967295 4294967295 4294967295)
                  #u32(4294967295 4294967295 4294967295 4294967295 4294967295))
(dotprod-test-u32 #u32(4294967295 1 2 3 4)
                  #u32(4294967295 1 2 3 4))
(dotprod-test-u32 #u32(1 2 3 4 4294967295)
                  #u32(1 2 3 4 4294967295))

(dotprod-test-s64 #s64(0 1 2 3) #s64(4 5 6 7))
(dotprod-test-s64 #s64(0 -1 2 -3) #s64(-4 5 -6 7))
(dotprod-test-s64 #s64(16384 16384 16384 16384 16384)
                  #s64(16384 16384 16384 16384 16384))
(dotprod-test-s64 #s64(16384 -16384 16384 -16384 16384)
                  #s64(16384 -16384 16384 -16384 16384))
(dotprod-test-s64 #s64(32767 32767 32767 32767 32767)
                  #s64(32767 32767 32767 32767 32767))
(dotprod-test-s64 #s64(214748367 214748367 214748367 214748367 214748367)
                  #s64(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-s64 #s64(9223372036854775807 1 2 3 4)
                  #s64(9223372036854775807 1 2 3 4))
(dotprod-test-s64 #s64(1 2 3 4 9223372036854775807 1)
                  #s64(1 2 3 4 9223372036854775807 1))
(dotprod-test-s64 #s64(32767 -32767 32767 -32767 32767)
                  #s64(32767 -32767 32767 -32767 32767))
(dotprod-test-s64 #s64(214748367 -214748367 214748367 -214748367 214748367)
                  #s64(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-s64 #s64(-214748368 -214748368 -214748368 -214748368 -214748368)
                  #s64(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-s64 #s64(9223372036854775807 -9223372036854775807 9223372036854775807 -9223372036854775807 9223372036854775807)
                  #s64(9223372036854775807 9223372036854775807 9223372036854775807 9223372036854775807 9223372036854775807))
(dotprod-test-s64 #s64(-9223372036854775808 -9223372036854775808 -9223372036854775808 -9223372036854775808 -9223372036854775808)
                  #s64(9223372036854775807 9223372036854775807 9223372036854775807 9223372036854775807 9223372036854775807))
(dotprod-test-u64 #u64(0 1 2 3) #u64(4 5 6 7))
(dotprod-test-u64 #u64(16384 16384 16384 16384 16384)
                  #u64(16384 16384 16384 16384 16384))
(dotprod-test-u64 #u64(18446744073709551615 18446744073709551615 18446744073709551615 18446744073709551615 18446744073709551615)
                  #u64(18446744073709551615 18446744073709551615 18446744073709551615 18446744073709551615 18446744073709551615))
(dotprod-test-u64 #u64(18446744073709551615 1 2 3 4)
                  #u64(18446744073709551615 1 2 3 4))
(dotprod-test-u64 #u64(1 2 3 4 18446744073709551615)
                  #u64(1 2 3 4 18446744073709551615))

(dotprod-test-f16 #f16(0 1 2 3) #f16(4 5 6 7))
(dotprod-test-f16 #f16(0 -1 2 -3) #f16(-4 5 -6 7))
(dotprod-test-f16 #f16(16384 16384 16384 16384 16384)
                  #f16(16384 16384 16384 16384 16384))
(dotprod-test-f16 #f16(16384 -16384 16384 -16384 16384)
                  #f16(16384 -16384 16384 -16384 16384))
(dotprod-test-f16 #f16(32767 32767 32767 32767 32767)
                  #f16(32767 32767 32767 32767 32767))
(dotprod-test-f16 #f16(214748367 214748367 214748367 214748367 214748367)
                  #f16(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-f16 #f16(9223372036854775807 1 2 3 4)
                  #f16(9223372036854775807 1 2 3 4))
(dotprod-test-f16 #f16(1 2 3 4 9223372036854775807 1)
                  #f16(1 2 3 4 9223372036854775807 1))
(dotprod-test-f16 #f16(32767 -32767 32767 -32767 32767)
                  #f16(32767 -32767 32767 -32767 32767))

(dotprod-test-f32 #f32(0 1 2 3) #f32(4 5 6 7))
(dotprod-test-f32 #f32(0 -1 2 -3) #f32(-4 5 -6 7))
(dotprod-test-f32 #f32(16384 16384 16384 16384 16384)
                  #f32(16384 16384 16384 16384 16384))
(dotprod-test-f32 #f32(16384 -16384 16384 -16384 16384)
                  #f32(16384 -16384 16384 -16384 16384))
(dotprod-test-f32 #f32(32767 32767 32767 32767 32767)
                  #f32(32767 32767 32767 32767 32767))
(dotprod-test-f32 #f32(214748367 214748367 214748367 214748367 214748367)
                  #f32(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-f32 #f32(9223372036854775807 1 2 3 4)
                  #f32(9223372036854775807 1 2 3 4))
(dotprod-test-f32 #f32(1 2 3 4 9223372036854775807 1)
                  #f32(1 2 3 4 9223372036854775807 1))
(dotprod-test-f32 #f32(32767 -32767 32767 -32767 32767)
                  #f32(32767 -32767 32767 -32767 32767))

(dotprod-test-f64 #f64(0 1 2 3) #f64(4 5 6 7))
(dotprod-test-f64 #f64(0 -1 2 -3) #f64(-4 5 -6 7))
(dotprod-test-f64 #f64(16384 16384 16384 16384 16384)
                  #f64(16384 16384 16384 16384 16384))
(dotprod-test-f64 #f64(16384 -16384 16384 -16384 16384)
                  #f64(16384 -16384 16384 -16384 16384))
(dotprod-test-f64 #f64(32767 32767 32767 32767 32767)
                  #f64(32767 32767 32767 32767 32767))
(dotprod-test-f64 #f64(214748367 214748367 214748367 214748367 214748367)
                  #f64(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-f64 #f64(9223372036854775807 1 2 3 4)
                  #f64(9223372036854775807 1 2 3 4))
(dotprod-test-f64 #f64(1 2 3 4 9223372036854775807 1)
                  #f64(1 2 3 4 9223372036854775807 1))
(dotprod-test-f64 #f64(32767 -32767 32767 -32767 32767)
                  #f64(32767 -32767 32767 -32767 32767))

;;-------------------------------------------------------------------
(test-section "range-check")

(expand-uvec
 (u8 s8 u16 s16 u32 s32 u64 s64 f16 f32 f64)
 (define (range-test-@ v min max result)
   (test* "@vector-range-check" result (@vector-range-check v min max))))

(range-test-s8 #s8(-4 -2 0 2 4) #f #f #f)
(range-test-s8 #s8(-4 -2 0 2 4) -4 4 #f)
(range-test-s8 #s8(-4 -2 0 2 4) -4 3 4)
(range-test-s8 #s8(-4 -2 0 2 4) -4 0 3)
(range-test-s8 #s8(-4 -2 0 2 4) -3 4 0)
(range-test-s8 #s8(-4 -2 0 2 4) 0 4 0)
(range-test-s8 #s8(-4 -2 0 2 4) #x-ffffffffffffffff #f #f)
(range-test-s8 #s8(-4 -2 0 2 4) #f #xffffffffffffffff  #f)
(range-test-s8 #s8(-4 -2 0 2 4) #xffffffffffffffff #f  0)
(range-test-s8 #s8(-4 -2 0 2 4) #f #x-ffffffffffffffff 0)
(range-test-s8 #s8(-4 -2 0 2 4) #s8(-4 -3 -2 -1 0) #s8(0 1 2 3 4) #f)
(range-test-s8 #s8(-4 -2 0 2 4) #s8(-4 -1 -2 -1 0) #s8(0 1 2 3 4) 1)
(range-test-s8 #s8(-4 -2 0 2 4) #s8(-4 -3 -2 -1 0) #s8(0 1 -1 3 4) 2)

(range-test-u8 #u8(0 2 4 6 9) #f #f #f)
(range-test-u8 #u8(0 2 4 6 9) -4 9 #f)
(range-test-u8 #u8(0 2 4 6 9) -4 8 4)
(range-test-u8 #u8(0 2 4 6 9) -4 4 3)
(range-test-u8 #u8(0 2 4 6 9) 3 4 0)
(range-test-u8 #u8(0 2 4 6 9) #x-ffffffffffffffff #f #f)
(range-test-u8 #u8(0 2 4 6 9) #f #xffffffffffffffff  #f)
(range-test-u8 #u8(0 2 4 6 9) #xffffffffffffffff #f  0)
(range-test-u8 #u8(0 2 4 6 9) #f #x-ffffffffffffffff 1)
(range-test-u8 #u8(0 2 4 6 9) #u8(0 1 2 3 4) #u8(5 6 7 8 9) #f)
(range-test-u8 #u8(0 2 4 6 9) #u8(0 1 5 3 4) #u8(5 6 7 8 9) 2)
(range-test-u8 #u8(0 2 4 6 9) #u8(0 1 2 3 4) #u8(5 6 7 5 6) 3)

(range-test-s16 #s16(-4 -2 0 2 4) #f #f #f)
(range-test-s16 #s16(-4 -2 0 2 4) -4 4 #f)
(range-test-s16 #s16(-4 -2 0 2 4) -4 3 4)
(range-test-s16 #s16(-4 -2 0 2 4) -4 0 3)
(range-test-s16 #s16(-4 -2 0 2 4) -3 4 0)
(range-test-s16 #s16(-4 -2 0 2 4) 0 4 0)
(range-test-s16 #s16(-4 -2 0 2 4) #x-ffffffffffffffff #f #f)
(range-test-s16 #s16(-4 -2 0 2 4) #f #xffffffffffffffff  #f)
(range-test-s16 #s16(-4 -2 0 2 4) #xffffffffffffffff #f  0)
(range-test-s16 #s16(-4 -2 0 2 4) #f #x-ffffffffffffffff 0)
(range-test-s16 #s16(-4 -2 0 2 4) #s16(-4 -3 -2 -1 0) #s16(0 1 2 3 4) #f)
(range-test-s16 #s16(-4 -2 0 2 4) #s16(-4 -1 -2 -1 0) #s16(0 1 2 3 4) 1)
(range-test-s16 #s16(-4 -2 0 2 4) #s16(-4 -3 -2 -1 0) #s16(0 1 -1 3 4) 2)

(range-test-u16 #u16(0 2 4 6 9) #f #f #f)
(range-test-u16 #u16(0 2 4 6 9) -4 9 #f)
(range-test-u16 #u16(0 2 4 6 9) -4 8 4)
(range-test-u16 #u16(0 2 4 6 9) -4 4 3)
(range-test-u16 #u16(0 2 4 6 9) 3 4 0)
(range-test-u16 #u16(0 2 4 6 9) #x-ffffffffffffffff #f #f)
(range-test-u16 #u16(0 2 4 6 9) #f #xffffffffffffffff  #f)
(range-test-u16 #u16(0 2 4 6 9) #xffffffffffffffff #f  0)
(range-test-u16 #u16(0 2 4 6 9) #f #x-ffffffffffffffff 1)
(range-test-u16 #u16(0 2 4 6 9) #u16(0 1 2 3 4) #u16(5 6 7 8 9) #f)
(range-test-u16 #u16(0 2 4 6 9) #u16(0 1 5 3 4) #u16(5 6 7 8 9) 2)
(range-test-u16 #u16(0 2 4 6 9) #u16(0 1 2 3 4) #u16(5 6 7 5 6) 3)

(range-test-s32 #s32(-4 -2 0 2 4) #f #f #f)
(range-test-s32 #s32(-4 -2 0 2 4) -4 4 #f)
(range-test-s32 #s32(-4 -2 0 2 4) -4 3 4)
(range-test-s32 #s32(-4 -2 0 2 4) -4 0 3)
(range-test-s32 #s32(-4 -2 0 2 4) -3 4 0)
(range-test-s32 #s32(-4 -2 0 2 4) 0 4 0)
(range-test-s32 #s32(-4 -2 0 2 4) #x-ffffffffffffffff #f #f)
(range-test-s32 #s32(-4 -2 0 2 4) #f #xffffffffffffffff  #f)
(range-test-s32 #s32(-4 -2 0 2 4) #xffffffffffffffff #f  0)
(range-test-s32 #s32(-4 -2 0 2 4) #f #x-ffffffffffffffff 0)
(range-test-s32 #s32(-4 -2 0 2 4) #s32(-4 -3 -2 -1 0) #s32(0 1 2 3 4) #f)
(range-test-s32 #s32(-4 -2 0 2 4) #s32(-4 -1 -2 -1 0) #s32(0 1 2 3 4) 1)
(range-test-s32 #s32(-4 -2 0 2 4) #s32(-4 -3 -2 -1 0) #s32(0 1 -1 3 4) 2)

(range-test-u32 #u32(0 2 4 6 9) #f #f #f)
(range-test-u32 #u32(0 2 4 6 9) -4 9 #f)
(range-test-u32 #u32(0 2 4 6 9) -4 8 4)
(range-test-u32 #u32(0 2 4 6 9) -4 4 3)
(range-test-u32 #u32(0 2 4 6 9) 3 4 0)
(range-test-u32 #u32(0 2 4 6 9) #x-ffffffffffffffff #f #f)
(range-test-u32 #u32(0 2 4 6 9) #f #xffffffffffffffff  #f)
(range-test-u32 #u32(0 2 4 6 9) #xffffffffffffffff #f  0)
(range-test-u32 #u32(0 2 4 6 9) #f #x-ffffffffffffffff 1)
(range-test-u32 #u32(0 2 4 6 9) #u32(0 1 2 3 4) #u32(5 6 7 8 9) #f)
(range-test-u32 #u32(0 2 4 6 9) #u32(0 1 5 3 4) #u32(5 6 7 8 9) 2)
(range-test-u32 #u32(0 2 4 6 9) #u32(0 1 2 3 4) #u32(5 6 7 5 6) 3)

(range-test-s64 #s64(-4 -2 0 2 4) #f #f #f)
(range-test-s64 #s64(-4 -2 0 2 4) -4 4 #f)
(range-test-s64 #s64(-4 -2 0 2 4) -4 3 4)
(range-test-s64 #s64(-4 -2 0 2 4) -4 0 3)
(range-test-s64 #s64(-4 -2 0 2 4) -3 4 0)
(range-test-s64 #s64(-4 -2 0 2 4) 0 4 0)
(range-test-s64 #s64(-4 -2 0 2 4) #x-ffffffffffffffff #f #f)
(range-test-s64 #s64(-4 -2 0 2 4) #f #xffffffffffffffff  #f)
(range-test-s64 #s64(-4 -2 0 2 4) #xffffffffffffffff #f  0)
(range-test-s64 #s64(-4 -2 0 2 4) #f #x-ffffffffffffffff 0)
(range-test-s64 #s64(-4 -2 0 2 4) #s64(-4 -3 -2 -1 0) #s64(0 1 2 3 4) #f)
(range-test-s64 #s64(-4 -2 0 2 4) #s64(-4 -1 -2 -1 0) #s64(0 1 2 3 4) 1)
(range-test-s64 #s64(-4 -2 0 2 4) #s64(-4 -3 -2 -1 0) #s64(0 1 -1 3 4) 2)

(range-test-u64 #u64(0 2 4 6 9) #f #f #f)
(range-test-u64 #u64(0 2 4 6 9) -4 9 #f)
(range-test-u64 #u64(0 2 4 6 9) -4 8 4)
(range-test-u64 #u64(0 2 4 6 9) -4 4 3)
(range-test-u64 #u64(0 2 4 6 9) 3 4 0)
(range-test-u64 #u64(0 2 4 6 9) #x-ffffffffffffffff #f #f)
(range-test-u64 #u64(0 2 4 6 9) #f #xffffffffffffffff  #f)
(range-test-u64 #u64(0 2 4 6 9) #xffffffffffffffff #f  0)
(range-test-u64 #u64(0 2 4 6 9) #f #x-ffffffffffffffff 1)
(range-test-u64 #u64(0 2 4 6 9) #u64(0 1 2 3 4) #u64(5 6 7 8 9) #f)
(range-test-u64 #u64(0 2 4 6 9) #u64(0 1 5 3 4) #u64(5 6 7 8 9) 2)
(range-test-u64 #u64(0 2 4 6 9) #u64(0 1 2 3 4) #u64(5 6 7 5 6) 3)

(range-test-f32 #f32(-4.0 -2.0 0.0 2.0 4.0) #f #f #f)
(range-test-f32 #f32(-4.0 -2.0 0.0 2.0 4.0) -4.0 4.0 #f)
(range-test-f32 #f32(-4.0 -2.0 0.0 2.0 4.0) -4.0 3.0 4)
(range-test-f32 #f32(-4.0 -2.0 0.0 2.0 4.0) -4.0 0.0 3)
(range-test-f32 #f32(-4.0 -2.0 0.0 2.0 4.0) -3.0 4.0 0)
(range-test-f32 #f32(-4.0 -2.0 0.0 2.0 4.0) 0.0 4.0 0)
(range-test-f32 #f32(-4.0 -2.0 0.0 2.0 4.0) #f32(-4.0 -3.0 -2.0 -1.0 0.0) #f32(0.0 1.0 2.0 3.0 4.0) #f)
(range-test-f32 #f32(-4.0 -2.0 0.0 2.0 4.0) #f32(-4.0 -1.0 -2.0 -1.0 0.0) #f32(0.0 1.0 2.0 3.0 4.0) 1)
(range-test-f32 #f32(-4.0 -2.0 0.0 2.0 4.0) #f32(-4.0 -3.0 -2.0 -1.0 0.0) #f32(0.0 1.0 -1.0 3.0 4.0) 2)

(range-test-f64 #f64(-4.0 -2.0 0.0 2.0 4.0) #f #f #f)
(range-test-f64 #f64(-4.0 -2.0 0.0 2.0 4.0) -4.0 4.0 #f)
(range-test-f64 #f64(-4.0 -2.0 0.0 2.0 4.0) -4.0 3.0 4)
(range-test-f64 #f64(-4.0 -2.0 0.0 2.0 4.0) -4.0 0.0 3)
(range-test-f64 #f64(-4.0 -2.0 0.0 2.0 4.0) -3.0 4.0 0)
(range-test-f64 #f64(-4.0 -2.0 0.0 2.0 4.0) 0.0 4.0 0)
(range-test-f64 #f64(-4.0 -2.0 0.0 2.0 4.0) #f64(-4.0 -3.0 -2.0 -1.0 0.0) #f64(0.0 1.0 2.0 3.0 4.0) #f)
(range-test-f64 #f64(-4.0 -2.0 0.0 2.0 4.0) #f64(-4.0 -1.0 -2.0 -1.0 0.0) #f64(0.0 1.0 2.0 3.0 4.0) 1)
(range-test-f64 #f64(-4.0 -2.0 0.0 2.0 4.0) #f64(-4.0 -3.0 -2.0 -1.0 0.0) #f64(0.0 1.0 -1.0 3.0 4.0) 2)

;;-------------------------------------------------------------------
(test-section "clamp")

(define (clamp-test tag class tagvector? tagvector-ref tagvector-length
                    clamp v minv maxv)
  (define (clamp-min index v)
    (cond [(tagvector? minv)
           (max v (tagvector-ref minv index) (tag->min tag))]
          [(not minv) (max v (tag->min tag))]
          [else (max v minv (tag->min tag))]))
  (define (clamp-max index v)
    (cond [(tagvector? maxv)
           (min v (tagvector-ref maxv index) (tag->max tag))]
          [(not maxv) (min v (tag->max tag))]
          [else (min v maxv (tag->max tag))]))
  (let1 result (map-to class
                       (^i (clamp-min i (clamp-max i (tagvector-ref v i))))
                       (iota (tagvector-length v)))
    (test* (format #f "~svector-clamp" tag)
           result
           (clamp v minv maxv))
    (when (or (tagvector? minv) (tagvector? maxv))
      (test* (format #f "~svector-clamp (list)" tag)
             result
             (clamp v
                    (if (tagvector? minv)
                        (coerce-to <list> minv)
                        minv)
                    (if (tagvector? maxv)
                        (coerce-to <list> maxv)
                        maxv)))
      (test* (format #f "~svector-clamp (vector)" tag)
             result
             (clamp v
                    (if (tagvector? minv)
                        (coerce-to <vector> minv)
                        minv)
                    (if (tagvector? maxv)
                        (coerce-to <vector> maxv)
                        maxv)))
      )
    ))

(expand-uvec
 (u8 s8 u16 s16 u32 s32 u64 s64)
 (define (clamp-test-@ v minv maxv)
   (clamp-test '@ <@vector> @vector? @vector-ref @vector-length
                  @vector-clamp v minv maxv)))

(clamp-test-s8 #s8(0 -127 -4 4 127) #f #f)
(clamp-test-s8 #s8(0 -127 -4 4 127) -8 #f)
(clamp-test-s8 #s8(0 -127 -4 4 127) 0 #f)
(clamp-test-s8 #s8(0 -127 -4 4 127) 8 #f)
(clamp-test-s8 #s8(0 -127 -4 4 127) #f -8)
(clamp-test-s8 #s8(0 -127 -4 4 127) #f 0)
(clamp-test-s8 #s8(0 -127 -4 4 127) #f 8)
(clamp-test-s8 #s8(0 -127 -4 4 127) -1 1)
(clamp-test-s8 #s8(0 -127 -4 4 127)
               #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-s8 #s8(0 -127 -4 4 127)
               0 #xffffffffffffffffffffffffffffff)
(clamp-test-s8 #s8(0 -127 -4 4 127)
               #s8(-3 -4 -6 -8 -19) #s8(3 7 9 2 4))

(clamp-test-u8 #u8(127 0 4 200 255) #f #f)
(clamp-test-u8 #u8(127 0 4 200 255) -4 #f)
(clamp-test-u8 #u8(127 0 4 200 255) 0 #f)
(clamp-test-u8 #u8(127 0 4 200 255) 199 #f)
(clamp-test-u8 #u8(127 0 4 200 255) #f -4)
(clamp-test-u8 #u8(127 0 4 200 255) #f 0)
(clamp-test-u8 #u8(127 0 4 200 255) #f 199)
(clamp-test-u8 #u8(127 0 4 200 255)
               #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-u8 #u8(127 0 4 200 255)
               0 #xffffffffffffffffffffffffffffff)
(clamp-test-u8 #u8(127 0 4 200 255)
               #u8(3 3 3 3 3) #u8(199 199 199 199 199))

(clamp-test-s16 #s16(0 -127 -4 4 127) #f #f)
(clamp-test-s16 #s16(0 -127 -4 4 127) -8 #f)
(clamp-test-s16 #s16(0 -127 -4 4 127) 0 #f)
(clamp-test-s16 #s16(0 -127 -4 4 127) 8 #f)
(clamp-test-s16 #s16(0 -127 -4 4 127) #f -8)
(clamp-test-s16 #s16(0 -127 -4 4 127) #f 0)
(clamp-test-s16 #s16(0 -127 -4 4 127) #f 8)
(clamp-test-s16 #s16(0 -127 -4 4 127) -1 1)
(clamp-test-s16 #s16(0 -127 -4 4 127)
                #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-s16 #s16(0 -127 -4 4 127)
                0 #xffffffffffffffffffffffffffffff)
(clamp-test-s16 #s16(0 -127 -4 4 127)
                #s16(-3 -4 -6 -8 -19) #s16(3 7 9 2 4))

(clamp-test-u16 #u16(127 0 4 200 255) #f #f)
(clamp-test-u16 #u16(127 0 4 200 255) -4 #f)
(clamp-test-u16 #u16(127 0 4 200 255) 0 #f)
(clamp-test-u16 #u16(127 0 4 200 255) 199 #f)
(clamp-test-u16 #u16(127 0 4 200 255) #f -4)
(clamp-test-u16 #u16(127 0 4 200 255) #f 0)
(clamp-test-u16 #u16(127 0 4 200 255) #f 199)
(clamp-test-u16 #u16(127 0 4 200 255)
                #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-u16 #u16(127 0 4 200 255)
                0 #xffffffffffffffffffffffffffffff)
(clamp-test-u16 #u16(127 0 4 200 255)
                #u16(3 3 3 3 3) #u16(199 199 199 199 199))

(clamp-test-s32 #s32(0 -127 -4 4 127) #f #f)
(clamp-test-s32 #s32(0 -127 -4 4 127) -8 #f)
(clamp-test-s32 #s32(0 -127 -4 4 127) 0 #f)
(clamp-test-s32 #s32(0 -127 -4 4 127) 8 #f)
(clamp-test-s32 #s32(0 -127 -4 4 127) #f -8)
(clamp-test-s32 #s32(0 -127 -4 4 127) #f 0)
(clamp-test-s32 #s32(0 -127 -4 4 127) #f 8)
(clamp-test-s32 #s32(0 -127 -4 4 127) -1 1)
(clamp-test-s32 #s32(0 -127 -4 4 127)
                #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-s32 #s32(0 -127 -4 4 127)
                0 #xffffffffffffffffffffffffffffff)
(clamp-test-s32 #s32(0 -127 -4 4 127)
                #s32(-3 -4 -6 -8 -19) #s32(3 7 9 2 4))

(clamp-test-u32 #u32(127 0 4 200 255) #f #f)
(clamp-test-u32 #u32(127 0 4 200 255) -4 #f)
(clamp-test-u32 #u32(127 0 4 200 255) 0 #f)
(clamp-test-u32 #u32(127 0 4 200 255) 199 #f)
(clamp-test-u32 #u32(127 0 4 200 255) #f -4)
(clamp-test-u32 #u32(127 0 4 200 255) #f 0)
(clamp-test-u32 #u32(127 0 4 200 255) #f 199)
(clamp-test-u32 #u32(127 0 4 200 255)
                #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-u32 #u32(127 0 4 200 255)
                0 #xffffffffffffffffffffffffffffff)
(clamp-test-u32 #u32(127 0 4 200 255)
                #u32(3 3 3 3 3) #u32(199 199 199 199 199))

(clamp-test-s64 #s64(0 -127 -4 4 127) #f #f)
(clamp-test-s64 #s64(0 -127 -4 4 127) -8 #f)
(clamp-test-s64 #s64(0 -127 -4 4 127) 0 #f)
(clamp-test-s64 #s64(0 -127 -4 4 127) 8 #f)
(clamp-test-s64 #s64(0 -127 -4 4 127) #f -8)
(clamp-test-s64 #s64(0 -127 -4 4 127) #f 0)
(clamp-test-s64 #s64(0 -127 -4 4 127) #f 8)
(clamp-test-s64 #s64(0 -127 -4 4 127) -1 1)
(clamp-test-s64 #s64(0 -127 -4 4 127)
                #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-s64 #s64(0 -127 -4 4 127)
                0 #xffffffffffffffffffffffffffffff)
(clamp-test-s64 #s64(0 -127 -4 4 127)
                #s64(-3 -4 -6 -8 -19) #s64(3 7 9 2 4))

(clamp-test-u64 #u64(127 0 4 200 255) #f #f)
(clamp-test-u64 #u64(127 0 4 200 255) -4 #f)
(clamp-test-u64 #u64(127 0 4 200 255) 0 #f)
(clamp-test-u64 #u64(127 0 4 200 255) 199 #f)
(clamp-test-u64 #u64(127 0 4 200 255) #f -4)
(clamp-test-u64 #u64(127 0 4 200 255) #f 0)
(clamp-test-u64 #u64(127 0 4 200 255) #f 199)
(clamp-test-u64 #u64(127 0 4 200 255)
                #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-u64 #u64(127 0 4 200 255)
                0 #xffffffffffffffffffffffffffffff)
(clamp-test-u64 #u64(127 0 4 200 255)
                #u64(3 3 3 3 3) #u64(199 199 199 199 199))

;;-------------------------------------------------------------------
(test-section "block i/o")

(sys-unlink "test.o")

(let* ([data1 '#u8(0 1 2 3 255 254 253 252 4 5 6 7 251 250 249 248)]
       [data2 '#u8(1 0 3 2 254 255 252 253 5 4 7 6 250 251 248 249)]
       [data3 '#u8(3 2 1 0 252 253 254 255 7 6 5 4 248 249 250 251)]
       [data4 '#u8(252 253 254 255 3 2 1 0 248 249 250 251 7 6 5 4)]

       [data5 '#u8(3 0 1 3 3 1 0 3 131 0 1 131 131 1 0 131)]
       )

  ;; Invaliance:
  ;; w = (uvector-alias T u8v)  <--> w = read-block! T + write-block u8v
  (define (test-default-endian data)
    (^[T maker size]
      (test* (format "native endian ~a" (class-name T))
             (uvector-alias T data)
             (rlet1 buf (maker size)
               (call-with-output-file "test.o" (cut write-block data <>))
               (call-with-input-file "test.o" (cut read-block! buf <>))))))

  (define (test-reverse-endian T maker size)
    (let* ([rev-endian (case (native-endian)
                         [(big-endian) 'little-endian]
                         [(little-endian arm-little-endian) 'big-endian])]
           [rev-vec (cond
                     [(memq T (list <u8vector> <s8vector>)) data1]
                     [(memq T (list <u16vector> <s16vector>)) data2]
                     [(memq T (list <u32vector> <s32vector>)) data3]
                     [(memq T (list <u64vector> <s64vector>)) data4])])
      (test* (format "reading ~a ~a" rev-endian (class-name T))
             (uvector-alias T rev-vec)
             (rlet1 buf (maker size)
               (call-with-output-file "test.o" (cut write-block data1 <>))
               (call-with-input-file "test.o"
                 (cut read-block! buf <> 0 -1 rev-endian))))
      (test* (format "reading ~a ~a, parameter" rev-endian (class-name T))
             (uvector-alias T rev-vec)
             (rlet1 buf (maker size)
               (call-with-output-file "test.o" (cut write-block data1 <>))
               (parameterize ([default-endian rev-endian])
                 (call-with-input-file "test.o" (cut read-block! buf <>)))))
      (test* (format "writing ~a ~a" rev-endian (class-name T))
             (uvector-alias T data1)
             (rlet1 buf (maker size)
               (call-with-output-file "test.o"
                 (cut write-block (uvector-alias T rev-vec) <> 0 -1 rev-endian))
               (call-with-input-file "test.o" (cut read-block! buf <>))))
      (test* (format "writing ~a ~a, parameter" rev-endian (class-name T))
             (uvector-alias T data1)
             (rlet1 buf (maker size)
               (parameterize ([default-endian rev-endian])
                 (call-with-output-file "test.o"
                   (cut write-block (uvector-alias T rev-vec) <>)))
               (call-with-input-file "test.o" (cut read-block! buf <>))))))

  (define (run-across fn)
    (let1 s (u8vector-length data1)
      (fn <u8vector> make-u8vector s)
      (fn <s8vector> make-s8vector s)
      (fn <u16vector> make-u16vector (/ s 2))
      (fn <s16vector> make-s16vector (/ s 2))
      (fn <u32vector> make-u32vector (/ s 4))
      (fn <s32vector> make-s32vector (/ s 4))
      (fn <u64vector> make-u64vector (/ s 8))
      (fn <s64vector> make-s64vector (/ s 8))
      ))
  (define (run-across-f fn)
    (let1 s (u8vector-length data5)
      (fn <f16vector> make-f16vector (/ s 2))
      (fn <f32vector> make-f32vector (/ s 4))
      (fn <f64vector> make-f64vector (/ s 8))
      ))

  (run-across (test-default-endian data1))
  (run-across-f (test-default-endian data5))
  (run-across test-reverse-endian)
  )

;;-------------------------------------------------------------------
(test-section "string <-> uvector")

(test* "string->u8vector" '#u8(64 65 66 67 68)
       (string->u8vector "@ABCD"))
(test* "string->u8vector (start)" '#u8(66 67 68)
       (string->u8vector "@ABCD" 2))
(test* "string->u8vector (start, end)" '#u8(65 66 67)
       (string->u8vector "@ABCD" 1 4))
(test* "string->u8vector (start, end)" '#u8(64 65 66 67 68)
       (string->u8vector "@ABCD" 0 5))
(test* "string->u8vector (OOB)" (test-error)
       (string->u8vector "abcde" 2 6))

(test* "string->u8vector (immutable)" '(#t #t)
       (let ([a (string->u8vector "@ABCD" 0 -1 #t)]
             [b (string->u8vector "@ABCD")])
         (list (uvector-immutable? a)
               (equal? a b))))

(test* "string->u8vector!" '#u8(64 65 66 67 68)
       (let1 v (u8vector 0 1 2 3 4)
         (string->u8vector! v 0 "@ABCD")))
(test* "string->u8vector!" '#u8(64 65 66 67 68)
       (let1 v (u8vector 0 1 2 3 4)
         (string->u8vector! v 0 "@ABCDEFGHIJKLMNOPQRTSUVWXYZ")))
(test* "string->u8vector!" '#u8(64 65 66 3 4)
       (let1 v (u8vector 0 1 2 3 4)
         (string->u8vector! v 0 "@AB")))
(test* "string->u8vector!" '#u8(0 64 65 66 4)
       (let1 v (u8vector 0 1 2 3 4)
         (string->u8vector! v 1 "@AB")))
(test* "string->u8vector!" '#u8(0 1 2 3 65)
       (let1 v (u8vector 0 1 2 3 4)
         (string->u8vector! v 4 "@ABCDE" 1)))
(test* "string->u8vector!" '#u8(0 1 2 3 4)
       (let1 v (u8vector 0 1 2 3 4)
         (string->u8vector! v 8 "@ABCDE" 1)))
(test* "string->u8vector!" '#u8(0 1 2 3 4)
       (let1 v (u8vector 0 1 2 3 4)
         (string->u8vector! v -1 "@ABCDE" 1)))

(test* "u8vector->string" "@ABCD"
       (u8vector->string '#u8(64 65 66 67 68)))
(test* "u8vector->string (start)" "ABCD"
       (u8vector->string '#u8(64 65 66 67 68) 1))
(test* "u8vector->string (start, end)" "BC"
       (u8vector->string '#u8(64 65 66 67 68) 2 4))
(test* "u8vector->string (start, end)" "@ABCD"
       (u8vector->string '#u8(64 65 66 67 68) 0 5))
(test* "u8vector->string (OOB)" (test-error)
       (u8vector->string '#u8(64 65 66 67 68) 0 8))

(test* "u8vector->string (terminator, 0)" "@ABCD"
       (u8vector->string '#u8(64 65 66 67 68 0 69 70) 0 8 0))
(test* "u8vector->string (terminator, 0)" "@ABCD\x00;EF"
       (u8vector->string '#u8(64 65 66 67 68 0 69 70) 0 8 1))
(test* "u8vector->string (terminator, 0)" "@AB"
       (u8vector->string '#u8(64 65 66 67 68 0 69 70) 0 8 67))
(test* "u8vector->string (terminator, 0)" "BCD"
       (u8vector->string '#u8(64 65 66 67 68 0 69 70) 2 8 0))

(test* "string->s8vector" '#s8(64 65 66 67 68)
       (string->s8vector "@ABCD"))
(test* "string->s8vector (start)" '#s8(66 67 68)
       (string->s8vector "@ABCD" 2))
(test* "string->s8vector (start, end)" '#s8(65 66 67)
       (string->s8vector "@ABCD" 1 4))
(test* "string->s8vector (start, end)" '#s8(64 65 66 67 68)
       (string->s8vector "@ABCD" 0 5))
(test* "string->s8vector (OOB)" (test-error)
       (string->s8vector "abcde" 2 6))

(test* "string->s8vector!" '#s8(64 65 66 67 68)
       (let1 v (s8vector 0 1 2 3 4)
         (string->s8vector! v 0 "@ABCD")))
(test* "string->s8vector!" '#s8(64 65 66 67 68)
       (let1 v (s8vector 0 1 2 3 4)
         (string->s8vector! v 0 "@ABCDEFGHIJKLMNOPQRTSUVWXYZ")))
(test* "string->s8vector!" '#s8(64 65 66 3 4)
       (let1 v (s8vector 0 1 2 3 4)
         (string->s8vector! v 0 "@AB")))
(test* "string->s8vector!" '#s8(0 64 65 66 4)
       (let1 v (s8vector 0 1 2 3 4)
         (string->s8vector! v 1 "@AB")))
(test* "string->s8vector!" '#s8(0 1 2 3 65)
       (let1 v (s8vector 0 1 2 3 4)
         (string->s8vector! v 4 "@ABCDE" 1)))
(test* "string->s8vector!" '#s8(0 1 2 3 4)
       (let1 v (s8vector 0 1 2 3 4)
         (string->s8vector! v 8 "@ABCDE" 1)))
(test* "string->s8vector!" '#s8(0 1 2 3 4)
       (let1 v (s8vector 0 1 2 3 4)
         (string->s8vector! v -1 "@ABCDE" 1)))

(test* "s8vector->string" "@ABCD"
       (s8vector->string '#s8(64 65 66 67 68)))
(test* "s8vector->string (start)" "ABCD"
       (s8vector->string '#s8(64 65 66 67 68) 1))
(test* "s8vector->string (start, end)" "BC"
       (s8vector->string '#s8(64 65 66 67 68) 2 4))
(test* "s8vector->string (start, end)" "@ABCD"
       (s8vector->string '#s8(64 65 66 67 68) 0 5))
(test* "s8vector->string (OOB)" (test-error)
       (s8vector->string '#s8(64 65 66 67 68) 0 8))

(test* "s8vector->string (terminator,0)" "@ABCD"
       (s8vector->string '#s8(64 65 66 67 68 0 69 70) 0 8 0))
(test* "s8vector->string (terminator,1)" "@ABCD\x00;EF"
       (s8vector->string '#s8(64 65 66 67 68 0 69 70) 0 8 1))
(test* "s8vector->string (terminator,-1)" "@ABCD"
       (s8vector->string '#s8(64 65 66 67 68 -1 69 70) 0 8 -1))

(test* "string->u32vector" '#u32(64 65 66 67 68)
       (string->u32vector "@ABCD"))
(test* "string->u32vector (start)" '#u32(66 67 68)
       (string->u32vector "@ABCD" 2))
(test* "string->u32vector (start, end)" '#u32(65 66 67)
       (string->u32vector "@ABCD" 1 4))
(test* "string->u32vector (start, end)" '#u32(64 65 66 67 68)
       (string->u32vector "@ABCD" 0 5))
(test* "string->u32vector (OOB)" (test-error)
       (string->u32vector "abcde" 2 6))

(test* "string->u32vector!" '#u32(64 65 66 67 68 1 1 1)
       (string->u32vector! (make-u32vector 8 1) 0 "@ABCD"))
(test* "string->u32vector!" '#u32(1 1 1 1 64 65 66 67)
       (string->u32vector! (make-u32vector 8 1) 4 "@ABCD"))
(test* "string->u32vector!" '#u32(1 1 1 66 67 1 1 1)
       (string->u32vector! (make-u32vector 8 1) 3 "@ABCD" 2 4))
       

(test* "u32vector->string" "@ABCD"
       (u32vector->string '#u32(64 65 66 67 68)))
(test* "u32vector->string (start)" "ABCD"
       (u32vector->string '#u32(64 65 66 67 68) 1))
(test* "u32vector->string (start, end)" "BC"
       (u32vector->string '#u32(64 65 66 67 68) 2 4))
(test* "u32vector->string (start, end)" "@ABCD"
       (u32vector->string '#u32(64 65 66 67 68) 0 5))
(test* "u32vector->string (OOB)" (test-error)
       (u32vector->string '#u32(64 65 66 67 68) 0 8))
(test* "u32vector->string (terminator)" "@AB"
       (u32vector->string '#u32(64 65 66 0 67 68) 0 -1 0))
(test* "u32vector->string (terminator)" "@AB\0CD"
       (u32vector->string '#u32(64 65 66 0 67 68) 0 -1 1))
(test* "u32vector->string (terminator)" "@AB\0C"
       (u32vector->string '#u32(64 65 66 0 67 68) 0 -1 68))

(test* "string->s32vector" '#s32(64 65 66 67 68)
       (string->s32vector "@ABCD"))
(test* "string->s32vector (start)" '#s32(66 67 68)
       (string->s32vector "@ABCD" 2))
(test* "string->s32vector (start, end)" '#s32(65 66 67)
       (string->s32vector "@ABCD" 1 4))
(test* "string->s32vector (start, end)" '#s32(64 65 66 67 68)
       (string->s32vector "@ABCD" 0 5))
(test* "string->s32vector (OOB)" (test-error)
       (string->s32vector "abcde" 2 6))

(test* "s32vector->string" "@ABCD"
       (s32vector->string '#s32(64 65 66 67 68)))
(test* "s32vector->string (start)" "ABCD"
       (s32vector->string '#s32(64 65 66 67 68) 1))
(test* "s32vector->string (start, end)" "BC"
       (s32vector->string '#s32(64 65 66 67 68) 2 4))
(test* "s32vector->string (start, end)" "@ABCD"
       (s32vector->string '#s32(64 65 66 67 68) 0 5))
(test* "s32vector->string (OOB)" (test-error)
       (s32vector->string '#s32(64 65 66 67 68) 0 8))
(test* "s32vector->string (terminator)" "@AB"
       (s32vector->string '#s32(64 65 66 0 67 68) 0 -1 0))
(test* "s32vector->string (terminator)" "@AB\0CD"
       (s32vector->string '#s32(64 65 66 0 67 68) 0 -1 1))
(test* "s32vector->string (terminator)" "@AB\0C"
       (s32vector->string '#s32(64 65 66 0 67 68) 0 -1 68))

;; test for multibyte chars
(cond-expand
 [gauche.ces.eucjp (include "test-eucjp")]
 [gauche.ces.utf8  (include "test-utf8")]
 [gauche.ces.sjis  (include "test-sjis")]
 [else])

;;-------------------------------------------------------------------
(test-section "uvector alias")

(test* "alias u8 u8" #u8(0 1 2 3)
       (let* ([src (u8vector 0 1 2 0)]
              [dst (uvector-alias <u8vector> src)])
         (u8vector-set! src 3 3)
         dst))
(test* "alias u8 u8 (range)" #u8(1 2)
       (let* ([src (u8vector 0 1 0 1)]
              [dst (uvector-alias <u8vector> src 1 3)])
         (u8vector-set! src 2 2)
         dst))
(test* "alias s8 u8" #s8(1 -1)
       (let* ([src (u8vector 0 0 0 0)]
              [dst (uvector-alias <s8vector> src 1 3)])
         (s8vector-set! dst 0 1)
         (u8vector-set! src 2 255)
         dst))
;; the following test cases avoid endian complexity
(test* "alias s8 u16" '(#s8(#x11 #x11 #x22 #x22) #x1111)
       (let* ([src (u16vector 0 0 0)]
              [dst (uvector-alias <s8vector> src 1)])
         (u16vector-set! src 2 #x2222)
         (s8vector-set! dst 0 #x11)
         (s8vector-set! dst 1 #x11)
         (list dst (u16vector-ref src 1))))
(test* "alias u32 u8" #u32(#xaaaaaaaa #xbbbbbbbb)
       (let* ([src (make-u8vector 8 #xaa)]
              [ali (uvector-alias <u8vector> src 4)]
              [dst (uvector-alias <u32vector> src)])
         (u8vector-fill! ali #xbb)
         dst))

;; tricky, but should work on IEEE754 compiant floats
(test* "alias u32 f32" #u32(0)
       (uvector-alias <u32vector> #f32(0.0)))
(test* "alias u32 f64" #u32(0 0)
       (uvector-alias <u32vector> #f64(0.0)))
(test* "alias f64 u32" #f64(0.0)
       (uvector-alias <f64vector> #u32(1 1 0 0) 2))
(test* "alias u64 f64" #u64(0)
       (uvector-alias <u64vector> #f64(0.0)))

(test* "alias turn-around" #f32(1.5)
       (uvector-alias <f32vector>
                      (uvector-alias <u8vector> #f32(1.0 1.5 2.0))
                      4 8))

;; test alignment check
(test* "alias u32 u8 (alignment violation)" (test-error)
       (let* ([src (make-u8vector 9)]
              [dst (uvector-alias <u32vector>)])
         dst))
(test* "alias u32 u8 (alignment violation)" (test-error)
       (let* ([src (make-u8vector 32)]
              [dst (uvector-alias <u32vector> 2)])
         dst))
(test* "alias u32 u8 (alignment violation)" (test-error)
       (let* ([src (make-u8vector 32)]
              [dst (uvector-alias <u32vector> 4 5)])
         dst))

;; test if immutable property propagates
(test* "immutability violation" (test-error)
       (let* ([src '#u8(0 1 2 3)]
              [dst (uvector-alias <u8vector> src)])
         (u8vector-set! dst 0 1)))

;;-------------------------------------------------------------------
; (use gauche.array)
(test-section "gauche.array")
(use gauche.array)
(test-module 'gauche.array)

(test-section "simple array op")

(test* "shape" #t
       (and (shape)
            (shape -1 -1)
            (shape -1 0)
            (shape -1 1)
            (shape 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
            #t))

(test* "shape" (test-error) (shape 1))
(test* "shape" (test-error) (shape 1 2 3))
(test* "shape" (test-error) (shape 3 1))

(test* "make-array" #t
       (and (make-array (shape))
            (make-array (shape) *)
            (make-array (shape -1 -1))
            (make-array (shape -1 -1) *)
            (make-array (shape -1 1))
            (make-array (shape 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4) *)
            #t))

(test* "array" #t
       (and (array (shape) *)
            (array (shape -1 -1))
            (array (shape -1 1) * *)
            (array (shape 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8) *)
            #t))

(test-section "array-rank")
(test* "array-rank (shape)" 2
       (array-rank (shape)))
(test* "array-rank (shape)" 2
       (array-rank (shape -1 -1)))
(test* "array-rank (shape)" 2
       (array-rank (shape 1 2 3 4 5 6 7 8)))

(test* "array-rank (make-array)" 0
       (array-rank (make-array (shape))))
(test* "array-rank (make-array)" 1
       (array-rank (make-array (shape -1 -1))))
(test* "array-rank (make-array)" 1
       (array-rank (make-array (shape -1 1))))
(test* "array-rank (make-array)" 4
       (array-rank (make-array (shape 1 2 3 4 5 6 7 8))))

(test* "array-rank (array)" 0
       (array-rank (array (shape) *)))
(test* "array-rank (array)" 1
       (array-rank (array (shape -1 -1))))
(test* "array-rank (array)" 1
       (array-rank (array (shape -1 1) * *)))
(test* "array-rank (array)" 4
       (array-rank (array (shape 1 2 3 4 5 6 7 8) *)))

(test-section "array-start and array-end")
(test* "array-start (shape)" 0
       (array-start (shape -1 -1) 0))
(test* "array-start (shape)" 0
       (array-start (shape -1 -1) 1))
(test* "array-start (shape)" 0
       (array-start (shape -1 1) 0))
(test* "array-start (shape)" 0
       (array-start (shape -1 1) 1))
(test* "array-start (shape)" 0
       (array-start (shape 1 2 3 4 5 6 7 8) 0))
(test* "array-start (shape)" 0
       (array-start (shape 1 2 3 4 5 6 7 8) 1))

(test* "array-end (shape)" 1
       (array-end (shape -1 -1) 0))
(test* "array-end (shape)" 2
       (array-end (shape -1 -1) 1))
(test* "array-end (shape)" 1
       (array-end (shape -1 1) 0))
(test* "array-end (shape)" 2
       (array-end (shape -1 1) 1))
(test* "array-end (shape)" 4
       (array-end (shape 1 2 3 4 5 6 7 8) 0))
(test* "array-end (shape)" 2
       (array-end (shape 1 2 3 4 5 6 7 8) 1))

(test* "array-start (make-array)" -1
       (array-start (make-array (shape -1 -1)) 0))
(test* "array-start (make-array)" -1
       (array-start (make-array (shape -1 1)) 0))
(test* "array-start (make-array)" '(1 3 5 7)
       (map (pa$ array-start (make-array (shape 1 2 3 4 5 6 7 8)))
            '(0 1 2 3)))

(test* "array-end (make-array)" -1
       (array-end (make-array (shape -1 -1)) 0))
(test* "array-end (make-array)" 1
       (array-end (make-array (shape -1 1)) 0))
(test* "array-end (make-array)" '(2 4 6 8)
       (map (pa$ array-end (make-array (shape 1 2 3 4 5 6 7 8)))
            '(0 1 2 3)))

(test* "array-start (array)" -1
       (array-start (array (shape -1 -1)) 0))
(test* "array-start (array)" -1
       (array-start (array (shape -1 1) * *) 0))
(test* "array-start (array)" '(1 3 5 7)
       (map (pa$ array-start (array (shape 1 2 3 4 5 6 7 8) *))
            '(0 1 2 3)))

(test* "array-end (array)" -1
       (array-end (array (shape -1 -1)) 0))
(test* "array-end (array)" 1
       (array-end (array (shape -1 1) * *) 0))
(test* "array-end (array)" '(2 4 6 8)
       (map (pa$ array-end (array (shape 1 2 3 4 5 6 7 8) *))
            '(0 1 2 3)))

(test-section "array-ref")
(for-each
 (^[ls]
   (let1 a (car ls)
     (for-each
      (^t (test* (format "array-ref ~S:" (cdr t)) (car t)
                 (apply array-ref a (cdr t))))
      (cdr ls))))
 '((#,(<array> () a) (a))
   (#,(<array> (-1 1) a b) (a -1) (b 0))
   (#,(<array> (1 2 3 4 5 6 7 8) a) (a 1 3 5 7))
   (#,(<array> (1 3 4 6 7 9 10 12) a b c d e f g h i j k l m n o p)
    (a 1 4 7 10) (b 1 4 7 11) (i 2 4 7 10) (n 2 5 7 11))
   (#,(<array> () a) (a #()))
   (#,(<array> (-1 1) a b) (a #(-1)) (b #(0)))
   (#,(<array> (1 2 3 4 5 6 7 8) a) (a #(1 3 5 7)))
   (#,(<array> (1 3 4 6 7 9 10 12) a b c d e f g h i j k l m n o p)
    (a #(1 4 7 10)) (b #(1 4 7 11)) (i #(2 4 7 10)) (n #(2 5 7 11)))
   (#,(<array> () a) (a #,(<array> (0 0))))
   (#,(<array> (-1 1) a b) (a #,(<array> (0 1) -1)) (b #,(<array> (0 1) 0)))
   (#,(<array> (1 2 3 4 5 6 7 8) a) (a #,(<array> (0 4) 1 3 5 7)))
   (#,(<array> (1 3 4 6 7 9 10 12) a b c d e f g h i j k l m n o p)
    (a #,(<array> (0 4) 1 4 7 10)) (b #,(<array> (0 4) 1 4 7 11))
    (i #,(<array> (0 4) 2 4 7 10)) (n #,(<array> (0 4) 2 5 7 11)))
   (#,(<u8array> () 2) (2))
   (#,(<u8array> (-1 1) 2 3) (2 -1) (3 0))
   (#,(<u8array> (1 2 3 4 5 6 7 8) 2) (2 1 3 5 7))
   (#,(<u8array> (1 3 4 6 7 9 10 12) 2 3 5 7 11 13 17 23 29 31 37 39 41 43 47 51)
    (2 1 4 7 10) (3 1 4 7 11) (29 2 4 7 10) (43 2 5 7 11))
   ))

(test-section "array-set!")
(for-each
 (^[ls]
   (let1 a (car ls)
     (for-each
      (^t (test* (format "array-set! ~S:" (cdr t)) (car t)
                 (begin (apply array-set! a (append (cdr t) (list (car t))))
                        (apply array-ref a (cdr t)))))
      (cdr ls))))
 '((#,(<array> () a) (x))
   (#,(<array> (-1 1) a b) (y -1) (z 0))
   (#,(<array> (1 2 3 4 5 6 7 8) a) (x 1 3 5 7))
   (#,(<array> (1 3 4 6 7 9 10 12) a b c d e f g h i j k l m n o p)
    (w 1 4 7 10) (x 1 4 7 11) (y 2 4 7 10) (z 2 5 7 11))
   (#,(<array> () a) (x #()))
   (#,(<array> (-1 1) a b) (x #(-1)) (y #(0)))
   (#,(<array> (1 2 3 4 5 6 7 8) a) (x #(1 3 5 7)))
   (#,(<array> (1 3 4 6 7 9 10 12) a b c d e f g h i j k l m n o p)
    (w #(1 4 7 10)) (x #(1 4 7 11)) (y #(2 4 7 10)) (z #(2 5 7 11)))
   (#,(<array> () a) (x #,(<array> (0 0))))
   (#,(<array> (-1 1) a b) (x #,(<array> (0 1) -1)) (y #,(<array> (0 1) 0)))
   (#,(<array> (1 2 3 4 5 6 7 8) a) (x #,(<array> (0 4) 1 3 5 7)))
   (#,(<array> (1 3 4 6 7 9 10 12) a b c d e f g h i j k l m n o p)
    (w #,(<array> (0 4) 1 4 7 10)) (x #,(<array> (0 4) 1 4 7 11))
    (y #,(<array> (0 4) 2 4 7 10)) (z #,(<array> (0 4) 2 5 7 11)))
   (#,(<u8array> () 2) (102))
   (#,(<u8array> (-1 1) 2 3) (102 -1) (103 0))
   (#,(<u8array> (1 2 3 4 5 6 7 8) 2) (102 1 3 5 7))
   (#,(<u8array> (1 3 4 6 7 9 10 12) 2 3 5 7 11 13 17 23 29 31 37 39 41 43 47 51)
    (102 1 4 7 10) (103 1 4 7 11) (129 2 4 7 10) (143 2 5 7 11))
   ))

(test* "array-valid-index? (list)" #t
       (array-valid-index? (make-array (shape) 'o)))
(test* "array-valid-index? (list)" #t
       (array-valid-index? (make-array (shape -1 1) 'o) -1))
(test* "array-valid-index? (list)" #t
       (array-valid-index? (make-array (shape 1 2 3 4 5 6 7 8) 'o) 1 3 5 7))
(test* "array-valid-index? (list)" #f
       (array-valid-index? (make-array (shape) 'o) 0))
(test* "array-valid-index? (list)" #f
       (array-valid-index? (make-array (shape -1 1) 'o) -2))
(test* "array-valid-index? (list)" #f
       (array-valid-index? (make-array (shape 1 2 3 4 5 6 7 8) 'o) 1 3 5))

(test* "array-valid-index? (vector)" #t
       (array-valid-index? (make-array (shape) 'o) #()))
(test* "array-valid-index? (vector)" #t
       (array-valid-index? (make-array (shape -1 1) 'o) #(0)))
(test* "array-valid-index? (vector)" #t
       (array-valid-index? (make-array (shape 1 2 3 4 5 6 7 8) 'o) #(1 3 5 7)))
(test* "array-valid-index? (vector)" #f
       (array-valid-index? (make-array (shape) 'o) #(0)))
(test* "array-valid-index? (vector)" #f
       (array-valid-index? (make-array (shape -1 1) 'o) #(1)))
(test* "array-valid-index? (vector)" #f
       (array-valid-index? (make-array (shape 1 2 3 4 5 6 7 8) 'o) #(1 3 5 7 9)))

(test* "array-valid-index? (array)" #t
       (array-valid-index? (make-array (shape) 'o) #,(<array> (0 0))))
(test* "array-valid-index? (array)" #t
       (array-valid-index? (make-array (shape -1 1) 'o) #,(<array> (0 1) 0)))
(test* "array-valid-index? (array)" #t
       (array-valid-index? (make-array (shape 1 2 3 4 5 6 7 8) 'o) #,(<array> (0 4) 1 3 5 7)))
(test* "array-valid-index? (array)" #f
       (array-valid-index? (make-array (shape) 'o) #,(<array> (0 1) 0)))
(test* "array-valid-index? (array)" #f
       (array-valid-index? (make-array (shape -1 1) 'o) #,(<array> (0 1) 1)))
(test* "array-valid-index? (array)" #f
      (array-valid-index? (make-array (shape 1 2 3 4 5 6 7 8) 'o) #,(<array> (0 4) 1 4 5 7)))

;;; Share and change:
;;;
;;;  org     brk     swp            box
;;;
;;;   0 1     1 2     5 6
;;; 6 a b   2 a b   3 d c   0 2 4 6 8: e
;;; 7 c d   3 e f   4 f e
;;; 8 e f

(test-section "shared change")

(let* ([org (array (shape 6 9 0 2) 'a 'b 'c 'd 'e 'f)]
       [brk (share-array org
                         (shape 2 4 1 3)
                         (^[r k] (values
                                  (+ 6 (* 2 (- r 2)))
                                  (- k 1))))]
       [swp (share-array org
                         (shape 3 5 5 7)
                         (^[r k] (values
                                  (+ 7 (- r 3))
                                  (- 1 (- k 5)))))]
       [box (share-array swp
                         (shape 0 1 2 3 4 5 6 7 8 9)
                         (^ _ (values 4 6)))]
       [org-contents (^[] (list (array-ref org 6 0) (array-ref org 6 1)
                                (array-ref org 7 0) (array-ref org 7 1)
                                (array-ref org 8 0) (array-ref org 8 1)))]
       [brk-contents (^[] (list (array-ref brk 2 1) (array-ref brk 2 2)
                                (array-ref brk 3 1) (array-ref brk 3 2)))]
       [swp-contents (^[] (list (array-ref swp 3 5) (array-ref swp 3 6)
                                (array-ref swp 4 5) (array-ref swp 4 6)))]
       [box-contents (^[] (list (array-ref box 0 2 4 6 8)))])
  (test "org-contents" '(a b c d e f) org-contents)
  (test "brk-contents" '(a b e f) brk-contents)
  (test "swp-contents" '(d c f e) swp-contents)
  (test "box-contents" '(e) box-contents)
  (begin (array-set! org 6 0 'x) #t)
  (test "org-contents" '(x b c d e f) org-contents)
  (test "brk-contents" '(x b e f) brk-contents)
  (test "swp-contents" '(d c f e) swp-contents)
  (test "box-contents" '(e) box-contents)
  (begin (array-set! brk 3 1 'y) #t)
  (test "org-contents" '(x b c d y f) org-contents)
  (test "brk-contents" '(x b y f) brk-contents)
  (test "swk-contents" '(d c f y) swp-contents)
  (test "box-contents" '(y) box-contents)
  (begin (array-set! swp 4 5 'z) #t)
  (test "org-contents" '(x b c d y z) org-contents)
  (test "brk-contents" '(x b y z) brk-contents)
  (test "swp-contents" '(d c z y) swp-contents)
  (test "box-contents" '(y) box-contents)
  (begin (array-set! box 0 2 4 6 8 'e) #t)
  (test "org-contents" '(x b c d e z) org-contents)
  (test "brk-contents" '(x b e z) brk-contents)
  (test "swp-contents" '(d c z e) swp-contents)
  (test "box-contents" '(e) box-contents)
  )

;;; Check that arrays copy the shape specification

(test-section "array-set! of shape")

(let1 shp (shape 10 12)
  (let ([arr (make-array shp)]
        [ars (array shp * *)]
        [art (share-array (make-array shp) shp identity)])
    (array-set! shp 0 0 '?)
    (array-set! shp 0 1 '!)
    (test* "modifying array shape"
           '(2 0 1 0 2 ? ! 1 10 12 1 10 12 1 10 12)
           (list (array-rank shp)
                 (array-start shp 0)
                 (array-end shp 0)
                 (array-start shp 1)
                 (array-end shp 1)
                 (array-ref shp 0 0)
                 (array-ref shp 0 1)
                 (array-rank arr)
                 (array-start arr 0)
                 (array-end arr 0)
                 (array-rank ars)
                 (array-start ars 0)
                 (array-end ars 0)
                 (array-rank art)
                 (array-start art 0)
                 (array-end art 0)))
    ))
;;; Check that index arrays work even when they share
;;;
;;; arr       ixn
;;;   5  6      0 1
;;; 4 nw ne   0 4 6
;;; 5 sw se   1 5 4

(test-section "array access with sharing index array")
(let ([arr (array (shape 4 6 5 7) 'nw 'ne 'sw 'se)]
      [ixn (array (shape 0 2 0 2) 4 6 5 4)])
  (let ([col0 (share-array ixn
                           (shape 0 2)
                           (^k (values k 0)))]
        [row0 (share-array ixn
                           (shape 0 2)
                           (^k (values 0 k)))]
        [wor1 (share-array ixn
                           (shape 0 2)
                           (^k (values 1 (- 1 k))))]
        [cod (share-array ixn
                          (shape 0 2)
                          (^k (case k
                                [(0) (values 1 0)]
                                [(1) (values 0 1)])))]
        [box (share-array ixn
                          (shape 0 2)
                          (^k (values 1 0)))])
    (test* "array-ref before change"
           '(nw ne nw se sw)
           (list (array-ref arr col0)
                 (array-ref arr row0)
                 (array-ref arr wor1)
                 (array-ref arr cod)
                 (array-ref arr box)))
    (array-set! arr col0 'ul)
    (array-set! arr row0 'ur)
    (array-set! arr cod 'lr)
    (array-set! arr box 'll)
    (test* "array-ref after change"
           '(ul ur ll lr)
           (list (array-ref arr 4 5)
                 (array-ref arr 4 6)
                 (array-ref arr 5 5)
                 (array-ref arr 5 6)))
    (array-set! arr wor1 'xx)
    (test* "array-ref after change" 'xx
           (array-ref arr 4 5))
    ))

;;; Check that shape arrays work even when they share
;;;
;;; arr             shp       shq       shr       shs
;;;    1  2  3  4      0  1      0  1      0  1      0  1
;;; 1 10 12 16 20   0 10 12   0 12 20   0 10 10   0 12 12
;;; 2 10 11 12 13   1 10 11   1 11 13   1 11 12   1 12 12
;;;                                     2 12 16
;;;                                     3 13 20

(test-section "sharing shape array")
(let1 arr (array (shape 1 3 1 5) 10 12 16 20 10 11 12 13)
  (let ([shp (share-array arr
                          (shape 0 2 0 2)
                          (^[r k] (values (+ r 1) (+ k 1))))]
        [shq (share-array arr
                          (shape 0 2 0 2)
                          (^[r k] (values (+ r 1) (* 2 (+ 1 k)))))]
        [shr (share-array arr
                          (shape 0 4 0 2)
                          (^[r k] (values (- 2 k) (+ r 1))))]
        [shs (share-array arr
                          (shape 0 2 0 2)
                          (^[r k] (values 2 3)))])
    (test* "using make-array shp"
           '(2 10 12 10 11)
           (let ((arr-p (make-array shp)))
             (list (array-rank arr-p)
                   (array-start arr-p 0)
                   (array-end arr-p 0)
                   (array-start arr-p 1)
                   (array-end arr-p 1))))
    (test* "using array shq"
           '(2 12 20 11 13)
           (let1 arr-q (array shq * * * *  * * * *  * * * *  * * * *)
             (list (array-rank arr-q)
                   (array-start arr-q 0)
                   (array-end arr-q 0)
                   (array-start arr-q 1)
                   (array-end arr-q 1))))
    (test* "using share-array"
           '(4 10 10 11 12 12 16 13 20)
           (let1 arr-r (share-array (array (shape) *)
                                    shr
                                    (^ _ (values)))
             (list (array-rank arr-r)
                   (array-start arr-r 0)
                   (array-end arr-r 0)
                   (array-start arr-r 1)
                   (array-end arr-r 1)
                   (array-start arr-r 2)
                   (array-end arr-r 2)
                   (array-start arr-r 3)
                   (array-end arr-r 3))))
    (test* "using make-array shs"
           '(2 12 12 12 12)
           (let1 arr-s (make-array shs)
             (list (array-rank arr-s)
                   (array-start arr-s 0)
                   (array-end arr-s 0)
                   (array-start arr-s 1)
                   (array-end arr-s 1))))
    ))

(test-section "sharing with sharing subshape")
(let ([super (array (shape 4 7 4 7)
                    1 * *
                    * 2 *
                    * * 3)]
      [subshape (share-array (array (shape 0 2 0 3)
                                    * 4 *
                                    * 7 *)
                             (shape 0 1 0 2)
                             (^[r k] (values k 1)))])
  (let1 sub (share-array super subshape (^k (values k k)))
    (test* "subshape check" #t
           (equal? subshape (shape 4 7)))
    (test* "sharing subshape" '(2 0 1 0 2 4 7)
           (list (array-rank subshape)
                 (array-start subshape 0)
                 (array-end subshape 0)
                 (array-start subshape 1)
                 (array-end subshape 1)
                 (array-ref subshape 0 0)
                 (array-ref subshape 0 1)))
    (test* "sub check" #t
           (equal? sub (array (shape 4 7) 1 2 3)))
    (test* "sharing with sharing subshape" '(1 4 7 1 2 3)
           (list (array-rank sub)
                 (array-start sub 0)
                 (array-end sub 0)
                 (array-ref sub 4)
                 (array-ref sub 5)
                 (array-ref sub 6)))
    ))

(test-section "subarray")
(let ([super (array (shape 0 3 0 3)
                    1 * *
                    * 2 *
                    * * 3)]
      [subshape (shape 1 3 1 3)])
  (let1 sub (subarray super subshape)
    (test* "sub check" #t
           (equal? sub (array (shape 0 2 0 2) 2 * * 3)))
    (test* "sharing with sharing subshape" (list 2 0 2 2 * * 3)
           (list (array-rank sub)
                 (array-start sub 0)
                 (array-end sub 0)
                 (array-ref sub 0 0)
                 (array-ref sub 0 1)
                 (array-ref sub 1 0)
                 (array-ref sub 1 1)))
    ))

;;----------------------------------------------------------------
(test-section "array-iteration")

(test* "array-for-each-index (list)" '((0 0) (0 1) (1 0) (1 1))
  (let ([ar (make-array (shape 0 2 0 2))]
        [ls '()])
    (array-for-each-index ar (^[a b] (push! ls (list a b))))
    (reverse ls)))

(test* "array-for-each-index (vector)" '(#(0 0) #(0 1) #(1 0) #(1 1))
  (let ([ar (make-array (shape 0 2 0 2))]
        [ls '()]
        [vec (make-vector 2)])
    (array-for-each-index ar (^v (push! ls (vector-copy v))) vec)
    (reverse ls)))

(test* "array-for-each-index (array)" '(#(0 0) #(0 1) #(1 0) #(1 1))
  (let ([ar (make-array (shape 0 2 0 2))]
        [ls '()]
        [ind (make-array (shape 0 2))])
    (array-for-each-index ar (^a (push! ls (array->vector a))) ind)
    (reverse ls)))

(test* "array-for-each-index-by-dimension (list)" '(0 1)
  (let ([ar (make-array (shape 0 2 5 7))]
        [ls '()])
    (array-for-each-index-by-dimension ar '(0) (^[a b] (push! ls a)))
    (reverse ls)))

(test* "array-for-each-index-by-dimension (vector)" '(#(3 5) #(3 6) #(3 7))
  (let ([ar (make-array (shape 0 2 5 8))]
        [ls '()])
    (array-for-each-index-by-dimension
     ar '(1) (^v (push! ls (vector-copy v))) (vector 3 4))
    (reverse ls)))

(test* "array-for-each-index-by-dimension (array)" '(#(5) #(6) #(7))
  (let ([ar (make-array (shape 5 8 0 2))]
        [ls '()])
    (array-for-each-index-by-dimension
     ar '(0) (^a (push! ls (array->vector a))) (make-array (shape 0 1)))
    (reverse ls)))

(test* "shape-for-each (list)" '((0 0) (0 1) (1 0) (1 1))
  (let ([sh (shape 0 2 0 2)]
        [ls '()])
    (shape-for-each sh (^[a b] (push! ls (list a b))))
    (reverse ls)))

(test* "shape-for-each (vector)" '(#(0 0) #(0 1) #(1 0) #(1 1))
  (let ([sh (shape 0 2 0 2)]
        [ls '()]
        [vec (make-vector 2)])
    (shape-for-each sh (^v (push! ls (vector-copy v))) vec)
    (reverse ls)))

(test* "shape-for-each (array)" '(#(0 0) #(0 1) #(1 0) #(1 1))
  (let ([sh (shape 0 2 0 2)]
        [ls '()]
        [ind (make-array (shape 0 2))])
    (shape-for-each sh (^a (push! ls (array->vector a))) ind)
    (reverse ls)))

(test* "tabulate-array (list)" #,(<array> (1 3 1 3) 11 12 21 22)
  (tabulate-array (shape 1 3 1 3) (^[a b] (+ (* a 10) b))))

(test* "tabulate-array (vector)" #,(<array> (2 4 2 4) 12 31 17 36)
  (let ([vec (make-vector 2)]
        [square (^x (* x x))]
        [cube (^x (* x x x))])
    (tabulate-array (shape 2 4 2 4) (^v (+ (square (ref v 0))
                                           (cube (ref v 1))))
                    vec)))

(test* "array-retabulate! (list)" #,(<array> (0 2 0 2) 1.0 0.5 0.25 0.125)
  (rlet1 ar #,(<array> (0 2 0 2) 1 2 4 8)
    (array-retabulate! ar (^[i j] (/ 1.0 (array-ref ar i j))))))

(test* "array-retabulate! (vector)" #,(<array> (0 2 0 2) 1.0 0.5 0.25 0.125)
  (let ([ar #,(<array> (0 2 0 2) 1 2 4 8)]
        [vec (make-vector 2)])
    (array-retabulate! ar (^v (/ 1.0 (array-ref ar v))) vec)
    ar))

(test* "array-retabulate! (uniform)" #,(<f64array> (0 2 0 2) 1.0 0.5 0.25 0.125)
  (let ([ar #,(<f64array> (0 2 0 2) 1 2 4 8)]
        [vec (make-vector 2)])
    (array-retabulate! ar (^v (/ 1.0 (array-ref ar v))) vec)
    ar))

(test* "array-map-1" #,(<array> (0 2 0 2) 1 4 9 16)
  (array-map (^x (* x x)) #,(<array> (0 2 0 2) 1 2 3 4)))

(test* "array-map-2" #,(<array> (0 2 0 2) 11 22 33 44)
  (array-map (^[a b] (+ a b))
             #,(<array> (0 2 0 2) 1 2 3 4)
             #,(<array> (0 2 0 2) 10 20 30 40)))

(test* "array-map-3" #,(<array> (0 2 0 2) 11.1 22.2 33.3 44.4)
  (array-map (lambda (a b c) (+ a b c))
             #,(<array> (0 2 0 2) 1 2 3 4)
             #,(<array> (0 2 0 2) 10 20 30 40)
             #,(<array> (0 2 0 2) .1 .2 .3 .4)))

(test* "array-map-4" #,(<array> (0 2 0 2) 111.0 22.2 3.33 .444)
  (array-map (lambda (a b c d) (* d (+ a b c)))
             #,(<array> (0 2 0 2) 1 2 3 4)
             #,(<array> (0 2 0 2) 10 20 30 40)
             #,(<array> (0 2 0 2) .1 .2 .3 .4)
             #,(<array> (0 2 0 2) 10.0 1.0 0.1 0.01)))

(test* "array-map-uniform" #,(<array> (0 2 0 2) 111.0 22.2 3.33 .444)
  (array-map (lambda (a b c d) (/ (truncate (* (* d (+ a b c)) 10000)) 10000))
             #,(<u8array> (0 2 0 2) 1 2 3 4)
             #,(<s32array> (0 2 0 2) 10 20 30 40)
             #,(<f32array> (0 2 0 2) .1 .2 .3 .4)
             #,(<f64array> (0 2 0 2) 10.0 1.0 0.1 0.01)))

(test* "array-every-1" #t
  (array-every even? #,(<array> (0 0))))
(test* "array-every-2" #t
  (array-every even? #,(<array> () 2)))
(test* "array-every-3" #t
  (array-every even? #,(<array> (0 2 0 2) 2 4 6 8)))
(test* "array-every-4" #f
  (array-every even? #,(<array> () 3)))
(test* "array-every-5" #f
  (array-every even? #,(<array> (0 2 0 2) 2 4 3 8)))

(test* "array-any-1" #t
  (array-any even? #,(<array> () 2)))
(test* "array-any-2" #t
  (array-any even? #,(<array> (0 2 0 2) 1 2 3 8)))
(test* "array-any-3" #f
  (array-any even? #,(<array> (0 0))))
(test* "array-any-4" #f
  (array-any even? #,(<array> () 3)))
(test* "array-any-5" #f
  (array-any even? #,(<array> (0 2 0 2) 1 3 5 7)))

(test-section "matrices")

;; zero-base the shape
(define (array-normalize a)
  (define (every-other pred ls)
    (if (null? ls) #t (and (pred (car ls)) (every-other pred (cddr ls)))))
  (if (every-other zero? (array->list (array-shape a)))
    a
    (subarray a (array-shape a))))

(define (approx-equal? x y . opt)
  (< (abs (- x y)) (get-optional opt 0.0000001)))

(define (array-approx-equal? a b)
  (or (eq? a b)
      (and (eq? (class-of a) (class-of b))
           (array-equal? a b approx-equal?))))

(let1 i 0
  (for-each
   (^t (let-optionals* t (ar (inv #f) (det 0))
         (test* (format "array-inverse-~D" (inc! i)) inv
                (array-inverse ar)
                array-approx-equal?)
         (when (and inv (eq? (class-of ar) (class-of inv)))
           (test* (format "array-inverse-~D" (inc! i)) (array-normalize ar)
                  (array-inverse inv)
                  array-approx-equal?))
         (test* (format "determinant-~D" (inc! i)) det
                (determinant ar)
                approx-equal?)))
   '((#,(<array> (0 2 0 2) 1 2 3 4)
      #,(<array> (0 2 0 2) -2.0 1.0 1.5 -0.5)
      -2)
     (#,(<array> (1 3 1 3) 1 2 3 4)
      #,(<array> (0 2 0 2) -2.0 1.0 1.5 -0.5)
      -2)
     (#,(<array> (3 5 7 9) 1 2 3 4)
      #,(<array> (0 2 0 2) -2.0 1.0 1.5 -0.5)
      -2)
     (#,(<array> (0 3 0 3) 1 5 2 1 1 7 0 -3 4)
      #,(<array> (0 3 0 3) -25 26 -33 4 -4 5 3 -3 4)
      -1)
     (#,(<array> (0 3 0 3) 2 0 1 1 1 0 3 2 1)
      #,(<array> (0 3 0 3) 1 2 -1 -1 -1 1 -1 -4 2)
      1)
     (#,(<array> (0 3 0 3) 1 -1 3 2 1 2 -2 -2 1)
      #,(<array> (0 3 0 3) 1 -1 -1 -1.2 1.4 0.8 -0.4 0.8 0.6)
      5)
     (#,(<array> (0 2 0 2) 1 2 3 6))
     (#,(<u8array> (0 2 0 2) 1 2 3 4)
      #,(<array>   (0 2 0 2) -2.0 1.0 1.5 -0.5)
      -2)
     (#,(<s16array> (0 2 0 2) 1 2 3 4)
      #,(<array>    (0 2 0 2) -2.0 1.0 1.5 -0.5)
      -2)
     (#,(<f32array> (0 2 0 2) 1 2 3 4)
      #,(<f32array> (0 2 0 2) -2.0 1.0 1.5 -0.5)
      -2)
     (#,(<f64array> (0 2 0 2) 1 2 3 4)
      #,(<f64array> (0 2 0 2) -2.0 1.0 1.5 -0.5)
      -2)
     )))

(let ((i 0))
  (for-each
   (^t (let-optionals* t (a b c)
         (test* (format "array-mul-~D" (inc! i)) c
                (array-mul a b)
                array-approx-equal?)))
   '((#,(<array> (0 2 0 2) 1 2 3 4)
      #,(<array> (0 2 0 2) 4 3 2 1)
      #,(<array> (0 2 0 2) 8 5 20 13))
     (#,(<array> (1 3 1 3) 1 1 1 1)
      #,(<array> (2 4 2 4) 2 2 2 2)
      #,(<array> (0 2 0 2) 4 4 4 4))
     (#,(<u8array> (3 5 7 9) 3 1 4 1)
      #,(<u8array> (55 57 17 19) 2 7 8 1)
      #,(<u8array> (0 2 0 2) 14 22 16 29))
     (#,(<s16array> (3 4 1 9) 1 -2 3 -4 5 -6 7 -8)
      #,(<s16array> (3 11 5 6) 1 -2 3 -4 5 -6 7 -8)
      #,(<s16array> (0 1 0 1) 204))
     (#,(<f32array> (0 2 0 2) 1 2 3 4)
      #,(<f32array> (0 2 0 2) 4 3 2 1)
      #,(<f32array> (0 2 0 2) 8 5 20 13))
     (#,(<f64array> (0 2 0 2) 1 2 3 4)
      #,(<f64array> (0 2 0 2) 4 3 2 1)
      #,(<f64array> (0 2 0 2) 8 5 20 13))
     (#,(<f64array> (0 2 0 3) 1 2 3 4 5 6)
      #,(<f64array> (0 3 0 2) 1 2 3 4 5 6)
      #,(<f64array> (0 2 0 2) 22 28 49 64))
     )))

(let ((i 0))
  (for-each
   (^t (let-optionals* t (a pow b)
         (test* (format "array-expt-~D" (inc! i)) b
                (array-expt a pow)
                array-approx-equal?)))
   '((#,(<array> (0 2 0 2) 1 2 3 4)
      0
      #,(<array> (0 2 0 2) 1 0 0 1))
     (#,(<u8array> (0 2 0 2) 1 2 3 4)
      0
      #,(<u8array> (0 2 0 2) 1 0 0 1))
     (#,(<s16array> (0 2 0 2) 1 2 3 4)
      0
      #,(<s16array> (0 2 0 2) 1 0 0 1))
     (#,(<f32array> (0 2 0 2) 1 2 3 4)
      0
      #,(<f32array> (0 2 0 2) 1 0 0 1))
     (#,(<f64array> (0 2 0 2) 1 2 3 4)
      0
      #,(<f64array> (0 2 0 2) 1 0 0 1))
     (#,(<array> (0 2 0 2) 1 2 3 4)
      2
      #,(<array> (0 2 0 2) 7 10 15 22))
     (#,(<u8array> (0 2 0 2) 1 2 3 4)
      2
      #,(<u8array> (0 2 0 2) 7 10 15 22))
     (#,(<s16array> (0 2 0 2) 1 2 3 4)
      2
      #,(<s16array> (0 2 0 2) 7 10 15 22))
     (#,(<f32array> (0 2 0 2) 1 2 3 4)
      2
      #,(<f32array> (0 2 0 2) 7 10 15 22))
     (#,(<f64array> (0 2 0 2) 1 2 3 4)
      2
      #,(<f64array> (0 2 0 2) 7 10 15 22))
     )))

(let ((i 0))
  (for-each
   (^t (let-optionals* t (a b c)
         (test* (format "array-div-left-~D" (inc! i)) c
                (array-div-left a b)
                array-approx-equal?)))
   '((#,(<array> (0 2 0 2) 1 2 3 4)
      #,(<array> (0 2 0 2) 5 6 7 8)
      #,(<array> (0 2 0 2) 5 4 -4 -3))
     (#,(<u8array> (0 2 0 2) 1 2 3 4)
      #,(<u8array> (0 2 0 2) 5 6 7 8)
      #,(<array>   (0 2 0 2) 5 4 -4 -3))
     (#,(<s16array> (0 2 0 2) 1 2 3 4)
      #,(<s16array> (0 2 0 2) 5 6 7 8)
      #,(<array>    (0 2 0 2) 5 4 -4 -3))
     (#,(<f32array> (0 2 0 2) 1 2 3 4)
      #,(<f32array> (0 2 0 2) 5 6 7 8)
      #,(<f32array> (0 2 0 2) 5 4 -4 -3))
     (#,(<f64array> (0 2 0 2) 1 2 3 4)
      #,(<f64array> (0 2 0 2) 5 6 7 8)
      #,(<f64array> (0 2 0 2) 5 4 -4 -3))
     )))

(let ((i 0))
  (for-each
   (^t (let-optionals* t (a b c)
         (test* (format "array-div-right-~D" (inc! i)) c
                (array-div-right a b)
                array-approx-equal?)))
   '((#,(<array> (0 2 0 2) 1 2 3 4)
      #,(<array> (0 2 0 2) 5 6 7 8)
      #,(<array> (0 2 0 2) 3 -2 2 -1))
     (#,(<u8array> (0 2 0 2) 1 2 3 4)
      #,(<u8array> (0 2 0 2) 5 6 7 8)
      #,(<array>   (0 2 0 2) 3 -2 2 -1))
     (#,(<s16array> (0 2 0 2) 1 2 3 4)
      #,(<s16array> (0 2 0 2) 5 6 7 8)
      #,(<array>    (0 2 0 2) 3 -2 2 -1))
     (#,(<f32array> (0 2 0 2) 1 2 3 4)
      #,(<f32array> (0 2 0 2) 5 6 7 8)
      #,(<f32array> (0 2 0 2) 3 -2 2 -1))
     (#,(<f64array> (0 2 0 2) 1 2 3 4)
      #,(<f64array> (0 2 0 2) 5 6 7 8)
      #,(<f64array> (0 2 0 2) 3 -2 2 -1))
     )))

(let ((i1 0) (i2 0))
  (for-each
   (^t (let-optionals* t (ans a . rest)
         (unless (number? a)
           (test* (format "array-add-elements-~D" (inc! i1)) ans
                  (apply array-add-elements a rest)
                  array-approx-equal?))
         (test* (format "array-add-elements!-~D" (inc! i2)) ans
                (apply array-add-elements!
                       (if (number? a) a (array-copy a))
                       rest)
                array-approx-equal?)))
   '((#,(<array> (0 2 0 2) 5 5 5 5)
      #,(<array> (0 2 0 2) 1 2 3 4)
      #,(<array> (0 2 0 2) 4 3 2 1))
     (#,(<array> (1 3 1 3) 1 2 3 4)
      #,(<array> (1 3 1 3) 1 2 3 4))
     (#,(<u8array> (3 5 7 9) 12 17 18 11)
      #,(<u8array> (3 5 7 9) 2 7 8 1)
      10)
     (#,(<s16array> (3 4 1 9) -9 -12 -7 -14 -5 -16 -3 -18)
      #,(<s16array> (3 4 1 9) 1 -2 3 -4 5 -6 7 -8)
      -10)
     (#,(<f32array> (0 2 0 2) 6 7 8 9)
      #,(<f32array> (0 2 0 2) 1 2 3 4)
      #,(<f32array> (0 2 0 2) 4 3 2 1)
      #,(<f32array> (0 2 0 2) 1 2 3 4))
     (#,(<f64array> (0 2 0 2) -3 1 1 -3)
      #,(<f64array> (0 2 0 2) 1 -2 3 -4)
      #,(<f64array> (0 2 0 2) -4 3 -2 1))
     (#,(<f64array> (0 2 0 3) 8 10 12 14 16 18)
      #,(<f64array> (0 2 0 3) 1 2 3 4 5 6)
      #,(<f64array> (0 2 0 3) 7 8 9 10 11 12))
     (#,(<f64array> (0 2 0 3) 11 8 13 6 15 4)
      10
      #,(<f64array> (0 2 0 3) 1 -2 3 -4 5 -6))
     )))

(let ((i1 0) (i2 0))
  (for-each
   (^t (let-optionals* t (ans a . rest)
         (unless (number? a)
           (test* (format "array-sub-elements-~D" (inc! i1)) ans
                  (apply array-sub-elements a rest)
                  array-approx-equal?))
         (test* (format "array-sub-elements!-~D" (inc! i2)) ans
                (apply array-sub-elements!
                       (if (number? a) a (array-copy a))
                       rest)
                array-approx-equal?)))
   '((#,(<array> (0 2 0 2) -3 -1 1 3)
      #,(<array> (0 2 0 2) 1 2 3 4)
      #,(<array> (0 2 0 2) 4 3 2 1))
     (#,(<array> (1 3 1 3) 1 2 3 4)
      #,(<array> (1 3 1 3) 1 2 3 4))
     (#,(<u8array> (3 5 7 9) 1 6 7 0)
      #,(<u8array> (3 5 7 9) 2 7 8 1)
      1)
     (#,(<s16array> (3 4 1 9) 11 8 13 6 15 4 17 2)
      #,(<s16array> (3 4 1 9) 1 -2 3 -4 5 -6 7 -8)
      -10)
     (#,(<f32array> (0 2 0 2) -4 -3 -2 -1)
      #,(<f32array> (0 2 0 2) 1 2 3 4)
      #,(<f32array> (0 2 0 2) 4 3 2 1)
      #,(<f32array> (0 2 0 2) 1 2 3 4))
     (#,(<f64array> (0 2 0 2) 5 -5 5 -5)
      #,(<f64array> (0 2 0 2) 1 -2 3 -4)
      #,(<f64array> (0 2 0 2) -4 3 -2 1))
     (#,(<f64array> (0 2 0 3) -6 -6 -6 -6 -6 -6)
      #,(<f64array> (0 2 0 3) 1 2 3 4 5 6)
      #,(<f64array> (0 2 0 3) 7 8 9 10 11 12))
     (#,(<f64array> (0 2 0 3) 9 12 7 14 5 16)
      10
      #,(<f64array> (0 2 0 3) 1 -2 3 -4 5 -6))
     )))

(let ((i1 0) (i2 0))
  (for-each
   (^t (let-optionals* t (ans a . rest)
         (unless (number? a)
           (test* (format "array-mul-elements-~D" (inc! i1)) ans
                  (apply array-mul-elements a rest)
                  array-approx-equal?))
         (test* (format "array-mul-elements!-~D" (inc! i2)) ans
                (apply array-mul-elements!
                       (if (number? a) a (array-copy a))
                       rest)
                array-approx-equal?)))
   '((#,(<array> (0 2 0 2) 4 6 6 4)
      #,(<array> (0 2 0 2) 1 2 3 4)
      #,(<array> (0 2 0 2) 4 3 2 1))
     (#,(<array> (1 3 1 3) 1 2 3 4)
      #,(<array> (1 3 1 3) 1 2 3 4))
     (#,(<u8array> (3 5 7 9) 20 70 80 10)
      #,(<u8array> (3 5 7 9) 2 7 8 1)
      10)
     (#,(<s16array> (3 4 1 9) -10 20 -30 40 -50 60 -70 80)
      #,(<s16array> (3 4 1 9) 1 -2 3 -4 5 -6 7 -8)
      -10)
     (#,(<f32array> (0 2 0 2) 4 12 18 16)
      #,(<f32array> (0 2 0 2) 1 2 3 4)
      #,(<f32array> (0 2 0 2) 4 3 2 1)
      #,(<f32array> (0 2 0 2) 1 2 3 4))
     (#,(<f64array> (0 2 0 2) -4 -6 -6 -4)
      #,(<f64array> (0 2 0 2) 1 -2 3 -4)
      #,(<f64array> (0 2 0 2) -4 3 -2 1))
     (#,(<f64array> (0 2 0 3) 7 16 27 40 55 72)
      #,(<f64array> (0 2 0 3) 1 2 3 4 5 6)
      #,(<f64array> (0 2 0 3) 7 8 9 10 11 12))
     (#,(<f64array> (0 2 0 3) 10 -20 30 -40 50 -60)
      10
      #,(<f64array> (0 2 0 3) 1 -2 3 -4 5 -6))
     )))

(let ((i1 0) (i2 0))
  (for-each
   (^t (let-optionals* t (ans a . rest)
         (unless (number? a)
           (test* (format "array-div-elements-~D" (inc! i1)) ans
                  (apply array-div-elements a rest)
                  array-approx-equal?))
         (test* (format "array-div-elements!-~D" (inc! i2)) ans
                (apply array-div-elements!
                       (if (number? a) a (array-copy a))
                       rest)
                array-approx-equal?)))
   '((#,(<array> (0 2 0 2) 1/4 2/3 3/2 4/1)
      #,(<array> (0 2 0 2) 1 2 3 4)
      #,(<array> (0 2 0 2) 4 3 2 1))
     (#,(<array> (1 3 1 3) 1 2 3 4)
      #,(<array> (1 3 1 3) 1 2 3 4))
     (#,(<u8array> (3 5 7 9) 2/10 7/10 8/10 1/10)
      #,(<u8array> (3 5 7 9) 2 7 8 1)
      10)
     (#,(<s16array> (3 4 1 9) -1/10 2/10 -3/10 4/10 -5/10 6/10 -7/10 8/10)
      #,(<s16array> (3 4 1 9) 1 -2 3 -4 5 -6 7 -8)
      -10)
     (#,(<f32array> (0 2 0 2) 1/4 2/6 3/6 4/4)
      #,(<f32array> (0 2 0 2) 1 2 3 4)
      #,(<f32array> (0 2 0 2) 4 3 2 1)
      #,(<f32array> (0 2 0 2) 1 2 3 4))
     (#,(<f64array> (0 2 0 2) -1/4 -2/3 -3/2 -4/1)
      #,(<f64array> (0 2 0 2) 1 -2 3 -4)
      #,(<f64array> (0 2 0 2) -4 3 -2 1))
     (#,(<f64array> (0 2 0 3) 1/7 2/8 3/9 4/10 5/11 6/12)
      #,(<f64array> (0 2 0 3) 1 2 3 4 5 6)
      #,(<f64array> (0 2 0 3) 7 8 9 10 11 12))
     (#,(<f64array> (0 2 0 3) 10/1 -10/2 10/3 -10/4 10/5 -10/6)
      10
      #,(<f64array> (0 2 0 3) 1 -2 3 -4 5 -6))
     )))

(let ((i 0))
  (for-each
   (^t (let-optionals* t (a b)
         (test* (format "array-transpose-~D" (inc! i)) b
                (array-transpose a)
                array-approx-equal?)))
   '((#,(<array> (0 2 0 2) 1 2 3 4)
      #,(<array> (0 2 0 2) 1 3 2 4))
     (#,(<u8array> (0 2 0 2) 1 2 3 4)
      #,(<u8array> (0 2 0 2) 1 3 2 4))
     (#,(<s16array> (0 2 0 2) 1 2 3 4)
      #,(<s16array> (0 2 0 2) 1 3 2 4))
     (#,(<f32array> (0 2 0 2) 1 2 3 4)
      #,(<f32array> (0 2 0 2) 1 3 2 4))
     (#,(<f64array> (0 2 0 2) 1 2 3 4)
      #,(<f64array> (0 2 0 2) 1 3 2 4))
     (#,(<f64array> (0 2 0 3) 1 2 3 4 5 6)
      #,(<f64array> (0 3 0 2) 1 4 2 5 3 6))
     )))

(let ((i 0))
  (for-each
   (^t (let-optionals* t (a b)
         (test* (format "array-rotate-90-~D" (inc! i)) b
                (array-rotate-90 a)
                array-approx-equal?)))
   '((#,(<array> (0 2 0 2) 1 2 3 4)
      #,(<array> (0 2 0 2) 3 1 4 2))
     (#,(<u8array> (0 2 0 2) 1 2 3 4)
      #,(<u8array> (0 2 0 2) 3 1 4 2))
     (#,(<s16array> (0 2 0 2) 1 2 3 4)
      #,(<s16array> (0 2 0 2) 3 1 4 2))
     (#,(<f32array> (0 2 0 2) 1 2 3 4)
      #,(<f32array> (0 2 0 2) 3 1 4 2))
     (#,(<f64array> (0 2 0 2) 1 2 3 4)
      #,(<f64array> (0 2 0 2) 3 1 4 2))
     (#,(<f64array> (0 2 0 3) 1 2 3 4 5 6)
      #,(<f64array> (0 3 0 2) 4 1 5 2 6 3))
     )))

(let ((i1 0) (i2 0))
  (for-each
   (^t (let-optionals* t (a b)
         (test* (format "array-flip-~D" (inc! i1)) b
                (array-flip a)
                array-approx-equal?)
         (test* (format "array-flip!-~D" (inc! i2)) b
                (array-flip! (array-copy a))
                array-approx-equal?)))
   '((#,(<array> (0 2 0 2) 1 2 3 4)
      #,(<array> (0 2 0 2) 3 4 1 2))
     (#,(<u8array> (0 2 0 2) 1 2 3 4)
      #,(<u8array> (0 2 0 2) 3 4 1 2))
     (#,(<s16array> (0 2 0 2) 1 2 3 4)
      #,(<s16array> (0 2 0 2) 3 4 1 2))
     (#,(<f32array> (0 2 0 2) 1 2 3 4)
      #,(<f32array> (0 2 0 2) 3 4 1 2))
     (#,(<f64array> (0 2 0 2) 1 2 3 4)
      #,(<f64array> (0 2 0 2) 3 4 1 2))
     )))

(let ((i1 0) (i2 0))
  (for-each
   (^t (let-optionals* t (a b)
         (test* (format "array-negate-elements-~D" (inc! i1)) b
                (array-negate-elements a)
                array-approx-equal?)
         (test* (format "array-negate-elements!-~D" (inc! i2)) b
                (array-negate-elements! (array-copy a))
                array-approx-equal?)))
   '((#,(<array> (0 2 0 2) 1 2 3 4)
      #,(<array> (0 2 0 2) -1 -2 -3 -4))
     (#,(<s8array> (0 2 0 2) 1 2 3 4)
      #,(<s8array> (0 2 0 2) -1 -2 -3 -4))
     (#,(<f32array> (0 2 0 2) 1 2 3 4)
      #,(<f32array> (0 2 0 2) -1 -2 -3 -4))
     )))

(let ((i1 0) (i2 0))
  (for-each
   (^t (let-optionals* t (a b)
         (test* (format "array-reciprocate-elements~D" (inc! i1)) b
                (array-reciprocate-elements a)
                array-approx-equal?)
         (test* (format "array-reciprocate-elements!-~D" (inc! i2)) b
                (array-reciprocate-elements! (array-copy a))
                array-approx-equal?)))
   '((#,(<array> (0 2 0 2) 1 2 3 4)
      #,(<array> (0 2 0 2) 1 1/2 1/3 1/4))
     (#,(<f64array> (0 2 0 2) 1 2 3 4)
      #,(<f64array> (0 2 0 2) 1 1/2 1/3 1/4))
     )))

;;-------------------------------------------------------------------
;; NB: copy-port uses read-block! and write-block for block copy,
;;     so we test it here.
(test-section "copy-port")

(define s (make-string 10000 #\z))

(test* "copy-port (default)" #t
       (equal? s (call-with-string-io s (^[in out] (copy-port in out)))))

(test* "copy-port (unit byte)" #t
       (equal? s (call-with-string-io s (^[in out]
                                          (copy-port in out :unit 'byte)))))

(test* "copy-port (unit char)" #t
       (equal? s (call-with-string-io s (^[in out]
                                          (copy-port in out :unit 'char)))))

(test* "copy-port (unit 10)" #t
       (equal? s (call-with-string-io s (^[in out]
                                          (copy-port in out :unit 10)))))

(test* "copy-port (unit 100000)" #t
       (equal? s (call-with-string-io s (^[in out]
                                          (copy-port in out :unit 100000)))))

;;-------------------------------------------------------------------
(test-section "binary search")

(use srfi-42)


(let ([data '(;; NB: keep the length of vector a multiple of 6 for skip test
              ;; (<vec> . <keys>)
              (#s8(-100 -99 -50 -41 -15 -2 -1 0 1 2 5 7 9 18 19 50 99 100)
                  -101 -100 -99 -98 -50 -49 -1 0 2 3 98 99 100)
              (#u8(1 2 3 4 5 9 20 50 70 75 89 100)
                  0 1 2 3 4 5 6 50 99 100)
              (#s16(-4515 -4514 -2 -1 0 1 2 19 27 55 32765 32767)
                   -32768 -4514 -3 -2 2 32765 32766 32767)
              (#u16(0 2 65529 65531 65533 65535)
                   0 1 2 65528 65529 65530 65531 65532 65533 65534)
              (#f64(-10000 -0.5 0 0.1 5.5 100)
                   -10000 -0.51 -0.5 0 0.1 0.2 100 100.1)
              )])
  (define (linear-search vec key start end :optional (skip 0))
    (let loop ((k start))
      (cond [(not (zero? (modulo (- end start) (+ skip 1)))) (test-error)]
            [(>= (+ k skip) end) #f]
            [(= (~ vec k) key) k]
            [(< (~ vec k) key) (loop (+ k skip 1))]
            [else #f])))

  (define (ranges vec)
    (let ([len (uvector-length vec)])
      `((0 ,len) (0 0) (0 1) (0 6))))

  ;; generates list of (<key> <start> <end> :optional <esize>)
  (define (all-args vec keys)
    (list-ec (: k keys)
             (: r (ranges vec))
             (: e '(() (1) (2)))
             (append (cons k r) e)))

  (define (expected vec arg) (apply linear-search vec arg))

  (define (test-1 vec . keys)
    (let1 args (all-args vec keys)
      (dolist [arg args]
        (test* #"binary search ~(cons (class-of vec) arg)"
               (apply linear-search vec arg)
               (let ([key (car arg)]
                     [start (cadr arg)]
                     [end (caddr arg)]
                     [maybe-skip (cdddr arg)])
                 (and-let1 r (apply uvector-binary-search vec
                                    key start end maybe-skip)
                   (+ r start)))))))

  (dolist [v data]
    (apply test-1 v))
  )

;; We test only on s8; the code path is the same so it should work
;; on other varieties
(let ((vec '#s8(-10 -7 -2 5 9 33))
      (data '(;; search-key expect-floor expect-ceiling
              (-25          #f           -10)
              (-10          -10          -10)
              (-9           -10          -7)
              (0            -2           5)
              (8            5            9)
              (33           33           33)
              (100          33           #f))))
  (define (test-1 entry)
    (let1 search-key (car entry)
      (list search-key
            (pick (uvector-binary-search vec search-key #f #f #f 'floor))
            (pick (uvector-binary-search vec search-key #f #f #f 'ceiling)))))
  (define (pick index)
    (and index (~ vec index)))
  (test* "binary search, floor and ceiling" data
         (map test-1 data)))

;;-------------------------------------------------------------------
(test-section "generator and uvector")

(use gauche.generator)

(test* "generator->bytevector" '#u8(0 1 2 3 0 1 2 3 0 1)
       (generator->bytevector (circular-generator 0 1 2 3) 10))
(test* "generator->bytevector" '#u8(0 1 2 3)
       (generator->bytevector (list->generator '(0 1 2 3)) 10))
(test* "generator->bytevector!" '(8 #u8(255 255 0 1 2 3 0 1 2 3))
       (let1 vec (make-u8vector 10 255)
         (list (generator->bytevector! vec 2 (circular-generator 0 1 2 3))
               vec)))

(test* "generator->uvector" '#u32(0 1 2 3 0 1 2 3 0 1)
       (generator->uvector (circular-generator 0 1 2 3) 10 <u32vector>))
(test* "generator->uvector!" '(7 #s32(-1 -1 -1 0 1 2 3 0 1 2))
       (let1 vec (make-s32vector 10 -1)
         (list (generator->uvector! vec 3 (circular-generator 0 1 2 3))
               vec)))

;;-------------------------------------------------------------------
;; (test-section "srfi-160 interface")

;; (use gauche.uvector.u8)
;; (test-module 'gauche.uvector.u8)
;; (use gauche.uvector.s8)
;; (test-module 'gauche.uvector.s8)
;; (use gauche.uvector.u16)
;; (test-module 'gauche.uvector.u16)
;; (use gauche.uvector.s16)
;; (test-module 'gauche.uvector.s16)
;; (use gauche.uvector.u32)
;; (test-module 'gauche.uvector.u32)
;; (use gauche.uvector.s32)
;; (test-module 'gauche.uvector.s32)
;; (use gauche.uvector.u64)
;; (test-module 'gauche.uvector.u64)
;; (use gauche.uvector.s64)
;; (test-module 'gauche.uvector.s64)
;; (use gauche.uvector.f16)
;; (test-module 'gauche.uvector.f16)
;; (use gauche.uvector.f32)
;; (test-module 'gauche.uvector.f32)
;; (use gauche.uvector.f64)
;; (test-module 'gauche.uvector.f64)
;; (use gauche.uvector.c32)
;; (test-module 'gauche.uvector.c32)
;; (use gauche.uvector.c64)
;; (test-module 'gauche.uvector.c64)
;; (use gauche.uvector.c128)
;; (test-module 'gauche.uvector.c128)

(test-end)
