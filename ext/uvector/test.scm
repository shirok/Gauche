;;
;; test for srfi-4 module
;;

(use gauche.test)
;(use srfi-4)
(load "uvector")
(import gauche.uvector)
(use srfi-1)

(test-start "srfi-4")
;;-------------------------------------------------------------------
(test-section "reader syntax")

(test "#s8()" #t (lambda () (s8vector? '#s8(0 1 2 3 4))))
(test "#s8()" #f (lambda () (s8vector? '#(0 1 2 3 4))))
(test "#u8()" #t (lambda () (u8vector? '#u8(0 1 2 3 4))))
(test "#u8()" #f (lambda () (u8vector? '#(0 1 2 3 4))))
(test "#s16()" #t (lambda () (s16vector? '#s16(0 1 2 3 4))))
(test "#s16()" #f (lambda () (s16vector? '#(0 1 2 3 4))))
(test "#u16()" #t (lambda () (u16vector? '#u16(0 1 2 3 4))))
(test "#u16()" #f (lambda () (u16vector? '#(0 1 2 3 4))))
(test "#s32()" #t (lambda () (s32vector? '#s32(0 1 2 3 4))))
(test "#s32()" #f (lambda () (s32vector? '#(0 1 2 3 4))))
(test "#u32()" #t (lambda () (u32vector? '#u32(0 1 2 3 4))))
(test "#u32()" #f (lambda () (u32vector? '#(0 1 2 3 4))))
(test "#s64()" #t (lambda () (s64vector? '#s64(0 1 2 3 4))))
(test "#s64()" #f (lambda () (s64vector? '#(0 1 2 3 4))))
(test "#u64()" #t (lambda () (u64vector? '#u64(0 1 2 3 4))))
(test "#u64()" #f (lambda () (u64vector? '#(0 1 2 3 4))))
(test "#f32()" #t (lambda () (f32vector? '#f32(0.0 1.0 2.0 3.0 4.0))))
(test "#f32()" #f (lambda () (f32vector? '#(0.0 1.0 2.0 3.0 4.0))))
(test "#f64()" #t (lambda () (f64vector? '#f64(0.0 1.0 2.0 3.0 4.0))))
(test "#f64()" #f (lambda () (f64vector? '#(0.0 1.0 2.0 3.0 4.0))))

;;-------------------------------------------------------------------
(test-section "writer syntax")

(test "#s8()" "#s8(0 1 2 3 4)"
      (lambda () (with-output-to-string
                   (lambda () (write (apply s8vector (iota 5)))))))
(test "#u8()" "#u8(0 1 2 3 4)"
      (lambda () (with-output-to-string
                   (lambda () (write (apply u8vector (iota 5)))))))
(test "#s16()" "#s16(0 1 2 3 4)"
      (lambda () (with-output-to-string
                   (lambda () (write (apply s16vector (iota 5)))))))
(test "#u16()" "#u16(0 1 2 3 4)"
      (lambda () (with-output-to-string
                   (lambda () (write (apply u16vector (iota 5)))))))
(test "#s32()" "#s32(0 1 2 3 4)"
      (lambda () (with-output-to-string
                   (lambda () (write (apply s32vector (iota 5)))))))
(test "#u32()" "#u32(0 1 2 3 4)"
      (lambda () (with-output-to-string
                   (lambda () (write (apply u32vector (iota 5)))))))
(test "#s64()" "#s64(0 1 2 3 4)"
      (lambda () (with-output-to-string
                   (lambda () (write (apply s64vector (iota 5)))))))
(test "#u64()" "#u64(0 1 2 3 4)"
      (lambda () (with-output-to-string
                   (lambda () (write (apply u64vector (iota 5)))))))
(test "#f32()" "#f32()"
      (lambda () (with-output-to-string
                   (lambda () (write (f32vector))))))
(test "#f64()" "#f64()"
      (lambda () (with-output-to-string
                   (lambda () (write (f64vector))))))

;;-------------------------------------------------------------------
(test-section "ref and set")

(define (uvrefset-tester make ref set numlist expvec)
  (let ((vec (make (length numlist)))
        (seq (iota (length numlist))))
    (for-each (lambda (n i) (set vec i n)) numlist seq)
    (and (equal? expvec vec)
         (equal? numlist (map (lambda (i) (ref vec i)) seq)))))

(test "s8vector-ref|set!" #t
      (lambda () (uvrefset-tester make-s8vector s8vector-ref s8vector-set!
                                  '(0 -1 1 -128 127)
                                  '#s8(0 -1 1 -128 127))))
(test "u8vector-ref|set!" #t
      (lambda () (uvrefset-tester make-u8vector u8vector-ref u8vector-set!
                                  '(0 1 2 3 255)
                                  '#u8(0 1 2 3 255))))
(test "s16vector-ref|set!" #t
      (lambda () (uvrefset-tester make-s16vector s16vector-ref s16vector-set!
                                  '(0 -1 1 -32768 32767)
                                  '#s16(0 -1 1 -32768 32767))))
(test "u16vector-ref|set!" #t
      (lambda () (uvrefset-tester make-u16vector u16vector-ref u16vector-set!
                                  '(0 1 2 3 65535)
                                  '#u16(0 1 2 3 65535))))
(test "s32vector-ref|set!" #t
      (lambda () (uvrefset-tester make-s32vector s32vector-ref s32vector-set!
                                      '(0 -1 1 #x-80000000 #x7fffffff)
                                  '#s32(0 -1 1 #x-80000000 #x7fffffff))))
(test "u32vector-ref|set!" #t
      (lambda () (uvrefset-tester make-u32vector u32vector-ref u32vector-set!
                                      '(0 1 2 #xffffffff)
                                  '#u32(0 1 2 #xffffffff))))
(test "s64vector-ref|set!" #t
      (lambda () (uvrefset-tester make-s64vector s64vector-ref s64vector-set!
                                  '(0 -1 1 #x-8000000000000000 #x7fffffffffffffff)
                                  '#s64(0 -1 1 #x-8000000000000000 #x7fffffffffffffff))))
(test "u64vector-ref|set!" #t
      (lambda () (uvrefset-tester make-u64vector u64vector-ref u64vector-set!
                                  '(0 1 2 #xffffffffffffffff)
                                  '#u64(0 1 2 #xffffffffffffffff))))
(test "f32vector-ref|set!" #t
      (lambda () (uvrefset-tester make-f32vector f32vector-ref f32vector-set!
                                  '(0.0 -1.0 1.0)
                                  '#f32(0.0 -1.0 1.0))))
(test "f64vector-ref|set!" #t
      (lambda () (uvrefset-tester make-f64vector f64vector-ref f64vector-set!
                                  '(0.0 -1.0 1.0)
                                  '#f64(0.0 -1.0 1.0))))

(define (uvset-clamp-tester make ref set value)
  (let ((v (make 1)))
    (list (with-error-handler
           (lambda (e) 'error)
           (lambda () (set v 0 value) (ref v 0)))
          (with-error-handler
           (lambda (e) 'error)
           (lambda () (set v 0 value 'low) (ref v 0)))
          (with-error-handler
           (lambda (e) 'error)
           (lambda () (set v 0 value 'high) (ref v 0)))
          (with-error-handler
           (lambda (e) 'error)
           (lambda () (set v 0 value 'both) (ref v 0))))))

(test "s8vector-set! clamp" '(error -128 error -128)
      (lambda ()
        (uvset-clamp-tester make-s8vector s8vector-ref s8vector-set! -129)))
(test "s8vector-set! clamp" '(error error 127 127)
      (lambda ()
        (uvset-clamp-tester make-s8vector s8vector-ref s8vector-set! 128)))

(test "u8vector-set! clamp" '(error 0 error 0)
      (lambda ()
        (uvset-clamp-tester make-u8vector u8vector-ref u8vector-set! -1)))
(test "u8vector-set! clamp" '(error error 255 255)
      (lambda ()
        (uvset-clamp-tester make-u8vector u8vector-ref u8vector-set! 256)))

(test "s16vector-set! clamp" '(error -32768 error -32768)
      (lambda ()
        (uvset-clamp-tester make-s16vector s16vector-ref s16vector-set! -32769)))
(test "s16vector-set! clamp" '(error error 32767 32767)
      (lambda ()
        (uvset-clamp-tester make-s16vector s16vector-ref s16vector-set! 32768)))

(test "u16vector-set! clamp" '(error 0 error 0)
      (lambda ()
        (uvset-clamp-tester make-u16vector u16vector-ref u16vector-set! -1)))
(test "u16vector-set! clamp" '(error error 65535 65535)
      (lambda ()
        (uvset-clamp-tester make-u16vector u16vector-ref u16vector-set! 65536)))

(test "s32vector-set! clamp" '(error -2147483648 error -2147483648)
      (lambda ()
        (uvset-clamp-tester make-s32vector s32vector-ref s32vector-set! -2147483649)))
(test "s32vector-set! clamp" '(error error 2147483647 2147483647)
      (lambda ()
        (uvset-clamp-tester make-s32vector s32vector-ref s32vector-set! 2147483648)))

(test "u32vector-set! clamp" '(error 0 error 0)
      (lambda ()
        (uvset-clamp-tester make-u32vector u32vector-ref u32vector-set! -1)))
(test "u32vector-set! clamp" '(error error 4294967295 4294967295)
      (lambda ()
        (uvset-clamp-tester make-u32vector u32vector-ref u32vector-set! 4294967296)))

(test "s64vector-set! clamp" '(error -9223372036854775808 error -9223372036854775808)
      (lambda ()
        (uvset-clamp-tester make-s64vector s64vector-ref s64vector-set! -9223372036854775809)))
(test "s64vector-set! clamp" '(error error 9223372036854775807 9223372036854775807)
      (lambda ()
        (uvset-clamp-tester make-s64vector s64vector-ref s64vector-set! 9223372036854775808)))

(test "u64vector-set! clamp" '(error 0 error 0)
      (lambda ()
        (uvset-clamp-tester make-u64vector u64vector-ref u64vector-set! -1)))
(test "u64vector-set! clamp" '(error error 18446744073709551615 18446744073709551615)
      (lambda ()
        (uvset-clamp-tester make-u64vector u64vector-ref u64vector-set! 18446744073709551616)))

;;-------------------------------------------------------------------
(test-section "conversions")

(define (uvconv-tester ->list list-> ->vec vec-> uvec nums)
  (let* ((lis (->list uvec))
         (uv2 (list-> lis))
         (vec (->vec  uvec))
         (uv3 (vec->  vec)))
    (and (equal? lis nums)
         (equal? uv2 uvec)
         (equal? vec (list->vector nums))
         (equal? uv3 uvec))))

(test "s8vector conversion" #t
      (lambda ()
        (uvconv-tester s8vector->list list->s8vector
                       s8vector->vector vector->s8vector
                       '#s8(0 -1 1 -128 127) '(0 -1 1 -128 127))))
(test "u8vector conversion" #t
      (lambda ()
        (uvconv-tester u8vector->list list->u8vector
                       u8vector->vector vector->u8vector
                       '#u8(0 1 254 255) '(0 1 254 255))))
(test "s16vector conversion" #t
      (lambda ()
        (uvconv-tester s16vector->list list->s16vector
                       s16vector->vector vector->s16vector
                       '#s16(0 -1 1 -32768 32767) '(0 -1 1 -32768 32767))))
(test "u16vector conversion" #t
      (lambda ()
        (uvconv-tester u16vector->list list->u16vector
                       u16vector->vector vector->u16vector
                       '#u16(0 1 65534 65535) '(0 1 65534 65535))))
(test "s32vector conversion" #t
      (lambda ()
        (uvconv-tester s32vector->list list->s32vector
                       s32vector->vector vector->s32vector
                       '#s32(0 -1 1 #x-80000000 #x7fffffff)
                       '(0 -1 1 #x-80000000 #x7fffffff))))
(test "u32vector conversion" #t
      (lambda ()
        (uvconv-tester u32vector->list list->u32vector
                       u32vector->vector vector->u32vector
                       '#u32(0 1 #xfffffffe #xffffffff)
                       '(0 1 #xfffffffe #xffffffff))))
(test "s64vector conversion" #t
      (lambda ()
        (uvconv-tester s64vector->list list->s64vector
                       s64vector->vector vector->s64vector
                       '#s64(0 -1 1 #x-8000000000000000 #x7fffffffffffffff)
                       '(0 -1 1 #x-8000000000000000 #x7fffffffffffffff))))
(test "u64vector conversion" #t
      (lambda ()
        (uvconv-tester u64vector->list list->u64vector
                       u64vector->vector vector->u64vector
                       '#u64(0 1 #xffffffffffffffff)
                       '(0 1 #xffffffffffffffff))))
(test "f32vector conversion" #t
      (lambda ()
        (uvconv-tester f32vector->list list->f32vector
                       f32vector->vector vector->f32vector
                       '#f32(0.0 -1.0 1.0)
                       '(0.0 -1.0 1.0))))
(test "f64vector conversion" #t
      (lambda ()
        (uvconv-tester f64vector->list list->f64vector
                       f64vector->vector vector->f64vector
                       '#f64(0.0 -1.0 1.0)
                       '(0.0 -1.0 1.0))))

;;-------------------------------------------------------------------
(test-section "copying and filling")

(define (uvcopy-tester copy copy! fill! ->list list-> uvec filler)
  (let* ((c0 (list-> (->list uvec)))
         (c1 (copy uvec)))
    (and (equal? c1 uvec)
         (begin (fill! c1 filler)
                (and (equal? c0 uvec)
                     (every (lambda (n) (= n filler))  (->list c1))
                     (begin (copy! c1 uvec)
                            (equal? c1 c0)))))))

(test "s8vector copy|fill!" #t
      (lambda ()
        (uvcopy-tester s8vector-copy s8vector-copy! s8vector-fill!
                       s8vector->list list->s8vector
                       '#s8(0 -1 1 -128 127) -128)))
(test "u8vector copy|fill!" #t
      (lambda ()
        (uvcopy-tester u8vector-copy u8vector-copy! u8vector-fill!
                       u8vector->list list->u8vector
                       '#u8(0 1 255) 255)))
(test "s16vector copy|fill!" #t
      (lambda ()
        (uvcopy-tester s16vector-copy s16vector-copy! s16vector-fill!
                       s16vector->list list->s16vector
                       '#s16(0 -1 1 -32768 32767) -32768)))
(test "u16vector copy|fill!" #t
      (lambda ()
        (uvcopy-tester u16vector-copy u16vector-copy! u16vector-fill!
                       u16vector->list list->u16vector
                       '#u16(0 1 65535) 32768)))
(test "s32vector copy|fill!" #t
      (lambda ()
        (uvcopy-tester s32vector-copy s32vector-copy! s32vector-fill!
                       s32vector->list list->s32vector
                       '#s32(0 -1 1 #x-80000000 #x7fffffff) #x7fffffff)))
(test "u32vector copy|fill!" #t
      (lambda ()
        (uvcopy-tester u32vector-copy u32vector-copy! u32vector-fill!
                       u32vector->list list->u32vector
                       '#u32(0 1 #xffffffff) #x80000000)))
(test "s64vector copy|fill!" #t
      (lambda ()
        (uvcopy-tester s64vector-copy s64vector-copy! s64vector-fill!
                       s64vector->list list->s64vector
                       '#s64(0 -1 1 #x-8000000000000000 #x7fffffffffffffff)
                       #x7fffffffffffffff)))
(test "u64vector copy|fill!" #t
      (lambda ()
        (uvcopy-tester u64vector-copy u64vector-copy! u64vector-fill!
                       u64vector->list list->u64vector
                       '#u64(0 1 #xffffffffffffffff) #x8000000000000000)))

(test "f32vector copy|fill!" #t
      (lambda ()
        (uvcopy-tester f32vector-copy f32vector-copy! f32vector-fill!
                       f32vector->list list->f32vector
                       '#f32(0 -1.0 1.0) 1.0)))
(test "f64vector copy|fill!" #t
      (lambda ()
        (uvcopy-tester f64vector-copy f64vector-copy! f64vector-fill!
                       f64vector->list list->f64vector
                       '#f64(0 -1.0 1.0) 1.0e64)))

(define (uvcopy-startend-test msg make copy fill)
  (test msg (list (make 1 2 3) (make 1 2) (make 0 9 9 3))
        (lambda ()
          (let1 v (make 0 1 2 3)
            (list (copy v 1)
                  (copy v 1 3)
                  (fill v 9 1 3))))))

(uvcopy-startend-test "uvcopy-startend s8vector"
                      s8vector s8vector-copy s8vector-fill!)
(uvcopy-startend-test "uvcopy-startend u8vector"
                      u8vector u8vector-copy u8vector-fill!)
(uvcopy-startend-test "uvcopy-startend s16vector"
                      s16vector s16vector-copy s16vector-fill!)
(uvcopy-startend-test "uvcopy-startend u16vector"
                      u16vector u16vector-copy u16vector-fill!)
(uvcopy-startend-test "uvcopy-startend s32vector"
                      s32vector s32vector-copy s32vector-fill!)
(uvcopy-startend-test "uvcopy-startend u32vector"
                      u32vector u32vector-copy u32vector-fill!)
(uvcopy-startend-test "uvcopy-startend s64vector"
                      s64vector s64vector-copy s64vector-fill!)
(uvcopy-startend-test "uvcopy-startend u64vector"
                      u64vector u64vector-copy u64vector-fill!)

;;-------------------------------------------------------------------
(test-section "collection interface")

(use gauche.collection)
(use gauche.sequence)

(define (collection-tester class vec)
  (and (eqv?   (fold + 0 vec) 10)
       (equal? (map identity vec) '(1 2 3 4))
       (eqv?   (find (lambda (e) (= e 2)) vec) 2)
       (equal? (coerce-to class '(1 2 3 4)) vec)
       (eqv?   (ref vec 2) 3)
       (equal? (begin (set! (ref vec 1) 0)
                      (coerce-to <list> vec))
               '(1 0 3 4))
       (equal? (coerce-to <vector> (subseq vec 1 3)) '#(0 3))))

(test "s8vector collection interface" #t
      (lambda () (collection-tester <s8vector> '#s8(1 2 3 4))))
(test "u8vector collection interface" #t
      (lambda () (collection-tester <u8vector> '#u8(1 2 3 4))))
(test "s16vector collection interface" #t
      (lambda () (collection-tester <s16vector> '#s16(1 2 3 4))))
(test "u16vector collection interface" #t
      (lambda () (collection-tester <u16vector> '#u16(1 2 3 4))))
(test "s32vector collection interface" #t
      (lambda () (collection-tester <s32vector> '#s32(1 2 3 4))))
(test "u32vector collection interface" #t
      (lambda () (collection-tester <u32vector> '#u32(1 2 3 4))))
(test "s64vector collection interface" #t
      (lambda () (collection-tester <s64vector> '#s64(1 2 3 4))))
(test "u64vector collection interface" #t
      (lambda () (collection-tester <u64vector> '#u64(1 2 3 4))))
(test "f32vector collection interface" #t
      (lambda () (collection-tester <f32vector> '#f32(1 2 3 4))))
(test "f64vector collection interface" #t
      (lambda () (collection-tester <f64vector> '#f64(1 2 3 4))))

;;-------------------------------------------------------------------
(test-section "arithmetic operations")

;; there are too many combinations to write down by hand.
;;
;; for each opertaion in add, sub, mul
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

(define-macro (arith-test-generate tag)
  `(arith-test ',tag ,(tag->min tag) ,(tag->max tag)
               ,(string->symbol #`",|tag|vector")
               ,(string->symbol #`",|tag|vector-add")
               ,(string->symbol #`",|tag|vector-sub")
               ,(string->symbol #`",|tag|vector-mul")))

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
      (with-error-handler
       (lambda (e) 'error)
       (lambda () (op v0 v1 clamp-flag))))
    (lambda ()
      (list (safe-test #f)
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
          ((s64 u64)
           (result-normal (make big32 (+ big32 1) (+ big32 2) (+ big32 3))))
          (else (result-hi-ok vmax)))
        (gen-tester add v0 big32))
  (test (format #f "~avector-add (v+b)" tag)
        (case tag
          ((s64)
           (result-normal (make (- big32) (- 1 big32) (- 2 big32) (- 3 big32))))
          (else (result-lo-ok vmin)))
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
          ((s64) (result-normal (make (- big32) (- 1 big32) (- 2 big32) (- 3 big32))))
          (else  (result-lo-ok vmin)))
        (gen-tester sub v0 big32))
  (test (format #f "~avector-sub (v-b)" tag)
        (case tag
          ((s64 u64)
           (result-normal (make big32 (+ big32 1) (+ big32 2) (+ big32 3))))
          (else (result-hi-ok vmax)))
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
          ((s8 s16 s32 s64) (result-lo-ok (make 0 (- (- max 1)) min min)))
          (else (result-lo-ok  vmin)))
        (gen-tester mul v0 (- (- max 1))))
  (test (format #f "~avector-mul (v*b)" tag)
        (case tag
          ((s64 u64) (result-normal (make 0 big32 (* big32 2) (* big32 3))))
          (else (result-hi-ok  (make 0 max max max))))
        (gen-tester mul v0 big32))
  (test (format #f "~avector-mul (v*b)" tag)
        (case tag
          ((s64)
           (result-normal (make 0 (- big32) (- (* big32 2))  (- (* big32 3)))))
          (else (result-lo-ok  (make 0 min min min))))
        (gen-tester mul v0 (- big32)))
  )
  
(arith-test-generate s8)
(arith-test-generate u8)
(arith-test-generate s16)
(arith-test-generate u16)
(arith-test-generate s32)
(arith-test-generate u32)
(arith-test-generate s64)
(arith-test-generate u64)

;; flonum vectors; no clamping, so it's a bit simple
(define-macro (flonum-arith-test-generate tag)
  `(flonum-arith-test ',tag
                      ,(string->symbol #`",|tag|vector")
                      ,(string->symbol #`",|tag|vector-add")
                      ,(string->symbol #`",|tag|vector-sub")
                      ,(string->symbol #`",|tag|vector-mul")
                      ,(string->symbol #`",|tag|vector-div")
                      ))

(define (flonum-arith-test tag make add sub mul div)
  (test (format #f "~svector-add (v+v)" tag)
        (make 4.0 6.0 8.0 10.0)
        (lambda () (add (make 0.0 1.0 2.0 3.0) (make 4.0 5.0 6.0 7.0))))
  (test (format #f "~svector-add (v+s)" tag)
        (make 4.0 5.0 6.0 7.0)
        (lambda () (add (make 0.0 1.0 2.0 3.0) 4.0)))
  (test (format #f "~svector-sub (v-v)" tag)
        (make -4.0 -4.0 -4.0 -4.0)
        (lambda () (sub (make 0.0 1.0 2.0 3.0) (make 4.0 5.0 6.0 7.0))))
  (test (format #f "~svector-sub (v-s)" tag)
        (make -4.0 -3.0 -2.0 -1.0)
        (lambda () (sub (make 0.0 1.0 2.0 3.0) 4.0)))
  (test (format #f "~svector-mul (v*v)" tag)
        (make 0.0 5.0 12.0 21.0)
        (lambda () (mul (make 0.0 1.0 2.0 3.0) (make 4.0 5.0 6.0 7.0))))
  (test (format #f "~svector-mul (v*s)" tag)
        (make 0.0 5.0 10.0 15.0)
        (lambda () (mul (make 0.0 1.0 2.0 3.0) 5.0)))
  (test (format #f "~svector-div (v/v)" tag)
        (make 0.0 0.5 0.5 0.375)
        (lambda () (div (make 0.0 1.0 2.0 3.0) (make 1.0 2.0 4.0 8.0))))
  (test (format #f "~svector-div (v/v)" tag)
        (make 0.0 0.5 1.0 1.5)
        (lambda () (div (make 0.0 1.0 2.0 3.0) 2.0)))
  )

(flonum-arith-test-generate f32)
(flonum-arith-test-generate f64)

;;-------------------------------------------------------------------
(test-section "bitwise operations")

(define (bit-test tag v0 v1 s0 s1 ->list list-> and ior xor)
  (define (tests opname op logop)
    (test (format #f "~svector-~s ~s ~s" tag opname v0 v1)
          (list-> (map logop (->list v0) (->list v1)))
          (lambda () (op v0 v1)))
    (test (format #f "~svector-~s ~s ~s" tag opname v0 s0)
          (list-> (map (pa$ logop s0) (->list v0)))
          (lambda () (op v0 s0)))
    (test (format #f "~svector-~s ~s ~s" tag opname v0 s1)
          (list-> (map (pa$ logop s1) (->list v0)))
          (lambda () (op v0 s1))))
  (tests 'and and logand)
  (tests 'ior ior logior)
  (tests 'xor xor logxor))

(define-macro (bit-test-generate tag v0 v1 s0 s1)
  `(bit-test ',tag ',v0 ',v1 ,s0 ,s1
             ,(string->symbol #`",|tag|vector->list")
             ,(string->symbol #`"list->,|tag|vector")
             ,(string->symbol #`",|tag|vector-and")
             ,(string->symbol #`",|tag|vector-ior")
             ,(string->symbol #`",|tag|vector-xor")))

(bit-test-generate s8
                   #s8(#x0f #x70 #x-0f #x-70)
                   #s8(#x55 #x2a #x-55 #x-2a)
                   #x55
                   #x-55)
(bit-test-generate u8
                   #u8(#x0f #x70 #xf0 #xcc)
                   #u8(#x55 #xaa #x5a #xa5)
                   #x55
                   #xaa)
(bit-test-generate s16
                   #s16(#x0fff #x7070 #x-0fff #x-7070)
                   #s16(#x3c3c #x-43c3 #x43c3 #x-3c3c)
                   #x55aa
                   #x-55aa)
(bit-test-generate u16
                   #u16(#x0fff #x7070 #xff00 #xc0c0)
                   #u16(#x3c3c #xc3c3 #x55aa #xaa55)
                   #x55aa
                   #x9696)
(bit-test-generate s32
                   #s32(#x0fffffff #x70707070 #x-0fffffff #x-70707070)
                   #s32(#x3c3c3c3c #x-43c3c3c3 #x43c3c3c3 #x-3c3c3c3c)
                   #x55aa55aa
                   #x-55aa55aa)
(bit-test-generate u32
                   #u32(#x0fffffff #x70707070 #xff00ff00 #xc0c0c0c0)
                   #u32(#x3c3c3c3c #xc3c3c3c3 #x55aa55aa #xaa55aa55)
                   #x55aa55aa
                   #x96966969)
(bit-test-generate s64
                   #s64(#x0fffffffffffffff #x7070707007070707
                        #x-0fffffffffffffff #x-7070707007070707)
                   #s64(#x3c3c3c3cc3c3c3c3 #x-43c3c3c33c3c3c3c
                        #x43c3c3c3c3c3c3c3 #x-3c3c3c3c3c3c3c3c)
                   #x55aa55aa55aa55aa
                   #x-55aa55aa55aa55aa)
(bit-test-generate u64
                   #u64(#x0fffffffffffffff #x70707070f0f0f0f0
                        #xff00ff00ff00ff00 #xc0c0c0c003030303)
                   #u64(#x3c3c3c3c3c3c3c3c #xc3c3c3c3c3c3c3c3
                        #x55aa55aa55aa55aa #xaa55aa55aa55aa55)
                   #x55aa55aa5a5a5a5a
                   #x9696696988778877)

;;-------------------------------------------------------------------
(test-section "dot product")

(define (dotprod-test tag v0 v1 dot)
  (let1 result (fold (lambda (e0 e1 sum)
                       (+ sum (* e0 e1)))
                     0
                     (coerce-to <list> v0)
                     (coerce-to <list> v1))
    (test (format #f "~svector-dot(~s, ~s)" tag v0 v1)
          result
          (lambda () (dot v0 v1)))
    (test (format #f "~svector-dot(~s, ~s)" tag v0 (coerce-to <list> v1))
          result
          (lambda () (dot v0 (coerce-to <list> v1))))
    (test (format #f "~svector-dot(~s, ~s)" tag v0 (coerce-to <vector> v1))
          result
          (lambda () (dot v0 (coerce-to <vector> v1))))
    ))

(define-macro (dotprod-test-generate tag v0 v1)
  `(dotprod-test ',tag ',v0 ',v1
                 ,(string->symbol #`",|tag|vector-dot")))

(dotprod-test-generate s8 #s8() #s8())
(dotprod-test-generate s8 #s8(0 1 2 3) #s8(4 5 6 7))
(dotprod-test-generate s8 #s8(0 -1 2 -3) #s8(-4 5 -6 7))
(dotprod-test-generate s8 #s8(127 127 127 127 127) #s8(127 127 127 127 127))
(dotprod-test-generate s8
                       #s8(-128 -128 -128 -128 -128)
                       #s8(127 127 127 127 127))
(dotprod-test-generate u8 #u8(0 1 2 3) #u8(4 5 6 7))
(dotprod-test-generate u8 #u8(255 255 255 255 255) #u8(255 255 255 255 255))

(dotprod-test-generate s16 #s16(0 1 2 3) #s16(4 5 6 7))
(dotprod-test-generate s16 #s16(0 -1 2 -3) #s16(-4 5 -6 7))
(dotprod-test-generate s16 #s16(16384 16384 16384 16384 16384)
                       #s16(16384 16384 16384 16384 16384))
(dotprod-test-generate s16 #s16(16384 -16384 16384 -16384 16384)
                       #s16(16384 -16384 16384 -16384 16384))
(dotprod-test-generate s16 #s16(32767 32767 32767 32767 32767)
                       #s16(32767 32767 32767 32767 32767))
(dotprod-test-generate s16 #s16(32767 1 2 3 4)
                       #s16(32767 1 2 3 4))
(dotprod-test-generate s16 #s16(1 2 3 4 32767)
                       #s16(1 2 3 4 32767))
(dotprod-test-generate s16 #s16(32767 -32767 32767 -32767 32767)
                       #s16(32767 32767 32767 32767 32767))
(dotprod-test-generate s16 #s16(-32768 -32768 -32768 -32768 -32768)
                       #s16(32767 32767 32767 32767 32767))
(dotprod-test-generate u16 #u16(0 1 2 3) #u16(4 5 6 7))
(dotprod-test-generate u16 #u16(16384 16384 16384 16384 16384)
                       #u16(16384 16384 16384 16384 16384))
(dotprod-test-generate u16 #u16(65535 65535 65535 65535 65535)
                       #u16(65535 65535 65535 65535 65535))
(dotprod-test-generate u16 #u16(32767 1 2 3 4)
                       #u16(32767 1 2 3 4))
(dotprod-test-generate u16 #u16(1 2 3 4 32767)
                       #u16(1 2 3 4 32767))

(dotprod-test-generate s32 #s32(0 1 2 3) #s32(4 5 6 7))
(dotprod-test-generate s32 #s32(0 -1 2 -3) #s32(-4 5 -6 7))
(dotprod-test-generate s32 #s32(16384 16384 16384 16384 16384)
                       #s32(16384 16384 16384 16384 16384))
(dotprod-test-generate s32 #s32(16384 -16384 16384 -16384 16384)
                       #s32(16384 -16384 16384 -16384 16384))
(dotprod-test-generate s32 #s32(32767 32767 32767 32767 32767)
                       #s32(32767 32767 32767 32767 32767))
(dotprod-test-generate s32 #s32(214748367 214748367 214748367 214748367 214748367)
                       #s32(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-generate s32 #s32(214748367 1 2 3 4)
                       #s32(214748367 1 2 3 4))
(dotprod-test-generate s32 #s32(1 2 3 4 214748367)
                       #s32(1 2 3 4 214748367))
(dotprod-test-generate s32 #s32(32767 -32767 32767 -32767 32767)
                       #s32(32767 32767 32767 32767 32767))
(dotprod-test-generate s32 #s32(214748367 -214748367 214748367 -214748367 214748367)
                       #s32(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-generate s32 #s32(-214748368 -214748368 -214748368 -214748368 -214748368)
                       #s32(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-generate u32 #u32(0 1 2 3) #u32(4 5 6 7))
(dotprod-test-generate u32 #u32(16384 16384 16384 16384 16384)
                       #u32(16384 16384 16384 16384 16384))
(dotprod-test-generate u32 #u32(4294967295 4294967295 4294967295 4294967295 4294967295)
                       #u32(4294967295 4294967295 4294967295 4294967295 4294967295))
(dotprod-test-generate u32 #u32(4294967295 1 2 3 4)
                       #u32(4294967295 1 2 3 4))
(dotprod-test-generate u32 #u32(1 2 3 4 4294967295)
                       #u32(1 2 3 4 4294967295))

(dotprod-test-generate s64 #s64(0 1 2 3) #s64(4 5 6 7))
(dotprod-test-generate s64 #s64(0 -1 2 -3) #s64(-4 5 -6 7))
(dotprod-test-generate s64 #s64(16384 16384 16384 16384 16384)
                       #s64(16384 16384 16384 16384 16384))
(dotprod-test-generate s64 #s64(16384 -16384 16384 -16384 16384)
                       #s64(16384 -16384 16384 -16384 16384))
(dotprod-test-generate s64 #s64(32767 32767 32767 32767 32767)
                       #s64(32767 32767 32767 32767 32767))
(dotprod-test-generate s64 #s64(214748367 214748367 214748367 214748367 214748367)
                       #s64(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-generate s64 #s64(9223372036854775807 1 2 3 4)
                       #s64(9223372036854775807 1 2 3 4))
(dotprod-test-generate s64 #s64(1 2 3 4 9223372036854775807 1)
                       #s64(1 2 3 4 9223372036854775807 1))
(dotprod-test-generate s64 #s64(32767 -32767 32767 -32767 32767)
                       #s64(32767 -32767 32767 -32767 32767))
(dotprod-test-generate s64 #s64(214748367 -214748367 214748367 -214748367 214748367)
                       #s64(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-generate s64 #s64(-214748368 -214748368 -214748368 -214748368 -214748368)
                       #s64(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-generate s64 #s64(9223372036854775807 -9223372036854775807 9223372036854775807 -9223372036854775807 9223372036854775807)
                       #s64(9223372036854775807 9223372036854775807 9223372036854775807 9223372036854775807 9223372036854775807))
(dotprod-test-generate s64 #s64(-9223372036854775808 -9223372036854775808 -9223372036854775808 -9223372036854775808 -9223372036854775808)
                       #s64(9223372036854775807 9223372036854775807 9223372036854775807 9223372036854775807 9223372036854775807))
(dotprod-test-generate u64 #u64(0 1 2 3) #u64(4 5 6 7))
(dotprod-test-generate u64 #u64(16384 16384 16384 16384 16384)
                       #u64(16384 16384 16384 16384 16384))
(dotprod-test-generate u64 #u64(18446744073709551615 18446744073709551615 18446744073709551615 18446744073709551615 18446744073709551615)
                       #u64(18446744073709551615 18446744073709551615 18446744073709551615 18446744073709551615 18446744073709551615))
(dotprod-test-generate u64 #u64(18446744073709551615 1 2 3 4)
                       #u64(18446744073709551615 1 2 3 4))
(dotprod-test-generate u64 #u64(1 2 3 4 18446744073709551615)
                       #u64(1 2 3 4 18446744073709551615))

(dotprod-test-generate f32 #f32(0 1 2 3) #f32(4 5 6 7))
(dotprod-test-generate f32 #f32(0 -1 2 -3) #f32(-4 5 -6 7))
(dotprod-test-generate f32 #f32(16384 16384 16384 16384 16384)
                       #f32(16384 16384 16384 16384 16384))
(dotprod-test-generate f32 #f32(16384 -16384 16384 -16384 16384)
                       #f32(16384 -16384 16384 -16384 16384))
(dotprod-test-generate f32 #f32(32767 32767 32767 32767 32767)
                       #f32(32767 32767 32767 32767 32767))
(dotprod-test-generate f32 #f32(214748367 214748367 214748367 214748367 214748367)
                       #f32(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-generate f32 #f32(9223372036854775807 1 2 3 4)
                       #f32(9223372036854775807 1 2 3 4))
(dotprod-test-generate f32 #f32(1 2 3 4 9223372036854775807 1)
                       #f32(1 2 3 4 9223372036854775807 1))
(dotprod-test-generate f32 #f32(32767 -32767 32767 -32767 32767)
                       #f32(32767 -32767 32767 -32767 32767))

(dotprod-test-generate f64 #f64(0 1 2 3) #f64(4 5 6 7))
(dotprod-test-generate f64 #f64(0 -1 2 -3) #f64(-4 5 -6 7))
(dotprod-test-generate f64 #f64(16384 16384 16384 16384 16384)
                       #f64(16384 16384 16384 16384 16384))
(dotprod-test-generate f64 #f64(16384 -16384 16384 -16384 16384)
                       #f64(16384 -16384 16384 -16384 16384))
(dotprod-test-generate f64 #f64(32767 32767 32767 32767 32767)
                       #f64(32767 32767 32767 32767 32767))
(dotprod-test-generate f64 #f64(214748367 214748367 214748367 214748367 214748367)
                       #f64(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-generate f64 #f64(9223372036854775807 1 2 3 4)
                       #f64(9223372036854775807 1 2 3 4))
(dotprod-test-generate f64 #f64(1 2 3 4 9223372036854775807 1)
                       #f64(1 2 3 4 9223372036854775807 1))
(dotprod-test-generate f64 #f64(32767 -32767 32767 -32767 32767)
                       #f64(32767 -32767 32767 -32767 32767))

;;-------------------------------------------------------------------
(test-section "range-check")

(define-macro (range-test-generate tag v min max result)
  `(test (format #f "~svector-range-check" ',tag) ,result
         (lambda ()
           (,(string->symbol #`",|tag|vector-range-check") ',v ',min ',max)))
  )

(range-test-generate s8 #s8(-4 -2 0 2 4) #f #f #f)
(range-test-generate s8 #s8(-4 -2 0 2 4) -4 4 #f)
(range-test-generate s8 #s8(-4 -2 0 2 4) -4 3 4)
(range-test-generate s8 #s8(-4 -2 0 2 4) -4 0 3)
(range-test-generate s8 #s8(-4 -2 0 2 4) -3 4 0)
(range-test-generate s8 #s8(-4 -2 0 2 4) 0 4 0)
(range-test-generate s8 #s8(-4 -2 0 2 4) #x-ffffffffffffffff #f #f)
(range-test-generate s8 #s8(-4 -2 0 2 4) #f #xffffffffffffffff  #f)
(range-test-generate s8 #s8(-4 -2 0 2 4) #xffffffffffffffff #f  0)
(range-test-generate s8 #s8(-4 -2 0 2 4) #f #x-ffffffffffffffff 0)
(range-test-generate s8 #s8(-4 -2 0 2 4) #s8(-4 -3 -2 -1 0) #s8(0 1 2 3 4) #f)
(range-test-generate s8 #s8(-4 -2 0 2 4) #s8(-4 -1 -2 -1 0) #s8(0 1 2 3 4) 1)
(range-test-generate s8 #s8(-4 -2 0 2 4) #s8(-4 -3 -2 -1 0) #s8(0 1 -1 3 4) 2)

(range-test-generate u8 #u8(0 2 4 6 9) #f #f #f)
(range-test-generate u8 #u8(0 2 4 6 9) -4 9 #f)
(range-test-generate u8 #u8(0 2 4 6 9) -4 8 4)
(range-test-generate u8 #u8(0 2 4 6 9) -4 4 3)
(range-test-generate u8 #u8(0 2 4 6 9) 3 4 0)
(range-test-generate u8 #u8(0 2 4 6 9) #x-ffffffffffffffff #f #f)
(range-test-generate u8 #u8(0 2 4 6 9) #f #xffffffffffffffff  #f)
(range-test-generate u8 #u8(0 2 4 6 9) #xffffffffffffffff #f  0)
(range-test-generate u8 #u8(0 2 4 6 9) #f #x-ffffffffffffffff 1)
(range-test-generate u8 #u8(0 2 4 6 9) #u8(0 1 2 3 4) #u8(5 6 7 8 9) #f)
(range-test-generate u8 #u8(0 2 4 6 9) #u8(0 1 5 3 4) #u8(5 6 7 8 9) 2)
(range-test-generate u8 #u8(0 2 4 6 9) #u8(0 1 2 3 4) #u8(5 6 7 5 6) 3)

(range-test-generate s16 #s16(-4 -2 0 2 4) #f #f #f)
(range-test-generate s16 #s16(-4 -2 0 2 4) -4 4 #f)
(range-test-generate s16 #s16(-4 -2 0 2 4) -4 3 4)
(range-test-generate s16 #s16(-4 -2 0 2 4) -4 0 3)
(range-test-generate s16 #s16(-4 -2 0 2 4) -3 4 0)
(range-test-generate s16 #s16(-4 -2 0 2 4) 0 4 0)
(range-test-generate s16 #s16(-4 -2 0 2 4) #x-ffffffffffffffff #f #f)
(range-test-generate s16 #s16(-4 -2 0 2 4) #f #xffffffffffffffff  #f)
(range-test-generate s16 #s16(-4 -2 0 2 4) #xffffffffffffffff #f  0)
(range-test-generate s16 #s16(-4 -2 0 2 4) #f #x-ffffffffffffffff 0)
(range-test-generate s16 #s16(-4 -2 0 2 4) #s16(-4 -3 -2 -1 0) #s16(0 1 2 3 4) #f)
(range-test-generate s16 #s16(-4 -2 0 2 4) #s16(-4 -1 -2 -1 0) #s16(0 1 2 3 4) 1)
(range-test-generate s16 #s16(-4 -2 0 2 4) #s16(-4 -3 -2 -1 0) #s16(0 1 -1 3 4) 2)

(range-test-generate u16 #u16(0 2 4 6 9) #f #f #f)
(range-test-generate u16 #u16(0 2 4 6 9) -4 9 #f)
(range-test-generate u16 #u16(0 2 4 6 9) -4 8 4)
(range-test-generate u16 #u16(0 2 4 6 9) -4 4 3)
(range-test-generate u16 #u16(0 2 4 6 9) 3 4 0)
(range-test-generate u16 #u16(0 2 4 6 9) #x-ffffffffffffffff #f #f)
(range-test-generate u16 #u16(0 2 4 6 9) #f #xffffffffffffffff  #f)
(range-test-generate u16 #u16(0 2 4 6 9) #xffffffffffffffff #f  0)
(range-test-generate u16 #u16(0 2 4 6 9) #f #x-ffffffffffffffff 1)
(range-test-generate u16 #u16(0 2 4 6 9) #u16(0 1 2 3 4) #u16(5 6 7 8 9) #f)
(range-test-generate u16 #u16(0 2 4 6 9) #u16(0 1 5 3 4) #u16(5 6 7 8 9) 2)
(range-test-generate u16 #u16(0 2 4 6 9) #u16(0 1 2 3 4) #u16(5 6 7 5 6) 3)

(range-test-generate s32 #s32(-4 -2 0 2 4) #f #f #f)
(range-test-generate s32 #s32(-4 -2 0 2 4) -4 4 #f)
(range-test-generate s32 #s32(-4 -2 0 2 4) -4 3 4)
(range-test-generate s32 #s32(-4 -2 0 2 4) -4 0 3)
(range-test-generate s32 #s32(-4 -2 0 2 4) -3 4 0)
(range-test-generate s32 #s32(-4 -2 0 2 4) 0 4 0)
(range-test-generate s32 #s32(-4 -2 0 2 4) #x-ffffffffffffffff #f #f)
(range-test-generate s32 #s32(-4 -2 0 2 4) #f #xffffffffffffffff  #f)
(range-test-generate s32 #s32(-4 -2 0 2 4) #xffffffffffffffff #f  0)
(range-test-generate s32 #s32(-4 -2 0 2 4) #f #x-ffffffffffffffff 0)
(range-test-generate s32 #s32(-4 -2 0 2 4) #s32(-4 -3 -2 -1 0) #s32(0 1 2 3 4) #f)
(range-test-generate s32 #s32(-4 -2 0 2 4) #s32(-4 -1 -2 -1 0) #s32(0 1 2 3 4) 1)
(range-test-generate s32 #s32(-4 -2 0 2 4) #s32(-4 -3 -2 -1 0) #s32(0 1 -1 3 4) 2)

(range-test-generate u32 #u32(0 2 4 6 9) #f #f #f)
(range-test-generate u32 #u32(0 2 4 6 9) -4 9 #f)
(range-test-generate u32 #u32(0 2 4 6 9) -4 8 4)
(range-test-generate u32 #u32(0 2 4 6 9) -4 4 3)
(range-test-generate u32 #u32(0 2 4 6 9) 3 4 0)
(range-test-generate u32 #u32(0 2 4 6 9) #x-ffffffffffffffff #f #f)
(range-test-generate u32 #u32(0 2 4 6 9) #f #xffffffffffffffff  #f)
(range-test-generate u32 #u32(0 2 4 6 9) #xffffffffffffffff #f  0)
(range-test-generate u32 #u32(0 2 4 6 9) #f #x-ffffffffffffffff 1)
(range-test-generate u32 #u32(0 2 4 6 9) #u32(0 1 2 3 4) #u32(5 6 7 8 9) #f)
(range-test-generate u32 #u32(0 2 4 6 9) #u32(0 1 5 3 4) #u32(5 6 7 8 9) 2)
(range-test-generate u32 #u32(0 2 4 6 9) #u32(0 1 2 3 4) #u32(5 6 7 5 6) 3)

(range-test-generate s64 #s64(-4 -2 0 2 4) #f #f #f)
(range-test-generate s64 #s64(-4 -2 0 2 4) -4 4 #f)
(range-test-generate s64 #s64(-4 -2 0 2 4) -4 3 4)
(range-test-generate s64 #s64(-4 -2 0 2 4) -4 0 3)
(range-test-generate s64 #s64(-4 -2 0 2 4) -3 4 0)
(range-test-generate s64 #s64(-4 -2 0 2 4) 0 4 0)
(range-test-generate s64 #s64(-4 -2 0 2 4) #x-ffffffffffffffff #f #f)
(range-test-generate s64 #s64(-4 -2 0 2 4) #f #xffffffffffffffff  #f)
(range-test-generate s64 #s64(-4 -2 0 2 4) #xffffffffffffffff #f  0)
(range-test-generate s64 #s64(-4 -2 0 2 4) #f #x-ffffffffffffffff 0)
(range-test-generate s64 #s64(-4 -2 0 2 4) #s64(-4 -3 -2 -1 0) #s64(0 1 2 3 4) #f)
(range-test-generate s64 #s64(-4 -2 0 2 4) #s64(-4 -1 -2 -1 0) #s64(0 1 2 3 4) 1)
(range-test-generate s64 #s64(-4 -2 0 2 4) #s64(-4 -3 -2 -1 0) #s64(0 1 -1 3 4) 2)

(range-test-generate u64 #u64(0 2 4 6 9) #f #f #f)
(range-test-generate u64 #u64(0 2 4 6 9) -4 9 #f)
(range-test-generate u64 #u64(0 2 4 6 9) -4 8 4)
(range-test-generate u64 #u64(0 2 4 6 9) -4 4 3)
(range-test-generate u64 #u64(0 2 4 6 9) 3 4 0)
(range-test-generate u64 #u64(0 2 4 6 9) #x-ffffffffffffffff #f #f)
(range-test-generate u64 #u64(0 2 4 6 9) #f #xffffffffffffffff  #f)
(range-test-generate u64 #u64(0 2 4 6 9) #xffffffffffffffff #f  0)
(range-test-generate u64 #u64(0 2 4 6 9) #f #x-ffffffffffffffff 1)
(range-test-generate u64 #u64(0 2 4 6 9) #u64(0 1 2 3 4) #u64(5 6 7 8 9) #f)
(range-test-generate u64 #u64(0 2 4 6 9) #u64(0 1 5 3 4) #u64(5 6 7 8 9) 2)
(range-test-generate u64 #u64(0 2 4 6 9) #u64(0 1 2 3 4) #u64(5 6 7 5 6) 3)

(range-test-generate f32 #f32(-4.0 -2.0 0.0 2.0 4.0) #f #f #f)
(range-test-generate f32 #f32(-4.0 -2.0 0.0 2.0 4.0) -4.0 4.0 #f)
(range-test-generate f32 #f32(-4.0 -2.0 0.0 2.0 4.0) -4.0 3.0 4)
(range-test-generate f32 #f32(-4.0 -2.0 0.0 2.0 4.0) -4.0 0.0 3)
(range-test-generate f32 #f32(-4.0 -2.0 0.0 2.0 4.0) -3.0 4.0 0)
(range-test-generate f32 #f32(-4.0 -2.0 0.0 2.0 4.0) 0.0 4.0 0)
(range-test-generate f32 #f32(-4.0 -2.0 0.0 2.0 4.0) #f32(-4.0 -3.0 -2.0 -1.0 0.0) #f32(0.0 1.0 2.0 3.0 4.0) #f)
(range-test-generate f32 #f32(-4.0 -2.0 0.0 2.0 4.0) #f32(-4.0 -1.0 -2.0 -1.0 0.0) #f32(0.0 1.0 2.0 3.0 4.0) 1)
(range-test-generate f32 #f32(-4.0 -2.0 0.0 2.0 4.0) #f32(-4.0 -3.0 -2.0 -1.0 0.0) #f32(0.0 1.0 -1.0 3.0 4.0) 2)

(range-test-generate f64 #f64(-4.0 -2.0 0.0 2.0 4.0) #f #f #f)
(range-test-generate f64 #f64(-4.0 -2.0 0.0 2.0 4.0) -4.0 4.0 #f)
(range-test-generate f64 #f64(-4.0 -2.0 0.0 2.0 4.0) -4.0 3.0 4)
(range-test-generate f64 #f64(-4.0 -2.0 0.0 2.0 4.0) -4.0 0.0 3)
(range-test-generate f64 #f64(-4.0 -2.0 0.0 2.0 4.0) -3.0 4.0 0)
(range-test-generate f64 #f64(-4.0 -2.0 0.0 2.0 4.0) 0.0 4.0 0)
(range-test-generate f64 #f64(-4.0 -2.0 0.0 2.0 4.0) #f64(-4.0 -3.0 -2.0 -1.0 0.0) #f64(0.0 1.0 2.0 3.0 4.0) #f)
(range-test-generate f64 #f64(-4.0 -2.0 0.0 2.0 4.0) #f64(-4.0 -1.0 -2.0 -1.0 0.0) #f64(0.0 1.0 2.0 3.0 4.0) 1)
(range-test-generate f64 #f64(-4.0 -2.0 0.0 2.0 4.0) #f64(-4.0 -3.0 -2.0 -1.0 0.0) #f64(0.0 1.0 -1.0 3.0 4.0) 2)

;;-------------------------------------------------------------------
(test-section "clamp")

(define (clamp-test tag class tagvector? tagvector-ref tagvector-length
                    clamp v minv maxv)
  (define (clamp-min index v)
    (cond ((tagvector? minv)
           (max v (tagvector-ref minv index) (tag->min tag)))
          ((not minv) (max v (tag->min tag)))
          (else (max v minv (tag->min tag)))))
  (define (clamp-max index v)
    (cond ((tagvector? maxv)
           (min v (tagvector-ref maxv index) (tag->max tag)))
          ((not maxv) (min v (tag->max tag)))
          (else (min v maxv (tag->max tag)))))
  (let1 result (map-to class
                       (lambda (i)
                         (clamp-min i (clamp-max i (tagvector-ref v i))))
                       (iota (tagvector-length v)))
    (test (format #f "~svector-clamp" tag)
          result
          (lambda () (clamp v minv maxv)))
    (when (or (tagvector? minv) (tagvector? maxv))
      (test (format #f "~svector-clamp (list)" tag)
            result
            (lambda () (clamp v
                              (if (tagvector? minv)
                                  (coerce-to <list> minv)
                                  minv)
                              (if (tagvector? maxv)
                                  (coerce-to <list> maxv)
                                  maxv))))
      (test (format #f "~svector-clamp (vector)" tag)
            result
            (lambda () (clamp v
                              (if (tagvector? minv)
                                  (coerce-to <vector> minv)
                                  minv)
                              (if (tagvector? maxv)
                                  (coerce-to <vector> maxv)
                                  maxv)))))
    ))

(define-macro (clamp-test-generate tag v minv maxv)
  `(clamp-test ',tag ,(string->symbol #`"<,|tag|vector>")
               ,(string->symbol #`",|tag|vector?")
               ,(string->symbol #`",|tag|vector-ref")
               ,(string->symbol #`",|tag|vector-length")
               ,(string->symbol #`",|tag|vector-clamp")
               ',v ',minv ',maxv))

(clamp-test-generate s8 #s8(0 -127 -4 4 127) #f #f)
(clamp-test-generate s8 #s8(0 -127 -4 4 127) -8 #f)
(clamp-test-generate s8 #s8(0 -127 -4 4 127) 0 #f)
(clamp-test-generate s8 #s8(0 -127 -4 4 127) 8 #f)
(clamp-test-generate s8 #s8(0 -127 -4 4 127) #f -8)
(clamp-test-generate s8 #s8(0 -127 -4 4 127) #f 0)
(clamp-test-generate s8 #s8(0 -127 -4 4 127) #f 8)
(clamp-test-generate s8 #s8(0 -127 -4 4 127) -1 1)
(clamp-test-generate s8 #s8(0 -127 -4 4 127)
                     #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-generate s8 #s8(0 -127 -4 4 127)
                     0 #xffffffffffffffffffffffffffffff)
(clamp-test-generate s8 #s8(0 -127 -4 4 127)
                     #s8(-3 -4 -6 -8 -19) #s8(3 7 9 2 4))

(clamp-test-generate u8 #u8(127 0 4 200 255) #f #f)
(clamp-test-generate u8 #u8(127 0 4 200 255) -4 #f)
(clamp-test-generate u8 #u8(127 0 4 200 255) 0 #f)
(clamp-test-generate u8 #u8(127 0 4 200 255) 199 #f)
(clamp-test-generate u8 #u8(127 0 4 200 255) #f -4)
(clamp-test-generate u8 #u8(127 0 4 200 255) #f 0)
(clamp-test-generate u8 #u8(127 0 4 200 255) #f 199)
(clamp-test-generate u8 #u8(127 0 4 200 255)
                     #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-generate u8 #u8(127 0 4 200 255)
                     0 #xffffffffffffffffffffffffffffff)
(clamp-test-generate u8 #u8(127 0 4 200 255)
                     #u8(3 3 3 3 3) #u8(199 199 199 199 199))

(clamp-test-generate s16 #s16(0 -127 -4 4 127) #f #f)
(clamp-test-generate s16 #s16(0 -127 -4 4 127) -8 #f)
(clamp-test-generate s16 #s16(0 -127 -4 4 127) 0 #f)
(clamp-test-generate s16 #s16(0 -127 -4 4 127) 8 #f)
(clamp-test-generate s16 #s16(0 -127 -4 4 127) #f -8)
(clamp-test-generate s16 #s16(0 -127 -4 4 127) #f 0)
(clamp-test-generate s16 #s16(0 -127 -4 4 127) #f 8)
(clamp-test-generate s16 #s16(0 -127 -4 4 127) -1 1)
(clamp-test-generate s16 #s16(0 -127 -4 4 127)
                     #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-generate s16 #s16(0 -127 -4 4 127)
                     0 #xffffffffffffffffffffffffffffff)
(clamp-test-generate s16 #s16(0 -127 -4 4 127)
                     #s16(-3 -4 -6 -8 -19) #s16(3 7 9 2 4))

(clamp-test-generate u16 #u16(127 0 4 200 255) #f #f)
(clamp-test-generate u16 #u16(127 0 4 200 255) -4 #f)
(clamp-test-generate u16 #u16(127 0 4 200 255) 0 #f)
(clamp-test-generate u16 #u16(127 0 4 200 255) 199 #f)
(clamp-test-generate u16 #u16(127 0 4 200 255) #f -4)
(clamp-test-generate u16 #u16(127 0 4 200 255) #f 0)
(clamp-test-generate u16 #u16(127 0 4 200 255) #f 199)
(clamp-test-generate u16 #u16(127 0 4 200 255)
                     #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-generate u16 #u16(127 0 4 200 255)
                     0 #xffffffffffffffffffffffffffffff)
(clamp-test-generate u16 #u16(127 0 4 200 255)
                     #u16(3 3 3 3 3) #u16(199 199 199 199 199))

(clamp-test-generate s32 #s32(0 -127 -4 4 127) #f #f)
(clamp-test-generate s32 #s32(0 -127 -4 4 127) -8 #f)
(clamp-test-generate s32 #s32(0 -127 -4 4 127) 0 #f)
(clamp-test-generate s32 #s32(0 -127 -4 4 127) 8 #f)
(clamp-test-generate s32 #s32(0 -127 -4 4 127) #f -8)
(clamp-test-generate s32 #s32(0 -127 -4 4 127) #f 0)
(clamp-test-generate s32 #s32(0 -127 -4 4 127) #f 8)
(clamp-test-generate s32 #s32(0 -127 -4 4 127) -1 1)
(clamp-test-generate s32 #s32(0 -127 -4 4 127)
                     #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-generate s32 #s32(0 -127 -4 4 127)
                     0 #xffffffffffffffffffffffffffffff)
(clamp-test-generate s32 #s32(0 -127 -4 4 127)
                     #s32(-3 -4 -6 -8 -19) #s32(3 7 9 2 4))

(clamp-test-generate u32 #u32(127 0 4 200 255) #f #f)
(clamp-test-generate u32 #u32(127 0 4 200 255) -4 #f)
(clamp-test-generate u32 #u32(127 0 4 200 255) 0 #f)
(clamp-test-generate u32 #u32(127 0 4 200 255) 199 #f)
(clamp-test-generate u32 #u32(127 0 4 200 255) #f -4)
(clamp-test-generate u32 #u32(127 0 4 200 255) #f 0)
(clamp-test-generate u32 #u32(127 0 4 200 255) #f 199)
(clamp-test-generate u32 #u32(127 0 4 200 255)
                     #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-generate u32 #u32(127 0 4 200 255)
                     0 #xffffffffffffffffffffffffffffff)
(clamp-test-generate u32 #u32(127 0 4 200 255)
                     #u32(3 3 3 3 3) #u32(199 199 199 199 199))

(clamp-test-generate s64 #s64(0 -127 -4 4 127) #f #f)
(clamp-test-generate s64 #s64(0 -127 -4 4 127) -8 #f)
(clamp-test-generate s64 #s64(0 -127 -4 4 127) 0 #f)
(clamp-test-generate s64 #s64(0 -127 -4 4 127) 8 #f)
(clamp-test-generate s64 #s64(0 -127 -4 4 127) #f -8)
(clamp-test-generate s64 #s64(0 -127 -4 4 127) #f 0)
(clamp-test-generate s64 #s64(0 -127 -4 4 127) #f 8)
(clamp-test-generate s64 #s64(0 -127 -4 4 127) -1 1)
(clamp-test-generate s64 #s64(0 -127 -4 4 127)
                     #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-generate s64 #s64(0 -127 -4 4 127)
                     0 #xffffffffffffffffffffffffffffff)
(clamp-test-generate s64 #s64(0 -127 -4 4 127)
                     #s64(-3 -4 -6 -8 -19) #s64(3 7 9 2 4))

(clamp-test-generate u64 #u64(127 0 4 200 255) #f #f)
(clamp-test-generate u64 #u64(127 0 4 200 255) -4 #f)
(clamp-test-generate u64 #u64(127 0 4 200 255) 0 #f)
(clamp-test-generate u64 #u64(127 0 4 200 255) 199 #f)
(clamp-test-generate u64 #u64(127 0 4 200 255) #f -4)
(clamp-test-generate u64 #u64(127 0 4 200 255) #f 0)
(clamp-test-generate u64 #u64(127 0 4 200 255) #f 199)
(clamp-test-generate u64 #u64(127 0 4 200 255)
                     #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-generate u64 #u64(127 0 4 200 255)
                     0 #xffffffffffffffffffffffffffffff)
(clamp-test-generate u64 #u64(127 0 4 200 255)
                     #u64(3 3 3 3 3) #u64(199 199 199 199 199))

(test-end)
