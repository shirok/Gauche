;;
;; test for srfi-4 module
;;

(use gauche.test)
(use srfi-4)
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

(test-end)
