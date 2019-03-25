(define-module foo
  (use foo.bar1)
  (use foo.bar3)
  (export foo-master foo-literals))
(select-module foo)

(define (foo-master x)
  (list (bar1 x) (bar3 x)))

;; The following tests literal generation
(define (foo-literals)
  '(1 1.0 +inf.0 -inf.0 +nan.0
    #e1e100 22/7 5+3i
    #\a "abcde?" a :a
    #(a b c d e)
    #u8(1 2 3 4 5)
    #s8(1 2 3 4 5)
    #u16(1 2 3 4 5)
    #s16(1 2 3 4 5)
    #u32(1 2 3 4 5)
    #s32(1 2 3 4 5)
    #u64(1 2 3 4 5)
    #s64(1 2 3 4 5)
    ;#f16(1 2 3 4 5)
    #f32(1 2 3 4 5)
    #f64(1 2 3 4 5)
    ))
