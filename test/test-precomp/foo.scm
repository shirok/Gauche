(define-module foo
  (use foo.bar1)
  (use foo.bar3)
  (export foo-master))
(select-module foo)

(define (foo-master x)
  (list (bar1 x) (bar3 x)))

