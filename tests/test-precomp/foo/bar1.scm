(define-module foo.bar1
  (use foo.bar2)
  (export bar1))
(select-module foo.bar1)

(define (bar1 n)
  (bar2 (* n 2)))

