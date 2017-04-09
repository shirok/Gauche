(define-module foo.bar3
  (use foo.bar2)
  (export bar3))
(select-module foo.bar3)

(define (bar3 n)
  (bar2 (/ n 2)))
