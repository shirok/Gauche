(define-module foo
  (use foo.bar1)
  (use foo.bar3)
  (export foo-master foo-literals))
(select-module foo)

(define (foo-master x)
  (list (bar1 x) (bar3 x)))

;; The following tests literal generation
(define (foo-literals)
  (include "literals.scm"))
