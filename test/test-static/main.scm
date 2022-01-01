(use gauche.sequence) ; test if prelinked modules works
(use foo.bar)

(define (main args)
  (print prefix (coerce-to <vector> (cdr args)))
  0)
