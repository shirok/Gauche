(define-module my.module
  (use your.library)
  (export my-module))
(select-module my.module)
(define (my-module)
  (print *your-library*))
