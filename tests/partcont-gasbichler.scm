;;
;; Run partcont test with Gasbichler-Sperber shift/reset implementation.
;;

(use gauche.test)
(use gauche.partcont-meta)

(test-start "partcont-gasbichler")

(define-syntax gauche-only
  (syntax-rules ()
    [(_ expr ...) (begin expr ...)]))

;; partcont-meta matches the older expected column for the divergent tests.
(define (srfi226-continuation?) #f)

(include "partcont.scm")

(test-end)
