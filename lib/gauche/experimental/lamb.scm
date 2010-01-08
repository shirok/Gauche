;;;
;;;  gauche.experimenta.lamb - shorthand notation of lambda
;;;

(define-module gauche.experimental.lamb
  (use util.match)
  (export ^. ^*))
(select-module gauche.experimental.lamb)

(define-syntax ^.
  (syntax-rules ()
    [(^. . clauses) (match-lambda . clauses)]))

(define-syntax ^*
  (syntax-rules ()
    [(^* . clauses) (match-lambda* . clauses)]))

