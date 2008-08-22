;;;
;;;  gauche.experimenta.lamb - shorthand notation of lambda
;;;

(define-module gauche.experimental.lamb
  (export ^))
(select-module gauche.experimental.lamb)

(define-syntax ^
  (syntax-rules ()
    [(^ formals . body) (lambda formals body)]))

(provide "gauche/experimental/lamb")
