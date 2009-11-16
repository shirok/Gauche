;;;
;;;  gauche.experimenta.lamb - shorthand notation of lambda
;;;

(define-module gauche.experimental.lamb
  (use util.match)
  (export ^ ^. ^* ^_ ^a ^b ^c ^d ^e ^f ^g ^h ^i ^j ^k ^l ^m
          ^n ^o ^p ^q ^r ^s ^t ^u ^v ^w ^x ^y ^z))
(select-module gauche.experimental.lamb)

(define-syntax ^
  (syntax-rules ()
    [(^ formals . body) (lambda formals . body)]))

(define-syntax ^.
  (syntax-rules ()
    [(^. . clauses) (match-lambda . clauses)]))

(define-syntax ^*
  (syntax-rules ()
    [(^* . clauses) (match-lambda* . clauses)]))

;; TODO: need to make 'lambda's hygineic!
(define-macro (^-generator var)
  (let1 name (string->symbol #`"^,var")
    `(define-macro (,name . body)
       `(lambda (,',var) ,@body))))

(define-macro (define-^x . vars)
  `(begin ,@(map (lambda (x) `(^-generator ,x)) vars)))

(define-^x _ a b c d e f g h i j k l m n o p q r s t u v w x y z)
    
(provide "gauche/experimental/lamb")
