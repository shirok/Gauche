(define-module gauche.partcont
  (export reset shift call/pc))
(select-module gauche.partcont)

(define %reset (with-module gauche.internal %reset))
(define %call/pc (with-module gauche.internal %call/pc))

(define-syntax reset
  (syntax-rules ()
    [(reset expr ...)
     (call-with-continuation-prompt (^[] expr ...))]))

(define (call/pc proc)
  (%call/pc
   (^k
    (abort-current-continuation
     (default-continuation-prompt-tag)
     (^[] (proc (^ args
                   (call-with-continuation-prompt
                    (^[] (apply k args))))))))))

(define-syntax shift
  (syntax-rules ()
    [(shift var expr ...)
     (call/pc (^[var] expr ...))]))
