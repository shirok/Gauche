(define-module gauche.partcont
  (export reset shift call/pc
          reset-at shift-at))
(select-module gauche.partcont)

(define %reset (with-module gauche.internal %reset))
(define %call/pc (with-module gauche.internal %call/pc))

(define-syntax reset
  (syntax-rules ()
    [(reset expr ...)
     (call-with-continuation-prompt (^[] expr ...))]))

(define-syntax reset-at
  (syntax-rules ()
    [(reset-at prompt-tag expr ...)
     (call-with-continuation-prompt (^[] expr ...) prompt-tag)]))

(define (call/pc proc :optional (prompt-tag #f))
  (%call/pc
   (^k
    (abort-current-continuation
     (or prompt-tag (default-continuation-prompt-tag))
     (^[] (proc (^ args
                   (call-with-continuation-prompt
                    (^[] (apply k args))
                    prompt-tag))))))
   prompt-tag))

(define-syntax shift
  (syntax-rules ()
    [(shift var expr ...)
     (call/pc (^[var] expr ...))]))

(define-syntax shift-at
  (syntax-rules ()
    [(shift-at prompt-tag var expr ...)
     (call/pc (^[var] expr ...) prompt-tag)]))
