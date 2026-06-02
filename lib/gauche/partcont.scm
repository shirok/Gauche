(define-module gauche.partcont
  (export reset shift call/pc reset-at shift-at
          prompt control prompt-at control-at))
(select-module gauche.partcont)

(define %reset (with-module gauche.internal %reset))
(define %call/pc (with-module gauche.internal %call/pc))

(define-syntax reset
  (syntax-rules ()
    [(reset expr ...)
     (%reset (^[] expr ...))]))

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

;; prompt / control

(define-syntax prompt
  (syntax-rules ()
    [(prompt expr ...)
     (call-with-continuation-prompt
      (^[] expr ...)
      (default-continuation-prompt-tag)
      (lambda (thunk) (thunk)))]))

(define-syntax prompt-at
  (syntax-rules ()
    [(prompt-at prompt-tag expr ...)
     (call-with-continuation-prompt
      (^[] expr ...)
      prompt-tag
      (lambda (thunk) (thunk)))]))

(define (call/control proc :optional (prompt-tag #f))
  (%call/pc
   (^k
    (abort-current-continuation
     (or prompt-tag (default-continuation-prompt-tag))
     (^[] (proc k))))
   prompt-tag))

(define-syntax control
  (syntax-rules ()
    [(control var expr ...)
     (call/control (^[var] expr ...))]))

(define-syntax control-at
  (syntax-rules ()
    [(control-at prompt-tag var expr ...)
     (call/control (^[var] expr ...) prompt-tag)]))
