(define-module gauche.partcont
  (export reset shift call/pc reset-at shift-at
          prompt control prompt-at control-at
          call-with-current-continuation call/cc
          guard))
(select-module gauche.partcont)

(define %reset (with-module gauche.internal %reset))
(define %call/pc (with-module gauche.internal %call/pc))

;; reset / shift

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
     (^[] (proc (^ args (apply k args))))))
   prompt-tag))

(define-syntax control
  (syntax-rules ()
    [(control var expr ...)
     (call/control (^[var] expr ...))]))

(define-syntax control-at
  (syntax-rules ()
    [(control-at prompt-tag var expr ...)
     (call/control (^[var] expr ...) prompt-tag)]))

;; call/cc (= non-composable partial continuation)

(define call-with-current-continuation call-with-non-composable-continuation)
(define call/cc call-with-non-composable-continuation)

;; guard (from R7RS)

(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) e1 e2 ...)
     ((call/cc
       (lambda (guard-k)
         (with-exception-handler
          (lambda (condition)
            ((call/cc
              (lambda (handler-k)
                (guard-k
                 (lambda ()
                   (let ((var condition))
                     (guard-aux
                      (handler-k
                       (lambda ()
                         (raise-continuable condition)))
                      clause ...))))))))
          (lambda ()
            (call-with-values
                (lambda () e1 e2 ...)
              (lambda args
                (guard-k
                 (lambda ()
                   (apply values args)))))))))))))

(define-syntax guard-aux
  (syntax-rules (else =>)
    ((guard-aux reraise (else result1 result2 ...))
     (begin result1 result2 ...))
    ((guard-aux reraise (test => result))
     (let ((temp test))
       (if temp
         (result temp)
         reraise)))
    ((guard-aux reraise (test => result)
                clause1 clause2 ...)
     (let ((temp test))
       (if temp
         (result temp)
         (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test))
     (or test reraise))
    ((guard-aux reraise (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
         temp
         (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test result1 result2 ...))
     (if test
       (begin result1 result2 ...)
       reraise))
    ((guard-aux reraise
                (test result1 result2 ...)
                clause1 clause2 ...)
     (if test
       (begin result1 result2 ...)
       (guard-aux reraise clause1 clause2 ...)))))
