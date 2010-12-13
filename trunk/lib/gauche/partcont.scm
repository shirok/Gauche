(define-module gauche.partcont
  (export reset shift call/pc))
(select-module gauche.partcont)

(define %reset (with-module gauche.internal %apply-rec0))
(define %call/pc (with-module gauche.internal %call/pc))

(define-syntax reset
  (syntax-rules ()
    [(reset expr ...)
     (%reset (lambda () expr ...))]))

(define (call/pc proc)
  (%call/pc (lambda (k) (proc (lambda args (reset (apply k args)))))))

(define-syntax shift
  (syntax-rules ()
    [(shift var expr ...)
     (call/pc (lambda (var) expr ...))]))
