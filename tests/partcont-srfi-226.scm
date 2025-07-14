;;
;; Run partcont test with srfi-226 reference implementation
;;
;; This is run with ChezScheme.
;;
;; chezscheme --libdirs $SRFI_226_DIR/lib ./partcont-srfi-226.scm

(import (rename (rnrs base (6)) (error r6rs:error))
        (control-features))

;; Compatibility layer

(define-syntax reset
  (syntax-rules ()
    ((reset e1 e2 ...)
     (call-with-continuation-prompt
      (lambda ()
        e1 e2 ...)))))

(define-syntax shift
  (syntax-rules ()
    ((shift k e1 e2 ...)
     (call-with-composable-continuation
      (lambda (c)
        (define k (lambda args (reset (apply c args))))
        (abort-current-continuation (default-continuation-prompt-tag)
          (lambda ()
            e1 e2 ...)))))))

(define-syntax push!
  (syntax-rules ()
    ((push! loc x) (set! loc (cons x loc)))))

(define-syntax pop!
  (syntax-rules ()
    ((pop! loc) (let ((v (car loc))) (set! loc (cdr loc)) v))))

(define-syntax values->list
  (syntax-rules ()
    ((values->list expr)
     (call-with-values (lambda () expr) list))))

(define-syntax while
  (syntax-rules (=>)
    ((_ expr guard => var . body)
     (do ((var expr expr))
         ((not (guard var)))
       . body))
    ((_ expr => var . body)
     (do ((var expr expr))
         ((not var))
       . body))
    ((_ expr . body)
     (do ()
         ((not expr))
       . body))
    ((_ . other)
     (syntax-error "malformed while" (while . other)))))

(define-syntax gauche-only
  (syntax-rules ()
    ((gauche-only x ...) (values))))

(define *discrepancies* '())

(define-syntax test*
  (syntax-rules (test-error)
    ((test* name expect expr)
     (run
      (lambda ()
        (display name)
        (newline)
        (display "Expect:" )
        (write expect)
        (newline)
        (let-values ((result expr))
          (display "Result:")
          (for-each (lambda (r) (write r) (newline)) result)
          (newline)
          (when (not (equal? (list expect) result))
            (set! *discrepancies*
                  (cons (list name expect result) *discrepancies*))))
        (newline))))))

(define (with-output-to-string thunk)
  (let ((p (open-output-string)))
    (parameterize ((current-output-port p))
      (thunk))
    (get-output-string p)))

(define (error msg . args) (apply r6rs:error 'partcont msg args))

(include "partcont.scm")

(when (not (null? *discrepancies*))
  (display (length *discrepancies*))
  (display " discrepanci(es)")
  (newline))

(exit)
