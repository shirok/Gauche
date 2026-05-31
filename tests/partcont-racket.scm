;;
;; Run partcont test with Racket.
;;
;; plt-r6rs ./partcont-racket

(import (rename (rnrs base (6)) (error r6rs:error))
        (rnrs io ports (6))
        (rnrs io simple (6))
        (rnrs files (6))
        (rnrs exceptions (6))
        (rnrs conditions (6))
        (rnrs control (6))
        (srfi :39)
        (racket include)
        (racket control))

;; Compatibility layer

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

(define-syntax temporarily
  (syntax-rules ()
    ((temporarily ((state init) ...) expr ...)
     (let ((tmp init) ...)
       (dynamic-wind
         (lambda () (set! tmp (state tmp)) ...)
         (lambda () expr ...)
         (lambda () (set! tmp (state tmp)) ...))))))

(define-syntax gauche-only
  (syntax-rules ()
    ((gauche-only x ...) (values))))

(define *discrepancies* '())

(define-syntax test*
  (syntax-rules (test-error)
    ((test* name expect expr)
     (begin
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
       (newline)))))

(define (with-output-to-string thunk)
  (with-output-to-file "tmp.log" (lambda () (reset (thunk))))
  (let ((s (with-input-from-file "tmp.log"
             (lambda ()
               (call-with-string-output-port
                (lambda (out)
                  (let loop ((ch (read-char)))
                    (unless (eof-object? ch)
                      (display ch out)
                      (loop (read-char))))))))))
    (delete-file "tmp.log")
    s))

(define (error msg . args) (apply r6rs:error 'partcont msg args))

(include "partcont.scm")

(when (not (null? *discrepancies*))
  (display (length *discrepancies*))
  (display " discrepanci(es)")
  (newline))
