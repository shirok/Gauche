;;
;; Prelude to run partcont test with srfi-226 reference implementation
;; w/ ChezScheme
;;
;; This needs to be kicked with tests/partcont-srfi-226.sh, for we
;; need special setup.  See partcont-srfi-226.sh for the details.

(import (rename (except (rnrs base (6))
                        dynamic-wind call/cc call-with-current-continuation)
                (error r6rs:error))
        (only (rnrs conditions (6)) condition-message)
        (control-features))

(define-syntax reset
  (syntax-rules ()
    ((_ e1 e2 ...) (call-with-continuation-prompt (lambda () e1 e2 ...)))))

(define-syntax shift
  (syntax-rules ()
    ((_ k e1 e2 ...)
     (call-with-composable-continuation
      (lambda (c)
        (define k (lambda args (reset (apply c args))))
        (abort-current-continuation (default-continuation-prompt-tag)
          (lambda () e1 e2 ...)))))))

(define-syntax push! (syntax-rules () ((_ loc x) (set! loc (cons x loc)))))
(define-syntax pop!  (syntax-rules () ((_ loc) (let ((v (car loc))) (set! loc (cdr loc)) v))))
(define-syntax values->list (syntax-rules () ((_ e) (call-with-values (lambda () e) list))))

(define-syntax while
  (syntax-rules (=>)
    ((_ e guard => var . body) (do ((var e e)) ((not (guard var))) . body))
    ((_ e => var . body) (do ((var e e)) ((not var)) . body))
    ((_ e . body) (do () ((not e)) . body))))

(define-syntax with-error-handler
  (syntax-rules ()
    ((_ handler thunk)
     (call-with-current-continuation
      (lambda (k)
        (with-exception-handler (lambda (e)
                                  (call-with-values (lambda () (handler e)) k))
                                thunk))))))

(define-syntax gauche-only (syntax-rules () ((_ x ...) (begin x ...))))

(define (with-output-to-string thunk)
  (let ((p (open-output-string)))
    (parameterize ((current-output-port p)) (thunk))
    (get-output-string p)))

(define (error msg . args) (apply r6rs:error 'partcont msg args))

;; Emit (RESULTROW name expect pass? actual) from INSIDE run.
(define-syntax test*
  (syntax-rules (test-error)
    ((_ name (test-error) expr)
     (run (lambda ()
       (let ((r (guard (e (#t 'GOT-ERROR))
                  (call-with-values (lambda () expr) list) 'NO-ERROR)))
         (write (list 'RESULTROW name '(test-error) (eq? r 'GOT-ERROR) r))
         (newline)))))
    ((_ name expect expr)
     (run (lambda ()
       (let ((v (guard (e (#t (list '*UNEXPECTED-ERROR*)))
                  (call-with-values (lambda () expr) list))))
         (write (list 'RESULTROW name (list expect) (equal? (list expect) v) v))
         (newline)))))))
