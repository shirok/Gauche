;;
;; Common auxiliary procedures to run test
;;
;; This file consists of excepts from Oleg Kiselyov's myenv.scm and
;; SSAX.scm.   They are macros and procedures only required for testing.
;;

;; Excerpt from myenv.scm --------------------------------------

(define-macro (assure exp error-msg) `(assert ,exp report: ,error-msg))


(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

(define (cerr . args)
  (for-each (lambda (x)
              (if (procedure? x)
                  (x (standard-error-port))
                  (display x (standard-error-port))))
            args))

;(##define-macro (nl) '(newline))
(define nl "\n")

;; Dummy
(define (pp arg) (write* arg) (newline))

; Some useful increment/decrement operators

                                ; Mutable increment
(define-macro (++! x) `(inc! ,x))

                                ; Read-only increment
(define-macro (++ x) `(+ 1 ,x))

                                ; Mutable decrement
(define-macro (--! x) `(dec! ,x))

                                ; Read-only decrement
(define-macro (-- x) `(- ,x 1))

;; Excerpt from util.scm --------------------------------------

(define (list-intersperse src-l elem)
  (if (null? src-l) src-l
    (let loop ((l (cdr src-l)) (dest (cons (car src-l) '())))
      (if (null? l) (reverse dest)
        (loop (cdr l) (cons (car l) (cons elem dest)))))))

;; Excerpt from SSAX.scm --------------------------------------

;; Kludge - trans.scm removes run-test definition.  We use different
;; name to prevent this from removed.
(define-macro %run-test
  (lambda body
    (define (re-write body)
      (cond
       ((vector? body)
        (list->vector (re-write (vector->list body))))
       ((not (pair? body)) body)
       ((and (eq? 'quote (car body)) (pair? (cdr body))
             (string? (cadr body)))
        (string->symbol (cadr body)))
       (else (cons (re-write (car body)) (re-write (cdr body))))))
    (cons 'begin (re-write body))))

;; Other stuff -----------------------------------------------

(define-syntax declare
  (syntax-rules () ((_ . foo) #f)))

(define-macro (include file)
  (cond ((member file '("myenv.scm" "catch-error.scm")) #f)
        (else `(load ,(string-append "sxml/" file)))))

(define-syntax failed?
  (syntax-rules ()
    ((_ . body)
     (with-error-handler
      (lambda (e) #t)
      (lambda () . body)))))

