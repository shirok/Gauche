;;
;; SRFI-31
;;

;; This implementation is taken from http://srfi.schemers.org/srfi-31/,
;; copyrighted by Mirko Luedde.
;; Shiro Kawai added a small Gauche module cliches and error clause.
;; standard gauche-init.scm autoloads this.

(define-module srfi-31
  (export rec))
(select-module srfi-31)

(define-syntax rec
  (syntax-rules ()
    ((rec (name . variables) . body)
     (letrec ((name (lambda variables . body)) ) name))
    ((rec name expression)
     (letrec ((name expression) ) name))
    ((rec . _)
     (syntax-error "malformed rec" (rec . _)))
    ))



