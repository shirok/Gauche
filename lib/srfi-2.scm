;;
;; SRFI-2: and-let*
;;

;; $Id: srfi-2.scm,v 1.3 2001-03-12 07:42:56 shiro Exp $

(define-module srfi-2
  (export and-let*))
(select-module srfi-2)

(define-syntax and-let*
  (syntax-rules ()
    ((_ () ?body ...)
     (begin ?body ...))
    ((_ ((?var ?expr) ?claw ...) ?body ...)
     (cond (?expr => (lambda (?var)
                       (and-let* (?claw ...) ?body ...)))
           (else #f)))
    ((_ ((?expr) ?claw ...) ?body ...)
     (if ?expr (and-let* (?claw ...) ?body ...) #f))
    ((_ (?var ?claw ...) ?body ...)
     (if ?var (and-let* (?claw ...) ?body ...) #f))))

(provide "srfi-2")

