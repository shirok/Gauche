;;;
;;; let-opt.scm - parsing optional arguments
;;;
;;;  Defines let-optionals* macro, hinted from scsh by Olin Shivers.
;;;  TODO: add let-optionals as well
;;;
;;;  $Id: let-opt.scm,v 1.2 2002-05-02 11:35:48 shirok Exp $
;;;

(define-module gauche.let-opt
  (export let-optionals* let-keywords*))
(select-module gauche.let-opt)

(define-syntax let-optionals*
  (syntax-rules ()
    ((_ ?arg () . ?body) (begin . ?body))
    ((_ ?arg ((?var ?default) . ?more) . ?body)
     (receive (?var next-arg)
         (if (pair? ?arg)
             (values (car ?arg) (cdr ?arg))
             (values ?default '()))
       (let-optionals* next-arg ?more . ?body)))
    ((_ ?arg (?var . ?more) . ?body)
     (receive (?var next-arg)
         (if (pair? ?arg)
             (values (car ?arg) (cdr ?arg))
             (values (undefined) '()))
       (let-optionals* next-arg ?more . ?body)))
    ((_ ?arg (?var) . ?body)
     (let ((?var ?arg)) . ?body))
    ))

(provide "gauche/let-opt")
