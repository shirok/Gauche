;;;
;;; signal.scm - with-signal-handlers
;;;
;;;  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: signal.scm,v 1.1 2002-01-23 10:18:36 shirok Exp $
;;;

;; This file is to be autoloaded

(use srfi-1)
(select-module gauche)

;; (with-signal-handlers
;;   ((signals expr ...)
;;    ...)
;;   thunk)
;;
;;  signals may be a list of integers or #t (all signals)

(define-macro (with-signal-handlers handlers body)
  `(%with-signal-handlers
    (list ,@(map (lambda (clause)
                   (unless (pair? clause)
                     (error "bad signal handler clause: ~s" clause))
                   `(cons (%make-sigset ,(car clause))
                          (lambda _ ,@(cdr clause))))
                 handlers))
    ,body))

(define (%make-sigset signals)
  (cond
   ((is-a? signals <sys-sigset>) signals)
   ((integer? signals)
    (sys-sigset-add! (make <sys-sigset>) signals))
   ((and (list? signals)
         (every integer? signals))
    (apply sys-sigset-add! (make <sys-sigset>) signals))
   (else
    (error "bad signal set: ~s" signals))))

(provide "gauche/signal")

    
