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
;;;  $Id: signal.scm,v 1.2 2002-01-24 10:33:35 shirok Exp $
;;;

;; This file is to be autoloaded

(select-module gauche)
(use srfi-1)

;; (with-signal-handlers
;;   (clause ...)
;;   thunk)
;;
;; clause := (signals expr ...) or (signals => proc)
;; signals := an expr that evaluates to an integer, a list of integers or
;;            a sigset object
;; proc   := an expr that evaluates to a procedure that takes one arg

(define-syntax with-signal-handlers
  (syntax-rules (=>)
    ((_ "loop" () handlers thunk)
     (%with-signal-handlers (list . handlers) thunk))
    ((_ "loop" ((sigs => proc) . clauses) (handlers ...) thunk)
     (with-signal-handlers "loop" clauses
                           (handlers ... (cons (%make-sigset sigs) proc))
                           thunk))
    ((_ "loop" ((sigs expr ...) . clauses) (handlers ...) thunk)
     (with-signal-handlers "loop" clauses
                           (handlers ...
                            (cons (%make-sigset sigs) (lambda _ expr ...)))
                           thunk))
    ((_ "loop" whatever handlers thunk)
     (syntax-error "bad clause in with-signal-handlers" whatever))
    ((_ (clause ...) thunk)
     (with-signal-handlers "loop" (clause ...) () thunk))
    ((_ . whatever)
     (syntax-error "malformed with-signal-handlers"
                   (with-signal-handlers . whatever)))
    ))

(define (%make-sigset signals)
  (cond
   ((is-a? signals <sys-sigset>) signals)
   ((or (integer? signals) (eq? signals #t))
    (sys-sigset-add! (make <sys-sigset>) signals))
   ((and (list? signals)
         (every integer? signals))
    (apply sys-sigset-add! (make <sys-sigset>) signals))
   (else
    (error "bad signal set" signals))))

(provide "gauche/signal")

    
