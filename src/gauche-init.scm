;;;
;;; gauche-init.scm - initialize standard environment
;;;
;;;  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, ditribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: gauche-init.scm,v 1.5 2001-02-16 06:58:22 shiro Exp $
;;;

;;
;; Macro
;;

(define DEFINE-MACRO
  (%make-macro-transformer
   'define-macro
   (lambda (_ spec . body)
     (unless (and (pair? spec)
                  (symbol? (car spec))
                  (not (null? body)))
       (error "badly formed define-macro: ~s" (list* _ spec body)))
     `(define ,(car spec)
        (%make-macro-transformer
         ',(car spec)
         (lambda ,spec ,@body))))))

;;
;; Some useful aliases
;;

(define CALL/CC call-with-current-continuation)

;;
;; Require and provide (temporary solution)
;;

(define *provided*
  '("srfi-6"                            ; string ports (builtin)
    "srfi-8"                            ; receive (builtin)
    ))

(define (REQUIRE feature)
  (unless (member feature *provided*)
    (load (string-append feature ".scm"))))

(define (PROVIDE feature)
  (set! *provided* (cons feature *provided*)))

(define (PROVIDED? feature)
  (member feature *provided*))

