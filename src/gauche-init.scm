;;;
;;; gauche-init.scm - initialize standard environment
;;;
;;;  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: gauche-init.scm,v 1.11 2001-03-06 08:54:30 shiro Exp $
;;;

(select-module gauche)

;;
;; Some useful aliases
;;

(define CALL/CC call-with-current-continuation)

;;
;; Auxiliary stuff for R5RS
;;

(define (CALL-WITH-VALUES producer consumer)
  (receive vals (producer) (apply consumer vals)))

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
  (unless (provided? feature)
    (set! *provided* (cons feature *provided*))))

(define (PROVIDED? feature)
  (member feature *provided*))

;;
;; Autoload
;;

(define-macro (AUTOLOAD file . vars)
  (cons 'begin (map (lambda (v) `(define ,v (%make-autoload ',v ,file)))
                    vars)))



