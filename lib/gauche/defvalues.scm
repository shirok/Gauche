;;;
;;; gauche/defvalues.scm - define-values and set!-values, to be autoloaded
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
;;;  $Id: defvalues.scm,v 1.1 2002-05-09 08:58:56 shirok Exp $
;;;

;; ChezScheme and MzScheme's define-values and set!-values.
;; To be autoloaded

(define-module gauche.defvalues
  (export define-values set!-values))
(select-module gauche.defvalues)

;; define-values
(define-syntax define-values
  (syntax-rules ()
    ((_ (var  ...) expr)
     (define-values-sub () (var ...) (var ...) expr))
    ((_ . else)
     (syntax-error "malformed define-values" (define-values . else)))
    ))

(define-syntax define-values-sub
  (syntax-rules ()
    ((_ (tmp ...) () (var ...) expr)
     (begin (define var (undefined)) ...
            (receive (tmp ...) expr
              (set! var tmp) ...
              (undefined))))
    ((_ (tmp ...) (v v2 ...) (var ...) expr)
     (define-values-sub (tmp ... tmp1) (v2 ...) (var ...) expr))
    ))

;; set!-values
(define-syntax set!-values
  (syntax-rules ()
    ((_ (var ...) expr)
     (set!-values-sub () (var ...) (var ...) expr))
    ((_ . else)
     (syntax-error "malformed set!-values" (set!-values . else)))
    ))

(define-syntax set!-values-sub
  (syntax-rules ()
    ((_ (tmp ...) () (var ...) expr)
     (receive (tmp ...) expr
       (set! var tmp) ...
       (undefined)))
    ((_ (tmp ...) (v v2 ...) (var ...) expr)
     (set!-values-sub (tmp ... tmp1) (v2 ...) (var ...) expr))
    ))

(provide "gauche/defvalues")

