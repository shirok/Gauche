;;;
;;; hook.scm - hook procedures
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
;;;  $Id: hook.scm,v 1.1 2002-12-10 10:07:48 shirok Exp $
;;;

;; The API is upper-comaptible of Guile's.   The differences are:
;;
;;  * Based on the object system.
;;  * Hook object itself is applicable.
;;  * delete-hook! is defined as an alias of remove-hook!.
;;    The use of delete is consistent to the SRFI-1 and others.

(define-module gauche.hook
  (use gauche.mop.validator)
  (use srfi-1)
  (export <hook> make-hook hook? hook-empty? add-hook!
          remove-hook! delete-hook! reset-hook!
          hook->list run-hook)
  )

(select-module gauche.hook)

(define-class <hook> ()
  ((procedures :init-keyword :procedures :init-value '())
   (arity      :init-keyword :arity :init-value 0
               :validator (lambda (o v)
                            (unless (and (integer? v) (>= v 0))
                              (errorf "invalid arity ~s: must a non-negative integer" v))
                            v))
   ))

;; make-hook [arity] - arity can be <arity-at-least> object.
(define (make-hook . maybe-arity)
  (make <hook> :arity (get-optional maybe-arity 0)))

(define (hook? obj) (is-a? obj <hook>))

(define-method hook-empty? ((hook <hook>))
  (null? (ref hook 'procedures)))

(define-method add-hook! ((hook <hook>) proc . maybe-append?)
  (unless (procedure-arity-includes? proc (ref hook 'arity))
    (errorf "can't add hook ~s: arity is incompatible with expected ~a"
            proc (ref hook 'arity)))
  (if (get-optional maybe-append? #f)
      (update! (ref hook 'procedures) (cut append <> (list proc)))
      (slot-push! hook 'procedures proc))
  (values))

(define-method delete-hook! ((hook <hook>) proc)
  (update! (ref hook 'procedures) (cut delete proc <> eq?))
  (values))

(define remove-hook! delete-hook!)

(define-method reset-hook! ((hook <hook>))
  (set! (ref hook 'procedures) '())
  (values))

(define-method hook->list ((hook <hook>))
  (list-copy (ref hook 'procedures)))

(define-method run-hook ((hook <hook>) . args)
  (unless (= (length args) (ref hook 'arity))
    (errorf "run-hook expects ~a arg(s), but got: ~s"
            (ref hook 'arity) args))
  (for-each (cut apply <> args) (ref hook 'procedures))
  (values))

;; make hook applicable.
(define-method object-apply ((hook <hook>) . args)
  (apply run-hook hook args))

(provide "gauche/hook")
