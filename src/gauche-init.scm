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
;;;  $Id: gauche-init.scm,v 1.26 2001-04-25 07:30:49 shiro Exp $
;;;

(select-module gauche)

;;
;; Loading, require and provide
;;

;; Load path needs to be dealt with at the compile time.  this is a
;; hack to do so.   Don't modify *load-path* directly, since it causes
;; weird compiler-evaluator problem.
;; I don't like the current name "add-load-path", though---looks like
;; more a procedure than a compiler syntax---any ideas?
(define-macro (add-load-path path)
  `',(%add-load-path path))

;; Same as above.
(define-macro (require feature)
  `',(%require feature))

;; Preferred way
;;  (use x.y.z) ==> (require "x/y/z") (import x.y.z)
;; NB: should this be:
;;  (use (x y z)) ==> (require "x/y/z") (import (x y z))
;;  it's more Scheme-ish, and similar to Guile-way.

(define-macro (use module)
  (unless (symbol? module) (error "use: symbol required: ~s" module))
  (let ((path (string-join (string-split (symbol->string module) #\.) "/")))
    `(begin
       (require ,path)
       (import ,module)))
  )

;; create built-in srfi-6 and srfi-8 modules, so that (use srfi-6)
;; won't complain.
(define-module srfi-6 )
(define-module srfi-8 )

;;
;; Autoload
;;

;; autoload doesn't work for syntactic binding for now...
(define-macro (AUTOLOAD file . vars)
  (cons 'begin (map (lambda (v) `(define ,v (%make-autoload ',v ,file)))
                    vars)))

;;
;; Auxiliary definitions
;;

(define call/cc call-with-current-continuation)

(with-module scheme
  (define (call-with-values producer consumer)
    (with-module gauche (receive vals (producer) (apply consumer vals)))))

(with-module gauche
  (autoload "gauche/with" with-output-to-string call-with-output-string
                          with-input-from-string call-with-input-string))

(with-module gauche
  (autoload "gauche/numerical" gcd lcm numerator denominator
                               make-polar real-part imag-part))

;; useful stuff
(define-syntax check-arg
  (syntax-rules ()
    ((_ ?test ?arg)
     (let ((tmp ?arg))
       (unless (?test tmp)
         (error "bad type of argument for ~s: ~s" '?arg tmp))))
    ))

(define-syntax let-optional*
  (syntax-rules ()
    ((_ ?arg () . ?body) (begin . ?body))
    ((_ ?arg ((?var ?default) . ?more) . ?body)
     (receive (?var next-arg)
         (if (pair? ?arg)
             (values (car ?arg) (cdr ?arg))
             (values ?default '()))
       (let-optional* next-arg ?more . ?body)))
    ((_ ?arg (?var . ?more) . ?body)
     (receive (?var next-arg)
         (if (pair? ?arg)
             (values (car ?arg) (cdr ?arg))
             (values (undefined) '()))
       (let-optional* next-arg ?more . ?body)))
    ))

;;
;; Load object system
;;
(require "srfi-17")                     ;generalized set!
(require "gauche/object")

;;
;; For convenience
;;

(let ((dotfile (sys-normalize-pathname "~/.gaucherc" :expand #t)))
  (when (sys-access dotfile F_OK)
    (load dotfile)))
