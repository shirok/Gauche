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
;;;  $Id: gauche-init.scm,v 1.44 2001-10-05 08:16:19 shirok Exp $
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

(define-macro (export-all)
  `',(%export-all))

;; Preferred way
;;  (use x.y.z) ==> (require "x/y/z") (import x.y.z)
;; NB: should this be:
;;  (use (x y z)) ==> (require "x/y/z") (import (x y z))
;;  it's more Scheme-ish, and similar to Guile-way.

(define-macro (use module)
  (unless (symbol? module) (error "use: symbol required:" module))
  (let ((path (string-join (string-split (symbol->string module) #\.) "/")))
    `(begin
       (require ,path)
       (import ,module)))
  )

;; create built-in srfi-6 and srfi-8 modules, so that (use srfi-6)
;; won't complain.
(define-module srfi-6 )
(define-module srfi-8 )
(define-module srfi-10 )
(define-module srfi-17 )

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
                          with-input-from-string call-with-input-string
                          with-string-io call-with-string-io))

(with-module gauche
  (autoload "gauche/port" port->string port->list
                          port->string-list port->sexp-list
                          port-fold port-fold-right
                          port-for-each port-map
                          port-position-prefix))

(with-module gauche
  (autoload "gauche/numerical" gcd lcm numerator denominator
                               make-polar real-part imag-part
                               %complex-exp %complex-log %complex-sqrt
                               %complex-expt
                               %complex-cos %complex-sin %complex-tan
                               %complex-acos %complex-asin %complex-atan
                               %complex-sinh %complex-cosh %complex-tanh
                               %complex-asinh %complex-acosh %complex-atanh))

;; for backward compatibility (will be gone)
(define r_ok (if (symbol-bound? 'r_ok) r_ok |R_OK|))
(define w_ok (if (symbol-bound? 'w_ok) w_ok |W_OK|))
(define x_ok (if (symbol-bound? 'x_ok) x_ok |X_OK|))
(define f_ok (if (symbol-bound? 'f_ok) f_ok |F_OK|))

;; these are so useful that I couldn't resist to add...
(define (file-exists? path)
  (sys-access path f_ok))
(define (file-is-regular? path)
  (and (sys-access path |F_OK|)
       (eq? (sys-stat->file-type (sys-stat path)) 'regular)))
(define (file-is-directory? path)
  (and (sys-access path |F_OK|)
       (eq? (sys-stat->file-type (sys-stat path)) 'directory)))

(define-syntax define-trans
  (syntax-rules ()
    ((_ ?name ?real-fn ?complex-fn)
     (define (?name z)
       (cond ((real? z) (?real-fn z))
             ((number? z) (?complex-fn z))
             (else (error "number required, but got" z)))))
    ))

(define-trans exp   %exp   %complex-exp)
(define-trans log   %log   %complex-log)
(define-trans sqrt  %sqrt  %complex-sqrt)

(define-trans sin   %sin   %complex-sin)
(define-trans cos   %cos   %complex-cos)
(define-trans tan   %tan   %complex-tan)
(define-trans asin  %asin  %complex-asin)
(define-trans acos  %acos  %complex-acos)
(define-trans sinh  %sinh  %complex-sinh)
(define-trans cosh  %cosh  %complex-cosh)
(define-trans tanh  %tanh  %complex-tanh)

(define (asinh z) (%complex-asinh z))
(define (acosh z) (%complex-acosh z))
(define (atanh z) (%complex-atanh z))

(define (atan z . x)
  (if (null? x)
      (cond ((real? z) (%atan z))
            ((number? z) (%complex-atan z))
            (else (error "number required, but got" z)))
      (%atan z (car x))))

(define (expt x y)
  (cond ((and (real? x) (real? y)) (%expt x y))
        ((and (number? x) (number? y)) (%complex-expt x y))
        (else (error "number required, but got" (if (number? x) y x)))))

(with-module scheme
  (define exp (with-module gauche exp))
  (define log (with-module gauche log))
  (define sin (with-module gauche sin))
  (define cos (with-module gauche cos))
  (define tan (with-module gauche tan))
  (define asin (with-module gauche asin))
  (define acos (with-module gauche acos))
  (define atan (with-module gauche atan))
  (define expt (with-module gauche expt)))

;; useful stuff
(define-syntax check-arg
  (syntax-rules ()
    ((_ ?test ?arg)
     (let ((tmp ?arg))
       (unless (?test tmp)
         (errorf "bad type of argument for ~s: ~s" '?arg tmp))))
    ))

;; srfi-17
(define (getter-with-setter get set)
  (let ((proc (lambda x (apply get x))))
    (set! (setter proc) set)
    proc))

;;
;; Load common macros
;;

(require "gauche/common-macros")

;;
;; Load object system
;;

(require "gauche/object")

;;
;; For convenience
;;

(let ((dotfile (sys-normalize-pathname "~/.gaucherc" :expand #t)))
  (when (sys-access dotfile F_OK)
    (load dotfile)))
