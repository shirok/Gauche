;;
;; A quick hack to run test suite written for Chibi
;;

(define-module compat.chibi-test
  (use gauche.parameter)
  (use gauche.test)
  (use util.match)
  (export chibi-test current-test-comparator))
(select-module compat.chibi-test)

(define current-test-comparator (make-parameter equal?))
(define-syntax gauche:parameterize parameterize)
(define gauche:test-error test-error)

;; chibi allows internal defines to interleave expressions.  Gauche can't
;; do that, so we translate the whole body of chibi-test into
;; nested lets.

;; we also cheat the scope - we want to replace macros such as
;; test, include and parameterize, but we want them to be effective
;; only inside chibi-test.

(define-syntax chibi-test
  (er-macro-transformer
   (^[f r c]
     `(let-syntax
          ([parameterize
            (syntax-rules ()
              [(_ bindings . fs)
               (,(r'gauche:parameterize) bindings
                (,(r'chibi-test:expand) fs))])]
           [use
            ;; NB: We ignore 'use' in the chibi test file; necessary modules
            ;; are supposed to be already used in the includer.
            (syntax-rules ()
              [(_ . args) (begin)])]
           [import
            ;; So as import 
            (syntax-rules ()
              [(_ . args) (begin)])]
           [include
            (syntax-rules ()
              [(_ file) (,(r'chibi-test:include) file)])]
           [test
            (syntax-rules ()
              [(_ name expected expr)
               (,(r'test*) name expected expr (current-test-comparator))]
              [(_ expected expr)
               (,(r'test*) 'expr expected expr (current-test-comparator))])]
           [test-group
            (syntax-rules ()
              [(_ name . fs)
               (begin
                 (,(r'test-section) name)
                 (,(r'chibi-test:expand) fs))])]
           [test-assert
            (syntax-rules ()
              [(_ expr)
               (,(r'test*) 'expr #t (,(r'boolean) expr))]
              [(_ name expr)
               (,(r'test*) name #t (,(r'boolean) expr))])]
           [test-not
            (syntax-rules ()
              [(_ expr)
               (,(r'test*) 'expr #f (,(r'boolean) expr))]
              [(_ name expr)
               (,(r'test*) name #f (,(r'boolean) expr))])]
           [test-error
            (syntax-rules ()
              [(_ expr)
               (,(r'test*) 'expr (,(r'gauche:test-error)) expr)])]
           [test-exit
            ;; ignore test-exit, for it is inside chibi-test and we don't
            ;; want to exit.
            (syntax-rules ()
              [(_ . args) (begin)])]
           )
       (,(r'chibi-test:expand) ,(cdr f))))))

;; We gather definitions at the same level, so that mutually recursive
;; definitions work
(define-syntax chibi-test:expand
  (er-macro-transformer
   (^[f r c]
     (let loop ([forms (cadr f)]
                [defs '()])
       (match forms
         [() 
          (if (null? defs)
            (quasirename r `(begin))
            (quasirename r `(let () ,@(reverse defs) (begin))))]
         [(form)
          (if (null? defs)
            (car forms)
            (quasirename r `(let () ,@(reverse defs) ,(car forms))))]
         [((and ((? (cut c <> (r'define))) . _) def) . forms)
          (loop forms (cons def defs))]
         [(form . forms)
          (if (null? defs)
            (quasirename r `(let () ,form (chibi-test:expand ,forms)))
            (quasirename r `(let () ,@(reverse defs) 
                                 ,form (chibi-test:expand ,forms))))])))))

(define-syntax chibi-test:include
  (er-macro-transformer
   (^[f r c]
     (let ([file (cadr f)])
       (let1 iport ((with-module gauche.internal pass1/open-include-file)
                    file (current-load-path))
         (unwind-protect
             `(,(r 'chibi-test:expand) ,(port->sexp-list iport))
           (close-port iport)))))))

;; A hack to make Gauche think it has loaded chibi.test so that it won't
;; tripped by (import (chibi test)) in the test code.
(define-module chibi.test (extend compat.chibi-test))
(provide "chibi/test")
(provide "compat/chibi-test")
