;;
;; A quick hack to run test suite written for Chibi
;;

(define-module compat.chibi-test
  (use gauche.parameter)
  (use gauche.test)
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
               (,(r'test*) 'expr #t (,(r'boolean) expr))])]
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

(define-syntax chibi-test:expand
  (syntax-rules ()
    [(_ ()) (begin)]
    [(_ (form)) form]
    [(_ (form . forms))
     (let () form (chibi-test:expand forms))]))

(define-syntax chibi-test:include
  (er-macro-transformer
   (^[f r c]
     (let ([file (cadr f)])
       (let1 iport ((with-module gauche.internal pass1/open-include-file)
                    file (current-load-path))
         (unwind-protect
             `(,(r 'chibi-test:expand) ,(port->sexp-list iport))
           (close-port iport)))))))
