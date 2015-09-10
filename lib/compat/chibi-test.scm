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

(define-syntax chibi-test
  (syntax-rules (test-group test test-assert test-error parameterize)
    [(_ (test-group name . forms) . rest)
     (begin (chibi-test . forms)
            (chibi-test . rest))]
    [(_ (test expected expr) . rest)
     (begin (test* 'expected expected expr (current-test-comparator))
            (chibi-test . rest))]
    [(_ (test name expected expr) . rest)
     (begin (test* name expected expr (current-test-comparator))
            (chibi-test . rest))]
    [(_ (test-assert expr) . rest)
     (begin (test* 'expr #t (boolean expr))
            (chibi-test . rest))]
    [(_ (test-error expr) . rest)
     (begin (test* 'expr (gauche:test-error) expr)
            (chibi-test . rest))]
    [(_ (parameterize bindings . forms) . rest)
     (begin (gauche:parameterize bindings (chibi-test . forms))
            (chibi-test . rest))]
    [(_ form . rest)
     (let () form (chibi-test . rest))]
    [(_) (begin)]))
