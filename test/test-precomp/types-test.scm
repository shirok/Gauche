;; Test file to check constructed types are serialized and deserialized
;; properly.

(define-module types-test
  (export <A> <B> foo))
(select-module types-test)

(define <A> (</> (<Tuple> (<?> <char>) <string> <integer>)
                 (<List> <integer> 3 10)))
(define <B> (</> (<Tuple> (<?> <char>) <string> <integer>)
                 (<List> <integer> 3 10)))

(define (foo x)
  (assume-type x <A>)
  x)

