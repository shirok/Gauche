;; Test file to check constructed types are serialized and deserialized
;; properly.

(define-module types-test
  (export <A> <B> foo))
(select-module types-test)

(define <A> (</> (<Tuple> (<?> <int8>) <string> <integer>)
                 (<List> <integer> 3 10)))
(define <B> (</> (<Tuple> (<?> <int8>) <string> <integer>)
                 (<List> <integer> 3 10)))
(define <C> (</> (<Tuple> (<?> <int8>) <string> <integer>)
                 (<List> <integer>)))

(define (foo x)
  (assume-type x <A>)
  x)

