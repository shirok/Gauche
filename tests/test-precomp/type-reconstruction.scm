;; Test file to check constructed types are serialized and deserialized
;; properly.

(define-module type-reconstruction
  (export <A> <B> foo))
(select-module type-reconstruction)

(define <A> (</> (<Tuple> (<?> <int8>) <string> <integer>)
                 (<List> <integer> 3 10)))
(define <B> (</> (<Tuple> (<?> <int8>) <string> <integer>)
                 (<List> <integer> 3 10)))
(define <C> (</> (<Tuple> (<?> <int8>) <string> <integer>)
                 (<List> <integer>)))

(define (foo x)
  (assume-type x <A>)
  x)
