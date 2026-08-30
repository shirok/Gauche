;; Test define-type binding
;; The point of this test is that precomp doesn't execute the toplevel forms,
;; so a type constructor expression can't see the value of the type defined
;; in the same file unless the compiler leaves a placeholder for it.

(define-module type-binding
  (export foo-myint foo-myclass foo-beginclass
          <myclass> <beginclass>
          <maybe-myint> <maybe-myclass> <maybe-beginclass>))
(select-module type-binding)

(define-type <myint_t> <int>)

(define-type <maybe-myint> (<?> <myint_t>))

(define (foo-myint x)
  (assume-type x <maybe-myint>)
  x)


(define-class <myclass> () ())

(define-type <maybe-myclass> (<?> <myclass>))

(define (foo-myclass x)
  (assume-type x <maybe-myclass>)
  x)

;; The class definition and its use are in the same toplevel form, so
;; the definition isn't executed before the use is compiled.
(begin
  (define-class <beginclass> () ())

  (define-type <maybe-beginclass> (<?> <beginclass>))

  (define (foo-beginclass x)
    (assume-type x (<?> <beginclass>))
    x))
