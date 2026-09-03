;; Test define-type binding
;; The point of this test is that precomp doesn't execute the toplevel forms,
;; so a type constructor expression can't see the value of the type defined
;; in the same file unless the compiler leaves a placeholder for it.

(define-module type-binding
  (export foo-myint foo-myclass foo-beginclass
          <myclass> <beginclass>
          <maybe-myint> <maybe-myclass> <maybe-beginclass>
          runtime-class-name))
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

;; A define-type whose value can't be computed at the compile time, and which
;; is also used as an ordinary runtime value.  While this file is precompiled,
;; the compiler only has a placeholder binding for <runtime-class>; the
;; placeholder's value is a stand-in, so it must not be used to constant-fold
;; the of-type? that assume-type expands into.
;; (If the file is loaded, the dummy binding of <runtime-class> is overwritten
;; when the toplevel form is executed).
(define-type <runtime-class> (car (list <myclass>)))

(define (runtime-class-name)
  (assume-type <runtime-class> <class>)
  (class-name <runtime-class>))
