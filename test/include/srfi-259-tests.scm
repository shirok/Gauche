(import (scheme base)
        (srfi 64)
        (srfi 259))

(test-begin "Tagged procedues with type safety")

(define-procedure-tag make-a-tagged a-tagged? a-tag)
(define (greet whom) (list 'hello whom))
(define greet-a (make-a-tagged 12 greet))

(test-eqv (a-tagged? greet-a) #t)
(test-eqv (a-tagged? greet) #f)
(test-eqv (a-tagged? 'a) #f)

(test-eqv (a-tag greet-a) 12)
(test-error (a-tag greet))
(test-error (a-tag 'a))

(test-equal (greet 'world) '(hello world))
(test-equal (greet-a 'world) '(hello world))

(define greet-a* (make-a-tagged 12 greet))
(test-eqv (eqv? greet-a greet-a*) #f)
(test-eqv (eq? greet-a greet-a*) #f)

(test-equal (greet-a* 'world) '(hello world))

(define-procedure-tag make-b-tagged b-tagged? b-tag)
(define greet-b (make-b-tagged 34 greet))
(test-eqv (a-tagged? greet-b) #f)
(test-eqv (b-tagged? greet-b) #t)
(test-eqv (b-tag greet-b) 34)

(define greet-ab (make-b-tagged 56 greet-a))
(test-eqv (a-tagged? greet-ab) #t)
(test-eqv (b-tagged? greet-ab) #t)
(test-eqv (a-tag greet-ab) 12)
(test-eqv (b-tag greet-ab) 56)

(define greet-ab* (make-a-tagged 1234 greet-ab))
(test-eqv (a-tagged? greet-ab*) #t)
(test-eqv (b-tagged? greet-ab*) #t)
(test-eqv (a-tag greet-ab*) 1234)
(test-eqv (b-tag greet-ab*) 56)
(test-eqv (a-tag greet-ab) 12)

(define (make-procedure-tag)
  (define-procedure-tag make is-a? ref)
  (values make is-a? ref))
(define-values (make-c-tagged c-tagged? c-tag) (make-procedure-tag))
(define-values (make-d-tagged d-tagged? d-tag) (make-procedure-tag))
(define greet-c (make-c-tagged 'alpha greet))
(define greet-d (make-d-tagged 'beta greet))
(test-eqv (c-tagged? greet-c) #t)
(test-eqv (d-tagged? greet-c) #f)
(test-eqv (c-tagged? greet-d) #f)
(test-eqv (d-tagged? greet-d) #t)
(test-eqv (c-tag greet-c) 'alpha)
(test-eqv (d-tag greet-d) 'beta)

(test-end "Tagged procedues with type safety")
