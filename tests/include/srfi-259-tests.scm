(import (scheme base)
        (scheme case-lambda)
        (srfi 64)
        (srfi 259))

(test-begin "Tagged procedues with type safety")

;; Trivial example from the SRFI

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

;; YASOS implementation

(define-procedure-tag make-object object? object-vtable)

(define-syntax object
  (syntax-rules ()
    ((_ proc-expr ((operation . formals) body_0 body_1 ...) ...)
     (letrec*
         ((proc
           (cond (proc-expr)
                 (else (lambda ignored
                         (error "object not callable" proc)))))
          (obj
           (make-object
            (lambda (op)
              (cond ((eqv? op operation)
                     (lambda formals body_0 body_1 ...)) ...
                    (else #f)))
            proc)))
       obj))))

(define-syntax operation
  (syntax-rules ()
    ((_ default-expr ((operation . formals) body_0 body_1 ...) ...)
     (letrec
         ((default default-expr)
          (op
           (object
            (case-lambda
              (()
               (if default
                 (default)
                 (error "operation not defined")))
              ((obj . args)
               (cond ((and (object? obj)
                           ((object-vtable obj) op))
                      => (lambda (method)
                           (apply method obj args)))
                     (default (apply default obj args))
                     (else (error "operation not defined")))))
            ((operation? self) #t)
            ((operation . formals) body_0 body_1 ...) ...)))
       op))))

(define operation? (operation (lambda obj #f)))

;;

(define get-x (operation #f))
(define get-y (operation #f))
(define move! (operation #f))
(define point? (operation (lambda _ #f)))
(define triangle? (operation (lambda _ #f)))
(define area (operation #f))

(define (make-point x y)
  (object (lambda () `(point (,x ,y)))
          ((point? self) #t)
          ((get-x self) x)
          ((get-y self) y)
          ((area self) 0)
          ((move! self dx dy)
           (set! x (+ x dx))
           (set! y (+ y dy))
           self)))

(define point1 (make-point 0 0))
(define point2 (make-point 1 2))

(test-assert (object? point1))
(test-assert (point? point1))
(test-eqv #f (point? 'a))

(test-equal '(point (0 0)) (point1))
(test-equal `(point (,(get-x point2) ,(get-y point2))) (point2))

(test-assert (point? (move! point2 0.5 -0.25)))
(test-equal '(point (1.5 1.75)) (point2))
(test-equal 0 (area point1))

(define (make-triangle p1 p2 p3)
  (object (lambda () `(triangle (,(get-x p1) ,(get-y p1))
                                (,(get-x p2) ,(get-y p2))
                                (,(get-x p3) ,(get-y p3))))
          ((triangle? self) #t)
          ((area self)
           (* 1/2 (abs (+ (* (get-x p1) (- (get-y p2) (get-y p3)))
                          (* (get-x p2) (- (get-y p3) (get-y p1)))
                          (* (get-x p3) (- (get-y p1) (get-y p2)))))))
          ((move! self dx dy)
           (move! p1 dx dy)
           (move! p2 dx dy)
           (move! p3 dx dy)
           self)))

(define point3 (make-point 2 0))
(define point4 (make-point 2 2))
(define tri (make-triangle point1 point3 point4))

(test-assert (triangle? tri))
(test-assert (not (point? tri)))
(test-assert (not (triangle? point1)))
(test-equal '(triangle (0 0) (2 0) (2 2)) (tri))
(test-equal 2 (area tri))
(move! tri -2 0)
(test-equal '(triangle (-2 0) (0 0) (0 2)) (tri))

(test-end "Tagged procedues with type safety")
