(define a-cont #f)

(define (a)
  (dynamic-wind
   (lambda () (display "A0\n"))
   (lambda ()
     (call/cc (lambda (c) (set! a-cont c)))
     (display "A1\n")
     (b))
   (lambda () (display "A2\n"))))

(define b-cont #f)
(define (b)
  (dynamic-wind
   (lambda () (display "B0\n"))
   (lambda ()
     (call/cc (lambda (c) (set! b-cont c)))
     (display "B1\n")
     (c))
   (lambda () (display "B2\n"))))

(define c-cont #f)
(define (c)
  (dynamic-wind
   (lambda () (display "C0\n"))
   (lambda ()
     (call/cc (lambda (c) (set! c-cont c)))
     (display "C1\n"))
   (lambda () (display "C2\n"))))

