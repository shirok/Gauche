
(define c #f)

(define (dynwind-test1)
  (let ((path '()))
    (let ((add (lambda (s) (set! path (cons s path)))))
      (dynamic-wind
       (lambda () (add 'connect))
       (lambda ()
         (add (call-with-current-continuation
               (lambda (c0) (set! c c0) 'talk1))))
       (lambda () (add 'disconnect)))
      (if (< (length path) 4)
          (c 'talk2)
          (reverse path)))))

(define (dynwind-test2)
  (dynamic-wind
   (lambda () (display "*** 1\n"))
   dynwind-test1
   (lambda () (display "*** 3\n"))))

