
(define c #f)

;; An example in R5RS
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

;; Test for handler stack.  "***1" and "***3" should be displayed only once.
(define (dynwind-test2)
  (dynamic-wind
   (lambda () (display "*** 1\n"))
   dynwind-test1
   (lambda () (display "*** 3\n"))))

;; Test for continuation with the saved environment is altered.
;; should print 2^1 to 2^10
(define (callcc-test1)
  (let ((w (let ((v 1))
             (set! v (+ (call-with-current-continuation
                         (lambda (c0) (set! c c0) v))
                        v))
             (display v) (newline)
             v)))
    (if (< w 1024) (c w))))

             
