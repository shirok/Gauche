;;
;; test error handlers
;;

;;  $Id: error.scm,v 1.3 2001-09-26 10:58:44 shirok Exp $

(use gauche.test)
(test-start "error handlers")

(test "basic" '(1 . 2)
      (lambda ()
        (cons 1 (with-error-handler (lambda (e) 2)
                                    (lambda () (car 2))))))
(test "basic" '(1 2 3)
      (lambda ()
        (list (with-error-handler (lambda (e) 1)
                                  (lambda () (car 2)))
              (with-error-handler (lambda (e) -1)
                                  (lambda () 2))
              (with-error-handler (lambda (e) 3)
                                  (lambda () (car 3))))))

(test "with let" 1
      (lambda ()
        (let ((x 1))
          (with-error-handler (lambda (e) x)
                              (lambda () (car 0))))))

(test "with let" 1
      (lambda ()
        (let ((x 1))
          (with-error-handler (lambda (e) x)
                              (lambda ()
                                (let ((x 2))
                                  (car x)))))))

(test "cascade" 3
      (lambda ()
        (with-error-handler
         (lambda (e) 3)
         (lambda ()
           (with-error-handler
            (lambda (e) (car 0))
            (lambda ()
              (car 4)))))))

(test "over c stack" '(1 . 2)
      (lambda ()
        (cons 1
              (with-error-handler
               (lambda (e) 2)
               (lambda ()
                 (sort '(1 8 3 7 4)
                       (lambda (a b) (car a))))))))

(test "with dynamic wind" '(a b c)
      (lambda ()
        (let ((x '()))
          (with-error-handler
           (lambda (e) (set! x (cons 'b x)))
           (lambda ()
             (dynamic-wind
              (lambda () (set! x (cons 'c x)))
              (lambda () (car 3))
              (lambda () (set! x (cons 'a x))))))
          x)))

(test "repeat" 10
      (lambda ()
        (let loop ((i 0))
          (if (< i 10)
              (begin (with-error-handler
                      (lambda (e) i)
                      (lambda () (car i)))
                     (loop (+ i 1)))
              i))))
              
(test-end)
