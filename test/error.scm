;;
;; test error handlers
;;

;;  $Id: error.scm,v 1.4 2001-12-19 20:12:29 shirok Exp $

(use gauche.test)
(test-start "error and exception handlers")

;;----------------------------------------------------------------
(test-section "with-error-handler")

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

(test "with dynamic wind" '(a b e c d f)
      (lambda ()
        (let ((x '()))
          (dynamic-wind
           (lambda () (push! x 'a))
           (lambda ()
             (with-error-handler
              (lambda (e) (push! x 'e))
              (lambda ()
                (dynamic-wind
                 (lambda () (push! x 'b))
                 (lambda () (car 3))
                 (lambda () (push! x 'c)))))
             (push! x 'd))
           (lambda () (push! x 'f)))
          (reverse x))))

(test "repeat" 10
      (lambda ()
        (let loop ((i 0))
          (if (< i 10)
              (begin (with-error-handler
                      (lambda (e) i)
                      (lambda () (car i)))
                     (loop (+ i 1)))
              i))))

;;----------------------------------------------------------------
(test-section "raise")

(test "cascading error" '(a b c e d)
      (lambda ()
        (let ((x '()))
          (with-error-handler
           (lambda (e) (push! x e))
           (lambda ()
             (dynamic-wind
              (lambda () (push! x 'a))
              (lambda ()
                (with-error-handler
                 (lambda (e) (push! x 'c) (raise 'e))
                 (lambda ()
                   (push! x 'b)
                   (car 3)
                   (push! x 'z))))
              (lambda () (push! x 'd)))))
          (reverse x))))

;;----------------------------------------------------------------
(test-section "with-exception-handler")

(test "manual restart (simple)" '(a b c)
      (lambda ()
        (let ((x '()))
          (push! x
                 (call/cc
                  (lambda (cont)
                    (with-exception-handler
                     (lambda (e)
                       (push! x 'b)
                       (cont 'c))
                     (lambda () (push! x 'a) (car 3))))))
          (reverse x))))
              
(test "manual restart (w/ dynamic-wind)" '(a b c e d)
      (lambda ()
        (let ((x '()))
          (push! x
                 (call/cc
                  (lambda (cont)
                    (dynamic-wind
                     (lambda () (push! x 'a))
                     (lambda ()
                       (with-exception-handler
                        (lambda (e)
                          (push! x 'c)
                          (cont 'd))
                        (lambda () (push! x 'b) (car 3))))
                     (lambda () (push! x 'e))))))
          (reverse x))))

(test-end)
