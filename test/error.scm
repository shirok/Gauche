;;
;; test error handlers
;;

;;  $Id: error.scm,v 1.6 2001-12-20 11:47:46 shirok Exp $

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
(test-section "cascading errors")

;; tests various interactions with with-error-handler and dynamic-wind
;; when an error is raised from error handler.

(test "cascading error" '(a b c e d)
      (lambda ()
        (let ((x '()))
          (with-error-handler
           (lambda (e) (push! x 'e))
           (lambda ()
             (dynamic-wind
              (lambda () (push! x 'a))
              (lambda ()
                (with-error-handler
                 (lambda (e) (push! x 'c) (car 9))
                 (lambda ()
                   (push! x 'b)
                   (car 3)
                   (push! x 'z))))
              (lambda () (push! x 'd)))))
          (reverse x))))

(test "cascading error 2" '(a b c d e f g)
      (lambda ()
        (let ((x '()))
          (dynamic-wind
           (lambda () (push! x 'a))
           (lambda ()
             (with-error-handler
              (lambda (e) (push! x e))
              (lambda ()
                (dynamic-wind
                 (lambda () (push! x 'b))
                 (lambda ()
                   (with-error-handler
                    (lambda (e) (push! x 'd) (raise 'e))
                    (lambda ()  (push! x 'c) (car 3) (push! x 'z))))
                 (lambda () (push! x 'f))))))
           (lambda () (push! x 'g)))
          (reverse x))))

(test "cascading error 3" '(a b c d f g)
      (lambda ()
        (let ((x '()))
          (dynamic-wind
           (lambda () (push! x 'a))
           (lambda ()
             (with-error-handler
              (lambda (e) (push! x e))
              (lambda ()
                (dynamic-wind
                 (lambda () (push! x 'b))
                 (lambda ()
                   (with-error-handler
                    (lambda (e) (push! x 'd))
                    (lambda ()  (push! x 'c) (car 3) (push! x 'z))))
                 (lambda () (push! x 'f))))))
           (lambda () (push! x 'g)))
          (reverse x))))

(test "cascading error 4" '(a b c d e f g h i j)
      (lambda ()
        (let ((x '()))
          (dynamic-wind
           (lambda () (push! x 'a))
           (lambda ()
             (with-error-handler
              (lambda (e) (push! x e))
              (lambda ()
                (dynamic-wind
                 (lambda () (push! x 'b))
                 (lambda ()
                   (with-error-handler
                    (lambda (e) (push! x e) (raise 'g))
                    (lambda ()
                      (dynamic-wind
                       (lambda () (push! x 'c))
                       (lambda ()
                         (with-error-handler
                          (lambda (e) (push! x 'e) (raise 'f))
                          (lambda () (push! x 'd) (car 3) (push! x 'z))))
                       (lambda () (push! x 'h))))))
                 (lambda () (push! x 'i))))))
           (lambda () (push! x 'j)))
          (reverse x))))

(test "cascading error 5" '(a b c d e f g)
      (lambda ()
        (let ((x '()))
          (dynamic-wind
           (lambda () (push! x 'a))
           (lambda ()
             (with-error-handler
              (lambda (e) (push! x e))
              (lambda ()
                (with-error-handler
                 (lambda (e) (push! x 'd) (raise 'e))
                 (lambda ()
                   (dynamic-wind
                    (lambda () (push! x 'b))
                    (lambda () (push! x 'c) (car 3) (push! x 'z))
                    (lambda () (push! x 'f))))))))
           (lambda () (push! x 'g)))
          (reverse x))))

(test "cascading error 6" '(a b c d e f g)
      (lambda ()
        (let ((x '()))
          (with-error-handler
           (lambda (e) (push! x e))
           (lambda () 
             (dynamic-wind
              (lambda () (push! x 'a))
              (lambda ()
                (dynamic-wind
                 (lambda () (push! x 'b))
                 (lambda ()
                   (with-error-handler
                    (lambda (e) (push! x 'd) (raise 'e))
                    (lambda ()  (push! x 'c) (open-input-file 3) (push! x 'z))))
                 (lambda () (push! x 'f))))
              (lambda () (push! x 'g)))))
          (reverse x))))

;;----------------------------------------------------------------
(test-section "error in before/after thunk")

(test "error in before thunk" '(a c)
      (lambda ()
        (let ((x '()))
          (with-error-handler
           (lambda (e) (push! x 'c))
           (lambda ()
             (dynamic-wind
              (lambda () (push! x 'a) (car 3) (push! x 'z))
              (lambda () (push! x 'b))
              (lambda () (push! x 'c)))))
          (reverse x))))

(test "error in after thunk" '(a b c d)
      (lambda ()
        (let ((x '()))
          (with-error-handler
           (lambda (e) (push! x 'd))
           (lambda ()
             (dynamic-wind
              (lambda () (push! x 'a))
              (lambda () (push! x 'b))
              (lambda () (push! x 'c) (car 3) (push! x 'z)))))
          (reverse x))))

(test "error in before thunk (nested)" '(a b c d)
      (lambda ()
        (let ((x '()))
          (dynamic-wind
           (lambda () (push! x 'a))
           (lambda ()
             (with-error-handler
              (lambda (e) (push! x 'c))
              (lambda ()
                (dynamic-wind
                 (lambda () (push! x 'b) (car 3) (push! x 'z))
                 (lambda () (push! x 'y))
                 (lambda () (push! x 'x))))))
           (lambda () (push! x 'd)))
          (reverse x))))

(test "error in after thunk (nested)" '(a b c d e f)
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
                 (lambda () (push! x 'c))
                 (lambda () (push! x 'd) (car 3) (push! x 'z))))))
           (lambda () (push! x 'f)))
          (reverse x))))

(test "error in before thunk (cascaded)" '(a b c d e)
      (lambda ()
        (let ((x '()))
          (with-error-handler
           (lambda (e) (push! x e))
           (lambda ()
             (dynamic-wind
              (lambda () (push! x 'a))
              (lambda ()
                (with-error-handler
                 (lambda (e) (push! x 'c) (raise 'd))
                 (lambda ()
                   (dynamic-wind
                    (lambda () (push! x 'b) (car 3) (push! x 'z))
                    (lambda () (push! x 'y))
                    (lambda () (push! x 'x))))))
              (lambda () (push! x 'e)))))
          (reverse x))))

(test "error in after thunk (cascaded)" '(a b c d e f g)
      (lambda ()
        (let ((x '()))
          (with-error-handler
           (lambda (e) (push! x e))
           (lambda ()
             (dynamic-wind
              (lambda () (push! x 'a))
              (lambda ()
                (with-error-handler
                 (lambda (e) (push! x 'e) (raise 'f))
                 (lambda ()
                   (dynamic-wind
                    (lambda () (push! x 'b))
                    (lambda () (push! x 'c))
                    (lambda () (push! x 'd) (car 3) (push! x 'z))))))
              (lambda () (push! x 'g)))))
          (reverse x))))

;;----------------------------------------------------------------
(test-section "restart and error handler")

(test "restart" '(a b x b x)
      (lambda ()
        (let ((x '())
              (c #f))
          (with-error-handler
           (lambda (e) (push! x 'x))
           (lambda ()
             (push! x 'a)
             (set! c (call/cc identity))
             (push! x 'b)
             (car 3)
             (push! x 'z)))
          (when c (c #f))
          (reverse x))))

(test "restart & dynamic-wind" '(a b c x e f z a b x e f z)
      (lambda ()
        (let ((x '())
              (c #f))
          (dynamic-wind
           (lambda () (push! x 'a))
           (lambda ()
             (with-error-handler
              (lambda (e) (push! x 'x))
              (lambda ()
                (dynamic-wind
                 (lambda () (push! x 'b))
                 (lambda ()
                   (push! x 'c)
                   (set! c (call/cc (lambda (k) k)))
                   (car 3)
                   (push! x 'd))
                 (lambda () (push! x 'e))))))
           (lambda () (push! x 'f)))
          (push! x 'z)
          (when c (c #f))
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
