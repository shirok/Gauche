;;
;; test error handlers
;;

(use gauche.test)
(test-start "error and exception handlers")

;; NB: this test is run just after the primitive syntax test,
;; and before tests of many standard features.  So we test
;; the minimal features here.  The full test for the exception
;; handling system is done in exception.scm.

;;----------------------------------------------------------------
(test-section "with-error-handler")

(prim-test "basic" '(1 . 2)
      (lambda ()
        (cons 1 (with-error-handler (lambda (e) 2)
                                    (lambda () (car 2))))))
(prim-test "basic" '(1 2 3)
      (lambda ()
        (list (with-error-handler (lambda (e) 1)
                                  (lambda () (car 2)))
              (with-error-handler (lambda (e) -1)
                                  (lambda () 2))
              (with-error-handler (lambda (e) 3)
                                  (lambda () (car 3))))))

(prim-test "with let" 1
      (lambda ()
        (let ((x 1))
          (with-error-handler (lambda (e) x)
                              (lambda () (car 0))))))

(prim-test "with let" 1
      (lambda ()
        (let ((x 1))
          (with-error-handler (lambda (e) x)
                              (lambda ()
                                (let ((x 2))
                                  (car x)))))))

(prim-test "cascade" 3
      (lambda ()
        (with-error-handler
         (lambda (e) 3)
         (lambda ()
           (with-error-handler
            (lambda (e) (car 0))
            (lambda ()
              (car 4)))))))

(prim-test "over c stack" '(1 . 2)
      (lambda ()
        (cons 1
              (with-error-handler
               (lambda (e) 2)
               (lambda ()
                 (sort '(1 8 3 7 4)
                       (lambda (a b) (car a))))))))

(prim-test "with dynamic wind" '(a b c)
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

(prim-test "with dynamic wind" '(a b e c d f)
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

(prim-test "repeat" 10
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

(prim-test "cascading error" '(a b c e d)
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

(prim-test "cascading error 2" '(a b c d e f g)
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
                 (lambda ()
                   (with-error-handler
                    (lambda (e) (push! x 'd) (raise e))
                    (lambda ()  (push! x 'c) (car 3) (push! x 'z))))
                 (lambda () (push! x 'f))))))
           (lambda () (push! x 'g)))
          (reverse x))))

(prim-test "cascading error 3" '(a b c d f g)
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

(prim-test "cascading error 4" '(a b c d e f g h i j)
      (lambda ()
        (let ((x '()))
          (dynamic-wind
           (lambda () (push! x 'a))
           (lambda ()
             (with-error-handler
              (lambda (e) (push! x 'g))
              (lambda ()
                (dynamic-wind
                 (lambda () (push! x 'b))
                 (lambda ()
                   (with-error-handler
                    (lambda (e) (push! x 'f) (raise e))
                    (lambda ()
                      (dynamic-wind
                       (lambda () (push! x 'c))
                       (lambda ()
                         (with-error-handler
                          (lambda (e) (push! x 'e) (raise e))
                          (lambda () (push! x 'd) (car 3) (push! x 'z))))
                       (lambda () (push! x 'h))))))
                 (lambda () (push! x 'i))))))
           (lambda () (push! x 'j)))
          (reverse x))))

(prim-test "cascading error 5" '(a b c d e f g)
      (lambda ()
        (let ((x '()))
          (dynamic-wind
           (lambda () (push! x 'a))
           (lambda ()
             (with-error-handler
              (lambda (e) (push! x 'e))
              (lambda ()
                (with-error-handler
                 (lambda (e) (push! x 'd) (raise e))
                 (lambda ()
                   (dynamic-wind
                    (lambda () (push! x 'b))
                    (lambda () (push! x 'c) (car 3) (push! x 'z))
                    (lambda () (push! x 'f))))))))
           (lambda () (push! x 'g)))
          (reverse x))))

(prim-test "cascading error 6" '(a b c d e f g)
      (lambda ()
        (let ((x '()))
          (with-error-handler
           (lambda (e) (push! x 'e))
           (lambda () 
             (dynamic-wind
              (lambda () (push! x 'a))
              (lambda ()
                (dynamic-wind
                 (lambda () (push! x 'b))
                 (lambda ()
                   (with-error-handler
                    (lambda (e) (push! x 'd) (raise e))
                    (lambda ()  (push! x 'c) (open-input-file 3) (push! x 'z))))
                 (lambda () (push! x 'f))))
              (lambda () (push! x 'g)))))
          (reverse x))))

;;----------------------------------------------------------------
(test-section "error in before/after thunk")

(prim-test "error in before thunk" '(a c)
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

(prim-test "error in after thunk" '(a b c d)
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

(prim-test "error in before thunk (nested)" '(a b c d)
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

(prim-test "error in after thunk (nested)" '(a b c d e f)
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

(prim-test "error in before thunk (cascaded)" '(a b c d e)
      (lambda ()
        (let ((x '()))
          (with-error-handler
           (lambda (e) (push! x 'd))
           (lambda ()
             (dynamic-wind
              (lambda () (push! x 'a))
              (lambda ()
                (with-error-handler
                 (lambda (e) (push! x 'c) (raise e))
                 (lambda ()
                   (dynamic-wind
                    (lambda () (push! x 'b) (car 3) (push! x 'z))
                    (lambda () (push! x 'y))
                    (lambda () (push! x 'x))))))
              (lambda () (push! x 'e)))))
          (reverse x))))

(prim-test "error in after thunk (cascaded)" '(a b c d e f g)
      (lambda ()
        (let ((x '()))
          (with-error-handler
           (lambda (e) (push! x 'f))
           (lambda ()
             (dynamic-wind
              (lambda () (push! x 'a))
              (lambda ()
                (with-error-handler
                 (lambda (e) (push! x 'e) (raise e))
                 (lambda ()
                   (dynamic-wind
                    (lambda () (push! x 'b))
                    (lambda () (push! x 'c))
                    (lambda () (push! x 'd) (car 3) (push! x 'z))))))
              (lambda () (push! x 'g)))))
          (reverse x))))

;;----------------------------------------------------------------
(test-section "restart and error handler")

(prim-test "restart" '(a b x b x)
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

(prim-test "restart & dynamic-wind" '(a b c x e f z a b x e f z)
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

(prim-test "simple" '(a b c)
      (lambda ()
        (let ((x '()))
          (with-exception-handler
           (lambda (e) (push! x e))
           (lambda ()
             (push! x 'a)
             (raise 'b)
             (push! x 'c)))
          (reverse x))))

(prim-test "w/dynamic-wind" '(a b c d e f g)
      (lambda ()
        (let ((x '()))
          (dynamic-wind
           (lambda () (push! x 'a))
           (lambda ()
             (with-exception-handler
              (lambda (e) (push! x e))
              (lambda ()
                (dynamic-wind
                 (lambda () (push! x 'b))
                 (lambda () (push! x 'c) (raise 'd) (push! x 'e))
                 (lambda () (push! x 'f))))))
           (lambda () (push! x 'g)))
          (reverse x))))

(prim-test "manual restart (simple)" '(a b c)
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

(prim-test "manual restart (w/ dynamic-wind)" '(a b c e d)
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

(prim-test "noncontinuable error" '(a b c g y)
      (lambda ()
        (let ((x '()))
          (with-error-handler
           (lambda (e) (push! x 'g))
           (lambda ()
             (with-exception-handler
              (lambda (e) (push! x 'c))
              (lambda ()
                (dynamic-wind
                 (lambda () (push! x 'a))
                 (lambda () (push! x 'b) (car 3) (push! x 'z))
                 (lambda () (push! x 'y)))))))
          (reverse x))))

;;----------------------------------------------------------------
(test-section "nesting exception/error handlers")

;(prim-test "propagating continuable exception 1" '(a b c)
(prim-test "nesting exception/error handlers 1" '(a z)
      (lambda ()
        (let ((x '()))
          (with-exception-handler
           (lambda (e) (push! x e))
           (lambda ()
             (with-error-handler
              (lambda (e) (push! x 'z))
              (lambda ()
                (push! x 'a)
                (raise 'b)
                (push! x 'c)))))
          (reverse x))))

;(prim-test "propagating continuable exception 2" '(a b c d e f g h)
(prim-test "nesting exception/error handlers 2" '(a b c f g h)
      (lambda ()
        (let ((x '()))
          (with-exception-handler
           (lambda (e) (push! x e))
           (lambda ()
             (dynamic-wind
              (lambda () (push! x 'a))
              (lambda ()
                (with-error-handler
                 (lambda (e) (push! x 'z))
                 (lambda ()
                   (dynamic-wind
                    (lambda () (push! x 'b))
                    (lambda ()
                      (with-error-handler
                       (lambda (e) (push! x 'f))
                       (lambda ()
                         (push! x 'c)
                         (raise 'd)
                         (push! x 'e)
                         (car 3)
                         (push! x 'z))))
                    (lambda () (push! x 'g))))))
              (lambda () (push! x 'h)))))
          (reverse x))))

;;----------------------------------------------------------------
(test-section "guard reraise")

(prim-test "guard reraise 1"
      "[d01][d02][d04][d01][w01]1[d03][d04]"
      (lambda ()
        (with-output-to-string
          (lambda()
            (with-exception-handler
             (lambda (e) (display "[w01]") (display e))
             (lambda ()
               (guard (exc)
                 (dynamic-wind
                  (lambda () (display "[d01]"))
                  (lambda ()
                    (display "[d02]")
                    (raise 1)
                    (display "[d03]"))
                  (lambda () (display "[d04]"))))))))))

(prim-test "guard reraise 2 (reraise x 2)"
      "[d01][d02][d05][d01][w01]1[d03][d05][d01][w01]2[d04][d05]"
      (lambda ()
        (with-output-to-string
          (lambda()
            (with-exception-handler
             (lambda (e) (display "[w01]") (display e))
             (lambda ()
               (guard (exc)
                 (dynamic-wind
                  (lambda () (display "[d01]"))
                  (lambda ()
                    (display "[d02]")
                    (raise 1)
                    (display "[d03]")
                    (raise 2)
                    (display "[d04]"))
                  (lambda () (display "[d05]"))))))))))

(prim-test "guard reraise 3 (dynamic-wind x 2)"
      "[d01][d02][d11][d12][d14][d04][d01][d11][w01]1[d13][d14][d03][d04]"
      (lambda ()
        (with-output-to-string
          (lambda()
            (with-exception-handler
             (lambda (e) (display "[w01]") (display e))
             (lambda ()
               (guard (exc)
                 (dynamic-wind
                  (lambda () (display "[d01]"))
                  (lambda ()
                    (display "[d02]")
                    (dynamic-wind
                     (lambda () (display "[d11]"))
                     (lambda ()
                       (display "[d12]")
                       (raise 1)
                       (display "[d13]"))
                     (lambda () (display "[d14]")))
                    (display "[d03]"))
                  (lambda () (display "[d04]"))))))))))

(prim-test "guard reraise 4 (guard x 2)"
      "[d01][d02][d11][d12][d14][d11][d14][d04][d01][d11][w01]1[d13][d14][d03][d04]"
      (lambda ()
        (with-output-to-string
          (lambda()
            (with-exception-handler
             (lambda (e) (display "[w01]") (display e))
             (lambda ()
               (guard (exc)
                 (dynamic-wind
                  (lambda () (display "[d01]"))
                  (lambda ()
                    (display "[d02]")
                    (guard (exc)
                      (dynamic-wind
                       (lambda () (display "[d11]"))
                       (lambda ()
                         (display "[d12]")
                         (raise 1)
                         (display "[d13]"))
                       (lambda () (display "[d14]"))))
                    (display "[d03]"))
                  (lambda () (display "[d04]"))))))))))

;;----------------------------------------------------------------
(test-section "interaction with empty environment frame")

(prim-test "empty do" 'ok
      (lambda ()
        (let ((x 0))
          (do () ((> x 0) 'ok)
            (with-error-handler
                (lambda (e) (inc! x))
              (lambda () (car x)))))))


(prim-test "empty let" 'ok
      (lambda ()
        (let ((x 0))
          (let loop ()
            (with-error-handler
                (lambda (e) (inc! x) (loop))
              (lambda ()
                (if (> x 2)
                    'ok
                    (car x))))))))

;;----------------------------------------------------------------
(test-section "error and errorf procedures")

(prim-test "error (<error>)" "Message 1 \"2\" (:a . #\\4)"
           (lambda ()
             (with-error-handler
                 (lambda (e)
                   (and (is-a? e <error>) (slot-ref e 'message)))
               (lambda ()
                 (error "Message" 1 "2" (cons :a #\4))))))

(prim-test "errorf (<error>)" "Message 1 and 2 or 3 and 4"
           (lambda ()
             (with-error-handler
                 (lambda (e)
                   (and (is-a? e <error>) (slot-ref e 'message)))
               (lambda ()
                 (errorf "Message ~a and ~a or ~a and ~a" 1 2 3 4)))))

(prim-test "error (<system-error>)" '("Wow: \"bang!\" 4" 111)
           (lambda ()
             (with-error-handler
                 (lambda (e)
                   (and (is-a? e <system-error>)
                        (list (slot-ref e 'message)
                              (slot-ref e 'errno))))
               (lambda ()
                 (error <system-error> :errno 111 "Wow:" "bang!" 4)))))

(prim-test "errorf (<system-error>)" '("Wow: \"bang!\" 4" 111)
           (lambda ()
             (with-error-handler
                 (lambda (e)
                   (and (is-a? e <system-error>)
                        (list (slot-ref e 'message)
                              (slot-ref e 'errno))))
               (lambda ()
                 (errorf <system-error> :errno 111
                         "Wow: ~s ~s" "bang!" 4)))))

(prim-test "error (base case)" #t
           (lambda ()
             (with-error-handler
                 (lambda (e)
                   (and (is-a? e <error>)
                        (eq? (slot-ref e 'message) #f)))
               (lambda ()
                 (error <error>)))))

(prim-test "errorf (base case)" #t
           (lambda ()
             (with-error-handler
                 (lambda (e)
                   (and (is-a? e <error>)
                        (eq? (slot-ref e 'message) #f)))
               (lambda ()
                 (errorf <error>)))))

(prim-test "error (explicit message)" "msg"
           (lambda ()
             (with-error-handler
                 (lambda (e)
                   (and (is-a? e <error>) (slot-ref e 'message)))
               (lambda ()
                 (error <error> :message "msg")))))

(prim-test "errorf (explicit message)" "msg~s"
           (lambda ()
             (with-error-handler
                 (lambda (e)
                   (and (is-a? e <error>) (slot-ref e 'message)))
               (lambda ()
                 (errorf <error> :message "msg~s")))))

;;----------------------------------------------------------------
(test-section "stack overflow inside handlers")

(define (stack-buster k)
  (if (zero? k) 1 (+ (stack-buster (- k 1)) 1)))

(prim-test "stack overflow in error handler" '(4 . ok)
           (lambda ()
             (cons
              4
              (with-error-handler
                  (lambda (e) (stack-buster 100000) 'ok)
                (lambda () (error "foo"))))))

(prim-test "stack overflow in error handler (nested)" '(4 . ok)
           (lambda ()
             (cons
              4
              (with-error-handler
                  (lambda (e)
                    (with-error-handler
                        (lambda (e) 'ok)
                      (lambda () (stack-buster 100000) (error "pop"))))
                (lambda () (error "foo"))))))

(prim-test "stack overflow in error handler (cascading error)" '(4 . ok)
           (lambda ()
             (cons
              4
              (with-error-handler
                  (lambda (e) 'ok)
                (lambda ()
                  (with-error-handler
                      (lambda (e)
                        (with-error-handler
                            (lambda (e) (error "bar"))
                          (lambda () (stack-buster 100000) (error "pop"))))
                    (lambda () (error "foo"))))))))

(test-end)
