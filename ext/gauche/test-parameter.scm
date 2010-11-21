;;
;; test for parameter & fluids
;;

(use gauche.test)
(test-start "parameters")

(use gauche.parameter)
(test-module 'gauche.parameter)

;;-------------------------------------------------------------------
(test-section "basics")

(define a #f)
(define b #f)

(test* "make-parameter" 3
       (begin
         (set! a (make-parameter 3))
         (a)))
(test* "make-parameter" 88
       (begin
         (set! b (make-parameter 88 x->integer))
         (b)))
(test* "parameter" "abc"
       (begin (a "abc") (a)))
(test* "parameter" 34
       (begin (b "34") (b)))

(test* "parameterize" '(("84" 0) (0 84) ("84" 0) ("abc" 34))
       (let* ((z (parameterize ((a "84")
                                (b (a)))
                   (let* ((z1 (list (a) (b)))
                          (z2 (parameterize ((a (b))
                                             (b (a)))
                                (list (a) (b))))
                          (z3 (list (a) (b))))
                     (list z1 z2 z3))))
              (zz (list (a) (b))))
         (append z (list zz))))

(test* "parameterize & dynamic-wind" '(("93" 112)
                                       (34 0)
                                       ("93" 112)
                                       (34 0))
       (let* ((z '())
              (k (parameterize ((a "93") (b 112))
                   (let ((k (call/cc identity)))
                     (push! z (list (a) (b)))
                     k)))
              )
         (parameterize ((a (b)) (b (a)))
           (push! z (list (a) (b)))
           (if k (k #f)))
         (reverse z)))

(test* "generalized set! for parameters" "foo"
       (begin (set! (a) "foo") (a)))
(test* "generalized set! for parameters" 99
       (begin (set! (b) "99") (b)))

;;-------------------------------------------------------------------
(test-section "observers")

(test* "observers" '((pre1 3 4 3) (pre2 3 4 3) (post2 3 4 4) (post1 3 4 4))
       (let ((p (make-parameter 3))
             (r '()))
         (parameter-observer-add! p (lambda (o v)
                                      (push! r `(pre1 ,o ,v ,(p))))
                                  'before)
         (parameter-observer-add! p (lambda (o v)
                                      (push! r `(post1 ,o ,v ,(p))))
                                  'after)
         (parameter-observer-add! p (lambda (o v)
                                      (push! r `(pre2 ,o ,v ,(p))))
                                  'before 'append)
         (parameter-observer-add! p (lambda (o v)
                                      (push! r `(post2 ,o ,v ,(p))))
                                  'after 'prepend)
         (p 4)
         (reverse r)))

(test* "observers" '((pre1 4 3) (post1 4 4))
       (let ((p (make-parameter 3))
             (r '())
             (pre2 (lambda (o v) (push! r `(pre2 ,v ,(p)))))
             (post2  (lambda (o v) (push! r `(post2 ,v ,(p)))))
             )
         (parameter-observer-add! p (lambda (o v)
                                      (push! r `(pre1 ,v ,(p))))
                                  'before)
         (parameter-observer-add! p pre2 'before 'append)
         (parameter-observer-add! p (lambda (o v)
                                      (push! r `(post1 ,v ,(p))))
                                  'after)
         (parameter-observer-add! p post2 'after 'prepend)
         (parameter-observer-delete! p pre2)
         (parameter-observer-delete! p post2 'after)
         (p 4)
         (reverse r)))


(test-end)
