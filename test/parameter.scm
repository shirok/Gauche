;;
;; test for parameter & fluids
;;

(use gauche.test)
(test-start "parameters")

;;-------------------------------------------------------------------
(test-section "parameter")
(use gauche.parameter)

(define a #f)
(define b #f)

(test "make-parameter" 3
      (lambda ()
        (set! a (make-parameter 3))
        (a)))
(test "make-parameter" 88
      (lambda ()
        (set! b (make-parameter 88 x->integer))
        (b)))
(test "parameter" "abc"
      (lambda () (a "abc") (a)))
(test "parameter" 34
      (lambda () (b "34") (b)))

(test "parameterize" '(("84" 0) (0 84) ("84" 0) ("abc" 34))
      (lambda ()
        (let* ((z (parameterize ((a "84")
                                 (b (a)))
                    (let* ((z1 (list (a) (b)))
                           (z2 (parameterize ((a (b))
                                              (b (a)))
                                 (list (a) (b))))
                           (z3 (list (a) (b))))
                      (list z1 z2 z3))))
               (zz (list (a) (b))))
          (append z (list zz)))))

(test "parameterize & dynamic-wind" '(("93" 112)
                                      (34 0)
                                      ("93" 112)
                                      (34 0))
      (lambda ()
        (let* ((z '())
               (k (parameterize ((a "93") (b 112))
                    (let ((k (call/cc identity)))
                      (push! z (list (a) (b)))
                      k)))
               )
          (parameterize ((a (b)) (b (a)))
            (push! z (list (a) (b)))
            (if k (k #f)))
          (reverse z))))

(test-end)
