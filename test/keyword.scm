;;;
;;; keyword test
;;;

(use gauche.test)
(test-start "keywords")

;;----------------------------------------------------------------
(test-section "keywords")

(test "reader" #t (lambda () (keyword? :abc)))
(test "keyword?" #f (lambda () (keyword? 'abc)))
(test "make-keyword" #t (lambda () (keyword? (make-keyword 'abc))))
(test "keyword->string" "abc"
      (lambda () (keyword->string (make-keyword 'abc))))
(test "eq?" #t (lambda () (eq? :a :a)))
(test "eq?" #f (lambda () (eq? :a 'a)))
(test "eqv?" #t (lambda () (eqv? :a :a)))
(test "eqv?" #f (lambda () (eqv? :a 'a)))

;;----------------------------------------------------------------
(test-section "key-value-list")

(define *key-value-list* '(:a 33 :b "foo" :c :d :d ook :a 99))

(test "get-keyword" "foo"
      (lambda () (get-keyword :b *key-value-list*)))
(test "get-keyword" 33
      (lambda () (get-keyword :a *key-value-list*)))
(test "get-keyword" 'ook
      (lambda () (get-keyword :d *key-value-list*)))
(test "get-keyword" *test-error*
      (lambda () (get-keyword :z *key-value-list*)))
(test "get-keyword" 88
      (lambda ()
        (with-error-handler
            (lambda (e) 'error)
          (lambda ()
            (get-keyword :z *key-value-list* 88)))))
(test "get-keyword" *test-error*
      (lambda ()
        (get-keyword :z (cdr *key-value-list*))))

(test "get-keyword*" "foo"
      (lambda () (get-keyword* :b *key-value-list*)))
(test "get-keyword*" 33
      (lambda () (get-keyword* :a *key-value-list*)))
(test "get-keyword*" 'ook
      (lambda () (get-keyword* :d *key-value-list*)))
(test "get-keyword*" 'ook
      (lambda () (get-keyword* :d *key-value-list* (error "oops"))))
(test "get-keyword*" *test-error*
      (lambda ()
        (get-keyword* :z *key-value-list*)))
(test "get-keyword*" 88
      (lambda ()
        (with-error-handler
            (lambda (e) 'error)
          (lambda ()
            (get-keyword* :z *key-value-list* 88)))))
(test "get-keyword*" *test-error*
      (lambda ()
        (get-keyword* :z (cdr *key-value-list*))))

(test-end)
