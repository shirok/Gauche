;;;
;;; keyword test
;;;

(use gauche.test)
(test-start "keywords")

;;----------------------------------------------------------------
(test-section "keywords")

(test* "reader" #t (keyword? :abc))
(test* "keyword?" #f (keyword? 'abc))
(test* "make-keyword" #t (keyword? (make-keyword 'abc)))
(test* "keyword->string" "abc" (keyword->string (make-keyword 'abc)))
(test* "eq?" #t (eq? :a :a))
(test* "eq?" #f (eq? :a 'a))
(test* "eqv?" #t (eqv? :a :a))
(test* "eqv?" #f (eqv? :a 'a))

;;----------------------------------------------------------------
(test-section "get-keyword")

(define *key-value-list* '(:a 33 :b "foo" :c :d :d ook :a 99))

(test* "get-keyword" "foo"
       (get-keyword :b *key-value-list*))
(test* "get-keyword" 33
       (get-keyword :a *key-value-list*))
(test* "get-keyword" 'ook
       (get-keyword :d *key-value-list*))
(test* "get-keyword" *test-error*
       (get-keyword :z *key-value-list*))
(test* "get-keyword" 88
       (get-keyword :z *key-value-list* 88))
(test* "get-keyword" *test-error*
       (get-keyword :z (cdr *key-value-list*)))

(test* "get-keyword*" "foo"
       (get-keyword* :b *key-value-list*))
(test* "get-keyword*" 33
       (get-keyword* :a *key-value-list*))
(test* "get-keyword*" 'ook
       (get-keyword* :d *key-value-list*))
(test* "get-keyword*" 'ook
       (get-keyword* :d *key-value-list* (error "oops")))
(test* "get-keyword*" *test-error*
       (get-keyword* :z *key-value-list*))
(test* "get-keyword*" 88
       (get-keyword* :z *key-value-list* 88))
(test* "get-keyword*" *test-error*
       (get-keyword* :z (cdr *key-value-list*)))

;;----------------------------------------------------------------
(test-section "delete-keyword")

(test* "delete-keyword" '((:a 3 :b 5) (:a 3))
       (let* ((x (list :a 3 :b 5))
              (y (delete-keyword :b x)))
         (list x y)))
(test* "delete-keyword" '((:a 3 :b 5) (:b 5))
       (let* ((x (list :a 3 :b 5))
              (y (delete-keyword :a x)))
         (list x y)))
(test* "delete-keyword" '((:a 3 :b 5) (:a 3 :b 5))
       (let* ((x (list :a 3 :b 5))
              (y (delete-keyword :c x)))
         (list x y)))
(test* "delete-keyword" '((:a 3) ())
       (let* ((x (list :a 3))
              (y (delete-keyword :a x)))
         (list x y)))
(test* "delete-keyword" '(() ())
       (let* ((x ())
              (y (delete-keyword :a x)))
         (list x y)))

(test* "delete-keyword" '((:a 3 :a 5) ())
       (let* ((x (list :a 3 :a 5))
              (y (delete-keyword :a x)))
         (list x y)))
(test* "delete-keyword" '((:a 3 :b 4 :b 5) (:a 3))
       (let* ((x (list :a 3 :b 4 :b 5))
              (y (delete-keyword :b x)))
         (list x y)))
(test* "delete-keyword" '((:a 3 :b 4 :a 5) (:b 4))
       (let* ((x (list :a 3 :b 4 :a 5))
              (y (delete-keyword :a x)))
         (list x y)))

(test* "delete-keyword!" '((:a 3) (:a 3))
       (let* ((x (list :a 3 :b 5))
              (y (delete-keyword! :b x)))
         (list x y)))
(test* "delete-keyword!" '((:a 3 :b 5) (:b 5))
       (let* ((x (list :a 3 :b 5))
              (y (delete-keyword! :a x)))
         (list x y)))
(test* "delete-keyword!" '((:a 3 :b 5) (:a 3 :b 5))
       (let* ((x (list :a 3 :b 5))
              (y (delete-keyword! :c x)))
         (list x y)))
(test* "delete-keyword!" '((:a 3) ())
       (let* ((x (list :a 3))
              (y (delete-keyword! :a x)))
         (list x y)))
(test* "delete-keyword!" '(() ())
       (let* ((x ())
              (y (delete-keyword! :a x)))
         (list x y)))

(test* "delete-keyword!" '((:a 3 :a 5) ())
       (let* ((x (list :a 3 :a 5))
              (y (delete-keyword! :a x)))
         (list x y)))
(test* "delete-keyword!" '((:a 3) (:a 3))
       (let* ((x (list :a 3 :b 4 :b 5))
              (y (delete-keyword! :b x)))
         (list x y)))
(test* "delete-keyword!" '((:a 3 :b 4) (:b 4))
       (let* ((x (list :a 3 :b 4 :a 5))
              (y (delete-keyword! :a x)))
         (list x y)))

(test-end)
