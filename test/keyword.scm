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
(test-section "key-value-list")

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

(test-end)
