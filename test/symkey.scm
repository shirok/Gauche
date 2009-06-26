;;;
;;; testing symbols and keywords
;;;

(use gauche.test)
(test-start "symbols and keywords")

;;----------------------------------------------------------------
(test-section "symbols")

(test* "symbol->string" "foo" (symbol->string 'foo))
(test* "string->symbol" 'foo  (string->symbol "foo") eq?)

(test* "gensym" '(#t #t #f)
       (let1 s (gensym "ooo")
         (list (symbol? s)
               (string=? (substring (symbol->string s) 0 3) "ooo")
               (eq? s (string->symbol (symbol->string s))))))

(test* "interned?" #t (symbol-interned? (string->symbol "foofoo")))
(test* "interned?" #f (symbol-interned? (gensym "foofoo")))

(test* "symbol reader" 'foo (read-from-string "foo"))
(test* "symbol reader" 'foo (read-from-string "foo bar"))
(test* "symbol reader escaped" 'foo (read-from-string "|foo|"))
(test* "symbol reader escaped" 'foo (read-from-string "|foo|bar"))
(test* "symbol reader escaped" '|foo bar| (read-from-string "|foo bar|"))

(test* "symbol writer" 'foo (read-from-string (write-to-string 'foo)))
(test* "symbol writer" '|foo bar|
       (read-from-string (write-to-string (string->symbol "foo bar"))))

(test* "symbol reader uninterned" "foo"
       (let1 s (read-from-string "#:foo")
         (and (symbol? s) (not (eq? 'foo s)) (symbol->string s))))
(test* "symbol reader uninterned" "foo bar"
       (let1 s (read-from-string "#:|foo bar|")
         (and (symbol? s) (not (eq? '|foo bar| s)) (symbol->string s))))
(test* "symbol reader uninterned" #f
       (eq? (read-from-string "#:foo") (read-from-string "#:foo")))

(test* "symbol writer uninterned" "#:foo"
       (write-to-string (string->uninterned-symbol "foo")))
(test* "symbol writer uninterned" "#:|foo bar|"
       (write-to-string (string->uninterned-symbol "foo bar")))
(test* "symbol writer uninterned" "(#0=#:foo #0#)"
       (write-to-string (let1 s (string->uninterned-symbol "foo")
                          (list s s))
                        write/ss))
(test* "symbol writer uninterned" "(#:foo #:foo)"
       (write-to-string (list (string->uninterned-symbol "foo")
                              (string->uninterned-symbol "foo"))
                        write/ss))

(test* "prefix" 'bar (symbol-sans-prefix 'foo:bar 'foo:))
(test* "prefix" #f   (symbol-sans-prefix 'foo:bar 'bar:))


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
