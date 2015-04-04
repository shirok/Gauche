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

(test* "symbol=?" '(#t #t #f #f)
       (list (symbol=? 'a 'a)
             (symbol=? 'a 'a 'a)
             (symbol=? 'a 'b)
             (symbol=? 'a 'a 'b)))
(test* "symbol=?" (test-error) (symbol=? 'a 'a "a"))

(test* "symbol reader" 'foo (read-from-string "foo"))
(test* "symbol reader" 'foo (read-from-string "foo bar"))
(test* "symbol reader escaped" 'foo (read-from-string "|foo|"))
(test* "symbol reader escaped" 'foo (read-from-string "|foo|bar"))
(test* "symbol reader escaped" '|foo bar| (read-from-string "|foo bar|"))

;; NB: guard these with reader mode (legacy mode should read it differently)
(test* "symbol reader hex escaped" 'abc (read-from-string "|a\\x62;c|"))
(test* "symbol reader hex escaped" 'abc (read-from-string "|a\\x0062;c|"))

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

(test* "symbol-append" 'ab:c45 (symbol-append 'ab ':c 45))
(test* "symbol-append" 'quux (symbol-append #t 'qu "ux"))
(test* "symbol-append" #t
       (let1 x (symbol-append #f 'qu "ux")
         (and (not (symbol-interned? x))
              (equal? (symbol->string x) "quux"))))
(test* "symbol-append" '|| (symbol-append))
(test* "symbol-append" '|| (symbol-append #t))

;;----------------------------------------------------------------
(test-section "keywords")

(test* "reader" #t (keyword? (read-from-string ":abc")))
(test* "reader" #t (keyword? (read-from-string ":")))
(test* "reader" #t (keyword? (read-from-string "::")))
(test* "reader" #t (keyword? (read-from-string ":|abc|")))
(test* "reader" #t (keyword? (read-from-string ":|a b c|")))
(test* "keyword?" #f (keyword? 'abc))
(test* "make-keyword" #t (keyword? (make-keyword 'abc)))
(test* "keyword->string" "abc" (keyword->string (make-keyword 'abc)))
(test* "keyword->string" "a b" (keyword->string (read-from-string ":|a b|")))
(test* "writer" ":abc" (write-to-string (make-keyword "abc")))
(test* "writer"
       (if (symbol? :x) "|:a b c|" ":|a b c|");transient during symbol-keyword integration
       (write-to-string (make-keyword "a b c")))
(test* "writer" ":" (write-to-string (make-keyword "")))
(test* "writer" "::" (write-to-string (make-keyword ":")))
(test* "writer" ":3" (write-to-string (make-keyword "3")))
(test* "writer" ":-i" (write-to-string (make-keyword "-i")))
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
(test* "get-keyword" (test-error)
       (get-keyword :z *key-value-list*))
(test* "get-keyword" 88
       (get-keyword :z *key-value-list* 88))
(test* "get-keyword" (test-error)
       (get-keyword :z (cdr *key-value-list*)))

(test* "get-keyword*" "foo"
       (get-keyword* :b *key-value-list*))
(test* "get-keyword*" 33
       (get-keyword* :a *key-value-list*))
(test* "get-keyword*" 'ook
       (get-keyword* :d *key-value-list*))
(test* "get-keyword*" 'ook
       (get-keyword* :d *key-value-list* (error "oops")))
(test* "get-keyword*" (test-error)
       (get-keyword* :z *key-value-list*))
(test* "get-keyword*" 88
       (get-keyword* :z *key-value-list* 88))
(test* "get-keyword*" (test-error)
       (get-keyword* :z (cdr *key-value-list*)))

;;----------------------------------------------------------------
(test-section "delete-keyword")

(let ()
  (define (tester source to-delete expected source-result)
    (define (non-destructive qfn fn arg0)
      (test* (format "~a ~s ~s" qfn arg0 source) expected (fn arg0 source)))
    (define (destructive qfn fn arg0)
      (test* (format "~a ~s ~s" qfn arg0 source)
             (list expected source-result)
             (let* ([orig (list-copy source)]
                    [res  (fn arg0 orig)])
               (list res orig))))
       
    (cond [(list? to-delete)
           (non-destructive 'delete-keywords delete-keywords to-delete)
           (destructive     'delete-keywords! delete-keywords! to-delete)]
          [else
           (non-destructive 'delete-keyword delete-keyword to-delete)
           (destructive     'delete-keyword! delete-keyword! to-delete)
           (non-destructive 'delete-keywords delete-keywords (list to-delete))
           (destructive     'delete-keywords! delete-keywords! (list to-delete))
           ]))

  (tester '(:a 3 :b 5) :b '(:a 3)      '(:a 3))
  (tester '(:a 3 :b 5) :a '(:b 5)      '(:a 3 :b 5))
  (tester '(:a 3 :b 5) :c '(:a 3 :b 5) '(:a 3 :b 5))
  (tester '(:a 3)      :a '()          '(:a 3))
  (tester '()          :a '()          '())
  (tester '(:a 3 :a 5) :a '()          '(:a 3 :a 5))
  (tester '(:a 3 :b 4 :b 5) :b '(:a 3) '(:a 3))
  (tester '(:a 3 :b 4 :a 5) :a '(:b 4) '(:a 3 :b 4))
  (tester '(:a 3 :b 4 :a 5) :b '(:a 3 :a 5) '(:a 3 :a 5))

  (tester '(:a 3 :b 4) '(:a :b) '() '(:a 3 :b 4))
  (tester '(:a 3 :b 4) '(:a :c) '(:b 4) '(:a 3 :b 4))
  (tester '(:a 3 :b 4) '(:c :b) '(:a 3) '(:a 3))
  (tester '(:a 3 :b 4) '()      '(:a 3 :b 4) '(:a 3 :b 4))
  (tester '(:a 3 :b 4 :c 5) '(:a :c)  '(:b 4) '(:a 3 :b 4))
  (tester '(:a 3 :b 4 :c 5) '(:b :c)  '(:a 3) '(:a 3))
  (tester '(:a 3 :b 4 :c 5) '(:a :b)  '(:c 5) '(:a 3 :b 4 :c 5))
  )

(test-end)
