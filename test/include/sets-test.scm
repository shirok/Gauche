(use test)
(use sets)
(use comparators)

(test-group "sets"
(define (big x) (> x 5))

(test-group "sets"
(test-group "sets/simple"
  (define nums (set number-comparator))
  ;; nums is now {}
  (define syms (set eq-comparator 'a 'b 'c 'd))
  ;; syms is now {a, b, c, d}
  (define nums2 (set-copy nums))
  ;; nums2 is now {}
  (define syms2 (set-copy syms))
  ;; syms2 is now {a, b, c, d}
  (define esyms (set eq-comparator))
  ;; esyms is now {}
  (test-assert (set-empty? esyms))
  (define total 0)
  (test-assert (set? nums))
  (test-assert (set? syms))
  (test-assert (set? nums2))
  (test-assert (set? syms2))
  (test-assert (not (set? 'a)))
  (set-adjoin! nums 2)
  (set-adjoin! nums 3)
  (set-adjoin! nums 4)
  (set-adjoin! nums 4)
  ;; nums is now {2, 3, 4}
  (test 4 (set-size (set-adjoin nums 5)))
  (test 3 (set-size nums))
  (test 3 (set-size (set-delete syms 'd)))
  (test 2 (set-size (set-delete-all syms '(c d))))
  (test 4 (set-size syms))
  (set-adjoin! syms 'e 'f)
  ;; syms is now {a, b, c, d, e, f}
  (test 4 (set-size (set-delete-all! syms '(e f))))
  ;; syms is now {a, b, c, d}
  (test 0 (set-size nums2))
  (test 4 (set-size syms2))
  (set-delete! nums 2)
  ;; nums is now {3, 4}
  (test 2 (set-size nums))
  (set-delete! nums 1)
  (test 2 (set-size nums))
  (set! nums2 (set-map (lambda (x) (* 10 x)) number-comparator nums))
  ;; nums2 is now {30, 40}
  (test-assert (set-contains? nums2 30))
  (test-assert (not (set-contains? nums2 3)))
  (set-for-each (lambda (x) (set! total (+ total x))) nums2)
  (test 70 total)
  (test 10 (set-fold + 3 nums))
  (set! nums (set eqv-comparator 10 20 30 40 50))
  ;; nums is now {10, 20, 30, 40, 50}
  (test-assert
    (set=? nums (set-unfold
       (lambda (i) (= i 0))
       (lambda (i) (* i 10))
       (lambda (i) (- i 1))
       5
       eqv-comparator)))
  (test '(a) (set->list (set eq-comparator 'a)))
  (set! syms2 (list->set eq-comparator '(e f)))
  ;; syms2 is now {e, f}
  (test 2 (set-size syms2))
  (test-assert (set-contains? syms2 'e))
  (test-assert (set-contains? syms2 'f))
  (list->set! syms2 '(a b))
  (test 4 (set-size syms2))
) ; end sets/simple

(test-group "sets/search"
  (define yam (set char-comparator #\y #\a #\m))
  (define (failure/insert insert ignore)
    (insert 1))
  (define (failure/ignore insert ignore)
    (ignore 2))
  (define (success/update element update remove)
    (update #\b 3))
  (define (success/remove element update remove)
    (remove 4))
  (define yam! (set char-comparator #\y #\a #\m #\!))
  (define bam (set char-comparator #\b #\a #\m))
  (define ym (set char-comparator #\y #\m))
  (define-values (set1 obj1)
    (set-search! (set-copy yam) #\! failure/insert error))
  (test-assert (set=? yam! set1))
  (test 1 obj1)
  (define-values (set2 obj2)
    (set-search! (set-copy yam) #\! failure/ignore error))
  (test-assert (set=? yam set2))
  (test 2 obj2)
  (define-values (set3 obj3)
    (set-search! (set-copy yam) #\y error success/update))
  (test-assert (set=? bam set3))
  (test 3 obj3)
  (define-values (set4 obj4)
    (set-search! (set-copy yam) #\a error success/remove))
  (test-assert (set=? ym set4))
  (test 4 obj4)
) ; end sets/search

(test-group "sets/subsets"
  (define set2 (set number-comparator 1 2))
  (define other-set2 (set number-comparator 1 2))
  (define set3 (set number-comparator 1 2 3))
  (define set4 (set number-comparator 1 2 3 4))
  (define setx (set number-comparator 10 20 30 40))
  (test-assert (set=? set2 other-set2))
  (test-assert (not (set=? set2 set3)))
  (test-assert (not (set=? set2 set3 other-set2)))
  (test-assert (set<? set2 set3 set4))
  (test-assert (not (set<? set2 other-set2)))
  (test-assert (set<=? set2 other-set2 set3))
  (test-assert (not (set<=? set2 set3 other-set2)))
  (test-assert (set>? set4 set3 set2))
  (test-assert (not (set>? set2 other-set2)))
  (test-assert (set>=? set3 other-set2 set2))
  (test-assert (not (set>=? other-set2 set3 set2)))
) ; end sets/subsets

(test-group "sets/ops"
  ;; Potentially mutable
  (define abcd (set eq-comparator 'a 'b 'c 'd))
  (define efgh (set eq-comparator 'e 'f 'g 'h))
  (define abgh (set eq-comparator 'a 'b 'g 'h))
  ;; Never get a chance to be mutated
  (define other-abcd (set eq-comparator 'a 'b 'c 'd))
  (define other-efgh (set eq-comparator 'e 'f 'g 'h))
  (define other-abgh (set eq-comparator 'a 'b 'g 'h))
  (define all (set eq-comparator 'a 'b 'c 'd 'e 'f 'g 'h))
  (define none (set eq-comparator))
  (define ab (set eq-comparator 'a 'b))
  (define cd (set eq-comparator 'c 'd))
  (define ef (set eq-comparator 'e 'f))
  (define gh (set eq-comparator 'g 'h))
  (define cdgh (set eq-comparator 'c 'd 'g 'h))
  (define abcdgh (set eq-comparator 'a 'b 'c 'd 'g 'h))
  (define abefgh (set eq-comparator 'a 'b 'e 'f 'g 'h))
  (test-assert (set-disjoint? abcd efgh))
  (test-assert (not (set-disjoint? abcd ab)))
  (parameterize ((current-test-comparator set=?))
    (test all (set-union abcd efgh))
    (test abcdgh (set-union abcd abgh))
    (test abefgh (set-union efgh abgh))
    (define efgh2 (set-copy efgh))
    (set-union! efgh2 abgh)
    (test abefgh efgh2)
    (test none (set-intersection abcd efgh))
    (define abcd2 (set-copy abcd))
    (set-intersection! abcd2 efgh)
    (test none abcd2)
    (test ab (set-intersection abcd abgh))
    (test ab (set-intersection abgh abcd))
    (test cd (set-difference abcd ab))
    (test abcd (set-difference abcd gh))
    (test none (set-difference abcd abcd))
    (define abcd3 (set-copy abcd))
    (set-difference! abcd3 abcd)
    (test none abcd3)
    (test cdgh (set-xor abcd abgh))
    (test all (set-xor abcd efgh))
    (test none (set-xor abcd other-abcd))
    (define abcd4 (set-copy abcd))
    ;; don't test xor! effect
    (test none (set-xor! abcd4 other-abcd))
    (test "abcd smashed?" other-abcd abcd)
    (test "efgh smashed?" other-efgh efgh)
    (test "abgh smashed?" other-abgh abgh))
) ; end sets/subsets

(test-group "sets/mismatch"
  (define nums (set number-comparator 1 2 3))
  (define syms (set eq-comparator 'a 'b 'c))
  (test-error (set=? nums syms))
  (test-error (set<? nums syms))
  (test-error (set<=? nums syms))
  (test-error (set>? nums syms))
  (test-error (set>=? nums syms))
  (test-error (set-union nums syms))
  (test-error (set-intersection nums syms))
  (test-error (set-difference nums syms))
  (test-error (set-xor nums syms))
  (test-error (set-union! nums syms))
  (test-error (set-intersection! nums syms))
  (test-error (set-difference! nums syms))
  (test-error (set-xor! nums syms))
) ; end sets/mismatch

(test-group "sets/whole"
  (define whole (set eqv-comparator 1 2 3 4 5 6 7 8 9 10))
  (define whole2 (set-copy whole))
  (define whole3 (set-copy whole))
  (define whole4 (set-copy whole))
  (define bottom (set eqv-comparator 1 2 3 4 5))
  (define top (set eqv-comparator 6 7 8 9 10))
  (define-values (topx bottomx)
    (set-partition big whole))
  (set-partition! big whole4)
  (parameterize ((current-test-comparator set=?))
    (test top (set-filter big whole))
    (test bottom (set-remove big whole))
    (set-filter! big whole2)
    (test-assert (not (set-contains? whole2 1)))
    (set-remove! big whole3)
    (test-assert (not (set-contains? whole3 10)))
    (test top topx)
    (test bottom bottomx)
    (test top whole4))
  (test 5 (set-count big whole))
  (define hetero (set eqv-comparator 1 2 'a 3 4))
  (define homo (set eqv-comparator 1 2 3 4 5))
  (test 'a (set-find symbol? hetero (lambda () (error "wrong"))))
  (test-error  (set-find symbol? homo (lambda () (error "wrong"))))
  (test-assert (set-any? symbol? hetero))
  (test-assert (set-any? number? hetero))
  (test-assert (not (set-every? symbol? hetero)))
  (test-assert (not (set-every? number? hetero)))
  (test-assert (not (set-any? symbol? homo)))
  (test-assert (set-every? number? homo))
) ; end sets/whole

(test-group "sets/lowlevel"
  (define bucket (set string-ci-comparator "abc" "def"))
  (test string-ci-comparator (set-element-comparator bucket))
  (test-assert (set-contains? bucket "abc"))
  (test-assert (set-contains? bucket "ABC"))
  (test "def" (set-member bucket "DEF" "fqz"))
  (test "fqz" (set-member bucket "lmn" "fqz"))
  (define nums (set number-comparator 1 2 3))
  ;; nums is now {1, 2, 3}
  (define nums2 (set-replace nums 2.0))
  ;; nums2 is now {1, 2.0, 3}
  (test-assert (set-any? inexact? nums2))
  (set-replace! nums 2.0)
  ;; nums is now {1, 2.0, 3}
  (test-assert (set-any? inexact? nums))
  (define sos
    (set set-comparator
      (set eqv-comparator 1 2)
      (set eqv-comparator 1 2)))
  (test 1 (set-size sos))
) ; end sets/lowlevel

) ; end sets

(test-group "bags"
(test-group "bags/simple"
  (define nums (bag number-comparator))
  ;; nums is now {}
  (define syms (bag eq-comparator 'a 'b 'c 'd))
  ;; syms is now {a, b, c, d}
  (define nums2 (bag-copy nums))
  ;; nums2 is now {}
  (define syms2 (bag-copy syms))
  ;; syms2 is now {a, b, c, d}
  (define esyms (bag eq-comparator))
  ;; esyms is now {}
  (test-assert (bag-empty? esyms))
  (define total 0)
  (test-assert (bag? nums))
  (test-assert (bag? syms))
  (test-assert (bag? nums2))
  (test-assert (bag? syms2))
  (test-assert (not (bag? 'a)))
  (bag-adjoin! nums 2)
  (bag-adjoin! nums 3)
  (bag-adjoin! nums 4)
  ;; nums is now {2, 3, 4}
  (test 4 (bag-size (bag-adjoin nums 5)))
  (test 3 (bag-size nums))
  (test 3 (bag-size (bag-delete syms 'd)))
  (test 2 (bag-size (bag-delete-all syms '(c d))))
  (test 4 (bag-size syms))
  (bag-adjoin! syms 'e 'f)
  ;; syms is now {a, b, c, d, e, f}
  (test 4 (bag-size (bag-delete-all! syms '(e f))))
  ;; syms is now {a, b, c, d}
  (test 3 (bag-size nums))
  (bag-delete! nums 1)
  (test 3 (bag-size nums))
  (set! nums2 (bag-map (lambda (x) (* 10 x)) number-comparator nums))
  ;; nums2 is now {20, 30, 40}
  (test-assert (bag-contains? nums2 30))
  (test-assert (not (bag-contains? nums2 3)))
  (bag-for-each (lambda (x) (set! total (+ total x))) nums2)
  (test 90 total)
  (test 12 (bag-fold + 3 nums))
  (set! nums (bag eqv-comparator 10 20 30 40 50))
  ;; nums is now {10, 20, 30, 40, 50}
  (test-assert
    (bag=? nums (bag-unfold
       (lambda (i) (= i 0))
       (lambda (i) (* i 10))
       (lambda (i) (- i 1))
       5
       eqv-comparator)))
  (test '(a) (bag->list (bag eq-comparator 'a)))
  (set! syms2 (list->bag eq-comparator '(e f)))
  ;; syms2 is now {e, f}
  (test 2 (bag-size syms2))
  (test-assert (bag-contains? syms2 'e))
  (test-assert (bag-contains? syms2 'f))
  (list->bag! syms2 '(e f))
  ;; syms2 is now {e, e, f, f}
  (test 4 (bag-size syms2))
) ; end bags/simple

(test-group "bags/search"
  (define yam (bag char-comparator #\y #\a #\m))
  (define (failure/insert insert ignore)
    (insert 1))
  (define (failure/ignore insert ignore)
    (ignore 2))
  (define (success/update element update remove)
    (update #\b 3))
  (define (success/remove element update remove)
    (remove 4))
  (define yam! (bag char-comparator #\y #\a #\m #\!))
  (define bam (bag char-comparator #\b #\a #\m))
  (define ym (bag char-comparator #\y #\m))
  (define-values (bag1 obj1)
    (bag-search! (bag-copy yam) #\! failure/insert error))
  (test-assert (bag=? yam! bag1))
  (test 1 obj1)
  (define-values (bag2 obj2)
    (bag-search! (bag-copy yam) #\! failure/ignore error))
  (test-assert (bag=? yam bag2))
  (test 2 obj2)
  (define-values (bag3 obj3)
    (bag-search! (bag-copy yam) #\y error success/update))
  (test-assert (bag=? bam bag3))
  (test 3 obj3)
  (define-values (bag4 obj4)
    (bag-search! (bag-copy yam) #\a error success/remove))
  (test-assert (bag=? ym bag4))
  (test 4 obj4)
) ; end bags/search

(test-group "bags/elemcount"
  (define mybag (bag eqv-comparator 1 1 1 1 1 2 2))
  (test 5 (bag-element-count mybag 1))
  (test 0 (bag-element-count mybag 3))
) ; end bags/elemcount

(test-group "bags/subbags"
  (define bag2 (bag number-comparator 1 2))
  (define other-bag2 (bag number-comparator 1 2))
  (define bag3 (bag number-comparator 1 2 3))
  (define bag4 (bag number-comparator 1 2 3 4))
  (define bagx (bag number-comparator 10 20 30 40))
  (test-assert (bag=? bag2 other-bag2))
  (test-assert (not (bag=? bag2 bag3)))
  (test-assert (not (bag=? bag2 bag3 other-bag2)))
  (test-assert (bag<? bag2 bag3 bag4))
  (test-assert (not (bag<? bag2 other-bag2)))
  (test-assert (bag<=? bag2 other-bag2 bag3))
  (test-assert (not (bag<=? bag2 bag3 other-bag2)))
  (test-assert (bag>? bag4 bag3 bag2))
  (test-assert (not (bag>? bag2 other-bag2)))
  (test-assert (bag>=? bag3 other-bag2 bag2))
  (test-assert (not (bag>=? other-bag2 bag3 bag2)))
) ; end bags/subbags

(test-group "bags/multi"
  (define one (bag eqv-comparator 10))
  (define two (bag eqv-comparator 10 10))
  (test-assert (not (bag=? one two)))
  (test-assert (bag<? one two))
  (test-assert (not (bag>? one two)))
  (test-assert (bag<=? one two))
  (test-assert (not (bag>? one two)))
  (test-assert (bag=? two two))
  (test-assert (not (bag<? two two)))
  (test-assert (not (bag>? two two)))
  (test-assert (bag<=? two two))
  (test-assert (bag>=? two two))
  (test '((10 . 2))
    (let ((result '()))
      (bag-for-each-unique
         (lambda (x y) (set! result (cons (cons x y) result)))
         two)
      result))
  (test 25 (bag-fold + 5 two))
  (test 12 (bag-fold-unique (lambda (k n r) (+ k n r)) 0 two))
) ; end bags/multi

(test-group "bags/ops"
  ;; Potentially mutable
  (define abcd (bag eq-comparator 'a 'b 'c 'd))
  (define efgh (bag eq-comparator 'e 'f 'g 'h))
  (define abgh (bag eq-comparator 'a 'b 'g 'h))
  ;; Never get a chance to be mutated
  (define other-abcd (bag eq-comparator 'a 'b 'c 'd))
  (define other-efgh (bag eq-comparator 'e 'f 'g 'h))
  (define other-abgh (bag eq-comparator 'a 'b 'g 'h))
  (define all (bag eq-comparator 'a 'b 'c 'd 'e 'f 'g 'h))
  (define none (bag eq-comparator))
  (define ab (bag eq-comparator 'a 'b))
  (define cd (bag eq-comparator 'c 'd))
  (define ef (bag eq-comparator 'e 'f))
  (define gh (bag eq-comparator 'g 'h))
  (define cdgh (bag eq-comparator 'c 'd 'g 'h))
  (define abcdgh (bag eq-comparator 'a 'b 'c 'd 'g 'h))
  (define abefgh (bag eq-comparator 'a 'b 'e 'f 'g 'h))
  (test-assert (bag-disjoint? abcd efgh))
  (test-assert (not (bag-disjoint? abcd ab)))
  (parameterize ((current-test-comparator bag=?))
    (test all (bag-union abcd efgh))
    (test abcdgh (bag-union abcd abgh))
    (test abefgh (bag-union efgh abgh))
    (define efgh2 (bag-copy efgh))
    (bag-union! efgh2 abgh)
    (test abefgh efgh2)
    (test none (bag-intersection abcd efgh))
    (define abcd2 (bag-copy abcd))
    (bag-intersection! abcd2 efgh)
    (test none abcd2)
    (test ab (bag-intersection abcd abgh))
    (test ab (bag-intersection abgh abcd))
    (test cd (bag-difference abcd ab))
    (test abcd (bag-difference abcd gh))
    (test none (bag-difference abcd abcd))
    (define abcd3 (bag-copy abcd))
    (bag-difference! abcd3 abcd)
    (test none abcd3)
    (test cdgh (bag-xor abcd abgh))
    (test all (bag-xor abcd efgh))
    (test none (bag-xor abcd other-abcd))
    (define abcd4 (bag-copy abcd))
    (test none (bag-xor! abcd4 other-abcd))
    (define abab (bag eq-comparator 'a 'b 'a 'b))
    (define ab2 (bag-copy ab))
    (test abab (bag-sum! ab2 ab))
    (test abab ab2)
    (test abab (bag-product 2 ab))
    (define ab3 (bag-copy ab))
    (bag-product! 2 ab3)
    (test abab ab3)
    (test "abcd smashed?" other-abcd abcd)
    (test "abcd smashed?" other-abcd abcd)
    (test "efgh smashed?" other-efgh efgh)
    (test "abgh smashed?" other-abgh abgh))
) ; end bags/ops

(test-group "bags/mismatch"
  (define nums (bag number-comparator 1 2 3))
  (define syms (bag eq-comparator 'a 'b 'c))
  (test-error (bag=? nums syms))
  (test-error (bag<? nums syms))
  (test-error (bag<=? nums syms))
  (test-error (bag>? nums syms))
  (test-error (bag>=? nums syms))
  (test-error (bag-union nums syms))
  (test-error (bag-intersection nums syms))
  (test-error (bag-difference nums syms))
  (test-error (bag-xor nums syms))
  (test-error (bag-union! nums syms))
  (test-error (bag-intersection! nums syms))
  (test-error (bag-difference! nums syms))
) ; end bags/mismatch

(test-group "bags/whole"
  (define whole (bag eqv-comparator 1 2 3 4 5 6 7 8 9 10))
  (define whole2 (bag-copy whole))
  (define whole3 (bag-copy whole))
  (define whole4 (bag-copy whole))
  (define bottom (bag eqv-comparator 1 2 3 4 5))
  (define top (bag eqv-comparator 6 7 8 9 10))
  (define-values (topx bottomx)
    (bag-partition big whole))
  (bag-partition! big whole4)
  (parameterize ((current-test-comparator bag=?))
    (test top (bag-filter big whole))
    (test bottom (bag-remove big whole))
    (bag-filter! big whole2)
    (test-assert (not (bag-contains? whole2 1)))
    (bag-remove! big whole3)
    (test-assert (not (bag-contains? whole3 10)))
    (test top topx)
    (test bottom bottomx)
    (test top whole4))
  (test 5 (bag-count big whole))
  (define hetero (bag eqv-comparator 1 2 'a 3 4))
  (define homo (bag eqv-comparator 1 2 3 4 5))
  (test 'a (bag-find symbol? hetero (lambda () (error "wrong"))))
  (test-error  (bag-find symbol? homo (lambda () (error "wrong"))))
  (test-assert (bag-any? symbol? hetero))
  (test-assert (bag-any? number? hetero))
  (test-assert (not (bag-every? symbol? hetero)))
  (test-assert (not (bag-every? number? hetero)))
  (test-assert (not (bag-any? symbol? homo)))
  (test-assert (bag-every? number? homo))
) ; end bags/whole

(test-group "bags/lowlevel"
  (define bucket (bag string-ci-comparator "abc" "def"))
  (test string-ci-comparator (bag-element-comparator bucket))
  (test-assert (bag-contains? bucket "abc"))
  (test-assert (bag-contains? bucket "ABC"))
  (test "def" (bag-member bucket "DEF" "fqz"))
  (test "fqz" (bag-member bucket "lmn" "fqz"))
  (define nums (bag number-comparator 1 2 3))
  ;; nums is now {1, 2, 3}
  (define nums2 (bag-replace nums 2.0))
  ;; nums2 is now {1, 2.0, 3}
  (test-assert (bag-any? inexact? nums2))
  (bag-replace! nums 2.0)
  ;; nums is now {1, 2.0, 3}
  (test-assert (bag-any? inexact? nums))
  (define bob
    (bag bag-comparator
      (bag eqv-comparator 1 2)
      (bag eqv-comparator 1 2)))
  (test 2 (bag-size bob))
) ; end bags/lowlevel


(test-group "bags/semantics"
  (define mybag (bag number-comparator 1 2))
  ;; mybag is {1, 2}
  (test 2 (bag-size mybag))
  (bag-adjoin! mybag 1)
  ;; mybag is {1, 1, 2}
  (test 3 (bag-size mybag))
  (test 2 (bag-unique-size mybag))
  (bag-delete! mybag 2)
  ;; mybag is {1, 1}
  (bag-delete! mybag 2)
  (test 2 (bag-size mybag))
  (bag-increment! mybag 1 3)
  ;; mybag is {1, 1, 1, 1, 1}
  (test 5 (bag-size mybag))
  (test-assert (bag-decrement! mybag 1 2))
  ;; mybag is {1, 1, 1}
  (test 3 (bag-size mybag))
  (bag-decrement! mybag 1 5)
  ;; mybag is {}
  (test 0 (bag-size mybag))
) ; end bags/semantics

(test-group "bags/convert"
  (define multi (bag eqv-comparator 1 2 2 3 3 3))
  (define single (bag eqv-comparator 1 2 3))
  (define singleset (set eqv-comparator 1 2 3))
  (define minibag (bag eqv-comparator 'a 'a))
  (define alist '((a . 2)))
  (test alist (bag->alist minibag))
  (test-assert (bag=? minibag (alist->bag eqv-comparator alist)))
  (test-assert (set=? singleset (bag->set single)))
  (test-assert (set=? singleset (bag->set multi)))
  (test-assert (bag=? single (set->bag singleset)))
  (test-assert (not (bag=? multi (set->bag singleset))))
  (set->bag! minibag singleset)
  ;; minibag is now {a, a, a, a, 1, 2, 3}
  (test-assert (bag-contains? minibag 1))
) ; end bags/convert

(test-group "bags/sumprod"
  (define abb (bag eq-comparator 'a 'b 'b))
  (define aab (bag eq-comparator 'a 'a 'b))
  (define total (bag-sum abb aab))
  (test 3 (bag-count (lambda (x) (eqv? x 'a)) total))
  (test 3 (bag-count (lambda (x) (eqv? x 'b)) total))
  (test 12 (bag-size (bag-product 2 total)))
  (define bag1 (bag eqv-comparator 1))
  (bag-sum! bag1 bag1)
  (test 2 (bag-size bag1))
  (bag-product! 2 bag1)
  (test 4 (bag-size bag1))
) ; end bag/sumprod

) ; end bags



(test-group "comparators"
  (define a (set number-comparator 1 2 3))
  (define b (set number-comparator 1 2 4))
  (define aa (bag number-comparator 1 2 3))
  (define bb (bag number-comparator 1 2 4))
  (test-assert (not (=? set-comparator a b)))
  (test-assert (=? set-comparator a (set-copy a)))
  (test-error (<? set-comparator a b))
  (test-assert (not (=? bag-comparator aa bb)))
  (test-assert (=? bag-comparator aa (bag-copy aa)))
  (test-error (<? bag-comparator aa bb))
  (test-assert (not (=? default-comparator a aa)))
) ; end comparators

) ; end r7rs-sets

(test-exit)
