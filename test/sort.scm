;;;
;;; Testing built-in sort functions
;;;

(use gauche.test)
(test-start "sort procedures")

(test-section "loading and binding")

(test* "autoload" #t (procedure? sort))  ; this triggers sortutil
(test-module 'gauche.sortutil)

(use gauche.generic-sortutil)
(test-module 'gauche.generic-sortutil)

(test-section "sort")

(test* "sorted?" #t (sorted? '(1 2 3 4 4 5)))
(test* "sorted?" #f (sorted? '(1 2 3 4 3 5)))
(test* "sorted? (less)" #t (sorted? '(5 4 3 3 2 1) >))
(test* "sorted? (less)" #f (sorted? '(5 4 3 1 2 1) >))
(test* "sorted? (cmpr)" #t (sorted? '(1 2 3 4 4 5) integer-comparator))
(test* "sorted? (cmpr)" #f (sorted? '(1 2 3 5 4 2) integer-comparator))
(test* "sorted? (key)" #t (sorted? '(1 3 1 2 4 2) boolean-comparator even?))
(test* "sorted? (key)" #f (sorted? '(1 3 1 2 1 3) boolean-comparator even?))

(test* "sort (base)" '() (sort '()))
(test* "sort (base)" '#() (sort '#()))
(test* "sort (base)" '"" (sort '""))

(define (sort-test name fn fn! xargs in exp)
  (define (test1 kind fn destructive? gensrc copy genexp)
    (test* (format "~a (~a) ~a" name kind (if destructive? "!" ""))
           exp
           (let* ((src  (gensrc in))
                  (src2 (copy src))
                  (res  (apply fn src2 xargs)))
             (and (or destructive?
                      (equal? src src2))
                  (genexp res)))))
  (define (test2 fn destructive?)
    (test1 "list"   fn destructive? values list-copy values)
    (test1 "vector" fn destructive? list->vector vector-copy vector->list)
    (when (every char? in)
      (test1 "string" fn destructive? list->string string-copy string->list)))

  (test2 fn  #f)
  (test2 fn! #t)
  )

(define (sort-test2 name fn fn! stname stfn stfn! xargs in exp)
  (sort-test name fn fn! xargs in exp)
  (sort-test stname stfn stfn! xargs in exp))

(define (sort-nocmp . in&exps)
  (for-each (lambda (in&exp)
              (sort-test2 "sort - nocmp" sort sort!
                          "stable-sort - nocmp" stable-sort stable-sort!
                          '()
                          (car in&exp) (cadr in&exp)))
            in&exps))

(sort-nocmp
 '((3 4 8 2 0 1 5 9 7 6)  (0 1 2 3 4 5 6 7 8 9))
 '((0 1 2 3 4 5 6 7 8 9)  (0 1 2 3 4 5 6 7 8 9))
 '((1/2 -3/4 0.1)         (-3/4 0.1 1/2))
 '((0)                    (0))
 '((#\a #\l #\o #\h #\a)  (#\a #\a #\h #\l #\o))
 '(("tic" "tac" "toe")    ("tac" "tic" "toe")))

(define (sort-cmp cmpfn . in&exps)
  (for-each (lambda (in&exp)
              (sort-test2 "sort - cmp" sort sort!
                          "stable-sort - cmp" stable-sort stable-sort!
                          (list cmpfn)
                          (car in&exp) (cadr in&exp)))
            in&exps))

(sort-cmp
 (lambda (a b) (> (abs a) (abs b)))
 '((3 -4 8 -2 0 -1 5 -9 7 -6) (-9 8 7 -6 5 -4 3 -2 -1 0))
 '((-9 -8 -7 -6 -5 -4 -3 -2 -1 0) (-9 -8 -7 -6 -5 -4 -3 -2 -1 0))
 '((0 1 2 3 4 5 6 7 8 9) (9 8 7 6 5 4 3 2 1 0))
 '(() ())
 '((0) (0))
 '((1/2 -3/4 0.1) (-3/4 1/2 0.1)))

(sort-cmp
 string-ci<?
 '(("Tic" "taC" "tOe") ("taC" "Tic" "tOe")))

(sort-cmp
 char-ci<?
 '((#\M #\a #\i #\P #\o #\n) (#\a #\i #\M #\n #\o #\P)))

(sort-cmp
 char-ci-comparator
 '((#\M #\a #\i #\P #\o #\n) (#\a #\i #\M #\n #\o #\P)))

;; stability

(sort-test "stable-sort stability"
           stable-sort stable-sort! (list string-ci<?)
           '("bbb" "CCC" "AAA" "aaa" "BBB" "ccc")
           '("AAA" "aaa" "bbb" "BBB" "CCC" "ccc"))

(sort-test "stable-sort stability"
           stable-sort stable-sort! (list string-ci>?)
           '("bbb" "CCC" "AAA" "aaa" "BBB" "ccc")
           '("CCC" "ccc" "bbb" "BBB" "AAA" "aaa"))

(test-section "sort-by")

(define (sort-by-nocmp key . in&exps)
  (for-each (lambda (in&exp)
              (sort-test2 "sort-by - nocmp" sort-by sort-by!
                          "stable-sort-by - nocmp" stable-sort-by stable-sort-by!
                          (list key)
                          (car in&exp) (cadr in&exp)))
            in&exps))

(sort-by-nocmp
 car
 '(((3 . 1) (2 . 8) (5 . 9) (4 . 7) (6 . 0))
   ((2 . 8) (3 . 1) (4 . 7) (5 . 9) (6 . 0))))

(sort-by-nocmp
 cdr
 '(((3 . 1) (2 . 8) (5 . 9) (4 . 7) (6 . 0))
   ((6 . 0) (3 . 1) (4 . 7) (2 . 8) (5 . 9))))

(define (sort-by-cmp key cmp . in&exps)
  (for-each (lambda (in&exp)
              (sort-test2 "sort-by - cmp" sort-by sort-by!
                          "stable-sort-by - cmp" stable-sort-by stable-sort-by!
                          (list key cmp)
                          (car in&exp) (cadr in&exp)))
            in&exps))

(sort-by-cmp
 cdr char-ci<?
 '(((#\a . #\q) (#\T . #\B) (#\s . #\S) (#\k . #\d))
   ((#\T . #\B) (#\k . #\d) (#\a . #\q) (#\s . #\S))))

(sort-by-cmp
 char->integer >
 '((#\a #\Z #\3 #\q #\P) (#\q #\a #\Z #\P #\3)))

(test-end)
