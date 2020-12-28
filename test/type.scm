;; Tests for typeutil

(use gauche.test)
(test-start "typeutil")

(use gauche.typeutil)
(test-module 'gauche.typeutil)

(test-section "built-in type constructors")

(define (validation-test type alist)
  (dolist [p alist]
    (test* (format "~a ~s" (class-name type) (car p))
           (cdr p)
           (of-type? (car p) type))))

(validation-test (<or> <string> <integer>)
                 '(("abc" . #t)
                   (123 . #t)
                   (abc . #f)
                   (#f . #f)
                   (#t . #f)
                   (("abc") . #f)))

(validation-test (<tuple> <char> <integer> <symbol>)
                 '(((#\a 1 a) . #t)
                   ((#\a 1) . #f)
                   (() . #f)
                   ((1 #\a b) . #f)
                   ((#\a 1 b x) . #f)))

(validation-test (<?> <integer>)
                 '((3 . #t)
                   (#f . #t)
                   (#t . #f)
                   (3.5 . #f)))

(validation-test (<tuple> (<?> <char>) (<?> <string>))
                 '((3 . #f)
                   ((#\a "a") . #t)
                   ((#f "a") . #t)
                   ((#\a #f) . #t)
                   ((#f #f) . #t)
                   ((#f) . #f)
                   ((#\a) . #f)
                   (("a") . #f)))

(validation-test (<^> '* :- '*)
                 `((,car . #t)
                   (,cons . #t)
                   (1 . #f)
                   (#/abc/ . #t)))

(validation-test (<^> <top> :- '*)
                 `((,car . #t)
                   (,cons . #f)
                   (,list . #t)
                   (,current-input-port . #t)
                   (,(lambda () #f) . #f)))

(validation-test (<^> :- '*)
                 `((,(lambda () #f) . #t)
                   (,car . #f)
                   (,list . #t)))

(validation-test (<^> <top> <top> :- '*)
                 `((,cons . #t)
                   (,car . #f)))

(test-end)
