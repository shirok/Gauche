;;
;; Test miscellaneous compiler subtleties
;;
;; This is run after basic are tested (in TEST2), so you can use them.

(use gauche.test)
(test-start "miscellaneous compiler features")

(test-section "uninitialized binding")

(test* "don't allow forward reference"
       (test-error <error> #/uninitialized variable: list/)
       (eval '(begin 
                (define orig-list list)
                (define (list x) (cons 'x (orig-list x)))
                (list 1))
             (current-module)))

(test-end)
