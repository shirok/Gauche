;;
;; sample test file
;;

(add-load-path "../../src")             ;allow 'in-the-place' testing
(use gauche.test)
(test-start "example")

(require "example")
(import example)
(test-module 'example)

(test "example" '(example) (lambda () (example)))
(test "example" '(example 1 2 3) (lambda () (example 1 2 3)))
(test-end)
