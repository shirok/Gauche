;;
;; test for gauche.atomic
;;

(use gauche.test)
(test-start "atomic operations")

(use gauche.atomic)
(test-module 'gauche.atomic)

(test-include-r7 "../../test/include/srfi-230-tests")

(test-end)
