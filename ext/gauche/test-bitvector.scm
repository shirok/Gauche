;;
;; test for bitvectors
;;   Note - srfi-178 procedures are tested with srfi
;;

(use gauche.test)
(test-start "bitvectors")
(use gauche.generator)

(use gauche.bitvector)
(test-module 'gauche.bitvector)

(test-section "index generators")


(test* "index-generator (#t)" '(0 4 6 9 11)
       (generator->list (bitvector->index-generator #*100010100101 #t)))
(test* "index-generator (1)" '(0 4 6 9 11)
       (generator->list (bitvector->index-generator #*100010100101 1)))
(test* "index-generator (#f)" '(1 2 3 5 7 8 10)
       (generator->list (bitvector->index-generator #*100010100101 #f)))
(test* "index-generator (0)" '(1 2 3 5 7 8 10)
       (generator->list (bitvector->index-generator #*100010100101 0)))

(test* "index-generator (1, range)" '(6 9 11)
       (generator->list (bitvector->index-generator #*100010100101 1 5)))
(test* "index-generator (1, range)" '(4 6)
       (generator->list (bitvector->index-generator #*100010100101 1 2 8)))

(test-end)
