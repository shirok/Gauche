;;
;; test for bitvectors
;;   Note - srfi-178 procedures are tested with srfi
;;

(use gauche.test)
(test-start "bitvectors")
(use gauche.generator)

(use gauche.bitvector)
(test-module 'gauche.bitvector)

(test-section "srfi-178 procedures")

(define-module srfi-178-tests
  (use gauche.test)
  (use srfi-78)
  (use srfi-178)
  (test-module 'srfi-178)
  (define-syntax import (syntax-rules () [(_ _) #f]))
  (include "../../test/include/srfi-178-tests"))

(test-section "extra procedures")

(test* "value-fold-index (#t)" '(0 4 6 9 11)
       (reverse
        (bitvector-value-fold-index cons '() #*100010100101 #t)))
(test* "value-fold-index (#f)" '(1 2 3 5 7 8 10)
       (reverse
        (bitvector-value-fold-index cons '() #*100010100101 #f)))

(test* "value-map-index->list (#t)" '(2 6 8 11 13)
       (bitvector-value-map-index->list (^i (+ i 2)) #*100010100101 #t))
(test* "value-map-index->list (#f)" '(3 4 5 7 9 10 12)
       (bitvector-value-map-index->list (^i (+ i 2)) #*100010100101 0))

(test* "value-for-each-index (#t)" '(11 9 6 4 0)
       (rlet1 r '()
         (bitvector-value-for-each-index (^i (push! r i)) #*100010100101 1)))
(test* "value-for-each-index (#f)" '(10 8 7 5 3 2 1)
       (rlet1 r '()
         (bitvector-value-for-each-index (^i (push! r i)) #*100010100101 #f)))


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


;; srfi-209 Enum set depends on gauche.bitvector
(test-section "srfi-209")
(use srfi-209)
(test-module 'srfi-209)

(define-module srfi-209-tests
  (use srfi-64)
  (use srfi-209)
  (test-begin "srfi-209-tests")
  (include "../../test/include/srfi-209-tests")
  (test-end))

(test-end)
