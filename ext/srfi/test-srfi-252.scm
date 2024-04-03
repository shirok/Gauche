;;;
;;;  SRFI-252 depends on math.mt-random and data.random
;;;

(use gauche.test)
(test-start "SRFI-252")
(test-section "SRFI-252")

(use srfi.252)
(test-module 'srfi.252)

(define-module srfi-252-tests
  (use gauche.test)
  (use srfi.64)
  (use srfi.252)
  (test-include-r7 "../../test/include/srfi-252-tests"
                   (exclude (property-test))))

(test-end)
