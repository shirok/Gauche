;; srfi.194 depends on data.random, so test it iere.

(use gauche.test)
(test-start "SRFI-194")
(test-section "SRFI-194")

(define-module srfi-194-tests
  (use gauche.test)
  (use srfi.194)
  (test-module 'srfi.194)
  (test-module 'srfi.194.zipf-zri)
  (test-module 'srfi.194.sphere)

  (test-include-r7 "../../test/include/srfi-194-test"))

(test-end)
