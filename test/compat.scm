;;
;; compat.* tests
;;

(use gauche.test)

(test-start "compat.*")

;; compat.chibi-test and compat.r7rs-srfi-tests are used in the tests
;; that are run by this time,
;; so we assume basic functionality works.  Just to make sure we missed
;; something.
(test-section "compat.chibi-test")
(use compat.chibi-test)
(test-module 'compat.chibi-test)

(test-section "compat.r7rs-srfi-tests")
(use compat.r7rs-srfi-tests)
(test-module 'compat.r7rs-srfi-tests)

(test-section "compat.norational")
(use compat.norational)
(test-module 'compat.norational)

(test-section "compat.real-elementary-functions")
(use compat.real-elementary-functions)
(test-module 'compat.real-elementary-functions)

(test-end)
