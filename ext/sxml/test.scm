;;
;; test sxml tools
;;

;; ssax test is derived from the original SSAX source.
(load "./ssax-test.scm")

;(load "./tree-trans-test.scm")
;(load "./to-html-test.scm")

;; we don't have new sxpath test suite yet.  for now, just check
;; module consistency.

(use gauche.test)

(test-start "sxpath")
(use sxml.sxpath)
(test-module 'sxml.sxpath)
(test-end)
