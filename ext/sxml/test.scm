;;
;; test sxml tools
;;

;; ssax test is derived from the original SSAX source.
(include "./ssax-test.scm")

;(load "./tree-trans-test.scm")
;(load "./to-html-test.scm")

;; we don't have new sxpath test suite yet.  for now, just check
;; module consistency.

(use gauche.test)

(test-start "sxpath")
(use sxml.sxpath)
(test-module 'sxml.sxpath)

;; regression test for sxpath bug fix
(let ((sxml '(*TOP* (rss:title "foo")))
      (ns-alist '((my . "rss"))))
  (test* "ns-trans" '((rss:title "foo"))
         ((sxpath "//my:title" ns-alist) sxml)))

(test-end)

;; sxml.serializer test

(test-start "serializer")
(use sxml.serializer)
(test-module 'sxml.serializer)

(let ((testdata '(foo (@ (a "b") (c "d\"e'f")) (g "h" (i "j" (k))))))
  (let-syntax
      ((t (syntax-rules () ((t p r) (test* 'p r (p testdata))))))
    (t srl:sxml->xml
       "<foo a=\"b\" c=\"d&quot;e&apos;f\">\n  <g>h<i>j<k/></i></g>\n</foo>")
    (t srl:sxml->xml-noindent
       "<foo a=\"b\" c=\"d&quot;e&apos;f\"><g>h<i>j<k/></i></g></foo>")
    (t srl:sxml->html
       "<foo a=\"b\" c=\"d&quot;e&apos;f\">\n  <g>h<i>j<k></k></i></g>\n</foo>")
    (t srl:sxml->html-noindent
       "<foo a=\"b\" c=\"d&quot;e&apos;f\"><g>h<i>j<k></k></i></g></foo>")
    ))

(test-end)

