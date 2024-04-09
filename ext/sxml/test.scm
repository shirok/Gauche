;;
;; test sxml tools
;;

;; ssax test is derived from the original SSAX source.
;; We use 'load' instead of 'include'.  ssax-test.scm is a generated file
;; and in case of out-of-tree build it resides in a build directory while
;; this file (test.scm) is in a source directory, so 'include' can't
;; find ssax-test.scm with a relative pathname.  OTOH, 'load' uses current
;; directory for files starting with "./".
(load "./ssax-test.scm")

;(load "./tree-trans-test.scm")
;(load "./to-html-test.scm")

;; module consistency.

(use gauche.test)

(test-start "SXML auxiliary utilities")

(test-section "sxml.tools")
(use sxml.tools)
(test-module 'sxml.tools)
(use text.tree)

(define (test-xml-html msg sxml xml-expect html-expect)
  (test* #"sxml:sxml->xml ~msg" xml-expect
         (tree->string (sxml:sxml->xml sxml)))
  (test* #"sxml:sxml->html ~msg" html-expect
         (tree->string (sxml:sxml->html sxml))))

;; Check rendering quirks
;; https://github.com/shirok/Gauche-makiki/issues/11
(test-xml-html
 "attrs"
 '(body (div (@ (foo "") (bar "abc\"def") (baz:quux xyzzy))))
 "<body><div foo=\"\" bar=\"abc&quot;def\" baz:quux=\"xyzzy\"/></body>"
 "<body><div foo bar=\"abc&quot;def\" baz:quux=\"xyzzy\"></div></body>")

;; HTML void elements
(test-xml-html
 "html void elements"
 '(div (img (@ (src "abc"))) (br) (hr))
 "<div><img src=\"abc\"/><br/><hr/></div>"
 "<div><img src=\"abc\"><br><hr></div>")

;; Check namespace prefix reassignment
(test* "sxml:sxml->xml namespace"
       "<z:Item xmlns:z=\"https://example.com/defs\">\
           <z:Serial>1234</z:Serial>\
           <z:Serial>5678</z:Serial>\
        </z:Item>"
       ($ tree->string $ sxml:sxml->xml
          '(https://example.com/defs:Item
            (https://example.com/defs:Serial "1234")
            (https://example.com/defs:Serial "5678"))
          '((z . "https://example.com/defs"))))

(test-section "sxml.sxpath")
(use sxml.sxpath)
(test-module 'sxml.sxpath)

;; regression test for sxpath bug fix
(let ((sxml '(*TOP* (rss:title "foo")))
      (ns-alist '((my . "rss"))))
  (test* "ns-trans" '((rss:title "foo"))
         ((sxpath "//my:title" ns-alist) sxml)))


;; sxml.serializer test

(test-section "sxml.serializer")
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
