;;
;; gauche.cgen.* tests
;;

(use gauche.test)
(test-start "gauche.cgen.*")

;;====================================================================
(test-section "gauche.cgen.unit")
(use gauche.cgen.unit)
(test-module 'gauche.cgen.unit)

;;====================================================================
(test-section "gauche.cgen.literal")
(use gauche.cgen.literal)
(test-module 'gauche.cgen.literal)

;;====================================================================
(test-section "gauche.cgen.type")
(use gauche.cgen.type)
(test-module 'gauche.cgen.type)

;;====================================================================
(test-section "gauche.cgen.cise")
(use gauche.cgen.cise)
(test-module 'gauche.cgen.cise)

(let ()
  (define (t in out)
    (test* (format "canonicalize-vardecl ~s" in) out
           ((with-module gauche.cgen.cise canonicalize-vardecl) in)))

  (t '(a b c) '((a :: ScmObj) (b :: ScmObj) (c :: ScmObj)))
  (t '((a) (b) (c)) '((a) (b) (c)))
  (t '(a::x b::y (c::z)) '((a :: x) (b :: y) (c :: z)))
  (t '(a :: x b :: y (c :: z)) '((a :: x) (b :: y) (c :: z)))
  (t '(a:: x b ::y (c:: z)) '((a :: x) (b :: y) (c :: z)))
  (t '(a::(x y z) b::p) '((a :: (x y z)) (b :: p)))

  (t '((a::x init) (b::(x) init) (c :: x init))
     '((a :: x init) (b :: (x) init) (c :: x init)))
  (t '((a init) (b init) (c init))
     '((a init) (b init) (c init)))
  )

;;====================================================================
(test-section "gauche.cgen.stub")
(use gauche.cgen.stub)
(test-module 'gauche.cgen.stub)



;;====================================================================
(test-section "gauche.cgen")
(use gauche.cgen)
(test-module 'gauche.cgen)

(test-end)
