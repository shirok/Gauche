;;
;; gauche.cgen.* tests
;;

(use gauche.test)
(test-start "gauche.cgen.*")

(test-section "gauche.cgen.unit")
(use gauche.cgen.unit)
(test-module 'gauche.cgen.unit)

(test-section "gauche.cgen.literal")
(use gauche.cgen.literal)
(test-module 'gauche.cgen.literal)

(test-section "gauche.cgen.type")
(use gauche.cgen.type)
(test-module 'gauche.cgen.type)

(test-section "gauche.cgen.cise")
(use gauche.cgen.cise)
(test-module 'gauche.cgen.cise)

(test-section "gauche.cgen.stub")
(use gauche.cgen.stub)
(test-module 'gauche.cgen.stub)

(test-section "gauche.cgen")
(use gauche.cgen)
(test-module 'gauche.cgen)

(test-end)
