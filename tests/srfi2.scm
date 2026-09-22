;;
;; Tests SRFIs that relies on multiple extension libraries and needs
;; to be run after they are tested.
;;

(use gauche.test)

(test-start "Additional SRFIs")

(test-section "SRFI-271")

(use srfi.271)
(test-module 'srfi.271)
(use srfi.271.randomized)
(test-module 'srfi.271.randomized)
(use srfi.271.determinized)
(test-module 'srfi.271.determinized)

(define-module srfi-271-tests
  (use gauche.test)
  (use srfi.64)
  (test-include-r7 "include/srfi-271-tests")
  )

(test-section "SRFI-274")

(use srfi.274)
(test-module 'srfi.274)



(test-section "SRFI-281")

(use srfi.281)
(test-module 'srfi.281)

(test-end)
