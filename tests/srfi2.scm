;;
;; Tests SRFIs that relies on multiple extension libraries and needs
;; to be run after they are tested.
;;

(use gauche.test)

(test-start "Additional SRFIs")

(test-section "SRFI-274")

(use srfi.274)
(test-module 'srfi.274)

(test-end)
