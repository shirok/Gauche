;;
;; Test alternative compiler backend
;;
;; The modules tested here is not yet in the production

(use gauche.test)
(test-start "alternative compiler backend")

(test-section "modules")
(use gauche.cgen.bbb)
(test-module 'gauche.cgen.bbb)

(use gauche.vm.register-machine)
(test-module 'gauche.vm.register-machine)






(test-end)
