(use gauche.test)

(test-start "jit")

(add-load-path ".")
(load "./jit")
(import gauche.vm.jit)
(test-module 'gauche.vm.jit)

(test-end)
