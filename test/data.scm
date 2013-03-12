;; testing data.*

;; data.* depends on quite a few modules, so we run this after
;; tests of extension modules are done.

(use gauche.test)
(test-start "data.* modules")

(test-section "data.random")
(use data.random)
(test-module 'data.random)

(test-end)
