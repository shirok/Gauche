;;;
;;; gauche.package.* tests
;;;

;; for now, we do simple tests..

(use gauche.test)
(test-start "gauche.package.*")

(test-section "gauche.package.util")
(use gauche.package.util)
(test-module 'gauche.package.util)

(test-section "gauche.package.build")
(use gauche.package.build)
(test-module 'gauche.package.build)

(test-section "gauche.package.compile")
(use gauche.package.compile)
(test-module 'gauche.package.compile)

(test-section "gauche.package.fetch")
(use gauche.package.fetch)
(test-module 'gauche.package.fetch)

(test-section "gauche.package")
(use gauche.package)
(test-module 'gauche.package)

(test-end)
