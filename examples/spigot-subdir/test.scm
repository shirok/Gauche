;;;
;;; Test math.spigot
;;;

(use gauche.test)

(test-start "math.spigot")
(use math.spigot)
(test-module 'math.spigot)

;; The following is a dummy test code.
;; Replace it for your tests.
(test* "test-spigot_subdir" "spigot_subdir is working"
       (test-spigot_subdir))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
