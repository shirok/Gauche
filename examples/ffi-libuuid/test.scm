;;;
;;; Test libuuid
;;;

(use gauche.test)

(test-start "ffi-libuuid")
(use ffi-libuuid)
(test-module 'ffi-libuuid)

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
