;;
;; Test srfi.207 (bytestring)
;;
;; It depends on gauche.uvector, gauche.unicode and gauche.generator, so
;; we test it after those dependencies are tested.

(use gauche.test)
(test-start "SRFI-207 (Bytestring)")

(use srfi.207)
(test-module 'srfi.207)

(test-include-r7 "../../test/include/srfi-207-test.scm")

(test-end)
