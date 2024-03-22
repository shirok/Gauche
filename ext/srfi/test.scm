;;;
;;; Test extension srfis
;;;

(use gauche.test)

(test-start "extension srfi modules")

(include "test-srfi-13")
(include "test-srfi-19")
(include "test-srfi-27")
(include "test-srfi-43")
(include "test-srfi-194")
(include "test-srfi-252")

(test-end)
