;; Test for gauche.interactive.* modules

(use gauche.test)

(test-start "interactive")

(test-section "gauche.interactive")
(use gauche.interactive)
(test-module 'gauche.interactive)

(test-section "gauche.interactive.ed")
(use gauche.interactive.ed)
(test-module 'gauche.interactive.ed)

(test-section "gauche.interactive.info")
(use gauche.interactive.info)
(test-module 'gauche.interactive.info)

(test-section "gauche.interactive.toplevel")
(use gauche.interactive.toplevel)
(test-module 'gauche.interactive.toplevel)

(test-section "gauche.interactive.editable-reader")
(use gauche.interactive.editable-reader)
(test-module 'gauche.interactive.editable-reader)

(test-end)
