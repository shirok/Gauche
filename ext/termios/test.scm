;;
;; testing termios
;;

(use gauche.test)
(test-start "termios")

(load "termios")
(import gauche.termios)
(test-module 'gauche.termios)

;; TODO: writeme

(test-end)
