;;
;; testing termios
;;

(use gauche.test)
(test-start "termios")

(if (member "." *load-path*) ;; trick to allow in-place test
  (load "termios")
  (load "gauche/termios"))
(import gauche.termios)
(test-module 'gauche.termios)

;; TODO: writeme

(test-end)
