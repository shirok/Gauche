;;
;; testing termios
;;

(use gauche.test)

(test-start "syslog")
(load "syslog")
(import gauche.syslog)
(test-module 'gauche.syslog)

;; TODO: writeme

(test-end)
