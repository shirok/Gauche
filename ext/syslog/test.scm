;;
;; testing termios
;;

(use gauche.test)

(test-start "syslog")
(use gauche.syslog)
(test-module 'gauche.syslog)

;; TODO: writeme

(test-end)
