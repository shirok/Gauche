;;
;; testing termios
;;

(use gauche.test)

(test-start "syslog")
(if (member "." *load-path*) ;; trick to allow in-place test
  (load "syslog")
  (load "gauche/syslog"))
(import gauche.syslog)
(test-module 'gauche.syslog)

;; TODO: writeme

(test-end)
