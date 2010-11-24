;;
;; testing os.windows
;;

(use gauche.test)

(cond-expand
 [gauche.os.windows
  (test-start "windows")
  (use os.windows)
  (test-module 'os.windows)
  (test-end)]
 [else])
