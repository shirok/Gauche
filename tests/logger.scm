;;
;; testing gauche.logger
;;

(use gauche.test)

;; NB: logger uses gauche.fcntl.  Before 'make link', it can't be
;; loaded by 'use'.
(when (file-exists? "../ext/fcntl/fcntl.scm")
  (add-load-path "../ext/fcntl")
  (load "../ext/fcntl/fcntl"))
(when (file-exists? "../ext/syslog/syslog.scm")
  (add-load-path "../ext/syslog")
  (load "../ext/syslog/syslog"))
(test-start "logger")
(use gauche.logger)
(test-module 'gauche.logger)

;;-------------------------------------------------------------------------
(test-section "log-open")

(sys-system "rm -f test.o")

;; these shouldn't go to the log
(log-format "testing...")
(log-format "testing ~a..." 2)
(log-format "testing ~a..." 3)

(log-open "test.o")

(log-format "real testing...")
(log-format "real testing ~a..." 2)
(log-format "output string\ncontaining newline\ncharacters")

(log-open #f)

(log-format "fake testing...")

(log-open "test.o")

(log-format "real testing again...")

(test "log-open"
      '("real testing..."
        "real testing 2..."
        "output string"
        "containing newline"
        "characters"
        "real testing again...")
      (lambda ()
        (map (lambda (line)
               (cond ((#/^... .. ..:..:.. .+\[\d+\]: (.*)$/ line)
                      => (^m (m 1)))
                     (else #f)))
             (call-with-input-file "test.o" port->string-list))))

(sys-system "rm -f test.o")

;;-------------------------------------------------------------------------
(test-section "customized formatter")

(sys-system "rm -f test.o")

(log-open "test.o" :prefix "zeepa:")
(log-format "booba bunba bomba")

(test "customized formatter"
      '("zeepa:booba bunba bomba")
      (lambda ()
        (call-with-input-file "test.o" port->string-list)))

(sys-system "rm -f test.o")

(log-open "test.o" :prefix (lambda (drain) "poopa:"))
(log-format "booba bunba bomba")

(test "customized formatter"
      '("poopa:booba bunba bomba")
      (lambda ()
        (call-with-input-file "test.o" port->string-list)))

(test-end)
