;;
;; This manually test AllocConsole() opeartion of gosh-noconsole.exe
;; on MinGW.  This can't be automated easily, so it's not run by
;; make check.
;;

(cond-expand
 [(not gauche.os.windows)
  (exit 1 "This script needs to be run on MinGW version of gosh-noconsole.")]
 [else])

(use gauche.process)

;; stdio test

(print "1+2+3=" (+ 1 2 3))
(print "HIT ENTER KEY!")
(read-line)
(display "4+5+6="  (current-error-port))
(display (+ 4 5 6) (current-error-port))
(newline (current-error-port))
(display "HIT ENTER KEY!" (current-error-port))
(read-line)

;; run-process input test

(let* ((p   (run-process "more.com" :input :pipe))
       (out (process-input p)))
  (display (iota 511) out)
  (newline out)
  (close-output-port out)
  (process-wait p))
(print "HIT ENTER KEY!")
(read-line)

;; run-process output test

(let* ((p   (run-process '(echo "12345\nabcde") :output :pipe))
       (in  (process-output p)))
  (display (read-line in))
  (newline)
  (display (read-line in))
  (newline)
  (close-input-port in)
  (process-wait p))
(print "HIT ENTER KEY!")
(read-line)

;; sys-system test

(sys-system "date /t")
(sys-system "time /t")
(print "HIT ENTER KEY!")
(read-line)

