;;
;; This manually test AllocConsole() opeartion of gosh-noconsole.exe
;; on MinGW.  This can't be automated easily, so it's not run by
;; make check.
;;

;; How to test:
;; Run this script with gosh-noconsole, compiled with --enable-threads=win32.
;; A few seconds after startup, the console should pop up,
;; containing messages from 10 individual threads.


(cond-expand
 [(not gauche.sys.wthreads)
  (exit 1 "This script needs to be run on MinGW version of gosh-noconsole compiled with --enable-threads=win32.")]
 [else])

(use gauche.threads)
(use data.queue)

(define (main args)
  (let1 q (make-mtqueue)
    (define (make-thunk k)
      (^[] (dequeue/wait! q) (display k) (flush)))
    (dotimes [n 10] (thread-start! (make-thread (make-thunk n))))
    (dotimes [n 10] (enqueue! q #t))
    (sys-sleep 10)
    0))
