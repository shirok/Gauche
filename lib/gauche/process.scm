;;
;; process implementation
;;  (not completed yet)
;;

(define-module gauche.process
  (export <process> run-process process? process-alive? process-pid
          process-input process-output process-error
          process-wait process-exit-status
          process-send-signal process-kill process-stop process-continue
          process-list))
(select-module gauche.process)

(define-class <process> ()
  ((pid       :initform -1 :getter process-pid)
   (status    :initform #f :getter process-exit-status)
   (input     :initform #f :getter process-input)
   (output    :initform #f :getter process-output)
   (error     :initform #f :getter process-error)
   (processes :allocation :class :initform '())
  ))

;; create process and run.
(define (run-process command . args)
  (let loop ((args args) (argv '())
             (input #f) (output #f) (error #f) (wait #f) (fork #t))
    (error "not implemented")))

;; other basic interfaces
(define (process? obj) (is-a? obj <process>))
(define (process-alive? process)
  (and (not (process-exit-status process))
       (>= (process-pid process) 0)))
(define (process-list) (class-slot-ref <process> 'processes))

;; wait
(define (process-wait process)
  (if (process-alive? process)
      (let ((result (sys-waitpid (process-pid process))))
        (slot-set! process 'status (cdr result))
        #t)
      #f))

;; signal
(define (process-send-signal process signal)
  (when (process-alive? process)
    (sys-kill (process-pid process) signal)))
(define (process-kill process) (process-send-signal process SIGKILL))
(define (process-stop process) (process-send-signal process SIGSTOP))
(define (process-continue process) (process-send-signal process SIGCONT))

(provide "gauche/process")
