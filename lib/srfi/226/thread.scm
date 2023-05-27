;;;
;;; (srfi 226 thread)
;;;

(define-module srfi.226.thread
  (use gauche.threads)
  (export &thread
          make-thread-condition
          thread-condition?
          &uncaught-exception
          make-uncaught-exception-condition
          uncaught-exception-condition?
          uncaught-exception-condition-reason
          &thread-already-terminated
          make-thread-already-terminated-condition
          thread-already-terminated-condition?
          &thread-timeout
          make-thread-timeout-condition
          thread-timeout-condition?
          &thread-abandoned-mutex
          make-thread-abandoned-mutex-condition
          thread-abandoned-mutex-condition?
          ;; &concurrent-modification
          ;; make-concurrent-modification-violation
          ;; concurrent-modification-violation?
          ;; thread
          make-thread
          thread?
          current-thread
          thread-start!
          thread-yield!
          thread-terminate!
          thread-schedule-terminate!
          thread-join!
          make-mutex
          mutex?
          mutex-state
          mutex-lock!
          mutex-unlock!
          make-condition-variable
          condition-variable?
          condition-variable-signal!
          condition-variable-broadcast!))
