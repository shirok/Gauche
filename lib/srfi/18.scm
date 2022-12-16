;;
;; SRFI-18 - multithreading support
;; In Gauche, SRFI-18 is covered by gauche.threads
;;

(define-module srfi.18
  (use gauche.threads)
  (export current-thread

          thread? make-thread thread-name thread-specific-set! thread-specific
          thread-start! thread-yield! thread-sleep!
          thread-join! thread-terminate!

          mutex? make-mutex mutex-name mutex-state
          mutex-specific-set! mutex-specific
          mutex-lock! mutex-unlock!

          condition-variable? make-condition-variable condition-variable-name
          condition-variable-specific condition-variable-specific-set!
          condition-variable-signal! condition-variable-broadcast!

          current-time time? time->seconds seconds->time

          current-exception-handler     ;re-export from built-in
          with-exception-handler        ; ditto
          raise                         ; ditto

          join-timeout-exception? abandoned-mutex-exception?
          terminated-thread-exception? uncaught-exception?
          uncaught-exception-reason
          ))
