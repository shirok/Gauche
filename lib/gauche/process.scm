;;;
;;; process.scm - process interface
;;;
;;;  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: process.scm,v 1.11 2002-07-12 05:01:38 shirok Exp $
;;;

;; process interface, mostly compatible with STk's, but implemented
;; as an object on top of basic system interface.

(define-module gauche.process
  (use srfi-1)
  (use srfi-13)
  (export <process> run-process process? process-alive? process-pid
          process-input process-output process-error
          process-wait process-exit-status
          process-send-signal process-kill process-stop process-continue
          process-list
          ;; process ports
          open-input-process-port   open-output-process-port
          call-with-input-process   call-with-output-process
          with-input-from-process   with-output-to-process
          call-with-process-io
          process-output->string    process-output->string-list
          ))
(select-module gauche.process)

(define-class <process> ()
  ((pid       :initform -1 :getter process-pid)
   (command   :initform #f :getter process-command :init-keyword :command)
   (status    :initform #f :getter process-exit-status)
   (input     :initform #f :getter process-input)
   (output    :initform #f :getter process-output)
   (error     :initform #f :getter process-error)
   (processes :allocation :class :initform '())
  ))

(define-method write-object ((p <process>) port)
  (format port "#<process ~a ~s ~a>"
          (process-pid p)
          (process-command p)
          (if (process-alive? p)
              "active"
              "inactive")))

;; create process and run.
(define (run-process command . args)
  (define (check-key args)
    (when (null? (cdr args))
      (errorf "~s key requires an argument following" (car args))))

  (define (check-iokey args)
    (check-key args)
    (unless (or (string? (cadr args)) (eqv? (cadr args) :pipe))
      (errorf "~s key requires a string or :pipe following, but got ~s"
              (car args) (cadr args))))
    
  (let loop ((args args) (argv '())
             (input #f) (output #f) (error #f) (wait #f) (fork #t))
    (cond ((null? args)
           (let ((proc  (make <process> :command command)))
             (receive (iomap toclose)
               (if (or input output error)
                   (%setup-iomap proc input output error)
                   (values #f '()))
               (%run-process proc (cons command (reverse argv))
                             iomap toclose wait fork))))
          ((eqv? (car args) :input)
           (check-iokey args)
           (loop (cddr args) argv (cadr args) output error wait fork))
          ((eqv? (car args) :output)
           (check-iokey args)
           (loop (cddr args) argv input (cadr args) error wait fork))
          ((eqv? (car args) :error)
           (check-iokey args)
           (loop (cddr args) argv input output (cadr args) wait fork))
          ((eqv? (car args) :fork)
           (check-key args)
           (loop (cddr args) argv input output error wait (cadr args)))
          ((eqv? (car args) :wait)
           (check-key args)
           (loop (cddr args) argv input output error (cadr args) fork))
          (else
           (loop (cdr args) (cons (car args) argv)
                 input output error wait fork))
          ))
  )

(define (%setup-iomap proc input output error)
  (let* ((toclose '())
         (iomap `(,(cons 0 (cond ((string? input) (open-input-file input))
                                 ((eqv? input :pipe)
                                  (receive (in out) (sys-pipe)
                                    (slot-set! proc 'input out)
                                    (set! toclose (cons in toclose))
                                    in))
                                 (else 0)))
                  ,(cons 1 (cond ((string? output) (open-output-file output))
                                 ((eqv? output :pipe)
                                  (receive (in out) (sys-pipe)
                                    (slot-set! proc 'output in)
                                    (set! toclose (cons out toclose))
                                    out))
                                 (else 1)))
                  ,(cons 2 (cond ((string? error) (open-output-file error))
                                 ((eqv? error :pipe)
                                  (receive (in out) (sys-pipe)
                                    (slot-set! proc 'error in)
                                    (set! toclose (cons out toclose))
                                    out))
                                 (else 2)))
                  ))
        )
    (values iomap toclose)))

(define (%run-process proc argv iomap toclose wait fork)
  (if fork
      (let ((pid (sys-fork)))
        (if (zero? pid)
            (sys-exec (car argv) argv iomap)
            (begin
              (slot-set! proc 'processes
                         (cons proc (slot-ref proc 'processes)))
              (slot-set! proc 'pid pid)
              (map (lambda (p)
                     (if (input-port? p)
                         (close-input-port p)
                         (close-output-port p)))
                   toclose)
              (when wait
                (slot-set! proc 'status
                           (receive (p code) (sys-waitpid pid) code)))
              proc)))
      (sys-exec (car argv) argv iomap)))

;; other basic interfaces
(define (process? obj) (is-a? obj <process>))
(define (process-alive? process)
  (and (not (process-exit-status process))
       (>= (process-pid process) 0)))
(define (process-list) (class-slot-ref <process> 'processes))

;; wait
(define (process-wait process)
  (if (process-alive? process)
      (receive (p code) (sys-waitpid (process-pid process))
        (slot-set! process 'status code)
        (slot-set! process 'processes
                   (delete process (slot-ref process 'processes)))
        #t)
      #f))

;; signal
(define (process-send-signal process signal)
  (when (process-alive? process)
    (sys-kill (process-pid process) signal)))
(define (process-kill process) (process-send-signal process |SIGKILL|))
(define (process-stop process) (process-send-signal process |SIGSTOP|))
(define (process-continue process) (process-send-signal process |SIGCONT|))

;; Process ports

(define (open-input-process-port command)
  ;; TODO: how to terminate & wait the process?
  (let ((p (run-process "/bin/sh" "-c" command
                        :input "/dev/null" :error "/dev/null"
                        :output :pipe)))
    (process-output p)))

(define (call-with-input-process command proc)
  (let* ((p (run-process "/bin/sh" "-c" command
                         :input "/dev/null" :output :pipe :error "/dev/null"))
         (i (process-output p)))
    (with-error-handler
        (lambda (e)
          (close-input-port i)
          (process-wait p)
          (raise e))
      (lambda ()
        (begin0 (proc i)
                (close-input-port i)
                (process-wait p))))))

(define (with-input-from-process command thunk)
  (call-with-input-process command
    (lambda (p) (with-input-from-port p thunk))))

(define (open-output-process-port command)
  (let ((p (run-process "/bin/sh" "-c" command
                        :input :pipe :output "/dev/null"
                        :error "/dev/null")))
    (process-input p)))

(define (call-with-output-process command proc)
  (let* ((p (run-process "/bin/sh" "-c" command
                         :input :pipe :output "/dev/null" :error "/dev/null"))
         (o (process-input p)))
    (dynamic-wind
     (lambda () #f)
     (lambda () (proc o))
     (lambda ()
       (close-output-port o)
       (process-wait p)))))

(define (with-output-to-process command thunk)
  (call-with-output-process command
    (lambda (p) (with-output-to-port p thunk))))

(define (call-with-process-io command proc)
  (let* ((p (run-process "/bin/sh" "-c" command
                         :input :pipe :output :pipe
                         :error "/dev/null"))
         (i (process-output p))
         (o (process-input p)))
    (dynamic-wind
     (lambda () #f)
     (lambda () (proc i o))
     (lambda ()
       (close-output-port o)
       (close-input-port i)
       (process-wait p)))))

;; Convenient thingies that can be used like `command` in shell scripts

(define (process-output->string command)
  (call-with-input-process command
    (lambda (p)
      (string-join (string-tokenize (port->string p)) " "))))

(define (process-output->string-list command)
  (call-with-input-process command port->string-list))

(provide "gauche/process")
