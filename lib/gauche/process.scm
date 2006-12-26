;;;
;;; process.scm - process interface
;;;  
;;;   Copyright (c) 2000-2006 Shiro Kawai, All rights reserved.
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  
;;;  $Id: process.scm,v 1.21 2006-12-26 22:02:16 shirok Exp $
;;;

;; process interface, mostly compatible with STk's, but implemented
;; as an object on top of basic system interface.

(define-module gauche.process
  (use srfi-1)
  (use srfi-2)
  (use srfi-13)
  (export <process> run-process process? process-alive? process-pid
          process-command process-input process-output process-error
          process-wait process-wait-any process-exit-status
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

;; Delay-load gauche.charconv 
(autoload gauche.charconv
          wrap-with-input-conversion wrap-with-output-conversion)

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
             (input #f) (output #f) (error #f) (wait #f) (fork #t) (mask #f))
    (cond ((null? args)
           (let ((proc  (make <process> :command (x->string command))))
             (receive (iomap toclose)
               (if (or input output error)
                   (%setup-iomap proc input output error)
                   (values #f '()))
               (%run-process proc (cons (x->string command) (reverse argv))
                             iomap mask toclose wait fork))))
          ((eqv? (car args) :input)
           (check-iokey args)
           (loop (cddr args) argv (cadr args) output error wait fork mask))
          ((eqv? (car args) :output)
           (check-iokey args)
           (loop (cddr args) argv input (cadr args) error wait fork mask))
          ((eqv? (car args) :error)
           (check-iokey args)
           (loop (cddr args) argv input output (cadr args) wait fork mask))
          ((eqv? (car args) :fork)
           (check-key args)
           (loop (cddr args) argv input output error wait (cadr args) mask))
          ((eqv? (car args) :wait)
           (check-key args)
           (loop (cddr args) argv input output error (cadr args) fork mask))
          ((eqv? (car args) :sigmask)
           (check-key args)
           (loop (cddr args) argv input output error wait fork (cadr args)))
          (else
           (loop (cdr args) (cons (x->string (car args)) argv)
                 input output error wait fork mask))
          ))
  )

(define (%setup-iomap proc input output error)

  (define toclose '())

  (define (file spec opener)
    (and (string? spec)
         (let ((p (opener spec)))
           (push! toclose p)
           p)))

  (define (in-pipe spec slot)
    (and (eqv? spec :pipe)
         (receive (in out) (sys-pipe)
           (slot-set! proc slot out)
           (push! toclose in)
           in)))

  (define (out-pipe spec slot)
    (and (eqv? spec :pipe)
         (receive (in out) (sys-pipe)
           (slot-set! proc slot in)
           (push! toclose out)
           out)))

  (let ((iomap `(,(cons 0 (or (file input open-input-file)
                              (in-pipe input 'input)
                              0))
                 ,(cons 1 (or (file output open-output-file)
                              (out-pipe output 'output)
                              1))
                 ,(cons 2 (or (file error open-output-file)
                              (out-pipe error 'error)
                              2)))))
    (values iomap toclose)))

(define (%ensure-mask mask)
  (cond
   ((is-a? mask <sys-sigset>) mask)
   ((and (list? mask) (every integer? mask))
    (fold (lambda (sig m) (sys-sigset-add! m sig) m) (make <sys-sigset>) mask))
   ((not mask) #f)
   (else (error "run-process: sigmask argument must be either #f, <sys-sigset>, or a list of integers, but got:" mask))))

(define (%run-process proc argv iomap sigmask toclose wait fork)
  (if fork
    (let1 pid (sys-fork-and-exec (car argv) argv
                                 :iomap iomap :sigmask (%ensure-mask sigmask))
      (push! (ref proc 'processes) proc)
      (set!  (ref proc 'pid) pid)
      (dolist (p toclose)
        (if (input-port? p) (close-input-port p) (close-output-port p)))
      (when wait
        ;; the following expr waits until the child exits
        (set! (ref proc 'status) (values-ref (sys-waitpid pid) 1))
        (update! (ref proc 'processes) (cut delete proc <>)))
      proc)
    (sys-exec (car argv) argv :iomap iomap :sigmask (%ensure-mask sigmask))))

;; other basic interfaces
(define (process? obj) (is-a? obj <process>))
(define (process-alive? process)
  (and (not (process-exit-status process))
       (>= (process-pid process) 0)))
(define (process-list) (class-slot-ref <process> 'processes))

;; wait
(define (process-wait process . args)
  (if (process-alive? process)
    (let-optionals* args ((nohang? #f))
      (receive (p code) (sys-waitpid (process-pid process) :nohang nohang?)
        (and (not (zero? p))
             (begin
               (slot-set! process 'status code)
               (slot-set! process 'processes
                          (delete process (slot-ref process 'processes)))
               #t))))
    #f))

(define (process-wait-any . args)
  (let-optionals* args ((nohang? #f))
    (and (not (null? (process-list)))
         (receive (pid status) (sys-waitpid -1 :nohang nohang?)
           (and pid
                (and-let* ((p (find (lambda (pp) (= (process-pid pp) pid))
                                    (process-list))))
                  (slot-set! p 'status status)
                  (update! (ref p 'processes) (cut delete p <>))
                  p))))))

;; signal
(define (process-send-signal process signal)
  (when (process-alive? process)
    (sys-kill (process-pid process) signal)))
(define (process-kill process) (process-send-signal process |SIGKILL|))
(define (process-stop process) (process-send-signal process |SIGSTOP|))
(define (process-continue process) (process-send-signal process |SIGCONT|))

;; Process ports

;; Common keyword args:
;;   :error    - specifies error destination.  filename (redirect to file),
;;               or #t (stderr).
;;   :encoding - if given, CES conversion port is inserted.
;;   :conversion-buffer-size - used when CES conversion is necessary.

(define (open-input-process-port command . opts)
   (let-keywords* opts ((input "/dev/null")
                       (err :error #f))
    (let1 p (apply-run-process command input :pipe err)
      (values (wrap-input-process-port p opts) p))))

(define (call-with-input-process command proc . opts)
  (let-keywords* opts ((input "/dev/null")
                       (err :error #f))
    (let* ((p (apply-run-process command input :pipe err))
           (i (wrap-input-process-port p opts)))
      (with-error-handler
          (lambda (e)
            (close-input-port i)
            (process-wait p)
            (raise e))
        (lambda ()
          (begin0 (proc i)
                  (close-input-port i)
                  (process-wait p)))))))

(define (with-input-from-process command thunk . opts)
  (apply call-with-input-process command
         (cut with-input-from-port <> thunk)
         opts))

(define (open-output-process-port command . opts)
  (let-keywords* opts ((output "/dev/null")
                       (err :error #f))
    (let1 p (apply-run-process command :pipe output err)
      (values (wrap-output-process-port p opts) p))))

(define (call-with-output-process command proc . opts)
  (let-keywords* opts ((output "/dev/null")
                       (err :error #f))
    (let* ((p (apply-run-process command :pipe output err))
           (o (wrap-output-process-port p opts)))
      (with-error-handler
          (lambda (e)
            (close-output-port o)
            (process-wait p)
            (raise e))
        (lambda ()
          (begin0 (proc o)
                  (close-output-port o)
                  (process-wait p)))))))

(define (with-output-to-process command thunk . opts)
  (apply call-with-output-process command
         (cut with-output-to-port <> thunk)
         opts))

(define (call-with-process-io command proc . opts)
  (let-keywords* opts ((err :error #f))
    (let* ((p (apply-run-process command :pipe :pipe err))
           (i (wrap-input-process-port p opts))
           (o (wrap-output-process-port p opts)))
      (with-error-handler
          (lambda (e)
            (close-output-port o)
            (close-input-port i)
            (process-wait p)
            (raise e))
        (lambda ()
          (begin0 (proc i o)
                  (close-output-port o)
                  (close-input-port i)
                  (process-wait p)))))))

;; Convenient thingies that can be used like `command` in shell scripts

(define (process-output->string command)
  (call-with-input-process command
    (lambda (p)
      (with-port-locking p
        (lambda ()
          (string-join (string-tokenize (port->string p)) " "))))))

(define (process-output->string-list command)
  (call-with-input-process command port->string-list))

;; A common utility for process ports.

;; If the given command is a string, return an argv to use /bin/sh.
(define (apply-run-process command stdin stdout stderr)
  (apply run-process
         (append
          (cond ((string? command) `("/bin/sh" "-c" ,command))
                ((list? command) (map x->string command))
                (else (error "Bad command spec" command)))
          `(:input ,stdin :output ,stdout)
          (cond ((string? stderr) `(:error ,stderr))
                (else '())))))

;; Possibly wrap the process port by a conversion port
(define (wrap-input-process-port process opts)
  (let-keywords* opts ((encoding #f)
                       (conversion-buffer-size 0))
    (if encoding
      (wrap-with-input-conversion (process-output process) encoding
                                  :buffer-size conversion-buffer-size)
      (process-output process))))

(define (wrap-output-process-port process opts)
  (let-keywords* opts ((encoding #f)
                       (conversion-buffer-size 0))
    (if encoding
      (wrap-with-output-conversion (process-input process) encoding
                                  :buffer-size conversion-buffer-size)
      (process-input process))))

(provide "gauche/process")
