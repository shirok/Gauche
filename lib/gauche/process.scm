;;;
;;; process.scm - process interface
;;;  
;;;   Copyright (c) 2000-2010  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: process.scm,v 1.33 2008-05-10 13:35:56 shirok Exp $
;;;

#!no-fold-case

;; process interface, mostly compatible with STk's, but implemented
;; as an object on top of basic system interface.

(define-module gauche.process
  (use srfi-1)
  (use srfi-2)
  (use srfi-13)
  (use srfi-14)
  (export <process> <process-abnormal-exit>
          run-process process? process-alive? process-pid
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
          ;; shell utilities
          shell-escape-string
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

;; When the process exits abnormally, this error is thrown.
(define-condition-type <process-abnormal-exit> <error> #f (process))

(define-method write-object ((p <process>) port)
  (format port "#<process ~a ~s ~a>"
          (process-pid p)
          (process-command p)
          (if (process-alive? p)
              "active"
              "inactive")))

;; null device
(define *nulldev*
  (cond-expand (gauche.os.windows "NUL") (else "/dev/null")))

;; create process and run.
(define (run-process command . args)
  (if (not (list? command))
    (%run-process-old command args) ;; backward compatibility
    (%run-process-new command args)))

(define (%run-process-new command args)
  (let-keywords* args ((input  #f) (output #f) (error  #f)
                       (wait   #f) (fork   #t)
                       (host   #f)    ;remote execution
                       (sigmask #f) (directory #f))
    (%check-iokey :input input)
    (%check-iokey :output output)
    (%check-iokey :error error)
    (let* ([argv (map x->string command)]
           [proc (make <process> :command (car argv))]
           [argv (if host (%prepare-remote host argv directory) argv)]
           [dir  (if host #f directory)])
      (%check-directory dir)
      (receive (iomap toclose)
          (if (or input output error)
            (%setup-iomap proc input output error)
            (values #f '()))
        (if fork
          (let1 pid (sys-fork-and-exec (car argv) argv
                                       :iomap iomap :directory dir
                                       :sigmask (%ensure-mask sigmask))
            (push! (ref proc 'processes) proc)
            (set!  (ref proc 'pid) pid)
            (dolist (p toclose)
              (if (input-port? p)
                (close-input-port p)
                (close-output-port p)))
            (when wait
              ;; the following expr waits until the child exits
              (set! (ref proc 'status) (values-ref (sys-waitpid pid) 1))
              (update! (ref proc 'processes) (cut delete proc <>)))
            proc)
          (sys-exec (car argv) argv
                    :iomap iomap :directory dir
                    :sigmask (%ensure-mask sigmask)))))))

(define (%check-iokey key arg)
  (unless (or (string? arg) (not arg) (eqv? arg :pipe))
    (errorf "~s key requires a string or :pipe following, but got ~s" key arg)))

(define (%check-directory dir)
  (when dir
    (unless (and (string? dir) (file-is-directory? dir) (sys-access dir X_OK))
      (errorf "cannot set ~s as the executing process's working directory"
              dir))))

;; The archane API, where one can mix keyword args and command arguments.
;; This API is taken from STk.  Now we don't need STk compatibility much,
;; so we support this only for backward compatibility.
(define (%run-process-old command args)
  (let loop ((args args) (argv (list command)) (keys '()))
    (cond [(null? args)
           (%run-process-new (reverse argv) (reverse keys))]
          [(keyword? (car args))
           (when (null? (cdr args))
             (errorf "~s key requires an argument following" (car args)))
           (loop (cddr args) argv (cons* (cadr args) (car args) keys))]
          [else
           (loop (cdr args) (cons (x->string (car args)) argv) keys)])))

;; Prepare remote execution via ssh
(define (%prepare-remote host argv dir)
  (rxmatch-let (#/^(?:([\w-]+):)?(?:([\w-]+)@)?([\w._]+)(?::(\d+))?$/ host)
      (#f proto user server port)
    (unless (or (not proto) (equal? proto "ssh"))
      (error "Remote execution protocol other than 'ssh' is not supported:"
             proto))
    `("ssh"
      ,@(if user `("-l" ,user) '())
      ,@(if port `("-p" ,port) '())
      ,server
      ,@(if dir `("cd" ,dir ";") '())
      ,@argv)))

(define (%setup-iomap proc input output error)

  (define toclose '())

  (define (file spec opener)
    (and (string? spec)
         (rlet1 p (opener spec) (push! toclose p))))

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

  (let1 iomap `(,(cons 0 (or (file input open-input-file)
                             (in-pipe input 'input)
                             0))
                ,(cons 1 (or (file output open-output-file)
                             (out-pipe output 'output)
                             1))
                ,(cons 2 (or (file error open-output-file)
                             (out-pipe error 'error)
                             2)))
    (values iomap toclose)))

(define (%ensure-mask mask)
  (cond
   [(is-a? mask <sys-sigset>) mask]
   [(and (list? mask) (every integer? mask))
    (fold (lambda (sig m) (sys-sigset-add! m sig) m) (make <sys-sigset>) mask)]
   [(not mask) #f]
   [else (error "run-process: sigmask argument must be either #f, \
                 <sys-sigset>, or a list of integers, but got:" mask)]))

(define (%check-normal-exit process)
  (let1 status (ref process 'status)
    (unless (and (sys-wait-exited? status)
                 (zero? (sys-wait-exit-status status)))
      (cond [(sys-wait-exited? status)
             (errorf <process-abnormal-exit> :process process
                     "~s exitted abnormally with exit code ~a"
                     process (sys-wait-exit-status status))]
            [(sys-wait-signaled? status)
             (errorf <process-abnormal-exit> :process process
                     "~s exitted by signal ~s"
                     process (sys-signal-name (sys-wait-termsig status)))]
            [else ;; we shouldn't be here, but just in case...
             (errorf <process-abnormal-exit> :process process
                     "~s exitted abnormally with status ~s"
                     process status)]))))

;; other basic interfaces
(define (process? obj) (is-a? obj <process>))
(define (process-alive? process)
  (and (not (process-exit-status process))
       (process-pid process)))
(define (process-list) (class-slot-ref <process> 'processes))

;;-----------------------------------------------------------------
;; wait
;;
(define (process-wait process :optional (nohang? #f) (raise-error? #f))
  (if (process-alive? process)
    (receive (p code) (sys-waitpid (process-pid process) :nohang nohang?)
      (and (not (eqv? p 0))
           (begin
             (slot-set! process 'status code)
             (slot-set! process 'processes
                        (delete process (slot-ref process 'processes)))
             (when raise-error? (%check-normal-exit process))
             #t)))
    #f))

(define (process-wait-any :optional (nohang? #f) (raise-error? #f))
  (and (not (null? (process-list)))
       (receive (pid status) (sys-waitpid -1 :nohang nohang?)
         (and (not (eqv? pid 0))
              (and-let* ((p (find (lambda (pp) (eqv? (process-pid pp) pid))
                                  (process-list))))
                (update! (ref p 'processes) (cut delete p <>))
                (set! (ref p 'status) status)
                (set! (ref p 'pid) #f)
                (when raise-error? (%check-normal-exit p))
                p)))))

;; signal
(define (process-send-signal process signal)
  (when (process-alive? process)
    (sys-kill (process-pid process) signal)))
(define (process-kill process) (process-send-signal process SIGKILL))
(define (process-stop process)
  (cond-expand
   [gauche.os.windows (undefined)]
   [else (process-send-signal process SIGSTOP)]))
(define (process-continue process)
  (cond-expand
   [gauche.os.windows (undefined)]
   [else (process-send-signal process SIGCONT)]))

;;===================================================================
;; Process ports
;;

;; Common keyword args:
;;   :error    - specifies error destination.  filename (redirect to file),
;;               or #t (stderr).
;;   :encoding - if given, CES conversion port is inserted.
;;   :conversion-buffer-size - used when CES conversion is necessary.
;;   :on-abmormal-exit - :error, :ignore, or a handler (called w/ process)

(define (open-input-process-port command :key (input *nulldev*)
                                 ((:error err) #f) (host #f)
                                 :allow-other-keys rest)
  (let1 p (%apply-run-process command input :pipe err host)
    (values (wrap-input-process-port p rest) p)))

(define (call-with-input-process command proc :key (input *nulldev*)
                                 ((:error err) #f) (host #f) (on-abnormal-exit :error)
                                 :allow-other-keys rest)
  (let* ((p (%apply-run-process command input :pipe err host))
         (i (wrap-input-process-port p rest)))
    (unwind-protect (proc i)
      (begin
        (close-input-port i)
        (process-wait p)
        (handle-abnormal-exit on-abnormal-exit p)))))

(define (with-input-from-process command thunk . opts)
  (apply call-with-input-process command
         (cut with-input-from-port <> thunk)
         opts))

(define (open-output-process-port command :key (output *nulldev*)
                                  ((:error err) #f) (host #f)
                                  :allow-other-keys rest)
  (let1 p (%apply-run-process command :pipe output err host)
    (values (wrap-output-process-port p rest) p)))

(define (call-with-output-process command proc :key (output *nulldev*)
                                  ((:error err) #f) (host #f)
                                  (on-abnormal-exit :error)
                                  :allow-other-keys rest)
  (let* ((p (%apply-run-process command :pipe output err host))
         (o (wrap-output-process-port p rest)))
    (unwind-protect (proc o)
      (begin
        (close-output-port o)
        (process-wait p)
        (handle-abnormal-exit on-abnormal-exit p)))))

(define (with-output-to-process command thunk . opts)
  (apply call-with-output-process command
         (cut with-output-to-port <> thunk)
         opts))

(define (call-with-process-io command proc :key ((:error err) #f)
                              (host #f) (on-abnormal-exit :error)
                              :allow-other-keys rest)
  (let* ((p (%apply-run-process command :pipe :pipe err host))
         (i (wrap-input-process-port p rest))
         (o (wrap-output-process-port p rest)))
    (unwind-protect (proc i o)
      (begin
        (close-output-port o)
        (close-input-port i)
        (process-wait p)
        (handle-abnormal-exit on-abnormal-exit p)))))

;;---------------------------------------------------------------------
;; Convenient thingies that can be used like `command` in shell scripts
;;

(define (process-output->string command . opts)
  (apply call-with-input-process command
         (lambda (p)
           (with-port-locking p
             (lambda ()
               (string-join (string-tokenize (port->string p)) " "))))
         opts))

(define (process-output->string-list command . opts)
  (apply call-with-input-process command port->string-list opts))

;;---------------------------------------------------------------------
;; Shell utility
;;

(define (shell-escape-string str)
  (cond-expand
   [gauche.os.windows
    ;; This is supported in src/scmlib.scm.  See the comment in it.
    (%sys-escape-windows-command-line str)]
   [else
    ;; We follow standard unix shell convention: if STR contains special
    ;; chars, we quote the entire STR by single-quotes.  If STR contains
    ;; a single quote, we replace it with '"'"'.
    (cond [(string-null? str) "''"]
          [(string-index str #[\s\\\"\'*?$<>!\[\](){}])
           (string-append "'" (regexp-replace-all #/'/ str "'\"'\"'") "'")]
          [else str])]))

;;----------------------------------------------------------------------
;; Internal utilities for process ports
;;

;; If the given command is a string, return an argv to use /bin/sh.
;; NB: on Windows we need to use cmd.exe.  But its command-line parsing
;; rule is too broken to use reliably.  Another possibility is to implement
;; much of high-level /bin/sh functionalities in Scheme, so that we can
;; provide consistent behavior.  Something to think about.
(define (%apply-run-process command stdin stdout stderr host)
  (apply run-process
         (cond ((string? command)
                (cond-expand
                 (gauche.os.windows
                  `("cmd" "/c" ,command))
                 (else
                  `("/bin/sh" "-c" ,command))))
               ((list? command) command)
               (else (error "Bad command spec" command)))
         :input stdin :output stdout :host host
         (cond ((string? stderr) `(:error ,stderr))
               (else '()))))

;; Possibly wrap the process port by a conversion port
(define (wrap-input-process-port process opts)
  (let-keywords opts ((encoding #f)
                      (conversion-buffer-size 0))
    (if encoding
      (wrap-with-input-conversion (process-output process) encoding
                                  :buffer-size conversion-buffer-size)
      (process-output process))))

(define (wrap-output-process-port process opts)
  (let-keywords opts ((encoding #f)
                      (conversion-buffer-size 0))
    (if encoding
      (wrap-with-output-conversion (process-input process) encoding
                                  :buffer-size conversion-buffer-size)
      (process-input process))))

(define (handle-abnormal-exit on-abnormal-exit process)
  (case on-abnormal-exit
    ((:error) (%check-normal-exit process))
    ((:ignore))
    (else (unless (zero? (process-exit-status process))
            (on-abnormal-exit process)))))

