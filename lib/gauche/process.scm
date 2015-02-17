;;;
;;; process.scm - process interface
;;;
;;;   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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
(autoload gauche.uvector
          write-block)

(define-class <process> ()
  ((pid       :init-value -1 :getter process-pid)
   (command   :init-value #f :getter process-command :init-keyword :command)
   (status    :init-value #f :getter process-exit-status)
   (in-pipes  :init-value '())          ;((<fd> . #<port>) ...)
   (out-pipes :init-value '())          ;ditto
   (input     :allocation :virtual :slot-ref (^o (process-input o 0)))
   (output    :allocation :virtual :slot-ref (^o (process-output o 1)))
   (error     :allocation :virtual :slot-ref (^o (process-output o 2)))
   (extra-inputs  :initform '())
   (extra-outputs :initform '())
   (processes :allocation :class :initform '())
  ))

;; Process I/O management
;; 'in-pipes' and 'out-pipes' slots contains an assoc list of
;; ((<name> . <port>) ...), where <name> is a symbol and <port> is
;; a pipe port connected to the child's I/O.  The direction of the port
;; is opposite to the direction of fd (e.g. child's stdin is connected
;; to an output port).
;; For the backward compatibility, (process-input p) and (process-output p)
;; returns pipes connected to child's stdin and stdout, respectively.
(define (process-input p :optional (name 'stdin))
  (cond [(assv name (~ p'in-pipes)) => cdr] [else #f]))

(define (process-output p :optional (name 'stdout))
  (cond [(assv name (~ p'out-pipes)) => cdr] [else #f]))

(define (process-error p) ; for the compatibility
  (process-output p 'stderr))

;; When the process exits abnormally, this error is thrown.
(define-condition-type <process-abnormal-exit> <error> #f (process))

(define-method write-object ((p <process>) port)
  (format port "#<process ~a ~s ~a>"
          (process-pid p)
          (process-command p)
          (if (process-alive? p)
            "active"
            "inactive")))

;; null device (avoid depending on file.util)
(define *nulldev* (cond-expand (gauche.os.windows "NUL") (else "/dev/null")))

;; create process and run.
(define (run-process command . args)
  (if (not (list? command))
    (%run-process-old command args) ;; backward compatibility
    (%run-process-new command args)))

;; Note: I/O redirection
;;  'Redirects' keyword argument is a generic way to wire child's I/Os.
;;  It takes a list of <io-spec>s, where each <io-spec> can be one of
;;  the followings:
;;
;;   (< fd source)   Make child's input FD read from SOURCE.
;;   (<< fd value)   Make child's input FD read from string VALUE.
;;   (<<< fd value)  Make child's input FD read from (write-to-string VALUE).
;;   (<& fd0 fd1)    Make child's input FD0 duplicate of child's input FD1.
;;   (> fd sink)     Make child's output FD write to SINK.
;;   (>> fd sink)    Make child's output FD write to SINK in append mode.
;;   (>& fd0 fd1)    Make child's output FD0 duplicate of child's output FD1.
;;
;;  * FD, FD0 and FD1 are nonnegative integers refer to the file
;;    descriptor in the child process.
;;  * SOURCE can be either one of the followings:
;;     a string - SOURCE names a file.  It is opened for reading and the child
;;                process can reads the file from FD.
;;     a symbol - A unidirectional pipe is created, whose 'reader' end
;;                is connected to the child's FD, and whose 'writer' end
;;                is available as an output port by
;;                (process-input PROCESS SOURCE).
;;     :null    - The child's FD is connected to the null device.
;;     an integer - It should specify a parent's file descriptor opened for
;;                input.  The child sees the duped file descriptor as FD.
;;     a file input port - The underlying file descriptor is duped into FD
;;                in the child process.
;;  * SINK can be either one of the followings:
;;     a string - Names a file.  It is opened for reading and the child process
;;                can writes to it via FD.
;;     a symbol - A unidirectional pipe is created, whose 'writer' end is
;;                connected to the child's FD, and whose 'reader' end is
;;                available as an input port by
;;                (process-output PROCESS SINK).
;;     :null    - the child's FD is connected to the null device.
;;     an integer - It should specify a parent's file descriptor opened for
;;                output.  The child sees the dupled file descriptor as FD.
;;     A file output port - The underlying file descriptor is dupled into FD
;;                in the child process.
;;
;;  :input source, :output sink and :error sink keyword arguments
;;  are a shorthand notation for (< 0 source), (> 1 sink) and (> 2 sink).
;;  furthermore, source and sink in these arguments can be :pipe, which
;;  will be equivalent to (< 0 stdin), (> 1 stdout) and (> 2 stderr),
;;  respectively.

(define (%run-process-new command args)
  (let-keywords* args ((input  #f) (output #f) (error  #f)
                       (redirects '())
                       (wait   #f) (fork   #t)
                       (host   #f)    ;remote execution
                       (sigmask #f) (directory #f) (detached #f))
    (let* ([redirs (%canon-redirects redirects input output error)]
           [argv (map x->string command)]
           [proc (make <process> :command (car argv))]
           [argv (if host (%prepare-remote host argv directory) argv)]
           [dir  (if host #f directory)])
      (%check-directory dir)
      (receive (iomap toclose ipipes opipes)
          (if (pair? redirs)
            (%setup-iomap proc redirs)
            (values #f '() '() '()))
        (set! (~ proc'in-pipes) ipipes)
        (set! (~ proc'out-pipes) opipes)
        (if fork
          (let1 pid (sys-fork-and-exec (car argv) argv
                                       :iomap iomap :directory dir
                                       :sigmask (%ensure-mask sigmask)
                                       :detached detached)
            (push! (ref proc 'processes) proc)
            (set!  (ref proc 'pid) pid)
            (dolist (p toclose)
              (if (input-port? p)
                (close-input-port p)
                (close-output-port p)))
            (when (and wait (not detached))
              ;; the following expr waits until the child exits
              (set! (ref proc 'status) (values-ref (sys-waitpid pid) 1))
              (update! (ref proc 'processes) (cut delete proc <>)))
            proc)
          (sys-exec (car argv) argv
                    :iomap iomap :directory dir
                    :sigmask (%ensure-mask sigmask)
                    :detached detached))))))

(define (%canon-redirects redirects in out err)
  (rlet1 redirs
      `(,@redirects
        ,@(if in  `((< 0 ,(if (eq? in  :pipe) 'stdin  in))) '())
        ,@(if out `((> 1 ,(if (eq? out :pipe) 'stdout out))) '())
        ,@(if err `((> 2 ,(if (eq? err :pipe) 'stderr err))) '()))
    (for-each %check-redirects redirs)))

(define (%check-redirects arg)
  (unless (and (pair? arg) (pair? (cdr arg)) (pair? (cddr arg))
               (null? (cdddr arg))
               (integer? (cadr arg)))
    (errorf "invalid redirection entry: ~s" arg))
  (let1 a2 (caddr arg)
    (case (car arg)
      [(<) (unless (or (string? a2) (symbol? a2) (eq? a2 :null) (integer? a2)
                       (and (input-port? a2) (port-file-number a2)))
             (errorf "input redirection '<' requires a filename, a symbol, \
                      :null, integer file descriptor or file input port, \
                      but got ~s for file descriptor ~s"
                     a2 (cadr arg)))]
      [(<<) (unless (string? a2)
              (errorf "input redirection '<<' requires a string, but got ~s \
                       for file descriptor ~s" a2 (cadr arg)))]
      [(<<<) #t]                        ;anything is ok
      [(> >>)
       (unless (or (string? a2) (symbol? a2) (eq? a2 :null) (integer? a2)
                   (and (output-port? a2) (port-file-number a2)))
         (errorf "output redirection '~a' requires a filename, a symbol, \
                  :null, integer file descriptor or file input port, \
                  but got ~s for file descriptor ~s"
                 (car arg) a2 (cadr arg)))]
      [(<& >&) (unless (integer? a2)
                 (errorf "redirection '~a' requires an integer file \
                          descriptor, but got ~s" (car arg) a2))]
      [else (errorf "invalid redirection entry: ~s" arg)])))

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
  (rxmatch-let (#/^(?:([\w-]+):)?(?:([\w-]+)@)?([-\w.]+)(?::(\d+))?$/ host)
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

;; Returns a temporary path prefix (suitable for sys-mkstemp).
;; We don't use temporary-directory to avoid depending on file.util.
(define %temp-path-prefix
  (let1 val #f
    (lambda ()
      (or val
          (rlet1 v (sys-normalize-pathname #`",(sys-tmpdir)/gauche"
                                           :canonicalize #t)
            (set! val v))))))

;; Build I/O map
(define (%setup-iomap proc redirs)

  (define toclose '())  ;list of ports to be closed in parent
  (define todup '())    ;list of (>& a b) and (<& a b)
  (define ipipes '())   ;list of (name . port)
  (define opipes '())   ;list of (name . port)
  (define iomap '())    ;list of (fd . port/fd)
  (define seen '())     ;list of seen fds

  (define (do-file dir fd arg)
    (let1 p (case dir
              [(<) (open-input-file arg)]
              [(>) (open-output-file arg)]
              [(>>) (open-output-file arg :if-exists :append)])
      (push! toclose p)
      (push! iomap `(,fd . ,p))))

  (define (do-pipe fd arg in? child-end parent-end)
    (push! toclose child-end)
    (if in?
      (push! ipipes `(,arg . ,parent-end))
      (push! opipes `(,arg . ,parent-end)))
    (push! iomap `(,fd . ,child-end)))

  (define (do-dup dir fd0 fd1)
    ;; (<& fd0 fd1) or (>& fd0 fd1).  Note that fd0 and fd1 are both
    ;; child's fds.  fd1 may be remapped from the current process, so
    ;; we first search iomap.  If it hasn't remapped, we transfer the
    ;; current process's fd1 to the child.
    ;; TODO: need to check the direction of
    (if-let1 p (assv fd1 iomap)
      (push! iomap `(,fd0 . ,(cdr p)))
      (guard (e [(<system-error> e)
                 (errorf "redirection spec ~s refers to unopened fd ~a"
                        `(,dir ,fd0 ,fd1) fd1)])
        ;; See if fd1 is open in the current process.   Note that
        ;; open-*-fd-port succeeds even the given fd is bogus.  We have to
        ;; try dup2 it into a dummy port to see if fd1 is valid.
        ((if (eq? dir '<&) call-with-input-file call-with-output-file)
         *nulldev*
         (^[dummy-port]
           (let1 p ((if (eq? dir '<&) open-input-fd-port open-output-fd-port)
                    fd1)
             (port-fd-dup! dummy-port p))))
        (push! iomap `(,fd0 . ,fd1)))))

  (dolist [r redirs]
    (let ([dir (car r)] [fd (cadr r)] [arg (caddr r)])
      (when (memv fd seen)
        (errorf "duplicates in redirection file descriptor (~a): ~s" fd redirs))      (push! seen fd)
      (case dir
        [(< > >>)
         (cond [(string? arg) (do-file dir fd arg)]
               [(symbol? arg)
                (receive (in out) (sys-pipe)
                  (if (eq? dir '<)
                    (do-pipe fd arg #t in out)
                    (do-pipe fd arg #f out in)))]
               [(eq? arg :null) (do-file dir fd *nulldev*)]
               [(or (port? arg) (integer? arg)) (push! iomap `(,fd . ,arg))]
               [else (error "invalid entry in process redirection" r)])]
        [(<< <<<)
         (letrec ([write-arg (^o (unwind-protect
                                     (cond [(eq? dir '<<<) (write arg o)]
                                           [(string? arg) (display arg o)]
                                           [else (write-block arg o)])
                                   (close-output-port o)))])
           (cond-expand
            [gauche.sys.pthreads
             (receive (in out) (sys-pipe)
               (push! iomap `(,fd . ,in))
               (thread-start! (make-thread (cut write-arg out))))]
            [else
             (receive (out nam) (sys-mkstemp (%temp-path-prefix))
               (write-arg out)
               (do-file '< fd nam))]))]
        [(<& >&) (push! todup r)] ;; process dups later
        [else (error "invalid redirection" dir)])))

  ;; process dups
  (for-each (cut apply do-dup <>) todup)

  ;; if we ever redirects, make sure stdios are avaialble in child even
  ;; it is not explicitly specified.
  (unless (assv 0 iomap) (push! iomap '(0 . 0)))
  (unless (assv 1 iomap) (push! iomap '(1 . 1)))
  (unless (assv 2 iomap) (push! iomap '(2 . 2)))

  (values iomap toclose ipipes opipes))

(define (%ensure-mask mask)
  (cond
   [(is-a? mask <sys-sigset>) mask]
   [(and (list? mask) (every integer? mask))
    (fold (^[sig m] (sys-sigset-add! m sig) m) (make <sys-sigset>) mask)]
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
              (and-let* ((p (find (^[pp] (eqv? (process-pid pp) pid))
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
         (^p (with-port-locking p
               (^[] (string-join (string-tokenize (port->string p)) " "))))
         opts))

(define (process-output->string-list command . opts)
  (apply call-with-input-process command port->string-list opts))

;;---------------------------------------------------------------------
;; Shell utility
;;

(define (shell-escape-string str)
  (cond-expand
   [gauche.os.windows
    ;; This is supported in src/libsys.scm.  See the comment in it.
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
         (cond [(string? command)
                (cond-expand [gauche.os.windows `("cmd" "/c" ,command)]
                             [else        `("/bin/sh" "-c" ,command)])]
               [(list? command) command]
               [else (error "Bad command spec" command)])
         :input stdin :output stdout :host host
         (cond [(string? stderr) `(:error ,stderr)]
               [else '()])))

;; Possibly wrap the process port by a conversion port
(define (wrap-input-process-port process opts)
  (let-keywords opts ([encoding #f]
                      [conversion-buffer-size 0])
    (if encoding
      (wrap-with-input-conversion (process-output process) encoding
                                  :buffer-size conversion-buffer-size)
      (process-output process))))

(define (wrap-output-process-port process opts)
  (let-keywords opts ([encoding #f]
                      [conversion-buffer-size 0])
    (if encoding
      (wrap-with-output-conversion (process-input process) encoding
                                  :buffer-size conversion-buffer-size)
      (process-input process))))

(define (handle-abnormal-exit on-abnormal-exit process)
  (case on-abnormal-exit
    [(:error) (%check-normal-exit process)]
    [(:ignore)]
    [else (unless (zero? (process-exit-status process))
            (on-abnormal-exit process))]))

