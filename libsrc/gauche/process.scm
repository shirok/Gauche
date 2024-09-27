;;;
;;; process.scm - process interface
;;;
;;;   Copyright (c) 2000-2024  Shiro Kawai  <shiro@acm.org>
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
  (use gauche.generator)
  (use gauche.connection)
  (use scheme.charset)
  (use srfi.13)
  (export <process> <process-abnormal-exit>
          run-process do-process do-process!
          process? process-alive? process-pid
          process-command process-input process-output process-error
          process-upstreams
          process-wait process-wait/poll process-wait-any process-exit-status
          process-send-signal process-kill process-stop process-continue
          process-shutdown
          process-list
          run-pipeline do-pipeline
          ;; process ports
          open-input-process-port   open-output-process-port
          call-with-input-process   call-with-output-process
          with-input-from-process   with-output-to-process
          call-with-process-io
          process-output->string    process-output->string-list
          ;; shell utilities (re-export from text.sh)
          shell-escape-string shell-tokenize-string
          ;; deprecated
          run-process-pipeline

          ;; connection interface (for process ports)
          <process-connection> make-process-connection
          connection-self-address connection-peer-address
          connection-input-port connection-output-port
          connection-shutdown connection-close
          connection-address-name
          ))
(select-module gauche.process)

;; Delay-load to avoid circular dependency
(autoload gauche.charconv wrap-with-input-conversion wrap-with-output-conversion)
(autoload gauche.uvector write-uvector)
(autoload gauche.threads make-thread thread-start!)

;; These two are moved to text.sh
(autoload text.sh shell-escape-string shell-tokenize-string)

;; Avoid build dependency issue
(autoload srfi.19
          make-time add-duration time>?)

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
   (upstreams :init-value '()) ; pipeline upstream #<process>es

   ;; class slot - keep reference to all processes whose status is unclaimed
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
;; NB: If P is the result of run-pipeline, (process-input p 'stdin) returns
;; the input pipe to the head of the pipeline, not the input pipe of
;; the process represented by P (which is, of course, connected to the
;; previous process and not available).
(define (process-input p :optional (name 'stdin))
  (if (and (eq? name 'stdin)
           (not (null? (~ p'upstreams))))
    (process-input (car (~ p'upstreams))) ; the input of the whole pipeline
    (cond [(assv name (~ p'in-pipes)) => cdr] [else #f])))

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

;; A typical use case---run process synchronously, returns #t for
;; success, #f for failure.
(define (do-process command :key (on-abnormal-exit #f) :allow-other-keys args)
  (let* ([raise-flag (ecase on-abnormal-exit
                       [(#f :exit-code) #f]
                       [(:error) #t])]
         [p (apply run-process command args)])
    (process-wait p #f raise-flag)
    (let1 status (process-exit-status p)
      (cond [(zero? status) #t]
            [(eq? on-abnormal-exit :exit-code)
             (if (sys-wait-exited? status)
               (sys-wait-exit-status status)
               (%check-normal-exit p))] ;this raises <process-abnormal-exit>
            [else #f]))))

;; Similar to do-process, but raise an error on abnormal exit.
;; This use case is typical, so we add a separate API.
(define (do-process! command . args)
  (apply do-process command :on-abnormal-exit :error
         (delete-keyword :on-abnormal-exit args)))

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
                       (sigmask #f) (directory #f) (detached #f)
                       (environment #t))
    (let* ([redirs (%canon-redirects redirects input output error)]
           [argv (map x->string command)]
           [proc (make <process> :command (car argv))]
           [argv (if host (%prepare-remote host argv directory) argv)]
           [dir  (if host #f directory)])
      (%check-directory dir)
      (receive (iomap toclose ipipes opipes tmpfiles)
          (if (pair? redirs)
            (%setup-iomap proc redirs)
            (values #f '() '() '() '()))
        (set! (~ proc'in-pipes) ipipes)
        (set! (~ proc'out-pipes) opipes)
        (if fork
          (let1 pid (sys-fork-and-exec (car argv) argv
                                       :iomap iomap :directory dir
                                       :sigmask (%ensure-mask sigmask)
                                       :detached detached
                                       :environment environment)
            (push! (ref proc 'processes) proc)
            (set!  (ref proc 'pid) pid)
            (dolist [p toclose]
              (if (input-port? p)
                (close-input-port p)
                (close-output-port p)))
            (when (and wait (not detached))
              ;; the following expr waits until the child exits
              (set! (ref proc 'status) (values-ref (sys-waitpid pid) 1))
              (update! (ref proc 'processes) (cut delete proc <>))
              (for-each sys-unlink tmpfiles))
            proc)
          (sys-exec (car argv) argv
                    :iomap iomap :directory dir
                    :sigmask (%ensure-mask sigmask)
                    :detached detached
                    :enrivonment environment))))))

(define (%canon-redirects redirects in out err)
  (rlet1 redirs
      (cond-list
       [#t @ redirects]
       [in  `(< 0 ,(if (eq? in  :pipe) 'stdin  in))]
       [out `(> 1 ,(if (eq? out :pipe) 'stdout out))]
       [err (case err
              [(:pipe)  '(> 2 stderr)]
              [(:merge) '(>& 2 1)]
              [else `(> 2 ,err)])])
    ;; Reject if the same pipe appears more than once in the reirect list.
    ;; We do allow the same file appears more than once; e.g. redirecting
    ;; both 1 and 2 to "/dev/null".
    (fold (^[redir seen]
            (%check-redirects redir)
            (let1 source-sink (caddr redir)
              (if (and (symbol? source-sink)
                       (not (memq source-sink '(:null :pipe))))
                (if (memq source-sink seen)
                  (errorf "Pipe name `~s' appears more than once in the \
                           redirection source or target" source-sink)
                  (cons source-sink seen))
                seen)))
          '() redirs)))

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
      [(<<) (unless (or (string? a2) (uvector? a2))
              (errorf "input redirection '<<' requires a string or a uvector, \
                       but got ~s for file descriptor ~s" a2 (cadr arg)))]
      [(<<<) #t]                        ;anything is ok
      [(> >>)
       (unless (or (string? a2) (symbol? a2) (eq? a2 :null) (integer? a2)
                   (and (output-port? a2) (port-file-number a2)))
         (errorf "output redirection '~a' requires a filename, a symbol, \
                  :null, integer file descriptor or file output port, \
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
          (rlet1 v (sys-normalize-pathname #"~(sys-tmpdir)/gauche"
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
  (define tmpfiles '()) ;list of temporary files

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
               [(eq? arg :null) (do-file dir fd *nulldev*)]
               [(symbol? arg)
                (receive (in out) (sys-pipe)
                  (if (eq? dir '<)
                    (do-pipe fd arg #t in out)
                    (do-pipe fd arg #f out in)))]
               [(or (port? arg) (integer? arg)) (push! iomap `(,fd . ,arg))]
               [else (error "invalid entry in process redirection" r)])]
        [(<< <<<)
         (set!-values [iomap toclose tmpfiles]
                      (%setup-<< fd do-file dir arg iomap toclose tmpfiles))]
        [(<& >&) (push! todup r)] ;; process dups later
        [else (error "invalid redirection" dir)])))

  ;; process dups
  (for-each (cut apply do-dup <>) todup)

  ;; if we ever redirects, make sure stdios are avaialble in child even
  ;; it is not explicitly specified.
  (unless (assv 0 iomap) (push! iomap '(0 . 0)))
  (unless (assv 1 iomap) (push! iomap '(1 . 1)))
  (unless (assv 2 iomap) (push! iomap '(2 . 2)))

  (values iomap toclose ipipes opipes tmpfiles))

;; Set up source of << and <<< redirection, used inside %setup-iomap.
(define (%setup-<< fd do-file dir arg iomap toclose tmpfiles)
  (define (write-arg o)
    (unwind-protect
        (cond [(eq? dir '<<<) (write arg o)]
              [(string? arg) (display arg o)]
              [else (write-uvector arg o)])
      (close-output-port o)))
  (receive (in out) (sys-pipe)
    (thread-start! (make-thread (cut write-arg out)))
    (values (acons fd in iomap) (cons in toclose) tmpfiles)))

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
                     "~s exited abnormally with exit code ~a"
                     process (sys-wait-exit-status status))]
            [(sys-wait-signaled? status)
             (errorf <process-abnormal-exit> :process process
                     "~s exited by signal ~s"
                     process (sys-signal-name (sys-wait-termsig status)))]
            [else ;; we shouldn't be here, but just in case...
             (errorf <process-abnormal-exit> :process process
                     "~s exited abnormally with status ~s"
                     process status)]))))

;; other basic interfaces
(define (process? obj) (is-a? obj <process>))
(define (process-alive? process)
  (and (not (process-exit-status process))
       (process-pid process)))
(define (process-list) (class-slot-ref <process> 'processes))

(define (process-upstreams p) (~ p'upstreams))

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
             ;; If we're the end of the pipeline, salvage status of
             ;; the upstream processes.  We won't raise error on non-zero
             ;; exit in those processes, but we do care nohang? flag.
             (dolist [p (~ process 'upstreams)]
               (process-wait p nohang?))
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

(define (process-wait/poll process :key (interval #e2e6) ; ns
                                        (max-wait #f) ; ns
                                        (continue-test #f)
                                        (raise-error? #f))
  (define limit (and max-wait
                     (add-duration (current-time)
                                   (make-time 'time-duration
                                              (modulo max-wait #e1e9)
                                              (quotient max-wait #e1e9)))))
  (let loop ([count 0])
    (cond [(and limit (time>? (current-time) limit)) #f]
          [(process-wait process #t raise-error?) #t]
          [(and continue-test (not (continue-test count))) #f]
          [else (sys-nanosleep interval) (loop (+ count 1))])))

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

(define (process-shutdown process
                          :key (ask #f)
                               (ask-interval #e50e6) ;ns
                               (ask-retry 1)
                               (signals `(,SIGTERM
                                          ,SIGTERM
                                          ,SIGKILL))
                               (signal-interval #e50e6) ;ns
                               )
  ;; each branch returns #t on successful termination
  (or (and ask
           (let loop ([count 0])
             (and (< count ask-retry)
                  (guard ([else #f])
                    (ask count)
                    #t)
                  (or (process-wait/poll process :max-wait ask-interval)
                      (loop (+ count 1))))))
      (let loop ([signals signals])
        (and (pair? signals)
             (begin (process-send-signal process (car signals)) #t)
             (or (process-wait/poll process :max-wait signal-interval)
                 (loop (cdr signals)))))))

;;-----------------------------------------------------------------
;; pipeline
;;

;; We might adopt scsh-like process forms eventually, but finding an
;; optimal DSL takes time.  Meanwhile, this intermediate-level API
;; would cover typical use case...
(define (run-pipeline commands
                      :key (input #f) (output #f) (error #f)
                      (wait #f) (sigmask #f) (directory #f) (detached #f)
                      (environment #f))
  (when (null? commands)
    (error "At least one command is required to run-command-pipeline"))
  (and-let1 offending (any (^c (and (not (pair? c)) (list c))) commands)
    (errorf "Command list contains non-list command line '~s': ~s"
            (car offending) commands))
  (let* ([pipe-pairs (map (^_ (call-with-values sys-pipe cons))
                          (cdr commands))]
         [cmds (map (^[cmdline in out]
                      `(,cmdline :input ,in :output ,out :error ,error
                                 :sigmask ,sigmask
                                 :directory ,directory :detached ,detached
                                 :environment ,environment))
                    commands
                    (cons input (map car pipe-pairs))
                    (fold-right cons (list output) (map cdr pipe-pairs)))]
         ;; We have to close output pipe after spawning the process, for
         ;; the process that reads from the pipe would wait until all the
         ;; ends are closed.  We have to treat the last command separately,
         ;; for we might :wait #t for it.
         [ps (map (cut apply run-process <>) (drop-right cmds 1))])
    (dolist [p pipe-pairs] (close-output-port (cdr p)))
    ;; Keep upstream processes in 'upstreams' slot.
    (rlet1 p (apply run-process (last cmds))
      (set! (~ p'upstreams) ps)
      (when wait (process-wait p)))))

(define (do-pipeline commands . args)
  (let* ([eflag (case (get-keyword :on-abnormal-exit args #f)
                  [(#f) #f]
                  [(:error) #t]
                  [else => (cut error
                                "Value for on-abnormal-exit argument \
                                 must be either #f or :error, but got:" <>)])]
         [p (apply run-pipeline commands
                   (delete-keyword :on-abnormal-exit args))])
    (process-wait p #f eflag)
    (zero? (process-exit-status p))))

;; For the backward compatibility.  DEPRECATED.
(define (run-process-pipeline . args) ; returns list of processes.
  (let1 p (apply run-pipeline args)
    (append (~ p'upstreams) (list p))))

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
  (let1 p (%apply-run-process command input :pipe err host rest)
    (values (wrap-input-process-port p rest) p)))

(define-syntax %with-handling-abnormal-exit
  (syntax-rules ()
    [(_ process closer on-abnormal-exit body ...)
     (let ([p process]
           [on-abnormal-exit on-abnormal-exit])
       (guard (e [else
                  (closer)
                  (process-wait p)
                  (case on-abnormal-exit
                    [(:error) (%check-normal-exit p) (raise e)]
                    [(:ignore) (raise e)]
                    [(#f) #f]
                    [else (unless (zero? (process-exit-status p))
                            (on-abnormal-exit p))])])
         (receive r (begin body ...)
           (closer)
           (process-wait p)
           (case on-abnormal-exit
             [(:error) (%check-normal-exit p) (apply values r)]
             [(:ignore) (apply values r)]
             [(#f) (and (zero? (process-exit-status p))
                        (apply values r))]
             [else (unless (zero? (process-exit-status p))
                     (on-abnormal-exit p))]))))]))

(define (call-with-input-process command proc :key (input *nulldev*)
                                 ((:error err) #f) (host #f)
                                 (on-abnormal-exit :error)
                                 :allow-other-keys rest)
  (let* ([p (%apply-run-process command input :pipe err host rest)]
         [i (wrap-input-process-port p rest)])
    (%with-handling-abnormal-exit
     p (^[] (close-input-port i)) on-abnormal-exit
     (proc i))))

(define (with-input-from-process command thunk . opts)
  (apply call-with-input-process command
         (cut with-input-from-port <> thunk)
         opts))

(define (open-output-process-port command :key (output *nulldev*)
                                  ((:error err) #f) (host #f)
                                  :allow-other-keys rest)
  (let1 p (%apply-run-process command :pipe output err host rest)
    (values (wrap-output-process-port p rest) p)))

(define (call-with-output-process command proc :key (output *nulldev*)
                                  ((:error err) #f) (host #f)
                                  (on-abnormal-exit :error)
                                  :allow-other-keys rest)
  (let* ([p (%apply-run-process command :pipe output err host rest)]
         [o (wrap-output-process-port p rest)])
    (%with-handling-abnormal-exit
     p (^[] (close-output-port o)) on-abnormal-exit
     (proc o))))

(define (with-output-to-process command thunk . opts)
  (apply call-with-output-process command
         (cut with-output-to-port <> thunk)
         opts))

(define (call-with-process-io command proc :key ((:error err) #f)
                              (host #f) (on-abnormal-exit :error)
                              :allow-other-keys rest)
  (let* ([p (%apply-run-process command :pipe :pipe err host rest)]
         [i (wrap-input-process-port p rest)]
         [o (wrap-output-process-port p rest)])
    (%with-handling-abnormal-exit
     p (^[] (close-output-port o) (close-input-port i)) on-abnormal-exit
     (proc i o))))

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

;;----------------------------------------------------------------------
;; Internal utilities for process ports
;;

;; COMMAND can be a string, a list, or a list of lists.
;; If it is a string, call it via /bin/sh.
;; If it is a simple list, treat it as args.
;; If it is a nested list, cuse run-pipeline.
;; NB: on Windows we need to use cmd.exe.  But its command-line parsing
;; rule is too broken to use reliably.  Another possibility is to implement
;; much of high-level /bin/sh functionalities in Scheme, so that we can
;; provide consistent behavior.  Something to think about.
;; NB: We explicitly remove :encoding and :conversion-buffer-size argument
;; from opts.  These can be passed down from high-level APIs but only used
;; by wrap-*-process-port.
(define (%apply-run-process command stdin stdout stderr host opts)
  (define rest (delete-keywords '(:encoding :conversion-buffer-size) opts))
  (define (rc cmd)
    (apply run-process cmd :input stdin :output stdout :host host
           (cond [(or (string? stderr) (keyword? stderr))
                  `(:error ,stderr ,@rest)]
                 [(or (undefined? stderr) (not stderr)) rest]
                 [else (error "Invalid :error argument:" stderr)])))
  (cond [(string? command)
         (rc (cond-expand [gauche.os.windows `("cmd.exe" "/c" ,command)]
                          [else              `("/bin/sh" "-c" ,command)]))]
        [(and (list? command) (every list? command))
         (apply run-pipeline command
                :input stdin :output stdout
                (cond [(string? stderr) `(:error ,stderr ,@rest)]
                      [else rest]))]
        [(list? command) (rc command)]
        [else (error "Bad command spec" command)]))

;; Possibly wrap the process port by a conversion port
(define (wrap-input-process-port process opts)
  (let-keywords opts ([encoding #f]
                      [conversion-buffer-size 0]
                      . opts)
    (if encoding
      (wrap-with-input-conversion (process-output process) encoding
                                  :buffer-size conversion-buffer-size)
      (process-output process))))

(define (wrap-output-process-port process opts)
  (let-keywords opts ([encoding #f]
                      [conversion-buffer-size 0]
                      . opts)
    (if encoding
      (wrap-with-output-conversion (process-input process) encoding
                                  :buffer-size conversion-buffer-size)
      (process-input process))))

;;----------------------------------------------------------------------
;; Process connection
;;

;; This allows to treat communication with external process as a connection.

(define-class <process-connection> (<connection>)
  ((process  :init-keyword :process)))

(define (make-process-connection process-or-spec)
  (cond [(process? process-or-spec)
         (make <process-connection> :process process-or-spec)]
        [(list? process-or-spec)
         (let1 p (run-process process-or-spec :input :pipe :output :pipe)
           (make <process-connection> :process p))]
        [else
         (error "A <process> or (cmd arg ...) is expected, but got:"
                process-or-spec)]))

(define-method connection-self-address ((c <process-connection>))
  #"Process ~(car (command-line)) ~(sys-getpid)")
(define-method connection-peer-address ((c <process-connection>))
  (let1 p (~ c'process)
    #"Process ~(~ p 'command) ~(~ p 'pid)"))
(define-method connection-input-port ((c <process-connection>))
  (process-output (~ c'process)))
(define-method connection-output-port ((c <process-connection>))
  (process-input (~ c'process)))

(define (%close-ports p how)
  (ecase how
    [(read)  (close-port (process-output p))]
    [(write) (close-port (process-input p))]
    [(both)  (close-port (process-input p))
             (close-port (process-output p))]))

(define-method connection-shutdown ((c <process-connection>) how)
  (let1 p (~ c'process)
    (%close-ports p how)
    (when (and (port-closed? (process-input p))
               (port-closed? (process-output p))
               (not (process-exit-status p)))
      ;; Usually, closing process-input should cause the process to exit.
      ;; We try 150ms after closing, then send signals.
      (process-shutdown p
                        :ask identity ;; doesn't matter
                        :ask-interval #e150e6 ; 50ms
                        :signal-interval #e100e6))))

(define-method connection-close ((c <process-connection>))
  (%close-ports (~ c'process) 'both))
