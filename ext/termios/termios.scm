;;;
;;; termios - termios interface
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

(define-module gauche.termios
  (use gauche.uvector)
  (export-all)
  )
(select-module gauche.termios)

(inline-stub
 "#include \"gauche-termios.h\""

 ;;---------------------------------------------------------------------
 ;; termios.h

 (define-type <sys-termios> "ScmSysTermios*")
 (define-type <sys-sigset>  "ScmSysSigset*")

 (when "!defined(GAUCHE_WINDOWS)"

   (define-enum TCSANOW)
   (define-enum TCSADRAIN)
   (define-enum TCSAFLUSH)
   (define-enum TCIFLUSH)
   (define-enum TCOFLUSH)
   (define-enum TCIOFLUSH)
   (define-enum TCOOFF)
   (define-enum TCOON)
   (define-enum TCIOFF)
   (define-enum TCION)

   (define-enum B0)
   (define-enum B50)
   (define-enum B75)
   (define-enum B110)
   (define-enum B134)
   (define-enum B150)
   (define-enum B200)
   (define-enum B300)
   (define-enum B600)
   (define-enum B1200)
   (define-enum B1800)
   (define-enum B2400)
   (define-enum B4800)
   (define-enum B9600)
   (define-enum B19200)
   (define-enum B38400)

   (define-cproc sys-tcgetattr (port-or-fd)
     (let* ([fd::int (Scm_GetPortFd port-or-fd TRUE)]
            [term::ScmSysTermios* (SCM_SYS_TERMIOS (Scm_MakeSysTermios))])
       (when (< (tcgetattr fd (& (-> term term))) 0)
         (Scm_SysError "tcgetattr failed"))
       (return (SCM_OBJ term))))

   (define-cproc sys-tcsetattr (port-or-fd option::<fixnum> term::<sys-termios>)
     ::<void>
     (let* ([fd::int (Scm_GetPortFd port-or-fd TRUE)])
       (when (< (tcsetattr fd option (& (-> term term))) 0)
         (Scm_SysError "tcsetattr failed"))))

   (define-cproc sys-tcsendbreak (port-or-fd duration::<fixnum>) ::<boolean>
     (let* ([fd::int (Scm_GetPortFd port-or-fd TRUE)])
       (return (>= (tcsendbreak fd duration) 0))))

   (define-cproc sys-tcdrain (port-or-fd) ::<void>
     (let* ([fd::int (Scm_GetPortFd port-or-fd TRUE)])
       (when (< (tcdrain fd) 0) (Scm_SysError "tcdrain failed"))))

   (define-cproc sys-tcflush (port-or-fd queue::<int>) ::<void>
     (let* ([fd::int (Scm_GetPortFd port-or-fd TRUE)])
       (when (< (tcflush fd queue) 0) (Scm_SysError "tcflush failed"))))

   (define-cproc sys-tcflow (port-or-fd action::<int>) ::<void>
     (let* ([fd::int (Scm_GetPortFd port-or-fd TRUE)])
       (when (< (tcflow fd action) 0) (Scm_SysError "tcflow failed"))))

   (define-cproc sys-tcgetpgrp (port-or-fd) ::<int>
     (let* ([fd::int (Scm_GetPortFd port-or-fd TRUE)]
            [r::pid_t (tcgetpgrp fd)])
       (when (< r 0) (Scm_SysError "tcgetpgrp failed"))
       (return r)))

   (define-cproc sys-tcsetpgrp (port-or-fd pgrp::<int>) ::<void>
     (let* ([fd::int (Scm_GetPortFd port-or-fd TRUE)])
       (when (< (tcsetpgrp fd pgrp) 0) (Scm_SysError "tcsetpgrp failed"))))

   (define-cproc sys-cfgetispeed (term::<sys-termios>) ::<int>
     (let* ([s::speed_t (cfgetispeed (& (-> term term)))])
       (return s)))

   (define-cproc sys-cfsetispeed (term::<sys-termios> speed::<int>) ::<void>
     (when (< (cfsetispeed (& (-> term term)) speed) 0)
       (Scm_SysError "cfsetispeed failed")))

   (define-cproc sys-cfgetospeed (term::<sys-termios>) ::<int>
     (let* ([s::speed_t (cfgetospeed (& (-> term term)))])
       (return s)))

   (define-cproc sys-cfsetospeed (term::<sys-termios> speed::<int>) ::<void>
     (when (< (cfsetospeed (& (-> term term)) speed) 0)
       (Scm_SysError "cfsetospeed failed")))

   ;; Returns fresh copy of <sys-termios>
   ;; The fields of struct termios are system-dependent, and it may contain
   ;; more fields than specified in POSIX.  However, we assume they're safe
   ;; for bitwise-copy.
   (define-cproc sys-termios-copy (src::<sys-termios>)
     (let* ([dest::(ScmSysTermios*) (SCM_SYS_TERMIOS (Scm_MakeSysTermios))])
       (set! (-> dest term) (-> src term))
       (return (SCM_OBJ dest))))

   ;; pty interface
   (when "defined(HAVE_OPENPTY)"
     (define-cproc sys-openpty (:optional term) Scm_Openpty)
     (initcode "Scm_AddFeature(\"gauche.sys.openpty\", NULL);")
     )
   (when "defined(HAVE_FORKPTY)"
     (define-cproc sys-forkpty (:optional term) Scm_Forkpty)
     (define-cproc sys-forkpty-and-exec (program::<string> args::<list>
                                                           :key (iomap ()) term (sigmask::<sys-sigset>? #f))
       Scm_ForkptyAndExec)
     (initcode "Scm_AddFeature(\"gauche.sys.forkpty\", NULL);")
     )

   ) ;; !defined(GAUCHE_WINDOWS)

 (initcode (Scm_Init_termios))
 ) ;; inline-stub

;;
;; High-level utilities
;;

(without-precompiling

 (cond-expand
  [gauche.os.windows (use os.windows)]
  [else])

 ;; NB: on windows, this only works with iport==#f.
 (define (without-echoing iport proc)
   (cond [(not iport) ;; open tty
          (call-with-input-file
              (cond-expand [gauche.os.windows "CON"] [else "/dev/tty"])
            (cut without-echoing <> proc))]
         [(sys-isatty iport)
          (let ()
            (cond-expand
             [gauche.os.windows
              (define ihandle (sys-get-std-handle STD_INPUT_HANDLE))
              (define orig-mode (sys-get-console-mode ihandle))
              (define (echo-off)
                (sys-set-console-mode ihandle
                                      (logand orig-mode
                                              (lognot ENABLE_ECHO_INPUT))))
              (define (echo-on)
                (sys-set-console-mode ihandle orig-mode))]
             [else
              (begin
                (define attr (sys-tcgetattr iport))
                (define lflag-save (ref attr'lflag))
                (define (echo-off)
                  (set! (ref attr'lflag)
                        (logand (ref attr'lflag)
                                (lognot (logior ECHO ICANON ISIG))))
                  (sys-tcsetattr iport TCSANOW attr))
                (define (echo-on)
                  (set! (ref attr'lflag) lflag-save)
                  (sys-tcsetattr iport TCSANOW attr)))])
            (unwind-protect (begin (echo-off) (proc iport)) (echo-on)))]
         [else (proc iport)]))

 #|
 ;; sample
 (define (get-password)
 (with-output-to-file
 (cond-expand [gauche.os.windows "CON"] [else "/dev/tty"])
 (lambda () (display "Password: ") (flush)))
 (without-echoing #f read-line))
 |#

 ;; mode should be either one of 'cooked, 'rare or 'raw
 ;; NB: Although we work on the given port and also calls PROC with port,
 ;; what's changed is actually a device connected to the port.  There can
 ;; be more than one port connected to the same device, and I/O thru those
 ;; ports would also be affected.
 ;; TODO: What to do with Windows console?
 (define (with-terminal-mode port mode proc :optional (cleanup #f))
   (cond-expand
    [gauche.os.windows
     ;; WRITEME
     (proc port)]
    [else
     (cond
      [(sys-isatty port)
       (let ()
         (define saved-attr (sys-tcgetattr port))
         (define new-attr
           (rlet1 attr (sys-termios-copy saved-attr)
             (case mode
               [(raw)
                (set! (~ attr'iflag)
                      (logand (~ attr'iflag)
                              (lognot (logior BRKINT ICRNL INPCK ISTRIP IXON))))
                (set! (~ attr'oflag) (logand (~ attr'oflag) (lognot OPOST)))
                (set! (~ attr'cflag)
                      (logior (logand (~ attr'cflag)
                                      (lognot (logior CSIZE PARENB)))
                              CS8))
                (set! (~ attr'lflag)
                      (logand (~ attr'lflag)
                              (lognot (logior ECHO ICANON IEXTEN ISIG))))]
               [(rare)
                (set! (~ attr'iflag)
                      (logior BRKINT
                              (logand (~ attr'iflag)
                                      (lognot (logior ICRNL INPCK ISTRIP IXON)))))
                (set! (~ attr'oflag) (logand (~ attr'oflag) (lognot OPOST)))
                (set! (~ attr'cflag)
                      (logior (logand (~ attr'cflag)
                                      (lognot (logior CSIZE PARENB)))
                              CS8))
                (set! (~ attr'lflag)
                      (logior ISIG
                              (logand (~ attr'lflag)
                                      (lognot (logior ECHO ICANON IEXTEN)))))]
               [(cooked)
                (set! (~ attr'iflag)
                      (logior (~ attr'iflag)
                              BRKINT ICRNL INPCK ISTRIP IXON))
                (set! (~ attr'oflag) (logior (~ attr'oflag) OPOST))
                (set! (~ attr'lflag)
                      (logior (~ attr'lflag) ECHO ICANON IEXTEN ISIG))]
               [else
                (error "terminal mode needs to be one of cooked, rare or raw, \
                          but got:" mode)])))
         (define (set)
           (sys-tcsetattr port TCSANOW new-attr))
         (define (reset)
           (sys-tcsetattr port TCSANOW saved-attr)
           (when cleanup (cleanup)))
         (unwind-protect (begin (set) (proc port)) (reset)))]
      [else (proc port)])]))
 ) ; without-precompiling
