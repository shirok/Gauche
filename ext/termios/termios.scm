;;;
;;; termios - termios interface
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

(define-module gauche.termios
  (use gauche.uvector)
  (export has-windows-console? without-echoing with-terminal-mode)
  )
(select-module gauche.termios)

(without-precompiling
 (cond-expand
  [gauche.os.windows
   (export msys-get-stty)]
  [else
   (export-if-defined
    <sys-termios-meta> <sys-termios> B0 B110 B115200 B1200 B134 B150
    B1800 B19200 B200 B230400 B2400 B300 B38400 B4800 B50 B57600
    B600 B75 B9600 BRKINT BS0 BS1 BSDLY CIBAUD CLOCAL CR0 CR1 CR2
    CR3 CRDLY CREAD CRTSCTS CS5 CS6 CS7 CS8 CSIZE CSTOPB ECHO
    ECHOCTL ECHOE ECHOK ECHOKE ECHONL ECHOPRT FF0 FF1 FFDLY HUPCL
    ICANON ICRNL IEXTEN IGNBRK IGNCR IGNPAR IMAXBEL INLCR INPCK
    ISIG ISTRIP IUCLC IXANY IXOFF IXON NCCS NL0 NL1 NLDLY NOFLSH
    OCRNL OFDEL OFILL OLCUC ONLCR ONLRET ONOCR OPOST PARENB PARODD
    PENDIN TCIFLUSH TCIOFF TCIOFLUSH TCION TCOFLUSH TCOOFF TCOON
    TCSADRAIN TCSAFLUSH TCSANOW TOSTOP VDISCARD VEOF VEOL VEOL2
    VERASE VINTR VKILL VLNEXT VMIN VQUIT VREPRINT VSTART VSTOP
    VSUSP VSWTC VT0 VT1 VTDLY VTIME VWERASE XCASE _POSIX_VDISABLE
    sys-cfgetispeed sys-cfgetospeed
    sys-cfsetispeed sys-cfsetospeed sys-forkpty
    sys-forkpty-and-exec sys-openpty sys-tcdrain sys-tcflow
    sys-tcflush sys-tcgetattr sys-tcgetpgrp sys-tcsendbreak
    sys-tcsetattr sys-tcsetpgrp sys-termios-copy)])
 )

 ;;---------------------------------------------------------------------
 ;; termios.h

(inline-stub
 (.unless (defined GAUCHE_WINDOWS)

   (declcode
    (.include <gauche/priv/portP.h>)
    (.include <termios.h>)
    (.when (defined HAVE_PTY_H)
      (.include <pty.h>))
    (.when (defined HAVE_UTIL_H)
      (.include <util.h>))
    ;; libutil.h is superseded by bsd/libutil.h
    (.if (defined HAVE_BSD_LIBUTIL_H)
         (.include <bsd/libutil.h>)
         (.if (defined HAVE_LIBUTIL_H)
              (.include <libutil.h>)))
    (.when (defined HAVE_UNISTD_H)
      (.include <unistd.h>)))

   ;; Constants for termios.  Non-POSIX symbols are defined conditionally.

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

   ;; c_iflag masks
   (define-enum IGNBRK)
   (define-enum BRKINT)
   (define-enum IGNPAR)
   (define-enum INPCK)
   (define-enum ISTRIP)
   (define-enum INLCR)
   (define-enum IGNCR)
   (define-enum ICRNL)
   (define-enum IXON)
   (define-enum IXOFF)
   (define-enum-conditionally IXANY)
   (define-enum-conditionally IUCLC)
   (define-enum-conditionally IMAXBEL)

   ;; c_oflag masks
   (define-enum OPOST)
   (define-enum-conditionally OLCUC)
   (define-enum-conditionally ONLCR)
   (define-enum-conditionally OCRNL)
   (define-enum-conditionally ONOCR)
   (define-enum-conditionally ONLRET)
   (define-enum-conditionally OFILL)
   (define-enum-conditionally OFDEL)
   (define-enum-conditionally NLDLY)
   (define-enum-conditionally NL0)
   (define-enum-conditionally NL1)
   (define-enum-conditionally CRDLY)
   (define-enum-conditionally CR0)
   (define-enum-conditionally CR1)
   (define-enum-conditionally CR2)
   (define-enum-conditionally CR3)
   (define-enum-conditionally BSDLY)
   (define-enum-conditionally BS0)
   (define-enum-conditionally BS1)
   (define-enum-conditionally VTDLY)
   (define-enum-conditionally VT0)
   (define-enum-conditionally VT1)
   (define-enum-conditionally FFDLY)
   (define-enum-conditionally FF0)
   (define-enum-conditionally FF1)

   ;; c_cflag masks
   (define-enum CLOCAL)
   (define-enum CREAD)
   (define-enum CSIZE)
   (define-enum CS5)
   (define-enum CS6)
   (define-enum CS7)
   (define-enum CS8)
   (define-enum CSTOPB)
   (define-enum HUPCL)
   (define-enum PARENB)
   (define-enum PARODD)
   (define-enum-conditionally CIBAUD)
   (define-enum-conditionally CRTSCTS)

   ;; c_lflag masks
   (define-enum ECHO)
   (define-enum ECHOE)
   (define-enum ECHOK)
   (define-enum ECHONL)
   (define-enum ICANON)
   (define-enum ISIG)
   (define-enum NOFLSH)
   (define-enum TOSTOP)
   (define-enum IEXTEN)
   (define-enum-conditionally XCASE)
   (define-enum-conditionally ECHOCTL)
   (define-enum-conditionally ECHOPRT)
   (define-enum-conditionally ECHOKE)
   (define-enum-conditionally FLUSH0)
   (define-enum-conditionally PENDIN)

   ;; c_cc size
   (define-enum NCCS)

   ;; disable character
   (define-enum _POSIX_VDISABLE)

   ;; c_cc subscripts
   (define-enum VEOF)
   (define-enum VEOL)
   (define-enum VERASE)
   (define-enum VINTR)
   (define-enum VKILL)
   (define-enum VMIN)
   (define-enum VQUIT)
   (define-enum VSTART)
   (define-enum VSTOP)
   (define-enum VSUSP)
   (define-enum VTIME)
   (define-enum-conditionally VDISCARD)
   (define-enum-conditionally VDSUSP)
   (define-enum-conditionally VEOL2)
   (define-enum-conditionally VLNEXT)
   (define-enum-conditionally VREPRINT)
   (define-enum-conditionally VSTATUS)
   (define-enum-conditionally VWERASE)
   (define-enum-conditionally VSWTCH)
   (define-enum-conditionally VSWTC)

   ;; baudrates
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
   (define-enum-conditionally B57600)
   (define-enum-conditionally B115200)
   (define-enum-conditionally B230400)

   (define-cstruct <sys-termios> "struct termios"
     (iflag::<ulong> "c_iflag"
      oflag::<ulong> "c_oflag"
      cflag::<ulong> "c_cflag"
      lflag::<ulong> "c_lflag"
      cc::(.array <uint8> :as <u8vector>) "c_cc[NCCS]"))

   (define-cproc sys-tcgetattr (port-or-fd) ::<sys-termios>
     (let* ([fd::int (Scm_GetPortFd port-or-fd TRUE)]
            [term::(struct termios)])
       (when (< (tcgetattr fd (& term)) 0)
         (Scm_SysError "tcgetattr failed"))
       (return (& term))))

   (define-cproc sys-tcsetattr (port-or-fd option::<fixnum> term::<sys-termios>)
     ::<void>
     (let* ([fd::int (Scm_GetPortFd port-or-fd TRUE)])
       (when (< (tcsetattr fd option term) 0)
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
     (let* ([s::speed_t (cfgetispeed term)])
       (return s)))

   (define-cproc sys-cfsetispeed (term::<sys-termios> speed::<int>) ::<void>
     (when (< (cfsetispeed term speed) 0)
       (Scm_SysError "cfsetispeed failed")))

   (define-cproc sys-cfgetospeed (term::<sys-termios>) ::<int>
     (let* ([s::speed_t (cfgetospeed term)])
       (return s)))

   (define-cproc sys-cfsetospeed (term::<sys-termios> speed::<int>) ::<void>
     (when (< (cfsetospeed term speed) 0)
       (Scm_SysError "cfsetospeed failed")))

   ;; Returns fresh copy of <sys-termios>
   ;; The fields of struct termios are system-dependent, and it may contain
   ;; more fields than specified in POSIX.  However, we assume they're safe
   ;; for bitwise-copy.  The cstruct boxer copies *src into the fresh object.
   (define-cproc sys-termios-copy (src::<sys-termios>) ::<sys-termios>
     (return src))

   ;; pty interface
   (.when (defined HAVE_OPENPTY)
     (define-cproc sys-openpty (:optional (term::<sys-termios>? #f))
       ::(<int> <int>)
       (let* ([master::int 0] [slave::int 0])
         (when (< (openpty (& master) (& slave) NULL term NULL) 0)
           (Scm_SysError "openpty failed"))
         (return master slave)))
     )
   (.when (defined HAVE_FORKPTY)
     (define-cproc sys-forkpty (:optional (term::<sys-termios>? #f))
       ::(<int> <int>)
       (let* ([master::int 0]
              [pid::pid_t (forkpty (& master) NULL term NULL)])
         (when (< pid 0)
           (Scm_SysError "forkpty failed"))
         (return pid master)))
     (define-cproc sys-forkpty-and-exec (program::<string> args::<list>
                                         :key (iomap ())
                                              (term::<sys-termios>? #f)
                                              (sigmask::<sys-sigset>? #f))
       ::(<int> <int>)
       (let* ([argc::ScmSmallInt (Scm_Length args)])
         (when (< argc 1)
           (Scm_Error "argument list must have at least one element: %S" args))
         (let* ([argv::char** (Scm_ListToCStringArray args TRUE NULL)]
                [cprogram::(const char *) (Scm_GetStringConst program)]
                [fds::int* (Scm_SysPrepareFdMap iomap)]
                [master::int 0]
                [pid::pid_t (forkpty (& master) NULL term NULL)])
           (when (< pid 0)
             (Scm_SysError "forkpty failed"))
           (when (== pid 0)
             (Scm_SysSwapFds fds)
             (when sigmask
               (Scm_ResetSignalHandlers (& (-> sigmask set)))
               (Scm_SysSigmask SIG_SETMASK sigmask))
             (execvp cprogram (cast (char * const *) argv))
             ;; here, we failed
             (Scm_Panic "exec failed: %s: %s" cprogram (strerror errno)))
           (return pid master))))
     )

   ) ;; !defined(GAUCHE_WINDOWS)
 ) ;; inline-stub

;;
;; High-level utilities
;;

(without-precompiling

 (cond-expand
  [gauche.os.windows (use os.windows)]
  [else])

 ;; Heuristics - check if we have a windows console.
 (define (has-windows-console?)
   (cond-expand
    [gauche.os.windows
     (sys-has-windows-console?)]
    [else #f]))

 (cond-expand
  [gauche.os.windows
   ;; For mintty on MSYS - we don't want to depend on cygwin's dll,
   ;; but if we're running on MSYS, we're likely to have stty.
   ;; NB: It's better if we could use gauche.process and directly read
   ;; from pipe, but at the time it isn't working yet.
   (define (msys-get-stty)
     ;; NB: We don't use build-path to avoid depending file.util
     (receive (out tempfile) (sys-mkstemp #"~(sys-tmpdir)/gauche-stty")
       (unwind-protect
           (begin
             (close-output-port out)
             (sys-system #"stty -g > ~tempfile")
             (with-input-from-file tempfile read-line))
         (sys-unlink tempfile))))]
  [else])

 (define (without-echoing iport proc)
   (cond-expand
    [gauche.os.windows
     (cond [(not iport)
            (if (has-windows-console?)
              (call-with-input-file "CON"
                (cut without-echoing <> proc))
              (without-echoing (standard-input-port) proc))] ;CON not avail.
           [(sys-isatty iport) ;NB: mintty yields #f here
            (let ()
              (define ihandle (sys-get-std-handle STD_INPUT_HANDLE))
              (define orig-mode (sys-get-console-mode ihandle))
              (define (echo-off)
                (sys-set-console-mode ihandle
                                      (logand orig-mode
                                              (lognot ENABLE_ECHO_INPUT))))
              (define (echo-on)
                (sys-set-console-mode ihandle orig-mode))
              (unwind-protect (begin (echo-off) (proc iport)) (echo-on)))]
           [((with-module gauche.internal %sys-mintty?) iport)
            ;; We're dealing with mintty
            (let ()
              (define saved-attr (msys-get-stty))
              (define (echo-off) (sys-system "stty -echo  icanon  iexten  isig"))
              (define (echo-on)  (sys-system #"stty ~saved-attr"))
              (unwind-protect (begin (echo-off) (proc iport)) (echo-on)))]
           [else (proc iport)])]
    [else ; not gauche.os.windows
     (cond [(not iport) ;; open tty
            (call-with-input-file "/dev/tty"
              (cut without-echoing <> proc))]
           [(sys-isatty iport)
            (let ()
              (define attr (sys-tcgetattr iport))
              (define lflag-save (ref attr'lflag))
              (define (echo-off)
                (set! (ref attr'lflag)
                      (logior ICANON IEXTEN ISIG
                              (logand (ref attr'lflag)
                                      (lognot ECHO))))
                (sys-tcsetattr iport TCSANOW attr))
              (define (echo-on)
                (set! (ref attr'lflag) lflag-save)
                (sys-tcsetattr iport TCSANOW attr))
              (unwind-protect (begin (echo-off) (proc iport)) (echo-on)))]
           [else (proc iport)])]))

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
 ;; NB: Windows console doesn't go well with this API, so we do nothing;
 ;; see text.console for abstraction of Windows console.
 (define (with-terminal-mode port mode proc :optional (cleanup #f))
   (cond-expand
    [gauche.os.windows
     (if (not ((with-module gauche.internal %sys-mintty?) port))
       (proc port) ; for windows console
       (let ()     ; for mintty on MSYS
         (define saved-attr (msys-get-stty))
         (define saved-buffering (port-buffering port))
         (define new-attr
           (case mode
             [(raw)    "-echo -icanon -iexten -isig -icrnl"]
             [(rare)   "-echo -icanon -iexten  isig -icrnl"]
             [(cooked) " echo  icanon  iexten  isig  icrnl"]
             [else
              (error "terminal mode needs to be one of cooked, rare or raw, \
                      but got:" mode)]))
         (define (set)
           (sys-system #"stty ~new-attr")
           (when (memq mode '(raw rare))
             (set! (port-buffering port) :none)))
         (define (reset)
           (sys-system #"stty ~saved-attr")
           (set! (port-buffering port) saved-buffering)
           (when cleanup (cleanup)))
         (unwind-protect (begin (set) (proc port)) (reset))))]
    [else ; not gauche.os.windows
     (if (not (sys-isatty port))
       (proc port)
       (let ()
         (define saved-attr (sys-tcgetattr port))
         (define saved-buffering (port-buffering port))
         (define new-attr
           (rlet1 attr (sys-termios-copy saved-attr)
             ;; NB: See cfmakeraw(3) for raw mode setting.
             (case mode
               [(raw)
                (update! (~ attr'iflag)
                         (cut logset+clear <>
                              0
                              (logior BRKINT ICRNL INPCK ISTRIP IXON)))
                (update! (~ attr'oflag)
                         (cut logset+clear <>
                              0
                              OPOST))
                (update! (~ attr'cflag)
                         (cut logset+clear <>
                              CS8
                              (logior CSIZE PARENB)))
                (update! (~ attr'lflag)
                         (cut logset+clear <>
                              0
                              (logior ECHO ICANON IEXTEN ISIG)))]
               [(rare)
                (update! (~ attr'iflag)
                         (cut logset+clear <>
                              BRKINT
                              (logior ICRNL INPCK ISTRIP IXON)))
                (update! (~ attr'oflag)
                         (cut logset+clear <>
                              OPOST
                              0))
                (update! (~ attr'cflag)
                         (cut logset+clear <>
                              CS8
                              (logior CSIZE PARENB)))
                (update! (~ attr'lflag)
                         (cut logset+clear <>
                              ISIG
                              (logior ECHO ICANON IEXTEN)))]
               [(cooked)
                (update! (~ attr'iflag)
                         (cut logset+clear <>
                              (logior BRKINT ICRNL INPCK ISTRIP IXON)
                              0))
                (update! (~ attr'oflag)
                         (cut logset+clear <>
                              OPOST
                              0))
                (update! (~ attr'lflag)
                         (cut logset+clear <>
                              (logior ECHO ICANON IEXTEN ISIG)
                              0))]
               [else
                (error "terminal mode needs to be one of cooked, rare or raw, \
                          but got:" mode)])))
         (define (set)
           (sys-tcsetattr port TCSANOW new-attr)
           (when (memq mode '(raw rare))
             (set! (port-buffering port) :none)))
         (define (reset)
           (sys-tcsetattr port TCSANOW saved-attr)
           (set! (port-buffering port) saved-buffering)
           (when cleanup (cleanup)))
         (unwind-protect (begin (set) (proc port)) (reset))))]))

 ) ; without-precompiling
