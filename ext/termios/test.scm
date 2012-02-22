;;
;; testing termios
;;

(use gauche.test)
(use gauche.uvector)

(test-start "termios")
(use gauche.termios)
(use srfi-1)
(use srfi-13)
(test-module 'gauche.termios)

(cond-expand
 [(not gauche.os.windows)
  ;;
  ;; POSIX version
  ;;
  (define (list-if-bound . cans)
    (let loop ([cans cans]
               [syms '()]
               [str ""])
      (if (null? cans)
        (map cons
             syms
             (let1 p (open-input-string (string-append "(list " str ")"))
               (eval (read p) (interaction-environment))))
        (if (global-variable-bound? 'gauche.termios (car cans))
          (loop (cdr cans)
                (cons (car cans) syms)
                (string-append str " " (symbol->string (car cans))))
          (loop (cdr cans) syms str)))))

  (define speeds
    (list-if-bound 'B0 'B50 'B75 'B110 'B134 'B150 'B200 'B300 'B600 'B1200
                   'B1800 'B2400 'B4800 'B9600 'B19200 'B38400 'B57600
                   'B115200 'B230400))

  (define iflags
    (list-if-bound 'IGNBRK 'BRKINT 'IGNPAR 'PARMRK 'INPCK 'ISTRIP 'INLCR
                   'IGNCR 'ICRNL 'IXON 'IXOFF 'IXANY 'IUCLC 'IMAXBEL))

  (define oflags
    (list-if-bound 'OPOST 'OLCUC 'ONLCR 'OCRNL 'ONOCR 'ONLRET 'OFILL 'OFDEL
                   'NLDLY 'NL0 'NL1 'CRDLY 'CR0 'CR1 'CR2 'CR3 'BSDLY 'BS0
                   'BS1 'VTDLY 'VT0 'VT1 'FFDLY 'FF0 'FF1))

  (define cflags
    (list-if-bound 'CLOCAL 'CREAD 'CSIZE 'CS5 'CS6 'CS7 'CS8 'CSTOPB 'HUPCL
                   'PARENB 'PARODD 'CIBAUD 'CRTSCTS))

  (define lflags
    (list-if-bound 'ECHO 'ECHOE 'ECHOK 'ECHONL 'ICANON 'ISIG 'NOFLSH 'TOSTOP
                   'IEXTEN 'XCASE 'ECHOCTL 'ECHOPRT 'ECHOKE 'FLUSH0
                   'PENDIN))

  (define ccs
    (list-if-bound 'VEOF 'VEOL 'VERASE 'VINTR 'VKILL 'VMIN 'VQUIT 'VSTART
                   'VSTOP 'VSUSP 'VTIME 'VDISCARD 'VDSUSP 'VEOL2 'VLNEXT
                   'VREPRINT 'VSTATUS 'VWERASE 'VSWTCH 'VSWTC))

  (define iterm #f)
  (define oterm #f)

  (define iport #f)
  (define oport #f)

  ;; If tests are run by a daemon, /dev/tty may not be available.
  ;; We try /dev/tty first, then using pty for fallback.
  (guard (e [(<system-error> e)
             (cond-expand
              [gauche.sys.openpty
               (receive (master slave) (sys-openpty)
                 (set! iport (open-input-fd-port slave))
                 (set! oport (open-output-fd-port slave)))]
              [else
               ;; we can't continue the tests.
               (test-end)
               (exit 0)])])
    (let1 term (sys-ctermid)
      (set! iport (open-input-file term))
      (set! oport (open-output-file term))))

  (test "termios-tcgetattr" #t
        (^[]
          (set! iterm (sys-tcgetattr iport))
          (set! oterm (sys-tcgetattr oport))
          #t))

  ;; NB: on cygwin (as of 1.5.25) tcdrain and tcflow does not seem to work.
  (unless (string-contains (gauche-architecture) "-cygwin")
    (test "termios-tcdrain" #t
          (^[]
            (sys-tcdrain oport)
            #t))

    (test "termios-tcflow" (make-list 4 (if #f #f))
          (^[] (map (cut sys-tcflow oport <>)
                    (list TCOOFF TCOON TCIOFF TCION))))
    ) ;!cygwin

  (test "termios-tcflush" #t
        (^[]
          (sys-tcflush iport TCIFLUSH)
          (sys-tcflush oport TCOFLUSH)
          #t))

  (test "termios-tcsetattr" (make-list 3 (undefined))
        (^[] (map (cut sys-tcsetattr iport <> iterm)
                  (list TCSANOW TCSADRAIN TCSAFLUSH))))

  ;; exclude B0 from this test, since it doesn't really set the baudrate
  ;; (and some architecture such as Solaris does not set the value to
  ;; termios structure).
  (let ([slist (remove zero? (map cdr speeds))]
        [orig-ispeed (sys-cfgetispeed iterm)]
        [orig-ospeed (sys-cfgetospeed oterm)])
    (test* "termios-set-n-get-speed" (map (^x (cons x x)) slist)
           (with-error-handler
               (^e
                 (sys-cfsetispeed iterm orig-ispeed)
                 (sys-cfsetospeed oterm orig-ospeed)
                 (raise e))
             (^[] (map (^[speed]
                         (sys-cfsetispeed iterm speed)
                         (sys-cfsetospeed oterm speed)
                         (cons (sys-cfgetispeed iterm)
                               (sys-cfgetospeed oterm)))
                       slist)))))

  (test "termios-cc" (make-list (length ccs) #t)
        (^[]
          (define test-char #x01)
          (map
           (^[ss-pair]
             (let* ([ss (cdr ss-pair)]
                    [orig-cc (slot-ref iterm 'cc)]
                    [cc (u8vector-copy orig-cc)])
               (dynamic-wind
                   (^[] (slot-set! iterm 'cc cc))
                   (^[]
                     (u8vector-set! cc ss test-char)
                     (slot-set! iterm 'cc cc)
                     (sys-tcsetattr iport TCSANOW iterm)
                     (let1 char (u8vector-ref
                                 (slot-ref (sys-tcgetattr iport) 'cc)
                                 ss)
                       (if (eqv? char (u8vector-ref cc ss))
                         #t
                         (format "~a of cc cannot change" (car ss-pair)))))
                   (^[]
                     (slot-set! iterm 'cc orig-cc)
                     (sys-tcsetattr iport TCSANOW iterm))
                 )))
           ccs)))

  ]
 [else
  ;;
  ;; Windows Version
  ;;
  ])

(test-end)
