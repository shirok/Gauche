;;
;; testing termios
;;

(use gauche.test)
(use gauche.uvector)

(test-start "termios")
(use gauche.termios)
(test-module 'gauche.termios)

(define (list-if-bound . cans)
  (let loop ((cans cans)
             (syms '())
             (str ""))
    (if (null? cans)
      (map cons
           syms
           (let ((p (open-input-string (string-append "(list " str ")"))))
             (eval (read p) (interaction-environment))))
      (if (symbol-bound? (car cans))
        (loop (cdr cans)
              (cons (car cans) syms)
              (string-append str " " (symbol->string (car cans))))
        (loop (cdr cans) syms str)))))

(define iport (open-input-file (sys-ctermid)))
(define oport (open-output-file (sys-ctermid)))

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

(test "termios-tcgetattr" #t
      (lambda ()
        (set! iterm (sys-tcgetattr iport))
        (set! oterm (sys-tcgetattr oport))
        #t))

(test "termios-tcdrain" #t
      (lambda ()
        (sys-tcdrain oport)
        #t))

(test "termios-tcflush" #t
      (lambda ()
        (sys-tcflush iport TCIFLUSH)
        (sys-tcflush oport TCOFLUSH)
        #t))

(test "termios-tcflow" (make-list 4 (if #f #f))
      (lambda ()
        (map
          (cut sys-tcflow oport <>)
          (list TCOOFF TCOON TCIOFF TCION))))

(test "termios-tcsetattr" (make-list 3 (if #f #f))
      (lambda ()
        (map
          (cut sys-tcsetattr iport <> iterm)
          (list TCSANOW TCSADRAIN TCSAFLUSH))))

(test "termios-set-n-get-speed" (make-list (length speeds) (if #f #f))
      (lambda ()
        (map
          (lambda (speed)
            (let ((orig-ispeed 0)
                  (orig-ospeed 0))
              (dynamic-wind
                (lambda ()
                  (set! orig-ispeed (sys-cfgetispeed iterm))
                  (set! orig-ospeed (sys-cfgetospeed oterm)))
                (lambda ()
                  (sys-cfsetispeed iterm speed)
                  (let ((result (sys-cfgetispeed iterm)))
                    (if (not (eq? result speed))
                      (errorf "sys-cfgetispeed expects ~a, but got ~a" speed result)))
                  (sys-cfsetospeed oterm speed)
                  (let ((result (sys-cfgetospeed oterm)))
                    (if (not (eq? result speed))
                      (errorf "sys-cfsetispeed expects ~a, but got ~a" speed result))))
                (lambda ()
                  (sys-cfsetispeed iterm orig-ispeed)
                  (sys-cfsetospeed oterm orig-ospeed))
                )))
          (map (lambda (dot) (apply cdr (list dot))) speeds))))

(test "termios-cc" (make-list (length ccs) #t)
      (lambda ()
        (define test-char #x01)
        (map
          (lambda (ss-pair)
            (let* ((ss (cdr ss-pair))
                   (orig-cc (slot-ref iterm 'cc))
                   (cc (u8vector-copy orig-cc)))
              (dynamic-wind
                (lambda ()
                  (slot-set! iterm 'cc cc))
                (lambda ()
                  (u8vector-set! cc ss test-char)
                  (slot-set! iterm 'cc cc)
                  (sys-tcsetattr iport TCSANOW iterm)
                  (let ((char (u8vector-ref
                                   (slot-ref (sys-tcgetattr iport) 'cc)
                                   ss)))
                    (if (eqv? char (u8vector-ref cc ss))
                      #t
                      (format "~a of cc cannot change" (car ss-pair)))))
                (lambda ()
                  (slot-set! iterm 'cc orig-cc)
                  (sys-tcsetattr iport TCSANOW iterm))
                )))
          ccs)))



(test-end)
