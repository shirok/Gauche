;;;
;;; termios - termios interface
;;;
;;;  Copyright(C) 2001-2002 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: termios.scm,v 1.3 2002-04-30 01:52:32 shirok Exp $
;;;

(define-module gauche.termios
  (export <sys-termios>
          sys-tcgetattr sys-tcsetattr
          sys-tcsendbreak sys-tcdrain sys-tcflush sys-tcflow
          sys-tcgetpgrp sys-tcsetpgrp
          sys-cfgetispeed sys-cfsetispeed sys-cfgetospeed sys-cfsetospeed
          sys-openpty sys-forkpty

          |IGNBRK|  |BRKINT|  |IGNPAR|  |PARMRK|  |INPCK|  |ISTRIP|
          |INLCR|   |IGNCR|   |ICRNL|   |IUCLC|   |IXON|   |IXANY|
          |IXOFF|   |IMAXBEL|
          |OPOST|   |OLCUC|   |ONLCR|   |OCRNL|   |ONOCR|  |ONLRET|
          |OFILL|   |OFDEL|   |NLDLY|   |NL0|     |NL1|    |CRDLY|
          |CR0|     |CR1|     |CR2|     |CR3|     |BSDLY|  |BS0|
          |BS1|     |VTDLY|   |VT0|     |VT1|     |FFDLY|  |FF0|
          |FF1|
          |CSIZE|   |CS5|     |CS6|     |CS7|     |CS8|    |CSTOPB|
          |CREAD|   |PARENB|  |PARODD|  |HUPCL|   |CLOCAL| |CIBAUD|
          |CRTSCTS| |ISIG|    |ICANON|  |XCASE|   |ECHO|   |ECHOE|
          |ECHOK|   |ECHONL|  |ECHOCTL| |ECHOPRT| |ECHOKE| |FLUSHO|
          |NOFLSH|  |TOSTOP|  |PENDIN|
          |TCSANOW| |TCSADRAIN| |TCSAFLUSH|
          |TCIFLUSH| |TCOFLUSH| |TCIOFLUSH|
          |TCOOFF|  |TCOON|   |TCIOFF|  |TCION|
          |B0|      |B50|     |B75|     |B110|    |B134|   |B150|
          |B200|    |B300|    |B600|    |B1200|   |B1800|  |B2400|
          |B4800|   |B9600|   |B19200|  |B38400|  |B57600| |B115200|
          |B230400|
          )
  )

(select-module gauche.termios)

(dynamic-load "termios" :export-symbols #t)


(provide "gauche/termios")


