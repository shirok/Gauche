;;;
;;; auxsys - Auxiliary system interface
;;;
;;;  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: auxsys.scm,v 1.6 2002-12-13 13:17:55 shirok Exp $
;;;

(define-module gauche.auxsys
  (export fmod frexp modf ldexp
          sys-abort sys-mkfifo
          sys-setgid sys-setpgid sys-getpgid sys-getpgrp
          sys-setsid sys-setuid sys-times sys-uname sys-ctermid
          sys-gethostname sys-getdomainname sys-putenv
          sys-gettimeofday sys-chown sys-utime
          sys-getgroups sys-getlogin sys-localeconv)
  )
(select-module gauche.auxsys)

(dynamic-load "auxsys")

;; define alternatives if the platform doesn't support...

(define sys-gethostname
  (if (symbol-bound? '%sys-gethostname)
      %sys-gethostname
      (lambda () (cadr (sys-uname)))))  ;utsname.nodename

(define sys-getdomainname
  (if (symbol-bound? '%sys-getdomainname)
      %sys-getdomainname
      (lambda () "localdomain")))

(define sys-putenv
  (if (symbol-bound? '%sys-putenv)
      %sys-putenv
      (lambda (var val) (error "sys-putenv not supported on this platform"))))

(define sys-setpgrp
  (if (symbol-bound? '%sys-setpgrp)
      %sys-setpgrp
      (lambda () (sys-setpgid 0 0))))

(define sys-getpgid
  (if (symbol-bound? '%sys-getpgid)
      %sys-getpgid
      (lambda (pid)
        (if (zero? pid)
            (sys-getpgrp)
            (error "sys-getpgid for arbitrary process id is not supported on this platform")))))

(define sys-gettimeofday
  (if (symbol-bound? '%sys-gettimeofday)
      %sys-gettimeofday
      (lambda () (values (sys-time) 0))))

(provide "gauche/auxsys")
