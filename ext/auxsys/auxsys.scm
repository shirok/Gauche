;;;
;;; auxsys - Auxiliary system interface
;;;  
;;;   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
;;;  $Id: auxsys.scm,v 1.9 2004-04-10 12:14:17 fuyuki Exp $
;;;

(define-module gauche.auxsys
  (export fmod frexp modf ldexp
          sys-abort sys-realpath sys-mkfifo
          sys-setgid sys-setpgid sys-getpgid sys-getpgrp
          sys-setsid sys-setuid sys-times sys-uname sys-ctermid
          sys-gethostname sys-getdomainname
          sys-putenv sys-setenv sys-unsetenv
          sys-gettimeofday sys-chown sys-lchown sys-utime
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

(define sys-setenv
  (if (symbol-bound? '%sys-setenv)
    %sys-setenv
    (lambda (var val val) (error "sys-setenv not supported on this platform"))))

(define sys-unsetenv
  (if (symbol-bound? '%sys-unsetenv)
    %sys-unsetenv
    (lambda (var) (error "sys-unsetenv not supported on this platform"))))

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
