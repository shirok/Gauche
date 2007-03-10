;;;
;;; auxsys - Auxiliary system interface
;;;  
;;;   Copyright (c) 2000-2007  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: auxsys.scm,v 1.17 2007-03-10 08:27:55 shirok Exp $
;;;

(define-module gauche.auxsys
  (export fmod frexp modf ldexp
          sys-abort sys-realpath sys-mkfifo
          sys-fdset list->sys-fdset sys-fdset->list
          sys-setgid sys-setpgid sys-getpgid sys-getpgrp
          sys-setsid sys-setuid sys-times sys-uname sys-ctermid
          sys-gethostname sys-getdomainname
          sys-putenv sys-setenv sys-unsetenv
          sys-chown sys-lchown sys-utime
          sys-getgroups sys-getlogin sys-localeconv
          sys-getloadavg)
  )
(select-module gauche.auxsys)

(dynamic-load "auxsys")

;; define alternatives if the platform doesn't support...

(define sys-realpath
  (cond-expand
   (gauche.sys.reaplath %sys-realpath)
   (else #f)))

(define sys-gethostname
  (if (global-variable-bound? 'gauche.auxsys '%sys-gethostname)
    %sys-gethostname
    (lambda ()
      (cond-expand
       ((not gauche.os.windows) (cadr (sys-uname))) ; nodename
       (else "localhost")))))   ; need better fallback

(define sys-getdomainname
  (if (global-variable-bound? 'gauche.auxsys '%sys-getdomainname)
    %sys-getdomainname
    (lambda () "localdomain"))) ; need better fallback

;; These are better to be in src/scmlib.scm, but right now we don't have
;; a nice way to make cond-expand work (when compiling src/scmlib.scm
;; cond-expand uses the host gosh's feature set, not the target gosh's.)
(define sys-fdset
  (cond-expand
   (gauche.sys.select (lambda pfs (list->sys-fdset pfs)))
   (else #f)))

(define sys-fdset->list
  (cond-expand
   (gauche.sys.select
    (lambda (fdset)
      (check-arg (cut is-a? <> <sys-fdset>) fdset)
      (do ((i (sys-fdset-max-fd fdset) (- i 1))
           (fds '() (if (sys-fdset-ref fdset i) (cons i fds) fds)))
          ((< i 0) fds)
        #f)))
   (else #f)))

(define list->sys-fdset
  (cond-expand
   (gauche.sys.select
    (lambda (pfs)
      (let1 fdset (make <sys-fdset>)
        (dolist (pf pfs)
          (cond ((or (integer? pf) (port? pf))
                 (sys-fdset-set! fdset pf #t))
                ((is-a? pf <sys-fdset>)
                 (dotimes (i (+ (sys-fdset-max-fd pf) 1))
                   (when (sys-fdset-ref pf i)
                     (sys-fdset-set! fdset i #t))))
                (else (error "sys-fdset requires a port, an integer, or a <sys-fdset> object, but got:" pf))))
        fdset)))
   (else #f)))

(define sys-putenv
  (cond-expand
   (gauche.sys.putenv %sys-putenv)
   (else #f)))

(define sys-setenv
  (cond-expand
   (gauche.sys.setenv %sys-setenv)
   (else #f)))

(define sys-unsetenv
  (cond-expand
   (gauche.sys.unsetenv %sys-unsetenv)
   (else #f)))

(define sys-setpgrp
  (if (global-variable-bound? 'gauche.auxsys '%sys-setpgrp)
    %sys-setpgrp
    (lambda () (sys-setpgid 0 0))))

(define sys-getpgid
  (if (global-variable-bound? 'gauche.auxsys '%sys-getpgid)
    %sys-getpgid
    (lambda (pid)
      (if (zero? pid)
        (sys-getpgrp)
        (error "sys-getpgid for arbitrary process id is not supported on this platform")))))

(define sys-lchown
  (cond-expand
   (gauche.sys.lchown %sys-lchown)
   (else #f)))

(define sys-getloadavg
  (cond-expand
   (gauche.sys.getloadavg %sys-getloadavg)
   (else #f)))

(provide "gauche/auxsys")
