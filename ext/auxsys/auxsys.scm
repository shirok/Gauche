;;;
;;; auxsys - Auxiliary system interface
;;;  
;;;   Copyright (c) 2000-2010  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: auxsys.scm,v 1.24 2008-05-10 13:35:32 shirok Exp $
;;;

(define-module gauche.auxsys
  (export fmod frexp modf ldexp
          sys-abort sys-realpath sys-mkfifo
          sys-fdset list->sys-fdset sys-fdset->list
          sys-setgid sys-setpgid sys-getpgid sys-getpgrp
          sys-setsid sys-setuid sys-times sys-uname sys-ctermid
          sys-gethostname sys-getdomainname
          sys-putenv sys-setenv sys-unsetenv sys-environ sys-environ->alist
          sys-utime
          sys-getgroups sys-getlogin sys-localeconv
          sys-getloadavg)
  )
(select-module gauche.auxsys)

(dynamic-load "gauche--auxsys")

;; define alternatives if the platform doesn't support...

(cond-expand
 [gauche.os.windows
  (define sys-mkfifo #f)
  (define sys-setgid #f)
  (define sys-setpgid #f)
  (define sys-getpgid #f)
  (define sys-getpgrp #f)
  (define sys-setsid #f)
  (define sys-setuid #f)
  (define sys-getgroups #f)
  (define sys-uname #f)
  ]
 [else])

(define sys-gethostname
  (if (global-variable-bound? 'gauche.auxsys '%sys-gethostname)
    %sys-gethostname
    (lambda ()
      (cond-expand
       [(not gauche.os.windows) (cadr (sys-uname))] ; nodename
       [else "localhost"]))))   ; need better fallback

(define sys-getdomainname
  (if (global-variable-bound? 'gauche.auxsys '%sys-getdomainname)
    %sys-getdomainname
    (lambda () "localdomain"))) ; need better fallback

;; These are better to be in src/scmlib.scm, but right now we don't have
;; a nice way to make cond-expand work (when compiling src/scmlib.scm
;; cond-expand uses the host gosh's feature set, not the target gosh's.)
(cond-expand
 [gauche.sys.select
  (define (sys-fdset . pfs)
    (list->sys-fdset pfs))
  (define (sys-fdset->list fdset)
    (check-arg (cut is-a? <> <sys-fdset>) fdset)
    (do ([i (sys-fdset-max-fd fdset) (- i 1)]
         [fds '() (if (sys-fdset-ref fdset i) (cons i fds) fds)])
        [(< i 0) fds]
      #f))
  (define (list->sys-fdset pfs)
    (rlet1 fdset (make <sys-fdset>)
      (dolist (pf pfs)
        (cond [(or (integer? pf) (port? pf)) (sys-fdset-set! fdset pf #t)]
              [(is-a? pf <sys-fdset>)
               (dotimes (i (+ (sys-fdset-max-fd pf) 1))
                 (when (sys-fdset-ref pf i) (sys-fdset-set! fdset i #t)))]
              [else (error "sys-fdset requires a port, an integer, \
                           or a <sys-fdset> object, but got:" pf)]))))
  ]
 [else
  ;; make autoload happy
  (define sys-fdset #f)
  (define sys-fdset->list #f)
  (define list->sys-fdset #f)
  ])

;; We support sys-setenv natively if the system has either
;; setenv(3) or putenv(3).  The feature symbol is gauche.sys.setenv.

(cond-expand
 [gauche.sys.setenv
  ;; We emulate putenv.  Somehow the old API was (sys-putenv name value),
  ;; which we support for backward compatibility.
  (define (sys-putenv name=value . other)
    (cond
     [(null? other)
      (check-arg string? name=value)
      (receive (name value) (string-scan name=value #\= 'both)
        (unless name
          (error "sys-putenv: argument doesn't contain '=':" name=value))
        (sys-setenv name value #t))]
     [else (sys-setenv name=value (car other) #t)]))
  ]
 [else
  ;; make autoload happy
  (define sys-putenv #t)
  (define sys-setenv #t)])

(cond-expand
 [(not gauche.sys.unsetenv) (define sys-unsetenv #f)] ; make autoload happy
 [else])

(define (sys-environ->alist . envlist)
  (map (lambda (envstr)
         (receive (pre post) (string-scan envstr #\= 'both)
           (if pre (cons pre post) (cons envstr ""))))
       (get-optional envlist (sys-environ))))

(define sys-setpgrp
  (if (global-variable-bound? 'gauche.auxsys '%sys-setpgrp)
    %sys-setpgrp
    (lambda () (sys-setpgid 0 0))))

(cond-expand
 [(not gauche.sys.getpgid) (define sys-getpgid #f)] ;make autoload happy
 [else])

(cond-expand
 [(not gauche.sys.getloadavg) (define sys-getloadavg #f)] ;make autoload happy
 [else])

;; Realpath implementation.
;; POSIX realpath(3) is flawed in a sense that there's no way to get
;; the reasonable and safe buffer size (PATH_MAX can return very large
;; number; see the manpage for the details).  So we implement it in
;; Scheme, making it portable and safe.
;; NB: we can't use utilities in file.util to avoid dependency hell.
(cond-expand
 [gauche.os.windows
  (define (sys-realpath path)
    (rlet1 p (sys-normalize-pathname path :absolute #t :canonicalize #t)
      (sys-stat p)))] ; check if the path exists
 [else
  (define (sys-realpath path)
    (define count 0)
    (define (loop-check!)
      (inc! count)
      (when (> count 100)
        (error "possible cycle in resolving symlinks for path:" path)))
    (define (decompose path) (string-split path "/"))
    (define (absolute? path)
      (and (>= (string-length path) 1) (eqv? (string-ref path 0) #\/)))
    (define (path-concat path)
      (string-append "/" (string-join (reverse path) "/")))
    (define (resolve path comps)
      (cond [(null? comps) (path-concat path)] ; we know path exists
            [(member (car comps) '("" "."))(resolve path (cdr comps))]
            [(string=? (car comps) "..")
             (resolve (if (pair? path) (cdr path) path) (cdr comps))]
            [else
             (let* ((q (cons (car comps) path))
                    (p (path-concat q))
                    (s (sys-lstat p)))    ; may raise ENOENT
               (cond [(eq? (slot-ref s'type) 'symlink)
                      (loop-check!)
                      (let1 p1 (sys-readlink p)
                        (resolve (if (absolute? p1) '() path)
                                 (append! (decompose p1) (cdr comps))))]
                     [(or (null? (cdr comps))
                          (eq? (slot-ref s'type) 'directory))
                      (resolve q (cdr comps))]
                     [else (error "not a directory" p)]))]))
                     
    (resolve '() (append! (if (absolute? path) '() (decompose (sys-getcwd)))
                          (decompose path))))])

