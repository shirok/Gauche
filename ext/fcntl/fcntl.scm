;;;
;;; fcntl - fcntl interface
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

#!no-fold-case

(define-module gauche.fcntl
  (export <sys-flock>
          sys-fcntl

          F_DUPFD  F_GETFD  F_SETFD  F_GETFL  F_SETFL
          F_GETLK  F_SETLK  F_SETLKW
          F_RDLCK  F_WRLCK  F_UNLCK
          O_RDONLY O_WRONLY O_RDWR   O_APPEND O_CREAT
          O_EXCL   O_ACCMODE
          )
  )
(select-module gauche.fcntl)

(inline-stub
 (declcode "#include \"gauche/fcntl.h\"")
 
 (define-enum F_DUPFD)
 (define-enum F_GETFD)
 (define-enum F_SETFD)
 (define-enum F_GETFL)
 (define-enum F_SETFL)
 (define-enum F_GETLK)
 (define-enum F_SETLK)
 (define-enum F_SETLKW)

 (define-enum F_RDLCK)
 (define-enum F_WRLCK)
 (define-enum F_UNLCK)

 (define-enum-conditionally F_GETOWN)
 (define-enum-conditionally F_SETOWN)
 (define-enum-conditionally F_GETSIG)
 (define-enum-conditionally F_SETSIG)
 (define-enum-conditionally F_GETLEASE)
 (define-enum-conditionally F_SETLEASE)
 (define-enum-conditionally F_NOTIFY)

 (define-enum-conditionally FD_CLOEXEC)

 (define-enum O_ACCMODE)
 (define-enum O_RDONLY)
 (define-enum O_WRONLY)
 (define-enum O_RDWR)
 (define-enum O_APPEND)
 (define-enum O_CREAT)
 (define-enum O_EXCL)
 (define-enum-conditionally O_NOCTTY)
 (define-enum-conditionally O_NONBLOCK)
 (define-enum-conditionally O_ASYNC)
 (define-enum O_TRUNC)

 ;; Linux specific F_NOTIFY flags (not available yet, w/o _GNU_SOURCE)
 (if "defined(F_NOTIFY)"
   (begin
     (define-enum DN_ACCESS)
     (define-enum DN_MODIFY)
     (define-enum DN_CREATE)
     (define-enum DN_DELETE)
     (define-enum DN_RENAME)
     (define-enum DN_ATTRIB)
     (define-enum DN_MULTISHOT)
     ))
 )

(export-if-defined
 F_GETOWN F_SETOWN F_GETSIG F_SETSIG
 F_GETLEASE F_SETLEASE F_NOTIFY
 FD_CLOEXEC
 DN_ACCESS DN_MODIFY DN_CREATE DN_DELETE
 DN_RENAME DN_ATTRIB DN_MULTISHOT
 O_ASYNC   O_NOCTTY  O_NONBLOCK O_TRUNC
 )

(inline-stub
 (define-cproc sys-fcntl (port-or-fd op::<fixnum> :optional arg) Scm_SysFcntl)
 (declcode "extern void Scm_Init_fcntl(void);")
 (initcode (Scm_Init_fcntl)))
