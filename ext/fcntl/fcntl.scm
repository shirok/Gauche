;;;
;;; fcntl - fcntl interface
;;;
;;;  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: fcntl.scm,v 1.1 2001-09-16 06:50:43 shirok Exp $
;;;


(define-module gauche.fcntl
  (export <sys-flock>
          sys-fcntl

          |F_DUPFD|  |F_GETFD|  |F_GETLK|  |F_GETFL|  |F_SETFL|
          |F_GETLK|  |F_SETLK|  |F_SETLKW|
          |F_RDLCK|  |F_WRLCK|  |F_UNLCK|  |FD_CLOEXEC|
          |O_RDONLY| |O_WRONLY| |O_RDWR|   |O_APPEND| |O_CREAT|
          |O_EXCL|   |O_NOCTTY| |O_NONBLOCK| |O_TRUNC|
          )
  )

(select-module gauche.fcntl)

(dynamic-load "fcntl" :export-symbols #t)


(provide "gauche/fcntl")
