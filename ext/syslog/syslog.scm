;;;
;;; syslog - syslog interface
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
;;;  $Id: syslog.scm,v 1.2 2002-09-22 00:40:30 shirok Exp $
;;;

(define-module gauche.syslog
  (export sys-openlog sys-syslog sys-closelog sys-setlogmask
          LOG_CONS
          LOG_NDELAY
          LOG_NOWAIT
          LOG_ODELAY
          LOG_PERROR
          LOG_PID
          LOG_AUTH
          LOG_AUTHPRIV
          LOG_CRON
          LOG_DAEMON
          LOG_FTP
          LOG_KERN
          LOG_LOCAL0
          LOG_LOCAL1
          LOG_LOCAL2
          LOG_LOCAL3
          LOG_LOCAL4
          LOG_LOCAL5
          LOG_LOCAL6
          LOG_LOCAL7
          LOG_LPR
          LOG_MAIL
          LOG_NEWS
          LOG_SYSLOG
          LOG_USER
          LOG_UUCP
          LOG_EMERG
          LOG_ALERT
          LOG_CRIT
          LOG_ERR
          LOG_WARNING
          LOG_NOTICE
          LOG_INFO
          LOG_DEBUG
          )
  )

(select-module gauche.syslog)

(dynamic-load "syslog")

(provide "gauche/syslog")


