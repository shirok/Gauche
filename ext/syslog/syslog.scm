;;;
;;; syslog - syslog interface
;;;  
;;;   Copyright (c) 2000-2008  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: syslog.scm,v 1.7 2008-05-10 13:35:43 shirok Exp $
;;;

(define-module gauche.syslog
  (export sys-openlog sys-syslog sys-closelog sys-logmask sys-setlogmask))

(select-module gauche.syslog)

(dynamic-load "syslog")

;; Provides dummy functions.  It is debatable whether we should raise
;; a "not implemented" error, or simply discards the requests.  For now,
;; we jsut discards the requests, i.e. log messages will go to void.
(cond-expand
 (gauche.sys.syslog)
 (else
  (define (sys-openlog ident option facility) (undefined))
  (define (sys-syslog prio message) (undefined))
  (define (sys-closelog) (undefined))))

(cond-expand
 (gauche.sys.setlogmask)
 (else
  (define (sys-logmask prio) 0)
  (define (sys-setlogmask mask) 0)))

;; We need to define at least these three, for they are referenced in
;; gauche.logger.
(define-macro (define-dummy sym val)
  (if (global-variable-bound? (find-module 'gauche.syslog) sym)
    #f
    `(define ,sym ,val)))

(define-dummy |LOG_PID| 0)
(define-dummy |LOG_INFO| 1)
(define-dummy |LOG_USER| 2)

(export-if-defined |LOG_CONS|
                   |LOG_NDELAY|
                   |LOG_NOWAIT|
                   |LOG_ODELAY|
                   |LOG_PERROR|
                   |LOG_PID|
                   |LOG_AUTH|
                   |LOG_AUTHPRIV|
                   |LOG_CRON|
                   |LOG_DAEMON|
                   |LOG_FTP|
                   |LOG_KERN|
                   |LOG_LOCAL0|
                   |LOG_LOCAL1|
                   |LOG_LOCAL2|
                   |LOG_LOCAL3|
                   |LOG_LOCAL4|
                   |LOG_LOCAL5|
                   |LOG_LOCAL6|
                   |LOG_LOCAL7|
                   |LOG_LPR|
                   |LOG_MAIL|
                   |LOG_NEWS|
                   |LOG_SYSLOG|
                   |LOG_USER|
                   |LOG_UUCP|
                   |LOG_EMERG|
                   |LOG_ALERT|
                   |LOG_CRIT|
                   |LOG_ERR|
                   |LOG_WARNING|
                   |LOG_NOTICE|
                   |LOG_INFO|
                   |LOG_DEBUG|
                   )

(provide "gauche/syslog")


