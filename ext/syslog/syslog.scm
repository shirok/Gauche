;;;
;;; syslog - syslog interface
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

(define-module gauche.syslog
  (export sys-openlog sys-syslog sys-closelog sys-logmask sys-setlogmask)
  )
(select-module gauche.syslog)

(inline-stub
 (declcode
  "#include <gauche/extend.h>"
  "#ifdef HAVE_SYSLOG_H"
  "#include <syslog.h>"
  "#endif")

 ;; On the platforms which don't have syslog facility, should we raise
 ;; a "not implemented" error, or simply discard the requests?  I tend to
 ;; think, for a logging feature, it is more useful to discard them.

 (define-cproc sys-openlog
   (ident::<const-cstring> option::<fixnum> facility::<fixnum>) ::<void>
   (.if "defined(HAVE_SYSLOG)" (openlog ident option facility)))

 (define-cproc sys-syslog (prio::<fixnum> message::<const-cstring>) ::<void>
   (.if "defined(HAVE_SYSLOG)" (syslog prio "%s" message)))

 (define-cproc sys-closelog () ::<void>
   (.if "defined(HAVE_SYSLOG)" (closelog)))

 (when "defined(HAVE_SYSLOG)"
   (initcode "Scm_AddFeature(\"gauche.sys.syslog\", NULL);"))

 (define-cproc sys-logmask (prio::<fixnum>) ::<fixnum>
   (.if "defined(HAVE_SETLOGMASK)" (return (LOG_MASK prio)) (return 0)))

 (define-cproc sys-setlogmask (mask::<fixnum>) ::<fixnum>
   (.if "defined(HAVE_SETLOGMASK)" (return (setlogmask mask)) (return 0)))

 (when "defined(HAVE_SETLOGMASK)"
   (initcode "Scm_AddFeature(\"gauche.sys.setlogmask\", NULL);"))

 ;; We need to define at least these three logging option constants,
 ;; for they are referenced in gauche.logger.  The actual value doesn't matter.
 (declcode
  "#ifndef LOG_PID"
  "#define LOG_PID 0"
  "#endif"
  "#ifndef LOG_INFO"
  "#define LOG_INFO 1"
  "#endif"
  "#ifndef LOG_USER"
  "#define LOG_USER 2"
  "#endif")

 ;; openlog options
 (define-enum-conditionally LOG_CONS)
 (define-enum-conditionally LOG_NDELAY)
 (define-enum-conditionally LOG_NOWAIT)
 (define-enum-conditionally LOG_ODELAY)
 (define-enum-conditionally LOG_PERROR)
 (define-enum-conditionally LOG_PID)

 ;; facility
 (define-enum-conditionally LOG_AUTH)
 (define-enum-conditionally LOG_AUTHPRIV)
 (define-enum-conditionally LOG_CRON)
 (define-enum-conditionally LOG_DAEMON)
 (define-enum-conditionally LOG_FTP)
 (define-enum-conditionally LOG_KERN)
 (define-enum-conditionally LOG_LOCAL0)
 (define-enum-conditionally LOG_LOCAL1)
 (define-enum-conditionally LOG_LOCAL2)
 (define-enum-conditionally LOG_LOCAL3)
 (define-enum-conditionally LOG_LOCAL4)
 (define-enum-conditionally LOG_LOCAL5)
 (define-enum-conditionally LOG_LOCAL6)
 (define-enum-conditionally LOG_LOCAL7)
 (define-enum-conditionally LOG_LPR)
 (define-enum-conditionally LOG_MAIL)
 (define-enum-conditionally LOG_NEWS)
 (define-enum-conditionally LOG_SYSLOG)
 (define-enum-conditionally LOG_USER)
 (define-enum-conditionally LOG_UUCP)

 ;; level
 (define-enum-conditionally LOG_EMERG)
 (define-enum-conditionally LOG_ALERT)
 (define-enum-conditionally LOG_CRIT)
 (define-enum-conditionally LOG_ERR)
 (define-enum-conditionally LOG_WARNING)
 (define-enum-conditionally LOG_NOTICE)
 (define-enum-conditionally LOG_INFO)
 (define-enum-conditionally LOG_DEBUG)
 )

(export-if-defined LOG_CONS LOG_NDELAY LOG_NOWAIT LOG_ODELAY
                   LOG_PERROR LOG_PID LOG_AUTH LOG_AUTHPRIV
                   LOG_CRON LOG_DAEMON LOG_FTP LOG_KERN
                   LOG_LOCAL0 LOG_LOCAL1 LOG_LOCAL2 LOG_LOCAL3
                   LOG_LOCAL4 LOG_LOCAL5 LOG_LOCAL6 LOG_LOCAL7
                   LOG_LPR LOG_MAIL LOG_NEWS LOG_SYSLOG
                   LOG_USER LOG_UUCP LOG_EMERG LOG_ALERT
                   LOG_CRIT LOG_ERR LOG_WARNING LOG_NOTICE
                   LOG_INFO LOG_DEBUG)



