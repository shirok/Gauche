;;;
;;; srfi-1981 - POSIX system call exceptions
;;;
;;;   Copyright (c) 2020  Shiro Kawai  <shiro@acm.org>
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

;; Gauche traditionally used <system-error> for system call exceptions.
;; srfi-198 adds a few fields that are not in <system-error>.  So we use
;; compound condition for that.   Note that syscall-error? responds #t
;; to a bare <system-error> as well, for the interoperability.
;; We may integrate those extra slots to <system-error> eventually, so
;; do not rely on this internal arrangement.

(define-module srfi-198
  (export make-syscall-error
          syscall-error?
          syscall-error:errno
          syscall-error:message
          syscall-error:procedure-name
          syscall-error:syscall-name
          syscall-error:data
          errno-error))
(select-module srfi-198)

(define-condition-type <syscall-error-mixin> <condition>
  #f
  (procedure-name)
  (syscall-name)
  (data))

(define (make-syscall-error errno message procedure-name syscall-name data)
  (condition (<system-error> 
              (errno errno)
              (message message))
             (<syscall-error-mixin>
              (procedure-name procedure-name)
              (syscall-name syscall-name)
              (data data))))

(define (syscall-error? x) (condition-has-type? x <system-error>))

(define (syscall-error:errno x)
  (assume (syscall-error? x))
  (condition-ref x 'errno))

(define (syscall-error:message x)
  (assume (syscall-error? x))
  (condition-ref x 'message))

(define (syscall-error:procedure-name x)
  (assume (syscall-error? x))
  (if (condition-has-type? x <syscall-error-mixin>)
    (condition-ref x 'procedure-name)
    'unknown))

(define (syscall-error:syscall-name x)
  (assume (syscall-error? x))
  (if (condition-has-type? x <syscall-error-mixin>)
    (condition-ref x 'syscall-name)
    'unknown))

(define (syscall-error:data x)
  (assume (syscall-error? x))
  (if (condition-has-type? x <syscall-error-mixin>)
    (condition-ref x 'data)
    '()))

(define (errno-error errno procedure-name syscall-name . objs)
  (raise (make-syscall-error errno 
                             (sys-strerror errno)
                             procedure-name 
                             syscall-name 
                             objs)))
