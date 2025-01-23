;;;
;;; control.timeout - evaluation with timeout
;;;
;;;   Copyright (c) 2025  Shiro Kawai  <shiro@acm.org>
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

(define-module control.timeout
  (use gauche.threads)
  (export with-timeout
          do/timeout))
(select-module control.timeout)

(define (with-timeout thunk timeout :optional (timeout-thunk (^[] #f)))
  (let1 t (thread-start! (make-thread (^[] (values->list (thunk)))))
    (guard (e [(uncaught-exception? e)
               (raise (uncaught-exception-reason e))]
              [(join-timeout-exception? e)
               (thread-terminate! t)
               (timeout-thunk)]
              [(terminated-thread-exception? e)
               (raise e)]
              [else (raise e)])
      (apply values (thread-join! t timeout)))))

(define-syntax do/timeout
  (syntax-rules ()
    [(_ (timeout timeout-expr) body ...)
     (with-timeout (^[] body ...) timeout (^[] timeout-expr))]
    [(_ (timeout) body ...)
     (with-timeout (^[] body ...) timeout)]))
