;;;
;;; signal.scm - with-signal-handlers
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
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

;; This file is to be autoloaded

(select-module gauche)
(use srfi-1)

;; (with-signal-handlers
;;   (clause ...)
;;   thunk)
;;
;; clause := (signals expr ...) or (signals => proc)
;; signals := an expr that evaluates to an integer, a list of integers or
;;            a sigset object
;; proc   := an expr that evaluates to a procedure that takes one arg

(define-syntax with-signal-handlers
  (syntax-rules (=>)
    [(_ "loop" () handlers thunk)
     (%with-signal-handlers (list . handlers) thunk)]
    [(_ "loop" ((sigs => proc) . clauses) handlers thunk)
     (with-signal-handlers "loop" clauses
                           ((cons (%make-sigset sigs) proc) . handlers)
                           thunk)]
    [(_ "loop" ((sigs expr ...) . clauses) handlers thunk)
     (with-signal-handlers "loop" clauses
                           ((cons (%make-sigset sigs) (^ _ expr ...))
                            . handlers)
                           thunk)]
    [(_ "loop" whatever handlers thunk)
     (syntax-error "bad clause in with-signal-handlers" whatever)]
    [(_ (clause ...) thunk)
     (with-signal-handlers "loop" (clause ...) () thunk)]
    [(_ . whatever)
     (syntax-error "malformed with-signal-handlers"
                   (with-signal-handlers . whatever))]
    ))

(define (%make-sigset signals)
  (cond [(is-a? signals <sys-sigset>) signals]
        [(or (integer? signals) (eq? signals #t))
         (sys-sigset-add! (make <sys-sigset>) signals)]
        [(and (list? signals)
              (every integer? signals))
         (apply sys-sigset-add! (make <sys-sigset>) signals)]
        [else (error "bad signal set" signals)]))

(define (%with-signal-handlers handlers-alist thunk)
  (let1 ohandlers (get-signal-handlers)
    (dynamic-wind
     (^[] (for-each (^p (set-signal-handler! (car p) (cdr p)))
                    handlers-alist))
     thunk
     (^[] (for-each (^p (set-signal-handler! (car p) (cdr p)))
                    ohandlers)))))



