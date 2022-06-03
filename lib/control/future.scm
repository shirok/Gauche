;;;
;;; control.future - future implementation
;;;
;;;   Copyright (c) 2020-2022  Shiro Kawai  <shiro@acm.org>
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

;; This is a provisional implementation; we'll use implicit thread pool
;; to avoid thread creation overhead eventually.

;; Guile and Racket uses 'touch' to retrive the result of a future, but
;; that name seems too generic.  We adopt 'future-get'.

;; If a future already finished computation, 'future-get' returns immediately
;; with the result value.  Otherwise, it blocks until the result is available,
;; unless timeout is specified.  Subsequent 'future-get' returns the same
;; result.
;;
;; If the concurrent computation raises an exception, it is caught, and
;; re-raised when first 'future-get'.  It is undefined if future-get is
;; called again on such a future---it depends on the behavior of thread-join!,
;; but the behavior of calling thread-join! again on exceptionally terminated
;; thread isn't defined either.  Currently, the second call of future-get
;; won't raise an exception and returns #<undef>, but do not count on
;; the behavior.

(define-module control.future
  (use gauche.threads)
  (export <future> future? future make-future future-done? future-get))
(select-module control.future)

(define-class <future> ()
  ;; all slots must be private
  ((%thread    :init-keyword :thread)))

(define-syntax future
  (syntax-rules ()
    [(_ expr)
     (make <future>
       :thread (thread-start! (make-thread (lambda () (values->list expr)))))]))

(define (make-future thunk)
  (future (thunk)))

(define (future? obj) (is-a? obj <future>))

(define (future-done? future)
  (assume-type future <future>)
  (eq? (thread-state (~ future'%thread)) 'terminated))

(define (future-get future :optional (timeout #f) (timeout-val #f))
  (assume-type future <future>)
  (guard (e [(<uncaught-exception> e) (raise (~ e'reason))])
    (apply values (thread-join! (~ future'%thread) timeout timeout-val))))
