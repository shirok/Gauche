;;;
;;; control.cseq - concurrent sequence
;;;
;;;   Copyright (c) 2021-2022  Shiro Kawai  <shiro@acm.org>
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

;; EXPERIMENTAL

;; cseq is like lseq, but producer and consumer run concurrently

(define-module control.cseq
  (use gauche.generator)
  (use gauche.threads)
  (use data.queue)
  (export generator->cseq
          coroutine->cseq)
  )
(select-module control.cseq)

(define (%concurrent-generator->lseq q thunk)
  (define t (thread-start! (make-thread thunk)))
  (generator->lseq (^[] (let1 r (dequeue/wait! q)
                          (when (eof-object? (car r))
                            (guard (e [(<uncaught-exception> e)
                                       (raise (~ e 'reason))]
                                      [else (raise e)])
                              (thread-join! t)))
                          (apply values r)))))

(define (generator->cseq producer :key (queue-length #f))
  (define q (make-mtqueue :max-length (or queue-length 64)))
  (define (thunk)
    (guard [e (else (enqueue/wait! q (list (eof-object))) (raise e))]
      (let loop ()
        (receive (v . rest) (producer)
          (enqueue/wait! q (cons v rest))
          (unless (eof-object? v)
            (loop))))))
  (%concurrent-generator->lseq q thunk))

(define (coroutine->cseq proc :key (queue-length #f))
  (define q (make-mtqueue :max-length (or queue-length 64)))
  (define (yielder . vals) (enqueue/wait! q vals))

  (define (thunk)
    (guard [e (else (enqueue/wait! q (list (eof-object))) (raise e))]
      (proc yielder)
      (enqueue/wait! q (list (eof-object)))))
  (%concurrent-generator->lseq q thunk))
