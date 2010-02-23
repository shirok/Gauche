;;;
;;; threads.scm - thread related procedures.  to be autoloaded
;;;  
;;;   Copyright (c) 2000-2010  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.threads
  (export-all))
(select-module gauche.threads)

(dynamic-load "gauche--threads")

;;
;; Thread
;;

(define (thread? obj) (is-a? obj <thread>))

(define (thread-name thread)
  (check-arg thread? thread)
  (slot-ref thread 'name))

(define (thread-specific-set! thread value)
  (check-arg thread? thread)
  (slot-set! thread 'specific value))

(define thread-specific
  (getter-with-setter
   (lambda (thread)
     (check-arg thread? thread)
     (slot-ref thread 'specific))
   thread-specific-set!))

;;
;; Mutex
;;

(define (mutex? obj)  (is-a? obj <mutex>))

(define (mutex-name mutex)
  (check-arg mutex? mutex)
  (slot-ref mutex 'name))

(define (mutex-state mutex)
  (check-arg mutex? mutex)
  (slot-ref mutex 'state))

(define (mutex-specific-set! mutex value)
  (check-arg mutex? mutex)
  (slot-set! mutex 'specific value))

(define mutex-specific
  (getter-with-setter
   (lambda (mutex)
     (check-arg mutex? mutex)
     (slot-ref mutex 'specific))
   mutex-specific-set!))

(define (with-locking-mutex mutex thunk)
  (dynamic-wind
   (lambda () (mutex-lock! mutex))
   thunk
   (lambda () (mutex-unlock! mutex))))

;;
;; Condition variable
;;

(define (condition-variable? obj) (is-a? obj <condition-variable>))

(define (condition-variable-name cv)
  (check-arg condition-variable? cv)
  (slot-ref cv 'name))

(define (condition-variable-specific-set! cv value)
  (check-arg condition-variable? cv)
  (slot-set! cv 'specific value))

(define condition-variable-specific
  (getter-with-setter
   (lambda (cv)
     (check-arg condition-variable? cv)
     (slot-ref cv 'specific))
   condition-variable-specific-set!))
     
;;
;; Exceptions
;;

(define (join-timeout-exception? obj)
  (is-a? obj <join-timeout-exception>))

(define (abandoned-mutex-exception? obj)
  (is-a? obj <abandoned-mutex-exception>))

(define (terminated-thread-exception? obj)
  (is-a? obj <terminated-thread-exception>))

(define (uncaught-exception? obj)
  (is-a? obj <uncaught-exception>))

(define (uncaught-exception-reason exc)
  (check-arg uncaught-exception? exc)
  (slot-ref exc 'reason))

