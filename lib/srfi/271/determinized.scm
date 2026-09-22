;;;
;;; SRFI-271 - random ports / determinized
;;;
;;;   Copyright (c) 2026  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi.271.determinized
  (use gauche.vport)
  (use gauche.uvector)
  (use math.random.xos)
  (use srfi.271.randomized :prefix rnd:)
  (export make-random-port
          random-port?
          random-port-state
          random-port-state?
          random-port-state=?
          random-port-initialization-error?)
  )
(select-module srfi.271.determinized)

(define-condition-type <random-port-initialization-error> <serious-condition>
  random-port-initialization-error?)

(define-class <random-port-state> ()
  (;; All slots private
   (state :init-keyword :state)))

;; API
(define (make-random-port :optional (initializer #f))
  (let1 xos (%get-xos initializer)
    (rlet1 p (make <buffered-input-port>
               :fill (^[buf]
                       (rlet1 len (u8vector-length buf)
                         (let outer ([i 0]
                                     [v (xos-random-u64 xos)])
                           (let inner ([k 0] [i i] [v v])
                             (unless (>= i len)
                               (if (= k 8)
                                 (outer i (xos-random-u64 xos))
                                 (begin
                                   (u8vector-set! buf i (logand v #xff))
                                   (inner (+ k 1) (+ i 1) (ash v -8))))))))))
      (port-attribute-set! p 'xos xos))))

(define (%get-xos initializer)
  (cond
   [(not initializer)
    (let ([v 0]
          [p (rnd:make-random-port)])
      (dotimes [8]
        (set! v (logior (ash v 8) (read-u8 p))))
      (close-port p)
      (make-xoshiro256 :seed v :private? #t))]
   [(input-port? initializer)
    (let loop ([i 0] [v 0])
      (if (= i 8)
        (make-xoshiro256 :seed v :private? #t)
        (let1 b (read-u8 initializer)
          (if (eof-object? b)
            (error <random-port-initialization-error>
                   "Initializer port does not have enough bytes:" initializer)
            (loop (+ i 1) (logior (ash v 8) b))))))]
   [(random-port-state? initializer)
    (copy-xoshiro256 (~ initializer'state))]
   [else
     (error <random-port-initialization-error>
            "Random port initializer must be an input port or random state, \
             but got:" initializer)]))

;; API
(define (random-port? obj)
  (and (port? obj)
       (is-a? (port-attribute-ref obj 'xos #f) <xoshiro256>)))

(define (%random-port-xos port)
  (port-attribute-ref port 'xos))

(define (random-port-state port)
  (assume (random-port? port))
  (make <random-port-state>
    :state (copy-xoshiro256 (%random-port-xos port))))

(define (random-port-state? st)
  (is-a? st <random-port-state>))

(define (random-port-state=? a b . rest)
  (assume (random-port-state? a))
  (assume (random-port-state? b))
  (and (xos-random-state=? (~ a'state) (~ b'state))
       (or (null? rest)
           (apply random-port-state=? b rest))))
