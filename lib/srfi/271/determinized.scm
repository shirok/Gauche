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

;; We handle buffering by ourselves, in order to allow saving the state.
(define-constant buffer-size 256)       ;must be a multiple of 8

(define-class <random-port-state> ()
  (;; All slots private
   (xos   :init-keyword :xos)           ;<xos-random>
   (buf   :init-keyword :buf)           ;u8vector - generated octets
   (index :init-keyword :index)         ;# of octets in buf already read
   ))

;; API
(define (make-random-port :optional (initializer #f))
  ;; We cache each component of state in local vars for speed.
  (define st (%get-state initializer))
  (define xos (~ st'xos))
  (define buf (~ st'buf))
  (define len (u8vector-length buf))
  (define u64buf (uvector-alias <u64vector> buf)) ;u64 view of buf
  (define index (~ st'index))

  ;; Generate the next chunk of octets.  Byte-swapping is to produce
  ;; the same sequence of octets from the same seed.
  (define fill-buffer!
    (if (eq? (native-endian) 'little-endian)
      (^[]
        (xos-random-fill-u64vector! xos u64buf)
        (set! index 0))
      (^[]
        (xos-random-fill-u64vector! xos u64buf)
        (u64vector-swap-bytes! u64buf)
        (set! index 0))))

  (define (getb)
    (when (= index len) (fill-buffer!))
    (rlet1 b (u8vector-ref buf index)
      (inc! index)))

  (define (gets size)
    (let1 v (make-u8vector size)
      (let loop ([i 0])
        (if (= i size)
          (u8vector->string v)
          (begin
            (when (= index len) (fill-buffer!))
            (let1 n (min (- size i) (- len index))
              (u8vector-copy! v i buf index (+ index n))
              (set! index (+ index n))
              (loop (+ i n))))))))

  (define (take-snapshot)
    (make <random-port-state>
      :xos (xos-random-copy xos)
      :buf (u8vector-copy buf)
      :index index))

  (rlet1 p (make <virtual-input-port> :getb getb :gets gets)
    (port-attribute-set! p 'random-state-snapshot take-snapshot)))

(define (%get-state initializer)
  (define (new-state seed)
    (make <random-port-state>
      :xos (make-xos-random :seed seed :private? #t)
      :buf (make-u8vector buffer-size 0)
      :index buffer-size))               ;the buffer is empty
  (cond
   [(not initializer)
    (let ([v 0]
          [p (rnd:make-random-port)])
      (dotimes [8]
        (set! v (logior (ash v 8) (read-u8 p))))
      (close-port p)
      (new-state v))]
   [(input-port? initializer)
    (let loop ([i 0] [v 0])
      (if (= i 8)
        (new-state v)
        (let1 b (read-u8 initializer)
          (if (eof-object? b)
            (error <random-port-initialization-error>
                   "Initializer port does not have enough bytes:" initializer)
            (loop (+ i 1) (logior (ash v 8) b))))))]
   [(random-port-state? initializer)
    ;; Copy it, so that the new port won't affect the given state.
    (make <random-port-state>
      :xos (xos-random-copy (~ initializer'xos))
      :buf (u8vector-copy (~ initializer'buf))
      :index (~ initializer'index))]
   [else
     (error <random-port-initialization-error>
            "Random port initializer must be an input port or random state, \
             but got:" initializer)]))

(define (%state-snapshot port)
  (and (port? port)
       (port-attribute-ref port 'random-state-snapshot #f)))

;; API
(define (random-port? obj)
  (procedure? (%state-snapshot obj)))

;; API
(define (random-port-state port)
  (assume (random-port? port))
  ((%state-snapshot port)))

;; API
(define (random-port-state? st)
  (is-a? st <random-port-state>))

;; API
(define (random-port-state=? a b . rest)
  (assume (random-port-state? a))
  (assume (random-port-state? b))
  (and (%state=? a b)
       (or (null? rest)
           (apply random-port-state=? b rest))))

;; Two states behave the same iff the PRNG states are the same and the
;; octets yet to be read are the same.  The part of the buffer that's
;; already read doesn't matter.
(define (%state=? a b)
  (and (xos-random-state=? (~ a'xos) (~ b'xos))
       (equal? (subuvector/shared (~ a'buf) (~ a'index))
               (subuvector/shared (~ b'buf) (~ b'index)))))
