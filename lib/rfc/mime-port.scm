;;;
;;; mime-port.scm - submodule to read from mime part body
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

;; This module is autoloaded from rfc.mime.  You don't need to "use" this
;; directly.
(define-module rfc.mime-port
  (use gauche.uvector)
  (use gauche.vport)
  (use data.queue)
  (use util.match)
  (export make-mime-port))
(select-module rfc.mime-port)

;;===============================================================
;; Virtual port to recognize mime boundary
;;

(define-class <mime-port> (<buffered-input-port>)
  ((state :init-form 'prologue)
   ;; prologue -> boundary <-> body -> eof
   ))

;; Creates a procedural port, which reads from SRCPORT until it reaches
;; either EOF or MIME boundary.  Basically it runs a DFA.
(define (make-mime-port boundary srcport)
  (define q (make-queue))
  (define --boundary (string->u8vector #"--~boundary"))

  (define port (make <mime-port>))

  (define eof (read-from-string ""))

  (define (deq! q)
    (if (queue-empty? q) eof (dequeue! q)))

  (define (fifo! q b)
    (enqueue! q b) (dequeue! q))

  (define (getb)
    (if (queue-empty? q)
      (case (ref port 'state)
        [(prologue) (skip-prologue)]
        [(boundary eof) eof]
        [else (newb)])
      (dequeue! q)))

  (define (newb)
    (match (read-byte srcport)
      [(and #x0d b) ;; CR, check to see LF
       (let1 b2 (peek-byte srcport)
         (if (eqv? b2 #x0a)
           (begin
             (read-byte srcport)
             (enqueue! q b #x0a)
             (check-boundary))
           b))]
      [(and #x0a b) ;; LF, check boundary
       (enqueue! q b) (check-boundary)]
      [(? eof-object?) (set! (ref port 'state) 'eof) eof]
      [b b]))

  (define (check-boundary)
    (let loop ((b   (peek-byte srcport))
               (ind 0)
               (max (u8vector-length --boundary)))
      (cond [(eof-object? b) (deq! q)]
            [(= ind max)
             (cond [(memv b '(#x0d #x0a)) ;;found boundary
                    (read-byte srcport)   ;;consume LF or CRLF
                    (when (and (eqv? #x0d b)
                               (eqv? #x0a (peek-byte srcport)))
                      (read-byte srcport))
                    (dequeue-all! q)
                    (set! (ref port 'state) 'boundary)
                    eof]
                   [(eqv? b #x2d) ;; maybe end boundary
                    (enqueue! q (read-byte srcport))
                    (cond [(eqv? (peek-byte srcport) #x2d);; yes, end boundary
                           (read-byte srcport)
                           (dequeue-all! q)
                           (skip-epilogue)]
                          [else (deq! q)])]
                   [else (deq! q)])]
            [(= b (u8vector-ref --boundary ind))
             (enqueue! q (read-byte srcport))
             (loop (peek-byte srcport) (+ ind 1) max)]
            [(queue-empty? q) (newb)]
            [else (dequeue! q)])))

  ;; Reads past the first boundary.  The first boundary may appear
  ;; at the beginning of the message (instead of after CRLF), so
  ;; we need slightly different handling than the normal getb.
  (define (skip-prologue)
    (let loop ((b (check-boundary)))
      (cond
       [(eof-object? b)
        (cond [(eq? (ref port 'state) 'boundary) ; we've found the boundary
               (set! (ref port 'state) 'body) (getb)]
              [else                              ; no boundary found
               (set! (ref port 'state) 'eof) eof])]
       [(queue-empty? q) (loop (newb))]
       [else (dequeue-all! q) (loop (newb))])))

  (define (skip-epilogue)
    (let loop ((b (read-byte srcport)))
      (if (eof-object? b)
        (begin (set! (ref port 'state) 'eof) b)
        (loop (read-byte srcport)))))

  ;; fills vector, until it sees either
  ;;   (1) vec got full
  ;;   (2) srcport reaches EOF
  ;;   (3) mime-boundary is read
  (define (fill vec)
    (let1 len (u8vector-length vec)
      (let loop ((ind 0))
        (if (= ind len)
          len
          (let1 b (getb)
            (if (eof-object? b)
              ind
              (begin (u8vector-set! vec ind b)
                     (loop (+ ind 1)))))))))

  (set! (ref port 'fill) fill)
  port)

