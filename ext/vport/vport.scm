;;;
;;; gauche.vport - virtual port
;;;
;;;   Copyright (c) 2004-2013  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.vport
  (use gauche.uvector)
  (export <virtual-input-port>
          <virtual-output-port>
          <buffered-input-port>
          <buffered-output-port>
          open-input-uvector
          open-output-uvector
          open-input-limited-length-port
          ))
(select-module gauche.vport)

(dynamic-load "gauche--vport")

;;=======================================================
;; A port backed up by an uniform vector
;;

(define (open-input-uvector uvector)
  (let* ([src (if (u8vector? uvector)
                uvector
                (uvector-alias <u8vector> uvector))]
         [index 0]
         [len (u8vector-length src)])
    (define (filler buf)
      (if (>= index len)
        #f
        (let1 req (u8vector-length buf)
          (if (>= req (- len index))
            (rlet1 count (- len index)
              (u8vector-copy! buf 0 src index)
              (inc! index count))
            (begin
              (u8vector-copy! buf 0 src index (+ index req))
              (inc! index req)
              req)))))
    (define (seeker offset whence)
      (cond [(= whence SEEK_SET)
             (set! index (clamp offset 0 len))]
            [(= whence SEEK_CUR)
             (set! index (clamp (+ index offset) 0 len))]
            [(= whence SEEK_END)
             (set! index (clamp (+ len offset) 0 len))])
      index)
    (make <buffered-input-port> :fill filler :seek seeker)))

(define (open-output-uvector uvector)
  (let* ([dst (if (u8vector? uvector)
                uvector
                (uvector-alias <u8vector> uvector))]
         [index 0]
         [len (u8vector-length dst)])
    (define (flusher buf force?)
      (let1 req (u8vector-length buf)
        (cond [(>= index len) req]  ;; simply discard the data
              [(> req (- len index))
               (let1 count (- len index)
                 (u8vector-copy! dst index buf 0 count)
                 (inc! index count)
                 req)]
              [else
               (u8vector-copy! dst index buf 0 req)
               (inc! index req)
               req])))
    (define (seeker offset whence)
      (cond [(= whence SEEK_SET)
             (set! index (clamp offset 0 len))]
            [(= whence SEEK_CUR)
             (set! index (clamp (+ index offset) 0 len))]
            [(= whence SEEK_END)
             (set! index (clamp (+ len offset) 0 len))])
      index)
    (make <buffered-output-port> :flush flusher :seek seeker)))

;;=======================================================
;; A port with limited-length input/output
;;

(define (open-input-limited-length-port source limit
                                        :key (limit-reached #f)
                                             (eof-reached #f) (closed #f))
  (let ([nrest limit]
        [eof #f])
    (define (filler buf)
      (if (or (<= nrest 0) eof)
        (if limit-reached (limit-reached buf) 0)
        (let* ([len   (u8vector-length buf)]
               [nread (read-block! buf source 0 (min nrest len))])
          (cond [(eof-object? nread)
                 (set! eof #t)
                 (if eof-reached (eof-reached buf) 0)]
                [else
                 (dec! nrest nread)
                 nread]))))
    (define (closer)
      (when closed (closed)))
    (make <buffered-input-port> :fill filler :close closer)))

