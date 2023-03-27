;;;
;;; SRFI-207 - String-notated bytevectors
;;;
;;;   Copyright (c) 2023  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi.207
  (use gauche.unicode)
  (use gauche.uvector)
  (use rfc.base64)
  (use util.match)
  (export bytestring make-bytestring
          bytevector->hex-string hex-string->bytevector
          bytevector->base64 base64->bytevector
          bytestring->list make-bytestring-generator

          bytestring-pad bytestring-pad-right
          bytestring-trim bytestring-trim-right bytestring-trim-both
          bytestring-replace
          bytestreing<? bytestring>? bytestring<=? bytestring>=?

          bytestring-index bytestring-index-right
          bytestring-break bytestring-span
          bytestring-join bytestring-split

          read-textual-bytestring write-textual-bytestring
          write-binary-bytestring

          bytestring-error?)
  )
(select-module srfi.207)

(define-condition-type <bytestring-error> <error> bytestring-error?)

;; TODO: We create intermediate u8vectors to concatenate.  It is debatable
;; whether filling individual elements may be faster.  U8vector-concatenate
;; uses block copy written in C, so it may compensate the overhead of
;; u8vector allocations.

(define (x->u8vector obj)
  (cond [(u8vector? obj) obj]
        [(number? obj)
         (if (and (exact-integer? obj) (<= 0 obj 255))
           (u8vector obj)
           (error <bytestring-error>
                  "Out of range number for an element of bytevector:" obj))]
        [(char? obj)
         ;; SRFI-207 only allows ASCII; we interpret non-ASCII chars
         ;; as utf8 octet sequence.
         (let1 c (char->integer obj)
           (if (<= 0 c 127)
             (u8vector c)
             (string->utf8 (string c))))]
        [(string? obj)
         ;; SRFI-207 only allows ASCII strings; we allow any string and
         ;; interpret it as utf8 octet sequence.
         (cond-expand
          [gauche.ces.utf8 (string->u8vector obj 0 -1 #t)] ;avoid copying
          [else (string->utf8 obj)])]
        [else
         (error <bytestring-error>
                "Ivalid object to construct a bytestring:" obj)]))

(define (bytestring . args) (make-bytestring args))

(define (make-bytestring lis)
  (u8vector-concatenate (map x->u8vector lis)))

(define (make-bytestring! bv start lis)
  (assume-type bv <u8vector>)
  ;; TODO: This may be added to gauche.uvector
  (let1 uvs (map x->u8vector lis)
    (assume (<= (+ start (fold (^[uv sum] (+ sum (u8vector-length uv))) 0 uvs))
                (u8vector-length bv))
            "Destination bytevector overflow" bv)
    (let loop ([uvs uvs] [start start])
      (match uvs
        [() (undefined)]
        [(uv . uvs)
         (u8vector-copy! bv start uv)
         (loop uvs (+ start (u8vector-length uv)))]))))

(define (bytevector->hex-string bv)
  (assume-type bv <u8vector>)
  (with-output-to-string
    (^[] (u8vector-for-each (^b (format "~2,'0x" b)) bv))))

(define (hex-string->bytevector str)
  (assume-type str <string>)
  (let1 slen (string-length str)
    (assume (even? slen) "Hex string must have an even length:" str)
    (rlet1 bv (make-u8vector (ash slen -1))
      (let1 in (open-input-string str)
        (let loop ([i 0])
          (let* ([a (read-char)]
                 [b (read-char)])
            (unless (eof-object? a)
              (let ([aa (digit->integer a 16)]
                    [bb (digit->integer b 16)])
                (assume aa "Invalid hexdigit char:" a)
                (assume bb "Invalid hexdigit char:" b)
                (u8vector-set! bv i (+ (* aa 16) bb))))))))))
