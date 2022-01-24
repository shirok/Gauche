;;;
;;; text.multicolumn - Show text in multicolumn format
;;;
;;;   Copyright (c) 2022  Shiro Kawai  <shiro@acm.org>
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

(define-module text.multicolumn
  (use util.match)
  (export display-multicolumn))
(select-module text.multicolumn)

;; Show a list of words in multicolumn format (imagine output of 'ls').

(define (display-multicolumn strs
                             :key (width 80) ; total width
                                  (minimum-width 8) ; minimum column width
                                  (max-columns 4)
                                  (order 'column))
  (let* ([col-width (max (+ 2 (apply max (map string-length strs)))
                         minimum-width)]
         [cols (min max-columns (quotient width col-width))]
         [rows (quotient (+ (length strs) cols -1) cols)])
    (define (show-1 words)
      (let loop ([words words])
        (match words
          [(w) (display w) (newline)]
          [(w #f . _) (display w) (newline)]
          [(w . ws) (format #t "~va" col-width w) (loop ws)])))
    (ecase order
      [(column) ($ for-each (^ ss (show-1 ss)) $* slices strs rows #t #f)]
      [(row) ($ for-each show-1 $ slices strs cols)])))
