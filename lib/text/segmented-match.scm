;;;
;;; text.segmented-match - Prefix match with segmented words
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

;; match c-w-i-f with call-with-input-file etc.

(define-module text.segmented-match
  (use srfi.13)
  (export make-segmented-prefix-matcher
          segmented-prefix?))
(select-module text.segmented-match)

;; This can be accelearated if we preprocess the corpus into a trie,
;; but let's start a simple case.

;; API
(define (segmented-prefix? pattern word :optional (separator #\-))
  (assume-type pattern <string>)
  (assume-type word <string>)
  (boolean ((make-segmented-prefix-matcher pattern separator) word)))

;; API
;; Returns a procedure that takes a string and...
;;  - Returns #f if pattern doesn't consist a segmented prefix
;;  - Returns #t if every segments of pattern is corresponding segments
;;    of input, and no more segments left in the input
;;  - Returns a string, which is a remaining segments of the input.
(define (make-segmented-prefix-matcher pattern separator)
  (define (rec segs rest)
    (cond [(null? segs) rest]
          [(string-prefix? (car segs) rest) ;(car segs) never contain separator
           (if-let1 rest (string-scan rest separator 'after)
             (rec (cdr segs) rest)
             (null? (cdr segs)))]
          [else #f]))
  (define segments (string-split pattern separator))
  (^[word] (rec segments word)))
