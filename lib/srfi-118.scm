;;;
;;; srfi-118 - simple adjustable-size strings
;;;
;;;   Copyright (c) 2015-2019  Shiro Kawai  <shiro@acm.org>
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

;; In Gauche, string bodies are immutable and we just use an extra
;; indirection to realize mutable strings.  So it is trivial to
;; implement size-changing mutation, but it doesn't give you
;; any performance advantage---the cost of string-append! and string-replace!
;; are the same as string-append and string-replace, respectively.
;; We include srfi-118 for the portability.

(define-module srfi-118
  (use srfi-13)
  (export string-append!
          string-replace!))
(select-module srfi-118)

(define (string-append! string . vals)
  ($ (with-module gauche.internal %string-replace-body!) string
     (with-output-to-string
       (^[]
         (display string)
         (for-each (^v (if (or (char? v) (string? v))
                         (display v)
                         (error "string-append! expects characters/strings in the arguments, but got:" v)))
                   vals)))))

(define (string-replace! dst dst-start dst-end src . args)
  ($ (with-module gauche.internal %string-replace-body!) dst
     (apply string-replace dst src dst-start dst-end args)))
