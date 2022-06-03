;;;
;;; srfi-185 - Linear adjustable-length strings
;;;
;;;   Copyright (c) 2020-2022  Shiro Kawai  <shiro@acm.org>
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
;; indirection to realize mutable strings.  Lenght-changing mutation
;; doesn't give any performance advantage at all.  So we chose not to
;; reuse the argument of linear-update procedures.

(define-module srfi-185
  (use srfi-13)
  (export string-append-linear!
          string-replace-linear!
          string-append!
          string-replace!))
(select-module srfi-185)

(define (string-append-linear! dst . args)
  (string-concatenate (cons dst (map (^s (if (char? s) (string s) s)) args))))

(define (string-replace-linear! dst dst-start dst-end src
                                :optional src-start src-end)
  (string-replace dst src dst-start dst-end src-start src-end))

(define-syntax string-append!
  (syntax-rules ()
    [(_ dst . args) (set! dst (string-append-linear! dst . args))]))

(define-syntax string-replace!
  (syntax-rules ()
    [(_ dst dstart dend src . opts)
     (set! dst (string-replace dst src dstart dend . opts))]))
