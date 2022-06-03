;;;
;;; srfi-229 - Procedure Tags
;;;
;;;   Copyright (c) 2021-2022  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi-229
  (export case-lambda/tag lambda/tag procedure/tag? procedure-tag))
(select-module srfi-229)

(define %procedure-copy
  (with-module gauche.internal %procedure-copy))
(define %procedure-tags-alist
  (with-module gauche.internal %procedure-tags-alist))

;; srfi-229 is implemented on top of Gauche's internal procedure tags.
(define-syntax lambda/tag
  (syntax-rules ()
    [(lambda/tag tag-expr formals body ...)
     (%procedure-copy (lambda formals body ...)
                      `((srfi-229-tag . ,tag-expr)))]))

(define-syntax case-lambda/tag
  (syntax-rules ()
    [(case-lambda/tag tag-expr (formals body ...) ...)
     (%procedure-copy (case-lambda (formals body ...) ...)
                      `((srfi-229-tag . ,tag-expr)))]))

(define (procedure/tag? proc)
  (and (procedure? proc)
       (boolean (assq 'srfi-229-tag (%procedure-tags-alist proc)))))

(define (procedure-tag proc)
  (assume-type proc <procedure>)
  (if-let1 p (assq 'srfi-229-tag (%procedure-tags-alist proc))
    (cdr p)
    (error "Procedure doesn't have a tag:" proc)))
