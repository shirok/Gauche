;;;
;;; libmacro.scm - built-in macros
;;;
;;;   Copyright (c) 2000-2018  Shiro Kawai  <shiro@acm.org>
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

(select-module gauche)
(use util.match)
(declare (keep-private-macro quasirename define-compiler-macro))

;; quasirename
(define-syntax quasirename
  (er-macro-transformer
   (^[f r c]
     (define unquote. (r'unquote))
     (define (unquote? x)
       (and (or (symbol? x) (identifier? x))
            (c (r x) unquote.)))
     (define unquote-splicing. (r'unquote-splicing))
     (define (unquote-splicing? x)
       (and (or (symbol? x) (identifier? x))
            (c (r x) unquote-splicing.)))
     (define cons. (r'cons))
     (define append. (r'append))
     (define vector. (r'vector))
     (define let. (r'let))
     (define tmp. (r'tmp))
     (match f
       [(_ rr ff)
        (define (rec ff)
          (match ff
            [((? unquote?) x) x]
            [(((? unquote-splicing?) x) . y)
             (if (null? y)
               x
               `(,append. ,x ,(rec y)))]
            [(x (? unquote?) y) `(,cons. ,(rec x) ,y)]
            [(x . y) `(,cons. ,(rec x) ,(rec y))]
            [(? symbol?) `(,tmp. ',ff)]
            [(? identifier?) `(,tmp. ',ff)]
            [(? vector?) (cons vector. (map rec (vector->list ff)))]
            [_ ff]))
        `(,let. ((,tmp. ,rr))
           ,(rec ff))]
       [_ (error "malformed quasirename:" f)]))))

;;;
;;; OBSOLETED - Tentative compiler macro 
;;;


;; TRANSIENT: Remove by 1.0
(define-macro (define-compiler-macro name xformer-spec)
  (error "define-compiler-macro is obsoleted.  Use define-inline/syntax."))
