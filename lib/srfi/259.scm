;;;
;;; SRFI-259 - tagged procedures with type safety
;;;
;;;   Copyright (c) 2025  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi.259
  (use util.match)
  (use gauche.record)
  (export define-procedure-tag))
(select-module srfi.259)

;; We diretly use Gauche's underlying tagging mechanism, instead of
;; building on top of srfi-229.

(define %procedure-copy
  (with-module gauche.internal %procedure-copy))
(define %procedure-tags-alist
  (with-module gauche.internal %procedure-tags-alist))

(define-syntax define-procedure-tag
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ ctor pred accessor)
        (quasirename r
          `(define-values [,ctor ,pred ,accessor]
             (make-procedure-tag-procedures ',ctor)))]))))

(define (make-procedure-tag-procedures name)
  (define unique (gensym (x->string name)))
  (define (ctor value orig-proc)
    (assume-type orig-proc <procedure>)
    (%procedure-copy orig-proc
                     (acons unique value
                            (%procedure-tags-alist orig-proc))))
  (define (pred proc)
    (and (procedure? proc)
         (boolean (assq unique (%procedure-tags-alist proc)))))
  (define (accessor proc)
    (assume-type proc <procedure>)
    (match (assq unique (%procedure-tags-alist proc))
      [(_ . val) val]
      [_ (errorf "Procedure not tagged with ~s: ~s" name proc)]))
  (values ctor pred accessor))
