;;;
;;; SRFI-239 - Destructuring lists
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

(define-module srfi.239
  (use util.match)
  (export list-case _))
(select-module srfi.239)

(define-syntax list-case
  (er-macro-transformer
   (^[f r c]
     (define id-_ (r '_))
     (define (default-pair-clause)
       (quasirename r
         `(^[x] (error "list-case: pair not allowed:" x))))
     (define (default-null-clause)
       (quasirename r
         `(^[x] (error "list-case: emptylist not allowed"))))
     (define (default-atom-clause)
       (quasirename r
         `(^[x] (error "list-case: non list not allowed:" x))))

     (define pair-clause #f)
     (define null-clause #f)
     (define atom-clause #f)

     (define (parse-clause! cl)
       (match cl
         [(((? identifier? h) . (? identifier? t)) body ...)
          (set! pair-clause
                (quasirename r
                  `(^[x]
                     (let ([,(replace_ h) (car x)]
                           [,(replace_ t) (cdr x)])
                       ,@body))))]
         [(() body ...)
          (set! null-clause
                (quasirename r
                  `(^[_] ,@body)))]
         [((? identifier? v) body ...)
          (set! atom-clause
                (quasirename r
                  `(^[x] (let ([,(replace_ v) x]) ,@body))))]
         [_ (error "Invalid clause for list-case:" cl)]))
     (define (replace_ x)
       (if (c id-_ x)
         (gensym "_")
         x))

     (match f
       [(_ expr clauses ...)
        (for-each parse-clause! clauses)
        (quasirename r
          `(let1 x ,expr
             (cond [(pair? x) (,(or pair-clause (default-pair-clause)) x)]
                   [(null? x) (,(or null-clause (default-null-clause)) x)]
                   [else (,(or atom-clause (default-atom-clause)) x)])))]
       [_ (error "Malformed list-case:" f)]))))
