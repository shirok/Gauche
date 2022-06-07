;;;
;;; srfi-232 - Flexible curried procedures
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

(define-module srfi-232
  (use util.match)
  (export curried define-curried))
(select-module srfi-232)

;; We carry around the name to be bound to the curreid procedure
;; if available, so that it can be displayed.

(define-syntax curried/name
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ name () body)
        (quasirename r `(begin ,@body))]
       [(_ name (? identifier? var) body)
        (quasirename r `(lambda ,var ,@body))]
       [(_ name (? proper-list? formals) body)
        (quasirename r
          `(rec ,name
             (case-lambda
               [() ,name]
               [,formals (begin ,@body)]
               [(,@formals . rest)
                ,(quasirename r
                   `(apply (,name ,@formals) rest))]
               [partial-args
                ,(quasirename r
                   `(apply pa$ ,name partial-args))])))]
       [(_ name dotted-formals body)
        (quasirename r
          `(rec ,name
             (case-lambda
               [() ,name]
               [,dotted-formals (begin ,@body)]
               [partial-args
                ,(quasirename r
                   `(apply pa$ ,name partial-args))])))]))))

(define-syntax curried
  (syntax-rules ()
    [(_ formals . body) (curried/name curried-procedure formals body)]))

(define-syntax define-curried
  (syntax-rules ()
    [(_ (name . formals) . body)
     (define name (curried/name name formals body))]))
