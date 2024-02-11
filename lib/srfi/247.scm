;;;
;;; SRFI-247 - Syntactic monads
;;;
;;;   Copyright (c) 2024  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi.247
  (use util.match)
  (export define-syntactic-monad))
(select-module srfi.247)

(define-syntax define-syntactic-monad
  (er-macro-transformer
    (^[f r c]
     (match f
       [(_ name svar ...)
        (quasirename r
          `(define-syntax ,name
             (er-macro-transformer
              (^[ff rr cc]
                (define lambda. (rr'lambda))
                (define define. (rr'define))
                (define case-lambda. (rr'case-lambda))
                (define let. (rr'let))
                (define let*-values. (rr'let*-values))
                (define svars ',svar)
                (match ff
                  [(_ (? (cut cc lambda. <>)) formals . body)
                   `(,lambda. (,@svars . ,formals) ,@body)]
                  [(_ (? (cut cc define. <>)) (name . formals) . body)
                   `(,define. (,name ,@svars . ,formals) ,@body)]
                  [(_ (? (cut cc case-lambda. <>)) (formals . body) ...)
                   `(,case-lambda.
                     ,@(map (^[ff bb] `([,@svars . ,ff] ,@bb)) formals body))]
                  [(_ (? (cut cc let*-values. <>)) ([formals init] ...) . body)
                   `(,let*-values.
                     ,(map (^[ff ii] `[(,@svars . ,ff) ,ii]) formals init)
                     ,@body)]
                  [(_ (? (cut cc let. <>)) loop ([var init] ...) . body)
                   (let* ([vars+init (map list var init)]
                          [svars+init
                           (map (^[svar]
                                  (if-let1 init (alist-ref vars+init svar)
                                    `(,svar ,(car init))
                                    `(,svar ,svar)))
                                svars)]
                          [others+init
                           (remove (^[var+init] (memv (car var+init) svars))
                                   vars+init)])
                     `(,let. ,loop
                             (,@svars+init
                              ,@others+init)
                             ,@body))]
                  [(_ proc ((var init) ...) arg ...)
                   (let* ([vars+init (map list var init)]
                          [svar-args
                           (map (^[svar]
                                  (if-let1 init (alist-ref vars+init svar)
                                    (car init)
                                    svar))
                                svars)])
                     `(,proc ,@svar-args ,@arg))]
                  [(_ proc)
                   `(,proc ,@svars)]
                  [else
                   (errorf "Malformed ~a: ~s" ',name ff)])))))]
       [_ (error "Malformed define-syntactic-monad:" f)]))))
