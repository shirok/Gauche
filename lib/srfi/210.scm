;;;
;;; SRFI-210 - Procedures and syntax for multiple values
;;;
;;;   Copyright (c) 2021-2023  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi.210
  (use util.match)
  (export apply/mv call/mv list/mv vector/mv box/mv value/mv
          coarity set!-values with-values case-receive bind/mv

          list-values vector-values box-values value identity
          compose-left compose-right map-values bind/list bind/box bind))
(select-module srfi.210)

;; These syntaxes can run more efficiently if supported in VM natively;
;; i.e. the producer could directly place mv results in the VM stack.
;; For now, we emulate them.

(define-syntax apply/mv
  (syntax-rules ()
    [(_ operator producer)
     (with-values producer operator)]
    [(_ operator operand ... producer)
     (apply operator operand ... (values->list producer))]))

(define-syntax call/mv
  (syntax-rules ()
    [(_ consumer) (consumer)]
    [(_ consumer producer) (with-values producer consumer)]
    [(_ consumer producer ...)
     (apply consumer (append (values->list producer) ...))]))

(define-syntax list/mv
  (syntax-rules ()
    [(_ producer) (values->list producer)]
    [(_ element ... producer)
     (list* element ... (values->list producer))]))

(define-syntax vector/mv
  (syntax-rules ()
    [(_ producer) (with-values producer vector)]
    [(_ element ... producer)
     (apply vector element ... (values->list producer))]))

(define-syntax box/mv
  (syntax-rules ()
    [(_ producer) (with-values producer box)]
    [(_ element ... producer)
     (apply box element ... (values->list producer))]))

(define-syntax value/mv
  (syntax-rules ()
    [(_ index producer) (values-ref producer index)]
    [(_ index element ... producer)
     (apply value index element ... (values->list producer))]))

(define-syntax coarity
  (syntax-rules ()
    [(_ producer)
     (length (values->list producer))]))

;; set!-values - builtin

(define-syntax with-values
  (syntax-rules ()
    [(_ producer consumer)
     (receive vals producer
       (apply consumer vals))]))

(define-syntax case-receive
  (syntax-rules ()
    [(_ producer clause ...)
     (with-values producer
       (case-lambda clause ...))]))

(define-syntax bind/mv
  (syntax-rules ()
    [(_ producer transducer ...)
     (bind/list (values->list producer) transducer ...)]))

(define (list-values list) (apply values list))

(define (vector-values vec) (apply values (vector->list vec)))

(define (box-values box) (unbox box))

(define (value k . objs) (list-ref objs k))

(define identity values) ;; This differs from Gauche's built-in

(define (compose-left . transducers) (apply compose (reverse transducers)))

(define (compose-right . transducers) (apply compose transducers))

(define (map-values proc)
  (^ xs (list-values (map proc xs))))

(define (bind/list lis . transducers)
  (match transducers
    [() (list-values lis)]
    [(transducer) (apply transducer lis)]
    [(transducer . transducers)
     (apply bind/list (values->list (apply transducer lis)) transducers)]))

(define (bind/box bx . transducers)
  (apply bind/list (values->list (unbox bx)) transducers))

(define (bind obj . transducers)
  (apply bind/list (list obj) transducers))
