;;;
;;; scheme.base - R7RS base library
;;;
;;;   Copyright (c) 2013  Shiro Kawai  <shiro@acm.org>
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

;; This is a compatibility module.

(define-module scheme.base
  (use gauche.uvector)
  (use gauche.record)
  (use gauche.parameter)
  (use text.unicode)
  (use srfi-11)
  (use srfi-13)

  (require "text/parse")
  (require "srfi-43")
  
  (export * + - ... / < <= = => > >= abs and append apply assoc assq
          assv begin binary-port?  boolean=?  boolean?  bytevector
          bytevector-append bytevector-copy bytevector-copy! bytevector-length
          bytevector-u8-ref bytevector-u8-set!  bytevector?  caar cadr
          call-with-current-continuation call-with-port call-with-values call/cc
          car case cdar cddr cdr ceiling char->integer char-ready?  char<=?
          char<?  char=?  char>=?  char>?  char?  close-input-port
          close-output-port close-port complex?  cond cond-expand cons
          current-error-port current-input-port current-output-port
          (rename r7rs:define define)
          define-record-type (rename r7rs:define-syntax define-syntax)
          (rename r7rs:define-values define-values)
          denominator do
          dynamic-wind else eof-object?  equal?  error error-object-message
          even?  exact-integer-sqrt exact?  features floor floor-remainder
          flush-output-port gcd get-output-string if include-ci inexact?
          input-port?  integer?  lcm let let*-values let-values letrec* list
          list->vector list-ref list-tail make-bytevector make-parameter
          make-vector max memq min negative?  not number->string numerator
          open-input-bytevector open-output-bytevector or output-port?
          parameterize peek-u8 positive?  quasiquote quotient raise-continuable
          rationalize read-bytevector!  read-error?  read-string real?  reverse
          set!  set-cdr!  string string->number string->utf8 string-append
          eof-object eq?  eqv?  error-object-irritants error-object?  exact
          exact-integer?  expt file-error?  floor-quotient floor/ for-each
          get-output-bytevector guard include inexact input-port-open?
          integer->char lambda length let* let-syntax letrec letrec-syntax
          list->string list-copy list-set!  list?  make-list make-string map
          member memv modulo newline null?  number?  odd?  open-input-string
          open-output-string output-port-open?  pair?  peek-char port?
          procedure?  quote raise rational?  read-bytevector read-char read-line
          read-u8 remainder round set-car!  square string->list string->symbol
          string->vector string-copy string-copy!  string-for-each string-map
          string-set!  string<?  string>=?  string?  symbol->string symbol?
          syntax-rules truncate truncate-remainder u8-ready?  unquote
          utf8->string vector vector->string vector-copy vector-fill!
          vector-length vector-ref vector?  with-exception-handler write-char
          write-u8 string-fill!  string-length string-ref string<=?
          string=?  string>?  substring symbol=?  syntax-error textual-port?
          truncate-quotient truncate/ unless unquote-splicing values
          vector->list vector-append vector-copy!  vector-for-each vector-map
          vector-set!  when write-bytevector write-string zero?
          ))
(select-module scheme.base)

(define-macro (define+ sym module)
  `(define ,sym (with-module ,module ,sym)))
(define-macro (define-syntax+ sym module)
  `(define-syntax ,sym (with-module ,module ,sym)))

;; 4.1 Primitive expression types
(define-syntax+ quote      gauche)
(define-syntax+ lambda     gauche)
(define-syntax+ if         gauche)
(define-syntax+ include    gauche)
(define-syntax+ include-ci gauche)

;; 4.2 Derived expression types
(define-syntax+ cond       gauche)
(define else (undefined)) ;; dummy binding - rather make it syntactic binding
(define =>   (undefined)) ;; dummy binding - rather make it syntactic binding
(define-syntax+ case       gauche)
(define-syntax+ and        gauche)
(define-syntax+ or         gauche)
(define-syntax+ when       gauche)
(define-syntax+ unless     gauche)
(define-syntax+ cond-expand gauche) ;;TODO: support 'library' form
(define-syntax+ let        gauche)
(define-syntax+ let*       gauche)
(define-syntax+ letrec     gauche)
;letrec*
(define-syntax+ let-values srfi-11)
(define-syntax+ let*-values srfi-11)
(define-syntax+ begin      gauche)
(define-syntax+ do         gauche)
(define+        make-parameter gauche.parameter)
(define-syntax+ parameterize   gauche.parameter)
(define-syntax+ guard      gauche)
(define-syntax+ quasiquote gauche)
(define-syntax+ unquote    gauche)
(define-syntax+ unquote-splicing gauche)
(define-syntax+ case-lambda gauche)

;; 4.3 Macros
;; TODO: Make Gauche's builtin hygienic macros compatible to R7RS.
(define-syntax+ let-syntax gauche)
(define-syntax+ letrec-syntax gauche)
(define-syntax+ syntax-rules gauche)
(define-syntax+ syntax-error gauche)

;; 5.3 Variable definitions
(define-syntax r7rs:define define)
(define-syntax r7rs:define-values define-values)

;; 5.4 Syntax definitions
;; TODO: make gauche's define-syntax R7RS compatible
(define-syntax r7rs:define-syntax define-syntax)
(define ... (undefined))                ; a dummy binding

;; 5.5 Record type definitions
(define-syntax+ define-record-type gauche.record)

;; 6.1 Equivalence predicates
;; TODO: make equal? not diverge
(define+ eqv?   gauche)
(define+ eq?    gauche)
(define+ equal? gauche)

;; 6.2 Numbers
(define+ number?   gauche)
(define+ complex?  gauche)
(define+ real?     gauche)
(define+ rational? gauche)
(define+ integer?  gauche)
(define+ exact?    gauche)
(define+ exact-integer? gauche)
(define+ =         gauche)
(define+ <         gauche)
(define+ >         gauche)
(define+ <=        gauche)
(define+ >=        gauche)
(define+ zero?     gauche)
(define+ positive? gauche)
(define+ negative? gauche)
(define+ odd?      gauche)
(define+ even?     gauche)
(define+ max       gauche)
(define+ min       gauche)
(define+ +         gauche)
(define+ *         gauche)
(define+ -         gauche)
(define+ /         gauche)
(define+ abs       gauche)

;; NB: div and mod accepts non-integer in y.  should we reject such cases?
(define (floor/ x y)
  (if (> y 0) (div-and-mod x y) (div0-and-mod0 x y)))
(define (floor-quotient x y)
  (if (> y 0) (div x y) (div0 x y)))
(define floor-remainder modulo)

(define truncate/  quotient&remainder)
(define truncate-quotient  quotient)
(define truncate-remainder remainder)

(define+ quotient  gauche)
(define+ modulo    gauche)
(define+ remainder gauche)
(define+ gcd       gauche)
(define+ lcm       gauche)

(define+ numerator gauche)
(define+ denominator gauche)
(define+ floor     gauche)
(define+ ceiling   gauche)
(define+ truncate  gauche)
(define+ round     gauche)
(define+ rationalize gauche)
(define (square z) (* z z))
(define+ exact-integer-sqrt gauche)
(define+ expt      gauche)
(define+ make-rectangular gauche)
(define+ make-polar gauche)
(define+ real-part gauche)
(define+ imag-part gauche)
(define+ magnitude gauche)
(define+ angle     gauche)
(define+ inexact   gauche)
(define+ exact     gauche)
(define+ number->string gauche)
(define+ string->number gauche)

;; 6.3 Booleans
(define+ not       gauche)
(define+ boolean?  gauche)
(define (boolean=? a b . args)
  (if-let1 z (find ($ not $ boolean? $) (list* a b args))
    (error "boolean value required, but got:" z))
  (if a
    (and b (every identity rest))
    (and (not b) (every not rest))))

;; 6.4 Pairs and lists
(define+ pair? gauche)
(define+ cons gauche)
(define+ car gauche)
(define+ cdr gauche)
(define+ set-car! gauche)
(define+ set-cdr! gauche)
(define+ caar gauche)
(define+ cadr gauche)
(define+ cdar gauche)
(define+ cddr gauche)
(define+ null? gauche)
(define+ list? gauche)
(define+ make-list gauche)
(define+ list gauche)
(define+ length gauche)
(define+ append gauche)
(define+ reverse gauche)
(define+ list-tail gauche)
(define+ list-ref gauche)
(define (list-set! lis k v) (set-car! (list-tail lis k) v))
(define+ memq gauche)
(define+ memv gauche)
(define+ member gauche)
(define+ assq gauche)
(define+ assv gauche)
(define+ assoc gauche)
(define+ list-copy gauche)

;; 6.5 Symbols
(define+ symbol? gauche)
(define (symbol=? x y . rest)
  (if-let1 z (find ($ not $ symbol? $) (list* a b args))
    (error "symbol required, but got:" z))
  (and (eq? x y) (every (cut eq? x <>) rest)))
(define+ symbol->string gauche)
(define+ string->symbol gauche)

;; 6.6 Characters
(define+ char? gauche)
(define+ char=? gauche)
(define+ char<? gauche)
(define+ char>? gauche)
(define+ char<=? gauche)
(define+ char<=? gauche)
(define+ char-ci=? gauche)
(define+ char-ci<? gauche)
(define+ char-ci>? gauche)
(define+ char-ci<=? gauche)
(define+ char-ci<=? gauche)
(define+ char-alphabetic? gauche)
(define+ char-numeric? gauche)
(define+ char-whitespace? gauche)
(define+ char-upper-case? gauche)
(define+ char-lower-case? gauche)

;; TODO: take into account of unicode Nd chars
(define (digit-value c) (digit->integer c))
;; TODO: handle when native encoding is not utf8
(define+ char->integer gauche)
(define+ integer->char gauche)
(define+ char-upcase gauche)
(define+ char-downcase gauche)
(define+ char-foldcase gauche)

;; 6.7 Strings
(define+ string? gauche)
(define+ make-string gauche)
(define+ string gauche)
(define+ string-length gauche)
(define+ string-ref gauche)
(define+ string-set! gauche)
(define+ string=? gauche)
(define+ string<? gauche)
(define+ string>? gauche)
(define+ string<=? gauche)
(define+ string>=? gauche)
(define+ string-ci=? gauche)
(define+ string-ci<? gauche)
(define+ string-ci>? gauche)
(define+ string-ci<=? gauche)
(define+ string-ci>=? gauche)
(define string-upcase (with-module text.unicode string-upcase)) ; not srfi-13's.
(define string-downcase (with-module text.unicode string-downcase)) ; not srfi-13's.
(define string-foldcase (with-module text.unicode string-foldcase)) ; not srfi-13's.
(define+ substring gauche)
(define+ string-append gauche)
(define+ string->list gauche)
(define+ list->string gauche)
(define+ string-copy gauche)
(define+ string-copy! srfi-13)
(define+ string-fill! gauche)

;; 6.8 Vectors
(define+ vector? gauche)
(define+ make-vector gauche)
(define+ vector gauche)
(define+ vector-length gauche)
(define+ vector-ref gauche)
(define+ vector-set! gauche)
(define+ vector->list gauche)
(define+ list->vector gauche)
;vector->string
;string->vector
(define+ vector-copy gauche)
(define+ vector-copy!  srfi-43)
(define+ vector-append srfi-43)
(define+ vector-fill! gauche)

;; 6.9 Bytevectors
(define bytevector         u8vector)
(define bytevector?        u8vector?)
(define make-bytevector    make-u8vector)
(define bytevector-length  u8vector-length)
(define bytevector-u8-ref  u8vector-ref)
(define bytevector-u8-set! u8vector-set!)
(define bytevector-copy    u8vector-copy)
(define bytevector-copy!   u8vector-copy!)
(define (bytevector-append . bvs)
  (let* ([size (apply + (map u8vector-length bvs))]
         [dest (make-u8vector size)])
    (do ([bvs bvs (cdr bvs)]
         [k   0   (+ k (u8vector-length (car bvs)))])
        [(null? bvs) dest]
      (u8vector-copy! dest k (car bvs)))))

;; TODO: when native encoding is not utf8
(define utf8->string       u8vector->string)
(define string->utf8       string->u8vector)

;; 6.10 Control features
(define+ procedure? gauche)
(define+ apply gauche)
(define+ map gauche)
(define+ string-map srfi-13)
; vector-map      - TODO: differs from srfi-43
(define+ for-each gauche)
(define+ string-for-each srfi-13)
; vector-for-each - TODO: differs from srfi-43
(define+ call-with-current-continuation gauche)
(define+ call/cc gauche)
(define+ values gauche)
(define+ call-with-values gauche)
(define+ dynamic-wind gauche)

;; 6.11 Exceptions
(define+ raise gauche)
; raise-continuable - TODO
(define+ error gauche)
(define (error-object? e) (condition-has-type? e <error>))
(define (error-object-message e)
  (if (condition-has-type? e <message-condition>)
    (condition-ref e 'message)
    (format "object is not a condition: ~s" e))) ;TODO: should reraise an error?
; error-object-irritants
; read-error?
; file-error?

;; 6.12 Enviornments and evaluation
; environment
(define+ scheme-report-environment gauche)
(define+ null-environment gauche)
(define+ eval gauche)

;; 6.13 Input and output
(define+ call-with-input-file gauche)
(define+ call-with-output-file gauche)
(define+ input-port? gauche)
(define+ output-port? gauche)
(define (textual-port? p) (port? p))     ; gauche's port can handle both
(define (binary-port? p) (port? p))     ; gauche's port can handle both
(define+ port? gauche)
(define (input-port-open? p) (and (input-port? p) (not (port-closed? p))))
(define (output-port-open? p) (and (output-port? p) (not (port-closed? p))))
(define+ current-input-port gauche)
(define+ current-output-port gauche)
(define+ current-error-port gauche)
(define+ with-input-from-file gauche)
(define+ with-output-to-file gauche)
(define+ open-input-file gauche)
(define open-binary-input-file open-input-file)
(define+ open-output-file gauche)
(define open-binary-output-file open-output-file)
(define (close-port p)
  (cond [(input-port? p)  (close-input-port p)]
        [(output-port? p) (close-output-port p)]
        [else (error "port required, but got:" p)]))
(define+ close-input-port gauche)
(define+ close-output-port gauche)
(define+ open-input-string gauche)
(define+ open-output-string gauche)
(define+ get-output-string gauche)
;open-input-bytevector
;open-output-bytevector
;get-output-bytevector
(define+ read-char gauche)
(define+ peek-char gauche)
(define+ read-line gauche)
(define+ eof-object? gauche)
(define+ eof-object gauche)
(define+ char-ready? gauche)
(define+ read-string text.parse)
(define read-u8 read-byte)
(define peek-u8 peek-byte)
(define u8-ready? byte-ready?)
;read-bytevector
;read-bytevector!
(define+ newline gauche)
(define+ write-char gauche)
(define (write-string string :optional (port (current-output-port))
                                       (start #f)
                                       (end #f))
  (if (or start end)
    (display (substring string (or start 0) (or end (string-length string)))
             port)
    (display string port)))
(define write-u8 write-byte)
(define write-bytevector write-block)
(define flush-output-port flush)

;; 6.14 System interface
(define+ file-exists? gauche)
(define (delete-file f) (sys-unlink f))
(define (features) (map car ((with-module gauche.internal cond-features))))
