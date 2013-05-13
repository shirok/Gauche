;;;
;;; r7rs - R7RS compatibility
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

;; This module sets up R7RS environment.
;; This is not intended to be just 'use'-d.  Instead, you say (extend r7rs)
;; and you'll enter the initial toplevel environment of R7RS, where
;; only r7rs-style 'import' and 'define-library' are available.
;; With gosh, you can start -r7 option to enter the R7RS environment directly.


;; r7rs.library-name - mapping R7RS library name to Gauche module name
;;
;; We simply maps R7RS (foo bar baz) to Gauche's foo.bar.baz . The caveat
;; is that R7RS library name component may include dots.  We map them in R7RS
;; library names into consecutive dots in Gauche module name, which will be
;; mapped to a single dot again in pathnames.
;; (The latter translation is done by module-name->path and path->module-name
;; in src/libmod.scm)
;;
;;  R7RS library name   Gauche module name      File pathname
;;
;;  (foo bar baz)       foo.bar.baz             foo/bar/baz
;;  (foo b.r baz)       foo.b..r.baz            foo/b.r/baz
;;
;; TODO: R7RS library name can contain weird characters, and we need a rule
;; to map them.
(define-module r7rs.library-name
  (export library-name->module-name)

  (define (stringify x)
    (cond [(keyword? x) (write-to-string x)]
          [(symbol? x) (symbol->string x)]
          [(and (integer? x) (exact? x) (>= x 0)) (number->string x)]
          [else (error "Bad name component in library name:" x)]))
  
  (define (library-name->module-name libname)
    ($ string->symbol $ (cut string-join <> ".")
       $ map ($ (cut regexp-replace-all #/\./ <> "..") $ stringify $) libname))
  )

;; r7rs.import - R7RS-style 'import'.
;;
;; We keep Gauche's traditional import as is, and introduce R7RS import
;; in this module.
(define-module r7rs.import
  (use util.match)
  (use srfi-1)
  (import r7rs.library-name)
  (export (rename import r7rs-import))

  ;; A trick - must be replaced once we have explicit-renaming macro.
  (define import.  ((with-module gauche.internal make-identifier)
                    'import (find-module 'gauche) '()))
  (define require. ((with-module gauche.internal make-identifier)
                    'require (find-module 'gauche) '()))
  (define begin.   ((with-module gauche.internal make-identifier)
                    'begin (find-module 'gauche) '()))

  (define-macro (r7rs-import . import-sets)
    `(,begin. ,@(append-map %transfer-import-spec import-sets)))
  (define-macro (require-if-module-doesnt-exist modname)
    (if (find-module modname)
      #f
      `(,require. ,(module-name->path modname))))

  (define require-if-module-doesnt-exist.
    ((with-module gauche.internal make-identifier)
     'require-if-module-doesnt-exist (current-module) '()))

  (define (%transfer-import-spec import-set)
    (define (rec import-set)
      (match import-set
        [('only import-set identifier ...)
         `(,@(rec import-set) :only ,identifier)]
        [('except import-set identifier ...)
         `(,@(rec import-set) :except ,identifier)]
        [('prefix import-set identifier)
         `(,@(rec import-set) :prefix ,identifier)]
        [('rename import-set mapping ...)
         `(,@(rec import-set) :rename ,mapping)]
        [else (list (library-name->module-name import-set))]))
    (let1 import-spec (rec import-set)
      `((,require-if-module-doesnt-exist. ,(car import-spec))
        (,import. ,import-spec)))))

;; r7rs.library - R7RS define-library form
(define-module r7rs.library
  (import r7rs.library-name)
  (export define-library)

  ;; A trick - must be replaced once we have explicit-renaming macro.
  (define define-module. ((with-module gauche.internal make-identifier)
                          'define-module (find-module 'gauche) '()))
  (define with-module.   ((with-module gauche.internal make-identifier)
                          'with-module (find-module 'gauche) '()))
  (define define-syntax. ((with-module gauche.internal make-identifier)
                          'define-syntax (find-module 'gauche) '()))
  (define extend.        ((with-module gauche.internal make-identifier)
                          'extend (find-module 'gauche) '()))
  
  (define-macro (define-library name . decls)
    `(,define-module. ,(library-name->module-name name)
       (,define-syntax. export      (,with-module. gauche export))
       (,define-syntax. begin       (,with-module. gauche begin))
       (,define-syntax. include     (,with-module. gauche include))
       (,define-syntax. include-ci  (,with-module. gauche include-ci))
       (,define-syntax. cond-expand (,with-module. gauche cond-expand))
       (,define-syntax. import      (,with-module. r7rs.import r7rs-import))
       (,extend.)
       ,@(map transform-decl decls)))

  (define (transform-decl decl)
    (cond [(eq? (car decl) 'include-library-declarations)
           ;; WRITEME
           (error "include-library-declarations isn't supported yet:" decl)]
          [(memq (car decl)
                 '(export import include include-ci begin cond-expand))
           decl]
          [else
           (error "Invalid library declaration:" decl)]))
  )

;;
;; The 'r7rs' module removes all bindings by empty (extend), except
;; 'import' and 'define-library'.
;;
(define-module r7rs
  (export define-library)
  (define-syntax import         (with-module r7rs.import r7rs-import))
  (define-syntax define-library (with-module r7rs.library define-library))
  (extend))

;; R7RS-small standard libraries.  First I thought to make them have
;; separate file for each, but most of its content is just a rebinding&
;; reexporting, and most files are small except scheme/base.  For now
;; I consolidate them here instead of cluttering the library directory.
;; We don't want to executing these kind of things every time we fire
;; up R7RS scripts, so I think eventually we should precompile the entire
;; r7rs compatibility thingy.

(define-module r7rs.aux
  (export define+ define-syntax+)
  (define-macro (define+ sym module)
    `(define ,sym (with-module ,module ,sym)))
  (define-macro (define-syntax+ sym module)
    `(define-syntax ,sym (with-module ,module ,sym))))

(define-module scheme.base
  (use gauche.uvector)
  (use gauche.record)
  (use gauche.parameter)
  (use srfi-11)
  (use srfi-13)

  (require "text/parse")
  (require "srfi-43")

  (import r7rs.aux)
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
          )

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
  (define letrec* (undefined))  ;;WRITEME
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

  (define+ floor/             gauche)
  (define+ floor-quotient     gauche)
  (define+ floor-remainder    gauche)
  (define+ truncate/          gauche)
  (define+ truncate-quotient  gauche)
  (define+ truncate-remainder gauche)

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
  (define+ square    gauche)
  (define+ exact-integer-sqrt gauche)
  (define+ expt      gauche)
  (define+ inexact   gauche)
  (define+ exact     gauche)
  (define+ number->string gauche)
  (define+ string->number gauche)

  ;; 6.3 Booleans
  (define+ not       gauche)
  (define+ boolean?  gauche)
  (define+ boolean=? gauche)

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
  (define+ list-set! gauche)
  (define+ memq gauche)
  (define+ memv gauche)
  (define+ member gauche)
  (define+ assq gauche)
  (define+ assv gauche)
  (define+ assoc gauche)
  (define+ list-copy gauche)

  ;; 6.5 Symbols
  (define+ symbol?        gauche)
  (define+ symbol=?       gauche)
  (define+ symbol->string gauche)
  (define+ string->symbol gauche)

  ;; 6.6 Characters
  (define+ char?   gauche)
  (define+ char=?  gauche)
  (define+ char<?  gauche)
  (define+ char>?  gauche)
  (define+ char<=? gauche)
  (define+ char<=? gauche)

  ;; TODO: handle when native encoding is not utf8
  (define+ char->integer gauche)
  (define+ integer->char gauche)

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
  (define vector-map (undefined)) ;WRITEME: differs from srfi-43
                                 
  (define+ for-each gauche)
  (define+ string-for-each srfi-13)
  (define vector-for-each (undefined)) ;WRITEME: differs from srfi-43
  (define+ call-with-current-continuation gauche)
  (define+ call/cc gauche)
  (define+ values gauche)
  (define+ call-with-values gauche)
  (define+ dynamic-wind gauche)

  ;; 6.11 Exceptions
  (define+ raise gauche)
  (define raise-continuable (undefined)) ;WRITEME
  (define+ error gauche)
  (define (error-object? e) (condition-has-type? e <error>))
  (define (error-object-message e)
    (if (condition-has-type? e <message-condition>)
      (condition-ref e 'message)
      (format "object is not a condition: ~s" e))) ;TODO: should reraise an error?
  (define error-object-irritants (undefined))      ;WRITEME
  (define read-error? (undefined))                 ;WRITEME
  (define file-error? (undefined))                 ;WRITEME

  ;; 6.12 Enviornments and evaluation
  (define+ scheme-report-environment gauche)
  (define+ null-environment gauche)

  ;; 6.13 Input and output
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
  (define (close-port p)
    (cond [(input-port? p)  (close-input-port p)]
          [(output-port? p) (close-output-port p)]
          [else (error "port required, but got:" p)]))
  (define+ close-input-port gauche)
  (define+ close-output-port gauche)
  (define+ open-input-string gauche)
  (define+ open-output-string gauche)
  (define+ get-output-string gauche)
  (define open-input-bytevector (undefined))  ;WRITEME
  (define open-output-bytevector (undefined)) ;WRITEME
  (define get-output-bytevector (undefined))  ;WRITEME
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
  (define read-bytevector (undefined))  ;WRTIEME
  (define read-bytevector! (undefined)) ;WRITEME
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
  (define (features) (map car ((with-module gauche.internal cond-features))))

  (provide "scheme/base"))

(define-module scheme.case-lambda
  (import r7rs.aux)
  (export case-lambda)
  (define-syntax+ case-lambda gauche)
  (provide "scheme/case-lambda"))

(define-module scheme.char
  (use text.unicode)
  (import r7rs.aux)
  (export char-alphabetic? char-ci<=? char-ci<?
          char-ci=? char-ci>=? char-ci>?
          char-downcase char-foldcase
          char-lower-case? char-numeric?
          char-upcase char-upper-case?
          char-whitespace? digit-value
          string-ci<=? string-ci<?
          string-ci=? string-ci>=?
          string-ci>? string-downcase
          string-foldcase string-upcase)
  (define+ char-alphabetic? gauche)
  (define+ char-numeric? gauche)
  (define+ char-whitespace? gauche)
  (define+ char-upper-case? gauche)
  (define+ char-lower-case? gauche)
  (define+ char-ci=? gauche)
  (define+ char-ci<? gauche)
  (define+ char-ci>? gauche)
  (define+ char-ci<=? gauche)
  (define+ char-ci<=? gauche)
  ;; TODO: take into account of unicode Nd chars
  (define (digit-value c) (digit->integer c))
  (define+ char-upcase gauche)
  (define+ char-downcase gauche)
  (define+ char-foldcase gauche)
  (define+ string-ci=? gauche)
  (define+ string-ci<? gauche)
  (define+ string-ci>? gauche)
  (define+ string-ci<=? gauche)
  (define+ string-ci>=? gauche)
  (define+ string-upcase text.unicode)   ; not srfi-13's.
  (define+ string-downcase text.unicode) ; not srfi-13's.
  (define+ string-foldcase text.unicode) ; not srfi-13's.
  (provide "scheme/char"))

(define-module scheme.complex
  (import r7rs.aux)
  (export angle imag-part magnitude make-polar make-rectangular real-part)
  (define+ real-part gauche)
  (define+ imag-part gauche)
  (define+ magnitude gauche)
  (define+ angle     gauche)
  (define+ make-rectangular gauche)
  (define+ make-polar gauche)
  (provide "scheme/complex"))

(define-module scheme.cxr
  (import r7rs.aux)
  (export caaar caadr cadar caddr cdaar cdadr cddar cdddr
          caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
          cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)
  (define+ caaar gauche)
  (define+ caadr gauche)
  (define+ cadar gauche)
  (define+ caddr gauche)
  (define+ cdaar gauche)
  (define+ cdadr gauche)
  (define+ cddar gauche)
  (define+ cdddr gauche)
  (define+ caaaar gauche)
  (define+ caaadr gauche)
  (define+ caadar gauche)
  (define+ caaddr gauche)
  (define+ cadaar gauche)
  (define+ cadadr gauche)
  (define+ caddar gauche)
  (define+ cadddr gauche)
  (define+ cdaaar gauche)
  (define+ cdaadr gauche)
  (define+ cdadar gauche)
  (define+ cdaddr gauche)
  (define+ cddaar gauche)
  (define+ cddadr gauche)
  (define+ cdddar gauche)
  (define+ cddddr gauche)
  (provide "scheme/cxr"))

(define-module scheme.eval
  (import r7rs.aux)
  (export environment eval)
  (define environment (undefined))      ;WRITEME
  (define+ eval gauche)
  (provide "scheme/eval"))

(define-module scheme.file
  (import r7rs.aux)
  (require "file/util")
  (export call-with-input-file call-with-output-file
          delete-file file-exists?
          open-binary-input-file open-binary-output-file
          open-input-file open-output-file
          with-input-from-file with-output-to-file)
  (define+ call-with-input-file gauche)
  (define+ call-with-output-file gauche)
  (define+ with-input-from-file gauche)
  (define+ with-output-to-file gauche)
  (define+ open-input-file gauche)
  (define open-binary-input-file open-input-file)
  (define+ open-output-file gauche)
  (define open-binary-output-file open-output-file)
  (define+ file-exists? gauche)
  (define+ delete-file file.util)
  (provide "scheme/file"))

(define-module scheme.inexact
  (import r7rs.aux)
  (export acos asin atan cos exp finite? infinite? log nan? sin sqrt tan)
  (define+ acos      gauche)
  (define+ asin      gauche)
  (define+ atan      gauche)
  (define+ cos       gauche)
  (define+ exp       gauche)
  (define+ finite?   gauche)
  (define+ infinite? gauche)
  (define+ log       gauche)
  (define+ nan?      gauche)
  (define+ sin       gauche)
  (define+ sqrt      gauche)
  (define+ tan       gauche)
  (provide "scheme/inexact"))

(define-module scheme.lazy
  (import r7rs.aux)
  (export delay force delay-force promise? make-promise)
  (define-syntax+ delay gauche)
  (define-syntax delay-force (with-module gauche lazy))
  (define+ force    gauche)
  (define+ promise? gauche)
  (define make-promise (undefined))     ;WRITEME
  (provide "scheme/lazy"))

(define-module scheme.load
  (export (rename r7rs-load load))
  (define (r7rs-load file :optional (env (interaction-environment)))
    (load file :environment env))
  (provide "scheme/load"))

(define-module scheme.process-context
  (import r7rs.aux)
  (use srfi-98)
  (export command-line emergency-exit exit
          get-environment-variable get-environment-variables)
  (define+ command-line gauche)
  (define+ exit         gauche)
  (define (emergency-exit :optional (obj 0)) (sys-exit obj))
  (define+ get-environment-variable  srfi-98)
  (define+ get-environment-variables srfi-98)
  (provide "scheme/process-context"))

(define-module scheme.read
  (import r7rs.aux)
  (export read)
  (define+ read gauche)
  (provide "scheme/read"))

(define-module scheme.repl
  (import r7rs.aux)
  (export interaction-environment)
  (define+ interaction-environment gauche)
  (provide "scheme/repl"))

(define-module scheme.time
  (export current-jiffy jiffies-per-second current-second)
  (define (current-second) (sys-time))
  (define current-jiffy (undefined))    ;;WRITEME
  (define jiffies-per-second (undefined)) ;;WRITEME
  (provide "scheme/time"))

(define-module scheme.write
  (export (rename r7rs-display display)
          (rename r7rs-write write)
          write-shared write-simple)
  (define r7rs-write    write/ss) ;not exactly...
  (define write-shared  write/ss)
  (define write-simple  write)
  (define r7rs-display  display)
  (provide "scheme/write"))

;; A trick: 'define-library' in Gauche module is set to be autoloaded.
;; When this module is loaded directly (not via autoload), however,
;; we don't want to trigger autoload from gauche#define-library anymore,
;; so we overwrite it.
(with-module gauche
  (define-syntax define-library (with-module r7rs define-library)))

