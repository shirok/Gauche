;;;
;;; r7rs - R7RS compatibility
;;;
;;;   Copyright (c) 2013-2015  Shiro Kawai  <shiro@acm.org>
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
;; If you start gosh with r7rs mode (-r7 option), not only the above bindings
;; but also most of r7rs-small bindings are avaialable (see gauche.interactive).

;; r7rs.import - R7RS-style 'import'.
;;
;; We keep Gauche's traditional import as is, and introduce R7RS import
;; in this module.
(define-module r7rs.import
  (use util.match)
  (use srfi-1)
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
        [else
         ;; Kludge: Warn if a programmer say (import gauche).
         (when (equal? import-set '(gauche))
           (warn "(import (gauche)) does not import anything.  \
                  If you intend to import Gauche's built-in bindings, \
                  say (import (gauche base)).\n"))
         (list (library-name->module-name import-set))]))
    (let1 import-spec (rec import-set)
      `((,require-if-module-doesnt-exist. ,(car import-spec))
        (,import. ,import-spec)))))

;; r7rs.library - R7RS define-library form
(define-module r7rs.library
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
           ;; TODO: This is too permissive---this allows the files
           ;; to have not only library decls but also ordinary scheme
           ;; expressions.  But this can delegate file searching business
           ;; to 'include' syntax so it's an easy path.
           `(include ,@(cdr decl))]
          [(memq (car decl)
                 '(export import include include-ci begin cond-expand))
           decl]
          [else
           (error "Invalid library declaration:" decl)]))
  )

;;
;; The 'r7rs' module removes all bindings by an empty (extend), except
;; 'import' and 'define-library'.
;;
(define-module r7rs
  (export define-library)
  (define-syntax import         (with-module r7rs.import r7rs-import))
  (define-syntax define-library (with-module r7rs.library define-library))
  (extend))

;;
;; The 'r7rs.user' module is the default module when gosh is invoked in
;; r7rs mode.
;;
(define-module r7rs.user
  (extend r7rs))

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
    `(define-inline ,sym (with-module ,module ,sym)))
  (define-macro (define-syntax+ sym module)
    `(define-syntax ,sym (with-module ,module ,sym))))

(define-module scheme.base
  (use gauche.uvector)
  (use gauche.record)
  (use gauche.parameter)
  (use text.unicode)
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
          procedure?  quote (rename r7rs:raise raise)
          rational?  read-bytevector read-char read-line
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

  (autoload gauche.vport
            open-input-uvector open-output-uvector get-output-uvector)

  ;; 4.1 Primitive expression types
  ;; quote, if, include, include-ci
  (define-syntax+ lambda     scheme)

  ;; 4.2 Derived expression types
  ;; cond case and or when unless cond-expand let let* letrec letrec*
  ;; let-values let*-values begin do make-parameter parameterize
  ;; guard quasiquote unquote unquote-splicing case-lambda

  ;; 4.3 Macros
  ;; let-synatx letrec-syntax syntax-rules syntax-error

  ;; 5.3 Variable definitions
  (define-syntax r7rs:define define)
  (define-syntax r7rs:define-values define-values)

  ;; 5.4 Syntax definitions
  (define-syntax r7rs:define-syntax define-syntax)

  ;; 5.5 Record type definitions
  ;; define-record-type

  ;; 6.1 Equivalence predicates
  ;; eqv? eq? equal?

  ;; 6.2 Numbers
  ;; TODO: exact complex
  ;; number? complex? real? rational? integer? exact? exact-integer?
  ;; = < > <= >= zero? positive? negative? odd? even? max min + * - / abs
  ;; floor/ floor-quotient floor-remainder
  ;; truncate/ truncate-quotient truncate-remainder
  ;; quotient modulo remainder gcd lcm numerator denominator
  ;; floor ceiling truncate round rationalize square exact-integer-sqrt
  ;; expt inexact exact number->string string->number

  ;; 6.3 Booleans
  ;; not boolean? boolean=?

  ;; 6.4 Pairs and lists
  ;; pair? cons car cdr set-car! set-cdr! caar cadr cdar cddr null? list?
  ;; make-list list length append reverse list-tail list-ref list-set!
  ;; memq memv member assq assv assoc list-copy

  ;; 6.5 Symbols
  ;; symbol? symbol=? symbol->string string->symbol

  ;; 6.6 Characters
  ;; char? char=? char<? char>? char<=? char>=?
  ;; char->integer integer->char

  ;; 6.7 Strings
  ;; string? make-string string string-length string-ref string-set!
  ;; string=? string<? string>? string<=? string>=? substring string-append
  ;; string->list list->string string-copy string-copy! string-fill!

  ;; 6.8 Vectors
  ;; vector? make-vector vector vector-length vector-ref vector-set!
  ;; vector->list list->vector vector->string string->vector
  ;; vector-copy vector-copy! vector-append vector-fill!
  (define+ vector-copy!  srfi-43)
  (define+ vector-append srfi-43)

  ;; 6.9 Bytevectors
  (define-inline bytevector         u8vector)
  (define-inline bytevector?        u8vector?)
  (define-inline make-bytevector    make-u8vector)
  (define-inline bytevector-length  u8vector-length)
  (define-inline bytevector-u8-ref  u8vector-ref)
  (define-inline bytevector-u8-set! u8vector-set!)
  (define-inline bytevector-copy    u8vector-copy)
  (define-inline bytevector-copy!   u8vector-copy!)
  (define-inline bytevector-append  u8vector-append)
  (define+ utf8->string  text.unicode)
  (define+ string->utf8  text.unicode)

  ;; 6.10 Control features
  ;; procedure? apply map
  ;; call-with-current-continuation call/cc values call-with-values dynamic-wind
  (define (string-map proc str . more-strs) ; TODO: can be more efficient
    (if-let1 a (find (^s (not (string? s))) (cons str more-strs))
      (error "non-string argument passed to string-map:" a)
      (apply (with-module gauche.sequence map-to) <string> proc str more-strs)))
  (define+ vector-map gauche)
  (define+ for-each gauche)
  (define (string-for-each proc str . more-strs) ; TODO: can be more efficient
    (if-let1 a (find (^s (not (string? s))) (cons str more-strs))
      (error "non-string argument passed to string-for-each:" a)
      (apply (with-module gauche.sequence for-each) proc str more-strs)))
  (define+ vector-for-each gauche)

  ;; 6.11 Exceptions
  ;; error - built-in
  
  ;; NB: In Gauche, 'raise' is continuable as far as the thrown exception
  ;; isn't fatal.
  (define (raise-continuable c) (raise c))
  (define (r7rs:raise c) ((with-module gauche.internal %raise) c #t))
  
  (define (error-object? e) (condition-has-type? e <error>))
  (define (error-object-message e)
    (if (condition-has-type? e <message-condition>)
      (condition-ref e 'message-prefix)
      "")) ; for now, we take permissive stance.
  (define (error-object-irritants e)
    (if (condition-has-type? e <message-condition>)
      (condition-ref e 'message-args)
      '()))
  (define (read-error? e) (condition-has-type? e <read-error>))
  (define (file-error? e) ;TODO: have a distinct type <file-error>
    ;; for the time being, we use heuristics
    (and (condition-has-type? e <system-error>)
         (boolean (memq (sys-errno->symbol (condition-ref e 'errno))
                        `(EACCES EAGAIN EBADF EBADFD EEXIST EFBIG EIO
                          EISDIR EISNAM ELNRNG ELOOP EMFILE EMLINK
                          ENAMETOOLONG ENFILE ENOBUFS ENODEV ENOENT
                          ENOSPC ENOTBLK ENOTDIR ENOTEMPTY ENXIO
                          EPERM EPIPE ESPIPE ESTALE ETXTBSY EXDEV)))))

  ;; 6.12 Enviornments and evaluation
  ;; scheme-report-environment null-environment

  ;; 6.13 Input and output
  ;; input-port? output-port? port? current-input-port current-output-port
  ;; current-error-port close-port close-input-port close-ouptut-port
  ;; open-input-string open-output-string get-output-string read-string
  ;; read-char peek-char read-line eof-object? eof-object char-ready?
  ;; newline write-char
  (define (textual-port? p) (port? p))    ; gauche's port can handle both
  (define (binary-port? p) (port? p))     ; gauche's port can handle both
  (define (input-port-open? p) (and (input-port? p) (not (port-closed? p))))
  (define (output-port-open? p) (and (output-port? p) (not (port-closed? p))))
  (define (open-input-bytevector bv)
    (check-arg u8vector? bv)
    (open-input-uvector bv))
  (define (open-output-bytevector) (open-output-uvector))
  (define (get-output-bytevector port)
    (or (get-output-uvector port)
        (error "get-output-bytevector needs a output uvector port, but got:"
               port)))
  (define-inline read-u8 read-byte)
  (define-inline peek-u8 peek-byte)
  (define u8-ready? byte-ready?)
  (define (read-bytevector k :optional (port (current-input-port)))
    (read-uvector <u8vector> k port))
  (define (read-bytevector! bv :optional (port (current-input-port))
                                         (start 0)
                                         (end (u8vector-length bv)))
    (read-block! bv port start end))
  (define (write-string string :optional (port (current-output-port))
                        (start #f)
                        (end #f))
    (if (or start end)
      (display (substring string (or start 0) (or end (string-length string)))
               port)
      (display string port)))
  (define-inline write-u8 write-byte)
  (define write-bytevector write-uvector)
  (define flush-output-port flush)

  ;; 6.14 System interface
  (define (features) (map car ((with-module gauche.internal cond-features))))

  (provide "scheme/base"))

(define-module scheme.case-lambda
  (import r7rs.aux)
  (export case-lambda)
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
  (define (digit-value c) (digit->integer c 10 #t))
  (define+ string-ci=?  text.unicode)   ; not gauche's.
  (define+ string-ci<?  text.unicode)   ; not gauche's.
  (define+ string-ci>?  text.unicode)   ; not gauche's.
  (define+ string-ci<=? text.unicode)   ; not gauche's.
  (define+ string-ci>=? text.unicode)   ; not gauche's.
  (define+ string-upcase text.unicode)   ; not srfi-13's.
  (define+ string-downcase text.unicode) ; not srfi-13's.
  (define+ string-foldcase text.unicode) ; not srfi-13's.
  (provide "scheme/char"))

(define-module scheme.complex
  (import r7rs.aux)
  (export angle imag-part magnitude make-polar make-rectangular real-part)
  (provide "scheme/complex"))

(define-module scheme.cxr
  (import r7rs.aux)
  (export caaar caadr cadar caddr cdaar cdadr cddar cdddr
          caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
          cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)
  (provide "scheme/cxr"))

(define-module scheme.eval
  (import r7rs.aux)
  (export environment eval)
  (define (environment . import-lists)
    (rlet1 m (make-module #f)
      (eval '(extend r7rs) m)
      (eval `(import ,@import-lists) m)))
  (provide "scheme/eval"))

(define-module scheme.file
  (import r7rs.aux)
  (require "file/util")
  (export call-with-input-file call-with-output-file
          delete-file file-exists?
          open-binary-input-file open-binary-output-file
          open-input-file open-output-file
          with-input-from-file with-output-to-file)
  (define open-binary-input-file open-input-file)
  (define open-binary-output-file open-output-file)
  (define+ delete-file file.util)
  (provide "scheme/file"))

(define-module scheme.inexact
  (import r7rs.aux)
  (export acos asin atan cos exp finite? infinite? log nan? sin sqrt tan)
  (provide "scheme/inexact"))

(define-module scheme.lazy
  (import r7rs.aux)
  (export delay force delay-force promise? make-promise)
  (define-syntax delay-force (with-module gauche lazy))
  (define (make-promise obj) (if (promise? obj) obj (delay obj)))
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
  (define (emergency-exit :optional (obj 0)) (sys-exit obj))
  (define+ get-environment-variable  srfi-98)
  (define+ get-environment-variables srfi-98)
  (provide "scheme/process-context"))

(define-module scheme.read
  (import r7rs.aux)
  (export read)
  (provide "scheme/read"))

(define-module scheme.repl
  (import r7rs.aux)
  (export interaction-environment)
  (provide "scheme/repl"))

(define-module scheme.time
  (export current-jiffy jiffies-per-second current-second)
  (define-constant tai-utc 35) ; TAI is ahead of this amount as of 2014
  (define-constant tai-off 8)  ; TAI epoch is ahead of this amount
  ;; We reduce resolution in 32bit platform so that we have more time
  ;; before current-jiffy falls out of fixnum range.  On 32bit machines,
  ;; 100us resolution gives 53687 seconds before we get bignum.  On 64bit
  ;; machines, we have enough bits with nanosec resolution.
  (define-constant jiffy-resolution
    (if (fixnum? (expt 2 32)) #e1e9 #e1e4))
  ;; We use clock_gettime(CLOCK_MONOTONIC) for current-jiffy if possible,
  ;; falling back to gettimeofday.
  (define (%gettime)
    (receive (sec nsec) (sys-clock-gettime-monotonic)
      (if sec
        (values sec nsec)
        (receive (sec usec) (sys-gettimeofday)
          (values (+ sec tai-utc) (* usec 1000))))))

  (define-values (%epoch-sec %epoch-nsec) (%gettime))
  (define (current-second)
    (receive (sec usec) (sys-gettimeofday)
      (+ sec (/. usec 1e6) (- tai-utc tai-off))))
  (define current-jiffy
    (if (fixnum? (expt 2 32))
      (^[] (receive (sec nsec) (%gettime)
             (+ (* (- sec %epoch-sec) jiffy-resolution)
                (- nsec %epoch-nsec))))
      (^[] (receive (sec nsec) (%gettime)
             (+ (* (- sec %epoch-sec) jiffy-resolution)
                (quotient (- nsec %epoch-nsec) (/ #e1e9 jiffy-resolution)))))))
  (define (jiffies-per-second) jiffy-resolution)
  (provide "scheme/time"))

(define-module scheme.write
  (export display write write-shared write-simple)
  (provide "scheme/write"))

(define-module scheme.r5rs
  (import r7rs.aux)
  (export * + - / < <= = > >= abs acos and angle append apply asin assoc assq
          assv atan begin boolean? caaaar caaadr caaar caadar caaddr caadr
          caar cadaar cadadr cadar caddar cadddr caddr cadr
          call-with-current-continuation call-with-input-file
          call-with-output-file call-with-values car case cdaaar cdaadr cdaar
          cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr cddr
          cdr ceiling char->integer char-alphabetic? char-ci<=? char-ci<?
          char-ci=? char-ci>=? char-ci>? char-downcase char-lower-case?
          char-numeric? char-ready? char-upcase char-upper-case? char-whitespace?
          char<=? char<? char=? char>=? char>? char? close-input-port
          close-output-port complex? cond cons cos current-input-port
          current-output-port define define-syntax delay denominator display
          do dynamic-wind eof-object? eq? equal? eqv? eval even? exact->inexact
          exact? exp expt floor for-each force gcd if imag-part inexact->exact
          inexact? input-port? integer->char integer? interaction-environment
          lambda lcm length let let* let-syntax letrec letrec-syntax list
          list->string list->vector list-ref list-tail list? load log magnitude
          make-polar make-rectangular make-string make-vector map max member
          memq memv min modulo negative? newline not null-environment null?
          number->string number? numerator odd? open-input-file open-output-file
          or output-port? pair? peek-char positive? procedure? quasiquote quote
          quotient rational? rationalize read read-char real-part real? remainder
          reverse round scheme-report-environment set! set-car! set-cdr! sin
          sqrt string string->list string->number string->symbol string-append
          string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>?
          string-copy string-fill! string-length string-ref string-set!
          string<=? string<? string=? string>=? string>? string? substring
          symbol->string symbol? tan truncate values vector vector->list
          vector-fill! vector-length vector-ref vector-set! vector?
          with-input-from-file with-output-to-file write write-char zero?
          )
  (define-syntax define (with-module scheme define))
  (define-syntax lambda (with-module scheme lambda))
  (provide "scheme/r5rs")
  )

;; A trick: 'define-library' in Gauche module is set to be autoloaded.
;; When this module is loaded directly (not via autoload), however,
;; we don't want to trigger autoload from gauche#define-library anymore,
;; so we overwrite it.
(with-module gauche
  (define-syntax define-library (with-module r7rs define-library)))

(provide "r7rs")
