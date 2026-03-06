;;;
;;; gauche.cgen.type.parse - Parsing typed var in stub file
;;;
;;;   Copyright (c) 2004-2025  Shiro Kawai  <shiro@acm.org>
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

;; This is a part of gauche.cgen.type, but also used from gauche.ctype.
;; This part is splitted to avoid dependency issues---gauche.ctype needs
;; to be built without depending too much other infrastructure.
;;
;; This moudle is not supposed to be directly 'use'd by users.  Using
;; gauche.cgen.type includes this too.

(define-module gauche.cgen.type.parse
  (use util.match)
  (export cgen-canonical-typed-var-1
          cgen-canonical-typed-var-list))
(select-module gauche.cgen.type.parse)

;;;
;;; Parsing typed variables.
;;;

;
;; For conciseness and readability, we allow varaible name and type
;; can be concatenated, so the following forms are allowed:
;;
;;    var :: type        ; proper form
;;    var::type          ; concatenated to a single symbol
;;    var:: type         ; type can be S-expr
;;    var ::type         ; non standard, but allowed
;;
;; Typed variables usually comes with list, something like this:
;;
;;   (a b::<int> c::(<List> <integer>) :optional (d::<int> 3))
;;
;; Canonicalization recognizes variations and produces this:
;;
;;   ((a :: default-type)
;;    (b :: <int>)
;;    (c :: (<List> <integer>))
;;    :optional (d :: <int> 3))
;;
;; Each typed variable becomes this form:
;;
;;   (varname :: type [init-val] [qual ...])
;;
;; If type is omitted, <top> is assumed for stubs, while ScmObj is
;; assumed for CiSEs, so the default-type is provided by the caller.
;;
;; This is used in muliple places with slightly different requirements.
;;
;;  1. Stubs
;;    1a. Parsing argument list of cproc
;;    1b. Parsing argument list of cmethod
;;    1c. Parsing return value types of them
;;
;;  2. CiSE
;;    1a. Parsing argument list of cfn
;;    1b. Parsing return value types of cfn
;;    1c. Parsing varaible declaration of let*
;;    1d. Parsing struct/union declaration
;;

;; API
;;   Take a list, an interpret its head element(s) as typed var.
;;
;;    typed-var :  (var :: type . rest)
;;              |  (var:: type . rest)
;;              |  (var ::type . rest)
;;              |  (var::type . rest)
;;              |  (var . rest)
;;              |  ...  (other than above)
;;
;;   Returns its canonical form and the rest of the input list.
;;   Canonical form may be #f if the tip of the typed-var is
;;   not a typed var.
;;   This does not deal with ((var::type init) (var2::type2 init)) type list;
;;   which is handled by cgen-canonical-typed-var.
(define (cgen-canonical-typed-var-1 typed-var default-type)
  (define (var::type? sym)
    (and (symbol? sym)
         (rxmatch-if (#/^([^:]+)::([^:]+)$/ (symbol->string sym))
             [#f V T]
           (list (string->symbol V) (string->symbol T))
           #f)))
  (define (var::? sym)
    (and (symbol? sym)
         (rxmatch-if (#/^([^:]+)::$/ (symbol->string sym))
             [#f V]
           (string->symbol V)
           #f)))
  (define (::type? sym)
    (and (keyword? sym)
         (rxmatch-if (#/^:([^:]+)$/ (keyword->string sym))
             [#f T]
           (string->symbol T)
           #f)))
  (define (var? sym)
    (and (symbol? sym)
         (not (string-scan (symbol->string sym) #\:))))

  (match typed-var
    [() (values #f '())]
    [(head . tail)
     (cond [(var::type? head)
            => (^[vt] (values `(,(car vt) :: ,(cadr vt)) tail))]
           [(var::? head)
            => (^v (match tail
                     [(type . tail) (values `(,v :: ,type) tail)]
                     [_ (error "missing type for " head)]))]
           [(keyword? head) (values #f typed-var)]
           [(var? head)
            (match tail
              [() (values `(,head :: ,default-type) tail)]
              [(':: type . tail) (values `(,head :: ,type) tail)]
              [(maybe-type . tail2)
               (cond [(::type? maybe-type)
                      => (^t (values `(,head :: ,t) tail2))]
                     [else (values `(,head :: ,default-type) tail)])])]
           [else (values #f typed-var)])])
  )

;; API
;;   Process entire typed-var-list.  This can handle more complex one
;;   than cgen-canonical-typed-var-1.
;;
;;    typed-var-list : (,@typed-var . typed-var-list)
;;                   | ((,@typed-var) . typed-var-list)
;;                   | (:keyword . typed-var-list)
;;
(define (cgen-canonical-typed-var-list typed-var-list default-type)
  (define (err decl) (error "invalid variable declaration:" decl))
  (define (scan typed-var-list)
    (match typed-var-list
      [() '()]
      [((typed-var ...) . tail)
       (receive (var&type opts)
           (cgen-canonical-typed-var-1 typed-var default-type)
         (if var&type
           (cons (append var&type opts) (scan tail))
           (err (car opts))))]
      [((? keyword? k) . tail) (cons k (scan tail))]
      [((? symbol? s) . _)
       (receive (var&type tail)
           (cgen-canonical-typed-var-1 typed-var-list default-type)
         (if var&type
           (cons var&type (scan tail))
           (err s)))]
      [_ (err typed-var-list)]))
  (scan typed-var-list))
