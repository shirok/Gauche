;;;
;;; libsym.scm - built-in symbol and keyword procedures
;;;
;;;   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

(select-module gauche.internal)
(inline-stub
 (declcode (.include <gauche/vminsn.h>)))

;;;
;;; Symbols
;;;

(select-module scheme)
(define-cproc symbol? (obj) ::<boolean> :fast-flonum :constant
  (inliner SYMBOLP) SCM_SYMBOLP)
(define-cproc symbol->string (obj::<symbol>) :constant
  (return (SCM_OBJ (SCM_SYMBOL_NAME obj))))
(define-cproc string->symbol (obj::<string>) :constant Scm_Intern)

(select-module gauche)
(define-cproc gensym (:optional (prefix::<string>? #f)) Scm_Gensym)
(define-cproc symbol-interned? (s::<symbol>) ::<boolean> SCM_SYMBOL_INTERNED)
(define-cproc string->uninterned-symbol (name::<string>)
  (return (Scm_MakeSymbol name FALSE)))
(define-cproc symbol-sans-prefix (s::<symbol> p::<symbol>)
  Scm_SymbolSansPrefix)

;; Bigloo has symbol-append symbol ... -> symbol
;; We enhance it a bit.
(select-module gauche.internal)
(define-in-module gauche symbol-append
  (letrec ([->string
            ;; to make it work regardless of keyword-symbol integration
            (^x (cond [(keyword? x) #":~(keyword->string x)"]
                      [(identifier? x) (identifier-name x)]
                      [else (x->string x)]))]
           [do-append
            (^[objs interned?]
              ((if interned? string->symbol string->uninterned-symbol)
               (apply string-append (map ->string objs))))])
    (case-lambda
      [() '||] ; edge case
      [(maybe-flag . syms)
       (if (boolean? maybe-flag)
         (do-append syms maybe-flag)
         (do-append (cons maybe-flag syms) #t))])))

;; R7RS
(select-module gauche)
(define (symbol=? x y . rest)
  (if-let1 z (find ($ not $ symbol? $) (list* x y rest))
    (error "symbol required, but got:" z))
  (and (eq? x y) (or (null? rest) (every (cut eq? x <>) rest))))


;;;
;;;  Keywords
;;;

(select-module gauche)
(define-cproc keyword? (obj) ::<boolean> :fast-flonum :constant SCM_KEYWORDP)

(define-cproc make-keyword (name)
  (let* ([sname::ScmString* NULL])
    (cond [(SCM_STRINGP name) (set! sname (SCM_STRING name))]
          [(SCM_SYMBOLP name) (set! sname (SCM_SYMBOL_NAME name))]
          [else (SCM_TYPE_ERROR name "string or symbol")])
    (return (Scm_MakeKeyword sname))))

(define-cproc get-keyword (key list :optional fallback) :constant
  Scm_GetKeyword)

(define-cproc delete-keyword (key list)  Scm_DeleteKeyword)
(define-cproc delete-keyword! (key list) Scm_DeleteKeywordX)

(define-cproc keyword->string (key::<keyword>) Scm_KeywordToString)

(define-in-module gauche (delete-keywords ks kvlist)
  (define (rec kvs)
    (cond [(null? kvs) '()]
          [(null? (cdr kvs)) (error "incomplete key list" kvlist)]
          [(memv (car kvs) ks) (rec (cddr kvs))]
          [else (list* (car kvs) (cadr kvs) (rec (cddr kvs)))]))
  (rec kvlist))

(define-in-module gauche (delete-keywords! ks kvlist)
  (define (head kvs)
    (cond [(null? kvs) '()]
          [(null? (cdr kvs)) (error "incomplete key list" kvlist)]
          [(memv (car kvs) ks) (head (cddr kvs))]
          [else (cut-tail! (cddr kvs) kvs) kvs]))
  (define (cut-tail! kvs prev)
    (cond [(null? kvs)]
          [(null? (cdr kvs)) (error "incomplete key list" kvlist)]
          [(memv (car kvs) ks)
           (set-cdr! (cdr prev) (cddr kvs))
           (cut-tail! (cddr kvs) prev)]
          [else (cut-tail! (cddr kvs) kvs)]))
  (head kvlist))

;;;
;;;  Identifiers
;;;

;; NB: Identifiers are mainly used for hygienic macro.  However,
;; we found a disjoit type for it is sometimes annoying (especially,
;; when you inspect the output of macro expander, you have to deal
;; with both symbols and identifiers.)  We have a vague plan to
;; integrate it to a symbol---that is, we'll have a special subtype
;; of symbols that can have auxiliary info needed for hygiene.

(select-module gauche)
(define-cproc identifier? (obj) ::<boolean> :constant
  (inliner IDENTIFIERP) SCM_IDENTIFIERP)
(define-cproc identifier->symbol (obj::<identifier>) :constant
  (return (SCM_OBJ (-> (SCM_IDENTIFIER obj) name))))

(select-module gauche.internal)
(define-cproc make-identifier (name mod::<module> env::<list>)
  Scm_MakeIdentifier)
(define-cproc identifier-module (id::<identifier>)
  (return (SCM_OBJ (-> id module))))
(define-cproc identifier-name (id::<identifier>)
  (return (SCM_OBJ (-> id name))))
(define-cproc identifier-env (id::<identifier>)
  (return (-> id env)))

(select-module gauche.internal)
;; EXPERIMENTAL
;; Concatenate symbols or identifiers.  If any one of args is an identifier,
;; the result is also an identifier with the same scope of the first
;; identifier in the args.
;; This sounds pretty kludgy.  I suspect this won't be necessary once
;; we use er-macro for everything low-level (rename procedure should take
;; care of identifier marking).  It is needed, for now, to add certain level
;; of hygiene to define-macro.
(define (identifier-append . args)
  (let1 r (apply symbol-append #t args)
    (if-let1 first-id (find identifier? args)
      (make-identifier r (identifier-module first-id) (identifier-env first-id))
      r)))
