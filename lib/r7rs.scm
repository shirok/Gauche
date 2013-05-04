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
;;  (foo b.r baz)       foo.b.r.baz             foo/b.r/baz
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
      `((,require. ,(module-name->path (car import-spec)))
        (,import. ,import-spec)))))

;; r7rs.library - R7RS define-library form
;; 

(define-module r7rs.library
  (import r7rs.library-name)
  (export define-library)

  (define-macro (define-library name . decls)
    `(define-module ,(library-name->module-name name)
       (define-syntax export      (with-module gauche export))
       (define-syntax begin       (with-module gauche begin))
       (define-syntax include     (with-module gauche include))
       (define-syntax include-ci  (with-module gauche include-ci))
       (define-syntax cond-expand (with-module gauche cond-expand))
       (define-syntax import      (with-module r7rs.import r7rs-import))
       (extend)
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
;; The 'r7rs' module removes all bindings by empty (extend), except the
;; one imported from r7rs.toplevel.
;;
(define-module r7rs
  (export define-library)
  (define-syntax import         (with-module r7rs.import r7rs-import))
  (define-syntax define-library (with-module r7rs.library define-library))
  (extend))

