;;;
;;; libmod.scm - modules, bindings, GLOCs
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

(select-module gauche)

(inline-stub
 (declcode (.include "gauche/vminsn.h")
           (.include "gauche/priv/moduleP.h")))

;;;
;;; Modules API
;;;

(define-cproc module? (obj) ::<boolean> :constant SCM_MODULEP)

(define-cproc module-name (mod::<module>)
  (return (SCM_OBJ (-> (SCM_MODULE mod) name))))

(define-cproc module-parents (mod::<module>) (return (-> mod parents)))
(define-cproc module-precedence-list (mod::<module>) (return (-> mod mpl)))
(define-cproc module-imports (mod::<module>) (return (-> mod imported)))
(define-cproc module-exports (mod::<module>) Scm_ModuleExports)
(define-cproc module-table (mod::<module>) (return (SCM_OBJ (-> mod internal))))

(define-cproc find-module (name::<symbol>) ::<module>?
  (return (Scm_FindModule name SCM_FIND_MODULE_QUIET)))

(define-cproc all-modules () Scm_AllModules)

(define-cproc make-module (name::<symbol>? :key (if-exists :error))
  (let* ([error_if_exists::int TRUE])
    (cond [(SCM_EQ if-exists ':error) (set! error_if_exists TRUE)]
          [(SCM_FALSEP if-exists)     (set! error_if_exists FALSE)]
          [else (Scm_TypeError ":if-exists" ":error or #f" if_exists)])
    (return (Scm_MakeModule name error_if_exists))))

;; (use x.y.z) ==> (requrie "x/y/z") (import x.y.z)
(define-macro (use module . options)
  ;; Kludge - remove these once we have low-level hygienic macro.
  (define begin.   ((with-module gauche.internal make-identifier)
                    'begin (find-module 'gauche) '()))
  (define require. ((with-module gauche.internal make-identifier)
                    'require (find-module 'gauche) '()))
  (define import.  ((with-module gauche.internal make-identifier)
                    'import (find-module 'gauche) '()))
  `(,begin.
     (,require. ,(module-name->path module))
     (,import. (,module ,@options))))

;; foo.bar.baz <=> "foo/bar/baz"
;;  - Two consecutive dots in module name becomes one dot in path
;;    foo..bar.baz <=> "foo.bar/baz".   This is to support R7RS library
;;    whose name is (foo.bar baz).
;;  - Todo: Escape unsafe characters in name.
(define (module-name->path name)
  (define (path-comp p)
    (rlet1 s (list->string (reverse p))
      (cond [(equal? s "")
             (error "Invalid module name (it can't end with #\\.):" name)]
            [(or (equal? s ".") (equal? s ".."))
             (error "Invalid module name (component can't be \".\" or \"..\"):"
                    name)])))
  (with-input-from-string
   (cond [(symbol? name) (symbol->string name)]
         [(identifier? name) (symbol->string (identifier-name name))]
         [else (error "symbol or identifier expected, but got:" name)])
   (^[] (let loop ([c (read-char)] [p '()] [ps '()])
          (cond
           [(eof-object? c)
            (string-join (reverse (cons (path-comp p) ps)) "/")]
           [(eqv? c #\.)
            (let1 c2 (read-char)
              (if (eqv? c2 #\.)
                (loop (read-char) (cons c2 p) ps)
                (loop c2 '() (cons (path-comp p) ps))))]
           [else (loop (read-char) (cons c p) ps)])))))

(define (path->module-name path)
  (unless (string? path) (error "string required, but got:" path))
  ($ string->symbol $ (cut string-join <> ".")
     $ map (cut regexp-replace-all #/\./ <> "..")
     $ string-split path "/"))

(inline-stub
 (define-cfn get-module-from-mod-or-name (id) ::ScmModule* :static
   (cond [(SCM_MODULEP id) (return (SCM_MODULE id))]
         [(SCM_SYMBOLP id) (return (Scm_FindModule (SCM_SYMBOL id) 0))]
         [(SCM_FALSEP id)  (return (SCM_CURRENT_MODULE))]
         [else (Scm_Error "module or symbol required, but got: %S" id)
               (return NULL)]))
 )

;; Global bindind access, public API
(define-cproc global-variable-bound? (mod-or-name name::<symbol>) ::<boolean>
  (return
   (not (SCM_UNBOUNDP
         (Scm_GlobalVariableRef (get-module-from-mod-or-name mod-or-name)
                                name 0)))))
(define-cproc global-variable-ref (mod_or_name name::<symbol>
                                               :optional
                                               fallback
                                               (stay-in-module::<boolean> #f))
  (let* ([module::ScmModule* (get_module_from_mod_or_name mod_or_name)]
         [flags::int (?: stay_in_module SCM_BINDING_STAY_IN_MODULE 0)]
         [r  (Scm_GlobalVariableRef module name flags)]
         [r2 (?: (SCM_UNBOUNDP r) fallback r)])
    (when (SCM_UNBOUNDP r2)
      (Scm_Error "global variable %S is not bound in module %S"
                 name module))
    (return r2)))

(define-in-module gauche (symbol-bound? name :optional (module #f)) ; Deprecated
  (global-variable-bound? module name))

;; Module import/export internal APIs.  Not public.
(select-module gauche.internal)
(define-cproc %export-all (module::<module>) Scm_ExportAll)
(define-cproc %extend-module (module::<module> supers::<list>)
  Scm_ExtendModule)
(define-cproc %insert-binding (mod::<module> name::<symbol> value
                                             :optional (flags '()))
  (let* ([z::int 0])
    (unless (SCM_FALSEP (Scm_Memq 'const flags))
      (logior= z SCM_BINDING_CONST))
    (unless (SCM_FALSEP (Scm_Memq 'inlinable flags))
      (logior= z SCM_BINDING_INLINABLE))
    (set! SCM_RESULT (SCM_OBJ (Scm_MakeBinding mod name value z)))))

;; Insert binding as a syntactic keyword.  VALUE must be #<macro> or #<syntax>.
;; Currently Gauche conflates toplevel variable bindings and syntax bindings,
;; so this is effectively the same as %insert-binding; but we may change it
;; in future, so use this API to bind syntax.
(define (%insert-syntax-binding mod name value)
  (unless (or (syntax? value) (macro? value))
    (error "Syntax or macro object required, but got" value))
  (%insert-binding mod name value))

(define-cproc %hide-binding (mod::<module> name::<symbol>) ::<void>
  Scm_HideBinding)

(define-cproc %alias-binding (target::<module> tname::<symbol>
                                               origin::<module> oname::<symbol>)
  ::<boolean>
  Scm_AliasBinding)

(define-cproc %make-wrapper-module (origin::<module> prefix)
  Scm__MakeWrapperModule)

(define-cproc %export-symbols (mod::<module> names) Scm_ExportSymbols)

(define-cproc %import-module (mod::<module> imported
                                            :optional (prefix #f)
                                            (flags::<ulong> 0))
  Scm_ImportModule)

(define-cproc %import-modules (mod::<module> mods) ;deprecated
  Scm_ImportModules)

;;;
;;; Universal import
;;;

(select-module gauche.internal)

(declare (keep-private-macro import))

;; Kludge: We let user#import handle both gauche#import and r7rs#import, so
;; that R7RS script can be run by gosh without any special treatment.
;; If we see r7rs syntax, not only we import the specified module, but
;; we switch the current module to r7rs.user.
(define (%expand-user-import args)
  (if (any (^[import-spec]
             (or (symbol? import-spec)
                 (and (list? import-spec)
                      (pair? (cdr import-spec))
                      (keyword? (cadr import-spec)))))
           args)
    `((with-module gauche import) ,@args)
    (begin
      ;; Some black magic needed here to inject the imported module to
      ;; r7rs.user.  We do this here instead of expanding into forms
      ;; to do the work, for "select-module" thingy is tricky.
      (unless (provided? "r7rs")
        (load "r7rs")
        (provide "r7rs"))
      (eval `(import ,@args) (find-module 'r7rs.user))
      '(select-module r7rs.user))))

(select-module user)

(define-macro (import . import-specs)
  ((with-module gauche.internal %expand-user-import) import-specs))

;;;
;;; GLOCs
;;;

(select-module gauche.internal)

;; GLOCs are not for public use, so all APIs are internal.

(inline-stub
 (define-type <gloc> "ScmGloc*" "GLOC"
   "SCM_GLOCP" "SCM_GLOC" "SCM_OBJ")
 )

;; Returns GLOC object or #f.
(define-cproc find-binding (mod::<module> name::<symbol>
                                          stay-in-module::<boolean>)
  ::<gloc>?
  (return (Scm_FindBinding mod name
                           (?: stay_in_module SCM_BINDING_STAY_IN_MODULE 0))))

;; This small piece of code encapsulates the common procedure in
;; pass1/variable to find whether the variable reference is a constant
;; or not.
(define-cproc find-const-binding (id::<identifier>)
  (let* ([g::ScmGloc* (Scm_IdentifierGlobalBinding id)])
    (cond [(or (not g)
               (not (SCM_GLOC_CONST_P g))
               (SCM_VM_COMPILER_FLAG_IS_SET (Scm_VM)
                                            SCM_COMPILE_NOINLINE_CONSTS))
           (return SCM_FALSE)]
          [else
           (return (SCM_GLOC_GET g))])))

(define-cproc gloc-bound? (gloc::<gloc>) ::<boolean>
  (return (not (SCM_UNBOUNDP (SCM_GLOC_GET gloc)))))
(define-cproc gloc-ref (gloc::<gloc> :optional fallback)
  (let* ([v::ScmObj (SCM_GLOC_GET gloc)])
    (if (SCM_UNBOUNDP v)
      (begin
        (when (SCM_UNBOUNDP fallback)
          (Scm_Error "gloc %S doesn't have a value" (SCM_OBJ gloc)))
        (return fallback))
      (return v))))
(define-cproc gloc-set! (gloc::<gloc> value) SCM_GLOC_SET)
(define-cproc gloc-const? (gloc::<gloc>) ::<boolean> Scm_GlocConstP)
(define-cproc gloc-inlinable? (gloc::<gloc>) ::<boolean> Scm_GlocInlinableP)

;;;
;;; Identifier and binding
;;;

;; NB: Identifier procedures are in libsym.scm, for we might integrate
;; symbols and identifiers (we're still not sure).

;; Returns GLOC if id is bound to one, or #f.  If GLOC is returned,
;; it is always bound.

;; (define (id->bound-gloc id)
;;   (and-let* ([gloc (find-binding (identifier-module id)
;;                                  (identifier-name id) #f)]
;;              [ (gloc-bound? gloc) ])
;;     gloc))
(inline-stub
 (define-cproc id->bound-gloc (id::<identifier>)
   (let* ([gloc::ScmGloc* (Scm_IdentifierGlobalBinding id)])
     (if (and gloc (not (SCM_UNBOUNDP (SCM_GLOC_GET gloc))))
       (return (SCM_OBJ gloc))
       (return SCM_FALSE))))
 )

;; Returns #t if id1 and id2 both refer to the same existing global binding.
;; Like free-identifier=? but we know id1 and id2 are both toplevel and
;; at least one is bound, so we skip local binding lookup.
(define (global-identifier=? id1 id2)
  (and-let* ([ (identifier? id1) ]
             [ (identifier? id2) ]
             [g1 (id->bound-gloc id1)]
             [g2 (id->bound-gloc id2)])
    (eq? g1 g2)))

;; Returns #t iff id1 and id2 would resolve to the same binding
;; (or both are free).
(define (free-identifier=? id1 id2)
  (define (lookup id)
    (env-lookup id (identifier-module id) (identifier-env id)))
  (define (deep-compare id1 id2)
    (let ([b1 (lookup id1)]
          [b2 (lookup id2)])
      (cond
       [(or (lvar? b1) (macro? b1))
        ;;must have the same local variable or syntactic binding
        (eq? b1 b2)]
       [(or (lvar? b2) (macro? b2)) #f]
       [else (let ([g1 (id->bound-gloc id1)]
                   [g2 (id->bound-gloc id2)])
               (or (and (not g1) (not g2)) ;both are free
                   (eq? g1 g2)))])))
  (and (identifier? id1)
       (identifier? id2)
       (or (eq? id1 id2)
           (deep-compare id1 id2))))
