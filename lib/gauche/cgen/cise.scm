;;;
;;; gauche.cgen.cise - C in S expression
;;;
;;;   Copyright (c) 2004-2019  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.cgen.cise
  (use srfi-13)
  (use gauche.sequence)
  (use gauche.parameter)
  (use gauche.cgen.unit)
  (use gauche.cgen.literal)
  (use gauche.experimental.lamb)
  (use util.match)
  (export cise-render cise-render-to-string cise-render-rec
          cise-translate
          cise-ambient cise-default-ambient cise-ambient-copy
          cise-push-static-decl! cise-ambient-decl-strings
          cise-register-macro!
          cise-lookup-macro
          cise-emit-source-line
          define-cise-macro
          define-cise-stmt
          define-cise-expr
          define-cise-toplevel
          )
  )
(select-module gauche.cgen.cise)

;;=============================================================
;; Parameters
;;

;; If true, include #line directive in the output.
(define cise-emit-source-line (make-parameter #t))

;; The global settings
(define-class <cise-ambient> ()
  (;; CiSE macro definitions
   ;;   The default cise-ambient holds all predefined macros; you can
   ;;   copy the default ambient and add custom macros.
   (macros :init-keyword :macros :init-form (make-hash-table 'eq?))
   ;; Stree for forward declarations.  Some macros, such as define-cfn,
   ;; insert this.  This must be emitted at toplevel, so local
   ;; transformation functions such as cise-render DOES NOT emit
   ;; the code put here.  Cise-translate does.   If the caller only calls
   ;; local transformation functions and need to expand macros that
   ;; generates static-decls, the caller has to call emit-static-decls
   ;; at the point where toplevel code is allowed.
   ;; NB: If you're translating entire unit (e.g. dealing with stubs)
   ;; you should use cgen-unit features direcly to register toplevel
   ;; code.  This is to keep transient info mainly used by cise-translate.
   (static-decls  :init-keyword :static-decls  :init-value '())))

;; The default ambient - this should be only modified during loading of
;; this module, in order to register all the default cise macros.  Once
;; this module is loaded, this must be treated as immutable, and the
;; user must get its copy to use via cise-default-ambient.
(define *default-ambient* (make <cise-ambient>))

;; Returns a copy of the default ambient
(define (cise-default-ambient) (cise-ambient-copy *default-ambient* '()))

;; Keeps the cise macro bindings.
;; We initialize it with *default-ambient* so that it gets all the cise
;; macros in this module.  At the end of this module we replace its
;; value with a copy, so that the default ambient is "sealed".
(define cise-ambient (make-parameter *default-ambient*))

;;=============================================================
;; Environment
;;

;; Environment keeps transient information during cise macro expansion.
;; It must be treated opaque from outside of CISE module.
(define-class <cise-env> ()
  ((context :init-keyword :context) ; toplevel, stmt or expr
   (decls   :init-keyword :decls)   ; list of extra decls
   ))

(define (make-env context decls)
  (make <cise-env> :context context :decls decls))
(define (env-ctx env)   (~ env'context))
(define (env-decls env) (~ env'decls))
(define (expr-ctx? env) (eq? (env-ctx env) 'expr))
(define (stmt-ctx? env) (eq? (env-ctx env) 'stmt))
(define (toplevel-ctx? env) (eq? (env-ctx env) 'toplevel))

(define (null-env)      (make-env 'stmt '()))

(define (expr-env env)
  (if (expr-ctx? env) env (make-env 'expr (env-decls env))))
(define (stmt-env env)
  (if (stmt-ctx? env) env (make-env 'stmt (env-decls env))))

(define (ensure-stmt-ctx form env)
  (unless (stmt-ctx? env)
    (if (expr-ctx? env)
      (error "cise: statement appears in an expression context:" form)
      (error "cise: statement appears in a toplevel context:" form))))

(define (ensure-toplevel-ctx form env)
  (unless (toplevel-ctx? env)
    (error "cise: form can only appear in toplevel:" form)))
(define (ensure-stmt-or-toplevel-ctx form env)
  (unless (or (toplevel-ctx? env) (stmt-ctx? env))
    (error "cise: form can only appear in toplevel or statement context:" form)))
(define (env-decl-add! env decl)
  (push! (~ env'decls) decl))

(define (wrap-expr form env)
  (if (expr-ctx? env) form `(,form ";")))

(define (render-env-decls env)
  (map (^.[(var type) `(,(cise-render-typed-var type var env) ";")])
       (env-decls env)))

;; Check source-info attribute of the input S-expr, and returns Stree
;; of "#line" line if necessary.
(define (source-info form env)
  (if (not (cise-emit-source-line))
    '()
    (match (debug-source-info form)
      [((? string? file) line)
       `((source-info ,file ,line))]
      [_ '()])))

;;=============================================================
;; Global decls
;; NB: See the note in cise-ambient definition above.
;; Usually you don't need to use these---just use cise-unit API instead.

(define (cise-push-static-decl! stree :optional (ambient (cise-ambient)))
  (push! (~ ambient'static-decls) stree))

(define (cise-push-static-decl-unique! stree :optional (ambient (cise-ambient)))
  (unless (memq stree (~ ambient'static-decls))
    (push! (~ ambient'static-decls) stree)))

(define (emit-static-decls port :optional (ambient (cise-ambient)))
  (dolist [stree (reverse (~ ambient'static-decls))]
    (render-finalize stree port))
  (set! (~ ambient'static-decls) '()))

;; external API
(define (cise-ambient-decl-strings ambient)
  (call-with-output-string
    (cut emit-static-decls <> ambient)))

;;=============================================================
;; Expander
;;
;;  Cgen expander knows little about C.  It handles literals
;;  (strings, numbers, booleans, and characters) and function calls.
;;  All other stuff is handled by "cise macros"

;;
;; cise-register-macro! NAME EXPANDER &optional AMBIENT
;;
;;   Register cise macro expander EXPANDER with the name NAME.
;;   EXPANDER takes twi arguments, the form to expand and a
;;   opaque cise environmen.
;;
(define (cise-register-macro! name expander :optional (ambient (cise-ambient)))
  (hash-table-put! (~ ambient'macros) name expander))

;;
;; cise-lookup-macro NAME &optional AMBIENT
;;
;;   Lookup cise macro.
;;
(define (cise-lookup-macro name :optional (ambient (cise-ambient)))
  (hash-table-get (~ ambient'macros) name #f))

;;
;; copy the current cise ambient
;;
;;   By default, static-decls are copied.  It's useful when you save
;;   the snapshot of ambient for the later retry.  Another usage is
;;   to have a transient "child" ambient, where you add new macros,
;;   emit something, and come back to the original ambient.  In that
;;   case you want to clear out static-decls.  Hence we have the second
;;   argument.   NB: This spec smells fishy.  May change later.
(define (cise-ambient-copy :optional
                           (ambient (cise-ambient))
                           (static-decls (~ ambient'static-decls)))
  (make <cise-ambient>
    :macros (hash-table-copy (~ ambient'macros))
    :static-decls static-decls))

;;
;; define-cise-macro (OP FORM ENV) . BODY
;;
;;   Default syntax to add new cise macro to the current ambient.
;;
(define-syntax define-cise-macro
  (syntax-rules ()
    [(_ (op form env) . body)
     (cise-register-macro! 'op (lambda (form env) . body))]
    [(_ op op2)                         ; alias
     (cise-register-macro! 'op (or (cise-lookup-macro 'op2)
                                   (error "unknown cise macro:" 'op2)))]))
;;
;; define-cise-stmt OP [ENV] CLAUSE ... [:where DEFINITION ...]
;; define-cise-expr OP [ENV] CLAUSE ... [:where DEFINITION ...]
;; define-cise-toplevel OP [ENV] CLAUSE ... [:where DEFINITION ...]
;;

(define-syntax define-cise-stmt
  (syntax-rules (:where)
    ;; recursion
    [(_ "clauses" op env clauses (:where defs ...))
     (define-cise-macro (op form env)
       defs ...
       (ensure-stmt-ctx form env)
       (match form . clauses))]
    [(_ "clauses" op env clauses ())
     (define-cise-stmt "clauses" op env clauses (:where))]
    [(_ "clauses" op env (clause ...) (x . y))
     (define-cise-stmt "clauses" op env (clause ... x) y)]
    ;; entry
    [(_ (op . args) . body)       ; single pattern case
     (define-cise-stmt "clauses" op env (((_ . args) . body)) ())]
    [(_ op (pat . body) .  clauses) ; (pat . body) rules out a single symbol
     (define-cise-stmt "clauses" op env ((pat . body)) clauses)]
    [(_ op env . clauses)
     (define-cise-stmt "clauses" op env () clauses)]))

(define-syntax define-cise-expr
  (syntax-rules (:where)
    ;; recursion
    [(_ "clauses" op env clauses (:where defs ...))
     (define-cise-macro (op form env)
       defs ...
       (let1 expanded (match form . clauses)
         (if (and (pair? expanded) (symbol? (car expanded)))
           (render-rec expanded env)
           (wrap-expr expanded env))))]
    [(_ "clauses" op env clauses ())
     (define-cise-expr "clauses" op env clauses (:where))]
    [(_ "clauses" op env (clause ...) (x . y))
     (define-cise-expr "clauses" op env (clause ... x) y)]
    ;; entry
    [(_ (op . args) . body)       ; single pattern case
     (define-cise-expr "clauses" op env (((_ . args) . body)) ())]
    [(_ op (pat . body) .  clauses)
     (define-cise-expr "clauses" op env ((pat . body)) clauses)]
    [(_ op env . clauses)
     (define-cise-expr "clauses" op env () clauses)]))

(define-syntax define-cise-toplevel
  (syntax-rules (:where)
    ;; recursion
    [(_ "clauses" op env clauses (:where defs ...))
     (define-cise-macro (op form env)
       defs ...
       (ensure-toplevel-ctx form env)
       (match form . clauses))]
    [(_ "clauses" op env clauses ())
     (define-cise-toplevel "clauses" op env clauses (:where))]
    [(_ "clauses" op env (clause ...) (x . y))
     (define-cise-toplevel "clauses" op env (clause ... x) y)]
    ;; entry
    [(_ (op . args) . body)       ; single pattern case
     (define-cise-toplevel "clauses" op env (((_ . args) . body)) ())]
    [(_ op (pat . body) .  clauses) ; (pat . body) rules out a single symbol
     (define-cise-toplevel "clauses" op env ((pat . body)) clauses)]
    [(_ op env . clauses)
     (define-cise-toplevel "clauses" op env () clauses)]))

;;
;; cise-render cise &optional port context
;;
;; context := 'toplevel | 'stmt | 'expr | #t (expr) | #f (stmt)
;;
;;   External entry of renderer
;;
(define (cise-render form :optional (port (current-output-port)) (ctx 'stmt))
  (let* ([env (case ctx
                [(toplevel) (make-env 'toplevel '())]
                [(stmt #f)  (null-env)]
                [(expr #t)  (expr-env (null-env))]
                [else (error "cise-render: invalid context:" ctx)])]
         [stree (render-rec form env)])
    (render-finalize `(,@(render-env-decls env) ,stree) port)))

(define (cise-render-to-string form :optional (ctx #f))
  (call-with-output-string (cut cise-render form <> ctx)))

(define (render-finalize stree port)
  (define current-file #f)
  (define current-line 1)
  (define (rec stree)
    (match stree
      [('source-info (? string? file) line)
       (cond [(and (equal? file current-file) (eqv? line current-line))]
             [(and (equal? file current-file) (eqv? line (+ 1 current-line)))
              (inc! current-line)
              (format port "\n")]
             [else
              (set! current-file file)
              (set! current-line line)
              (when (cise-emit-source-line)
                (format port "\n#line ~a ~s\n" line file))])]
      ['|#reset-line| ; reset source info
       (set! current-file #f) (set! current-line 0)]
      [(x . y) (rec x) (rec y)]
      [(? (any-pred string? symbol? number?) x) (display x port)]
      [_ #f]))
  (rec stree))

;;
;; cise-render-rec cise stmt/expr env
;;
;;   External interface to call back to cise expander recursively.
;;   stmt/expr should be either a symbol stmt or expr.
;;   env must be treated as opaque object.
;;
(define (cise-render-rec form stmt/expr env)
  (case stmt/expr
    [(stmt) (render-rec form (stmt-env env))]
    [(expr) (render-rec form (expr-env env))]
    [else (error "cise-render-rec: second argument must be either \
                  stmt or expr, but got:" stmt/expr)]))

;; render-rec :: Cise, Env -> Stree
;;   Recursively expands Cise and generates Stree
(define (render-rec form env)
  (match form
    [([? symbol? key] . args)
     (cond [(cise-lookup-macro key)
            => (^[expander] `(,@(source-info form env)
                              ,@(render-rec (expander form env) env)))]
           [(or (type-decl-initial? key)
                (any type-decl-subsequent? args))
            (cise-render-typed-var form "" env)]
           [else
            (let1 eenv (expr-env env)
              (wrap-expr
               `(,@(source-info form env)
                 ,(cise-render-identifier key) "("
                 ,@(intersperse "," (map (cut render-rec <> eenv) args))
                 ")")
               env))])]
    [(x . y)     form]   ; already stree
    ['|#reset-line| '|#reset-line|] ; special directive to reset line info
    [[? type-decl-initial?] (wrap-expr (cise-render-typed-var form "" env) env)]
    [[? symbol?] (wrap-expr (cise-render-identifier form) env)]
    [[? identifier?] (wrap-expr (cise-render-identifier (unwrap-syntax form))
                                env)]
    [[? string?] (wrap-expr (write-to-string form) env)]
    [[? real?]   (wrap-expr form env)]
    [()          '()]
    [#\'         (wrap-expr "'\\''"  env)]
    [#\\         (wrap-expr "'\\\\'" env)]
    [#\newline   (wrap-expr "'\\n'"  env)]
    [#\return    (wrap-expr "'\\r'"  env)]
    [#\tab       (wrap-expr "'\\t'"  env)]
    [[? char?]
     (if (>= (char->integer form) 128)
       (error "CISE: Cannot embed non-ASCII character literal (yet):" form)
       (wrap-expr `("'" ,(if (char-set-contains? #[[:alnum:]] form)
                           (string form)
                           (format "\\x~2,'0x" (char->integer form)))
                    "'") env))]
    [_           (error "Invalid CISE form: " form)]))

;;
;; cise-translate inp outp &key enviroment
;;
;;   External interface to translate entire CiSE file into C.
;;   CiSE expressions are read from INP and the resulting C code
;;   is written to OUTP.
;;
;;   If CISE-TRANSLATE encounters a form (.static-decls),
;;   it expands the rest of CiSE forms into a temporary string,
;;   then emits the forward declarations of static functions
;;   into outp, followed by the accumulated C code.  With this
;;   you don't need to write forward declarations in CiSE source.

(define (cise-translate inp outp
                        :key (environment (make-module #f))
                             (ambient (cise-ambient-copy)))
  (define (finish toutp)
    (unless (eq? outp toutp)
      (emit-static-decls outp)
      (display (get-output-string toutp) outp))
    (newline outp))

  (eval '(use gauche.cgen.cise) environment)
  (eval '(use util.match) environment)
  (parameterize ([cise-ambient ambient])
    (let loop ([toutp outp])
      (match (read inp)
        [(? eof-object?) (finish toutp)]
        [('.raw-c-code . cs)
         (dolist [c cs] (newline toutp) (display c toutp)) (loop toutp)]
        [(and ((or 'define-cise-stmt 'define-cise-expr 'define-cise-toplevel)
               . _)
              f)
         (eval f environment)
         (loop toutp)]
        [('.static-decls) (loop (open-output-string))]
        [(and (op . _) f)
         (if (cise-lookup-macro op)
           (cise-render f toutp 'toplevel)
           (eval f environment))
         (loop toutp)]))))

;;=============================================================
;; Built-in macros
;;

;;------------------------------------------------------------
;; C function definition
;;

;; (define-cfn <name> (<arg> ...) [<rettype> [<qualifier> ...]] <body>)
;; (declare-cfn <name> (<arg> ...) [<rettype> [<qualifier> ...]])

(define-cise-macro (define-cfn form env)
  (expand-cfn form env))
(define-cise-macro (declare-cfn form env)
  (expand-cfn form env))

(define (expand-cfn form env)
  (define (gen-args args env)
    (let1 eenv (expr-env env)
      ($ intersperse ","
         $ map (^.[(var . type) (cise-render-typed-var type var eenv)]) args)))

  (define (gen-qualifiers quals) ; we might support more qualifiers in future
    (intersperse " "
                 (map (^[qual] (ecase qual
                                 [(:static) "static"]
                                 [(:inline) "inline"]
                                 [(:extern) "extern"]))
                      (reverse quals))))

  (define (gen-cfn name quals args rettype body)
    `(,@(gen-qualifiers quals) " "
      ,(cise-render-typed-var rettype name env)
      "(" ,(gen-args args env) ")"
      "{",(cise-render-to-string `(begin ,@body) 'stmt)"}"))
  ;; Another ugly hack to allow both :: rettype and ::rettype as
  ;; return type specification.   Duplication in stub.scm.
  (define (type-symbol? s)
    (and (keyword? s) (#/^:[^:]/ (keyword->string s))))
  (define (type-symbol-type s)
    (string->symbol (string-drop (keyword->string s) 1)))

  (define (gen-ret-type ret-type)
    (match ret-type
      [(x ...) (intersperse " " (map x->string x))]
      [x (x->string x)]))
  (define (record-static name quals args ret-type)
    (cise-push-static-decl!
     `(,(source-info form env)
       ,@(gen-qualifiers quals) " "
       ,(gen-ret-type ret-type)" ",(cise-render-identifier name)
       "(",(gen-args args env)");")))

  (define (check-quals name quals args ret-type body)
    (match body
      [(':static . body)
       (check-quals name `(:static ,@quals) args ret-type body)]
      [(':inline . body)
       (check-quals name `(:inline ,@quals) args ret-type body)]
      [((? keyword? z) . body)
       (errorf "Invalid qualifier in define-cfn ~s: ~s" name z)]
      [_
       (case (car form)
         [(define-cfn)
          (when (memq :static quals)
            (record-static name quals args ret-type))
          (gen-cfn name quals args ret-type body)]
         [(declare-cfn)
          (unless (null? body)
            (errorf "declare-cfn ~s must not have a body" name))
          (when (or (memq :static quals) (memq :inline quals))
            (errorf "declare-cfn ~s cannot have qualifier(s)" name))
          (record-static name '(:extern) args ret-type)
          ;; no function implementation
          '()])]))

  (ensure-toplevel-ctx form env)
  (match form
    [(_ name (args ...) ':: ret-type . body)
     (check-quals name '() (canonicalize-argdecl args) ret-type body)]
    [(_ name (args ...) [? type-symbol? ts] . body)
     (check-quals name '() (canonicalize-argdecl args) (type-symbol-type ts) body)]
    [(_ name (args ...) . body)
     (check-quals name '() (canonicalize-argdecl args) 'ScmObj body)]))

;;------------------------------------------------------------
;; Global variable definition and typedef
;;

;; (define-cvar <name> [::<type>] [<qualifiers>...] [<init>])
;; (declare-cvar <name> [::<type>])
;; (define-ctype <name> [::<type>])

(define-cise-macro (define-cvar form env)
  (expand-cvar form env #t))
(define-cise-macro (declare-cvar form env)
  (expand-cvar form env #f))
(define-cise-macro (define-ctype form env)
  (expand-cvar form env #f))

(define (expand-cvar form env toplevel-only?)
  (define (gen-qualifiers quals)
    (intersperse " "
                 (map (^[qual] (ecase qual
                                      [(:static) "static"]
                                      [(:extern) "extern"]
                                      [(:typedef) "typedef"]))
                      (reverse quals))))

  (define (gen-cvar var type quals has-init? init)
    `(,@(gen-qualifiers quals) " "
      ,(cise-render-typed-var type var env)
      ,@(cond-list [has-init? `(" = ",(render-rec init (expr-env env)))])
      ";"))

  (define (check-quals var type quals init-and-quals)
    (match init-and-quals
      [(':static . init-and-quals)
       (check-quals var type `(:static ,@quals) init-and-quals)]
      [((? keyword? z) . body)
       (errorf "Invalid qualifier in define-cvar ~s: ~s" var z)]
      [()
       (case (car form)
         [(define-cvar) (gen-cvar var type quals #f #f)]
         [(declare-cvar)
          (unless (null? quals)
            (errorf "declare-cvar ~s cannot have qualifier(s)" var))
          (gen-cvar var type '(:extern) #f #f)]
         [(define-ctype)
          (unless (null? quals)
            (errorf "define-ctype ~s cannot have qualifier(s)" var))
          (gen-cvar var type '(:typedef) #f #f)])]
      [(init)
       (if (eq? (car form) 'define-cvar)
         (gen-cvar var type quals #t init)  
         (errorf "declare-cvar ~s cannot have initializer" var))]
      [else
       (errorf "Invalid syntax in ~s ~s: ~s"
               (car form) var init-and-quals)]))

  ;; We allow define-cvar only on toplevel, but declare-cvar and
  ;; define-ctype can appear in stmts.
  (if toplevel-only?
    (ensure-toplevel-ctx form env)
    (ensure-stmt-or-toplevel-ctx form env))

  (let* ([canon (car (canonicalize-vardecl (list (cdr form))))]
         [var (car canon)]
         [spec (cdr canon)])
    (receive (type init-and-quals)
        (match spec
          [()         (values 'ScmObj '())]
          [('::)      (errorf "invalid variable decl in ~s: (~s ~s)" 
                              (car form) var spec)]
          [(':: type) (values type '())]
          [(':: type . init-and-quals) (values type init-and-quals)]
          [else (values 'ScmObj spec)])
      (check-quals var type '() init-and-quals))))

;;------------------------------------------------------------
;; CPS transformation
;;
;;  (define-cproc ...
;;    ...
;;    (let1/cps resultvar expr
;;      (closevar ...)
;;      expr2 ...))
;;
;;  =>
;;  (define-cfn tmp_cc (resultvar data::(void**))
;;    (let* ([closevar (aref data 0)]
;;           ...)
;;      expr2 ...))
;;
;;  (define-cproc
;;    ...
;;    (let* ([data ...])
;;      (set! (aref data 0) closevar)
;;      ...
;;      (Scm_VMPushCC tmp_cc data k)
;;      expr))
;;
;; NB: This macro assumes the outer cproc returns one ScmObj, via
;; SCM_RESULT.  So it doesn't work well if the outer cproc is declared
;; with some other return values.  For example, if the outer cproc
;; is supposed to have ::<void> return val, you actually should
;; write something like the following:
;;
;;   (define-cproc foo (args ...)   ;; don't declare return type here
;;     ...
;;     (let1/cps r (Scm_VMApply1 proc x ...)
;;       [var ...]
;;       ...
;;       (return SCM_UNDEFINED)))   ;; explicitly return #<undef>
;;

(define-cise-macro (let1/cps form env)
  (match form
    [(_ rvar expr vars . body)
     (let* ([tmp-cc (gensym "tmp_cc_")]
            [data (gensym "data")]
            [closed (canonicalize-argdecl vars)]
            [cc-env (make-env 'toplevel '())])
       ;; NB: We want to check the # of closed variables is smaller
       ;; than SCM_CCONT_DATA_SIZE, but it's not available at runtime
       ;; (and if we're cross-compiling, our runtime's value may be
       ;; different from the target system's.

       ;; KLUDGE! If we're in stub generation, cise-ambient is set up
       ;; to alter 'return' macro.  But we need the original 'return'
       ;; macro in order to expand define-cfn.  We need better mechanism
       ;; to handle it smoothly.
       (let1 amb (cise-ambient-copy)
         (cise-register-macro! 'return
                               (cise-lookup-macro 'return
                                                  (cise-default-ambient))
                               amb)
         (parameterize ([cise-ambient amb])
           (cise-push-static-decl!
            (cise-render-to-string
             `(define-cfn ,tmp-cc (,rvar ,data :: void**) :static
                ,(if (null? closed)
                   `(begin (cast void ,data)
                           ,@body)
                   `(let* ,(map-with-index
                            (^[i p]
                              `(,(car p) :: ,(cdr p)
                                (cast (,(cdr p)) (aref ,data ,i))))
                            closed)
                      ,@body)))
             'toplevel)))
         (for-each cise-push-static-decl-unique!
                   (reverse (~ amb'static-decls))))

       (if (null? closed)
         `(begin (Scm_VMPushCC ,tmp-cc NULL 0)
                 (return ,expr))
         `(let* ([,data :: (.array void* (,(length closed)))])
            ,@(map-with-index
               (^[i p] `(set! (aref ,data ,i) (cast void* ,(car p))))
               closed)
            (Scm_VMPushCC ,tmp-cc ,data ,(length closed))
            (return ,expr))))]))

;;------------------------------------------------------------
;; Syntax
;;

;; [cise stmt]  begin STMT ...
;;    Grouping.
(define-cise-macro (begin form env)
  (cond
   [(stmt-ctx? env)
    `("{" ,@(map (cut render-rec <> env) (cdr form)) "}")]
   [(toplevel-ctx? env)
    `(,@(map (cut render-rec <> env) (cdr form)))]
   [else
    (intersperse "," (map (cut render-rec <> env) (cdr form)))]))

;; [cise stmt]  let* ((VAR [:: TYPE] [INIT-EXPR]) ...) STMT ...
;;    Local variables.   Because of C semantics, we only support
;;    let*-style scoping.
;;    :: TYPE can be omitted if the type of VAR is ScmObj.
(define-cise-macro (let* form env)
  (ensure-stmt-ctx form env)
  (match form
    [(_ vars . body)
     (match (canonicalize-vardecl vars)
       [((var . spec) ...)
        (let1 eenv (expr-env env)
          `(begin
             ,@(map (^[var spec]
                      (receive (type has-init? init)
                          (match spec
                            [()         (values 'ScmObj #f #f)]
                            [('::)      (errorf "invalid variable decl in let* form: (~s ~s)" var spec)]
                            [(init)     (values 'ScmObj #t init)]
                            [(':: type) (values type #f #f)]
                            [(':: type init) (values type #t init)])
                        `(,(cise-render-typed-var type var env)
                          ,@(cond-list [has-init? `("=",(render-rec init eenv))])
                          ";")))
                    var spec)
             ,@(map (cut render-rec <> env) body)))]
       [_ (error "invalid variable decls in let* form:" form)])]
    ))

;; [cise stmt] if TEST-EXPR THEN-STMT [ELSE-STMT]
;;    Conditional.
(define-cise-macro (if form env)
  (ensure-stmt-ctx form env)
  (let1 eenv (expr-env env)
    (match form
      [(_ test then)
       `("if (",(render-rec test eenv)")"
         "{",(render-rec then env)"}")]
      [(_ test then else)
       `("if (",(render-rec test eenv)")"
         "{",(render-rec then env)"} else {" ,(render-rec else env) "}")]
      )))

;; [cise stmt] when TEST-EXPR STMT ...
;; [cise stmt] unless TEST-EXPR STMT ...
(define-cise-stmt when
  [(_ test . forms) `(if ,test (begin ,@forms))])

(define-cise-stmt unless
  [(_ test . forms) `(if (not ,test) (begin ,@forms))])

;; [cise stmt] cond (TEST STMT ...) ... [ (else STMT ...) ]
;;   Nested if.
(define-cise-macro (cond form env)
  (ensure-stmt-ctx form env)
  (let1 eenv (expr-env env)
    (define (a-clause test rest)
      `("(" ,(render-rec test eenv) ")" ,(render-rec `(begin ,@rest) env)))
    (match form
      [(_ (test . rest) ...)
       (fold-right (^[test rest r]
                     (cond
                      [(and (null? r) (eq? test 'else))
                       `(" else ",(render-rec `(begin ,@rest) env))]
                      [(eq? test (caadr form)) ; first form
                       `("if ",(a-clause test rest) ,@r)]
                      [else
                       `("else if" ,(a-clause test rest) ,@r)]))
                   '() test rest)]
      )))

;; [cise stmt] case EXPR ((VAL ...) STMT ...) ... [ (else STMT ...) ]
;; [cise stmt] case/fallthrough EXPR ((VAL ...) STMT ...) ... [ (else STMT ...) ]
;;    Expands to switch-case statement.   The 'case' form does not
;;    fallthrough, while 'case/fallthrough' does.
(define (case-generator form env fallthrough?)
  (let1 eenv (expr-env env)
    (match form
      [(_ expr (literalss . clauses) ...)
       `("switch (",(render-rec expr eenv)") {"
         ,@(map (^[literals clause]
                  `(,@(source-info literals env)
                    ,@(if (eq? literals 'else)
                        '("default: ")
                        (map (^[literal]
                               `("case ",(render-rec literal eenv)" : "))
                             literals))
                    ,@(render-rec `(begin ,@clause
                                          ,@(if fallthrough? '() '((break))))
                                  env)
                    ,@(cond-list [fallthrough? '("/*FALLTHROUGH*/")])))
                literalss clauses)
         "}")]
      )))

(define-cise-macro (case form env)
  (ensure-stmt-ctx form env)
  (case-generator form env #f))

(define-cise-macro (case/fallthrough form env)
  (ensure-stmt-ctx form env)
  (case-generator form env #t))

;; [cise stmt] for (START-EXPR TEST-EXPR UPDATE-EXPR) STMT ...
;; [cise stmt] for () STMT ...
;;   Loop.
(define-cise-macro (for form env)
  (ensure-stmt-ctx form env)
  (let1 eenv (expr-env env)
    (match form
      [(_ (start test update) . body)
       `("for (",(render-rec start eenv)"; "
         ,(render-rec test eenv)"; "
         ,(render-rec update eenv)")"
         ,(render-rec `(begin ,@body) env))]
      [(_ () . body)
       `("for (;;)" ,(render-rec `(begin ,@body) env))]
      )))

;; [cise stmt] loop STMT ...
;;   Alias of (for () STMT ...)
(define-cise-stmt loop
  [form `(for () ,@(cdr form))])

;; [cise stmt] while TEST-EXPR STMT ...
;;   Loop.
(define-cise-macro (while form env)
  (ensure-stmt-ctx form env)
  (let1 eenv (expr-env env)
    (match form
      [(_ test . body)
       `("while"
         "(",(render-rec test eenv)")"
         ,(render-rec `(begin ,@body) env))])))

;; [cise stmt] for-each (lambda (VAR) STMT ...) EXPR
;;   EXPR must yield a list.  Traverse the list, binding each element
;;   to VAR and executing STMT ....
;;   The lambda form is a fake; you don't really create a closure.
(define-cise-macro (for-each form env)
  (ensure-stmt-ctx form env)
  (let ([eenv (expr-env env)]
        [tmp  (gensym "cise__")])
    (match form
      [(_ ('lambda (var) . body) list-expr)
       (env-decl-add! env `(,tmp ScmObj))
       `("SCM_FOR_EACH(" ,(cise-render-identifier tmp) ","
         ,(render-rec list-expr eenv) ") {"
         ,(if (eq? var '_)
            (render-rec `(begin ,@body) env)
            (render-rec `(let* ((,var :: ScmObj (SCM_CAR ,tmp)))
                           ,@body) env))
         "}")])))

;; [cise stmt] dolist [VAR EXPR] STMT ...
(define-cise-macro (dolist form env)
  (ensure-stmt-ctx form env)
  (let1 eenv (expr-env env)
    (match form
      [(_ (var expr) . body)
       `(for-each (lambda (,var) ,@body) ,expr)])))

;; [cise stmt] pair-for-each (lambda (VAR) STMT ...) EXPR
;;   Like for-each, but VAR is bound to each 'spine' cell instead of
;;   each element of the list.
(define-cise-macro (pair-for-each form env)
  (ensure-stmt-ctx form env)
  (let1 eenv (expr-env env)
    (match form
      [(_ ('lambda (var) . body) list-expr)
       (env-decl-add! env `(,var ScmObj))
       `("SCM_FOR_EACH(" ,(cise-render-identifier var) ","
         ,(render-rec list-expr eenv) ")"
         ,(render-rec `(begin ,@body) env)
         )])))

;; [cise stmt] dopairs [VAR EXPR] STMT ...
(define-cise-macro (dopairs form env)
  (ensure-stmt-ctx form env)
  (let1 eenv (expr-env env)
    (match form
      [(_ (var expr) . body)
       `(pair-for-each (lambda (,var) ,@body) ,expr)])))

;; [cise stmt] dotimes (VAR EXPR) STMT ...
;;   EXPR must yield an integer, N.  Repeat STMT ... by binding VAR from 0
;;   to (N-1).
(define-cise-macro (dotimes form env)
  (ensure-stmt-ctx form env)
  (let ([eenv (expr-env env)]
        [n    (gensym "cise__")])
    (match form
      [(_ (var expr) . body)
       `(let* ((,var :: int 0) (,n :: int ,expr))
          (for [() (< ,var ,n) (post++ ,var)] ,@body))])))

;; [cise stmt] return [EXPR]
;;   Return statement.
;;   NB: While processing cproc body in stubs, return macro is overwritten
;;   to handle multiple value returns.  See cgen-stub-cise-ambient
;;   in stub.scm.
(define-cise-macro (return form env)
  (ensure-stmt-ctx form env)
  (match form
    [(_ expr) `("return (" ,(render-rec expr (expr-env env)) ");")]
    [(_)      `("return;")]))

;; [cise stmt] break
;; [cise stmt] continue
;;   Break and continue.
(define-cise-stmt break
  [(_) '("break;")])

(define-cise-stmt continue
  [(_) '("continue;")])

;; [cise stmt] label NAME
;; [cise stmt] goto NAME
;;   Label and goto.
;;   We always add null statement after the label, so that we can place
;;   (label NAME) at the end of compound statement.
(define-cise-stmt label
  [(_ name) `(,(cise-render-identifier name) " :; ")])

(define-cise-stmt goto
  [(_ name) `("goto " ,(cise-render-identifier name) ";")])

;;
;; Preprocessor directives
;;

;; [cise toplevel/stmt] .if STRING STMT [STMT]
;;   c preprocessor directive
(define-cise-macro (.if form env)
  (ensure-stmt-or-toplevel-ctx form env)
  (match form
    [(_ condition stmt1)
     `("\n" |#reset-line|               ;make sure we start from the fresh line
       "#if " ,(cpp-condition->string condition) "\n" |#reset-line|
       ,(render-rec stmt1 env) "\n"
       "#endif /* " ,(cpp-condition->string condition) " */\n" |#reset-line|)]
    [(_ condition stmt1 stmt2)
     `("\n" |#reset-line|               ;make sure we start from the fresh line
       "#if " ,(cpp-condition->string condition) "\n" |#reset-line|
       ,(render-rec stmt1 env) "\n"
       "#else /* !",(cpp-condition->string condition) " */\n" |#reset-line|
       ,(render-rec stmt2 env) "\n"
       "#endif /* " ,(cpp-condition->string condition) " */\n" |#reset-line|)]))

;; [cise toplevel/stmt] .when STRING STMT [STMT]
;;   c preprocessor directive
(define-cise-macro (.when form env)
  (ensure-stmt-or-toplevel-ctx form env)
  (match form
    [(_ condition stmt ...)
     `("\n" |#reset-line|               ;make sure we start from the fresh line
       "#if " ,(cpp-condition->string condition) "\n" |#reset-line|
       ,(intersperse "\n" (map (cut render-rec <> env) stmt)) "\n"
       "#endif /* " ,(cpp-condition->string condition) " */\n" |#reset-line|)]))

;; [cise toplevel/stmt] .unless STRING STMT [STMT]
;;   c preprocessor directive
(define-cise-macro (.unless form env)
  (ensure-stmt-or-toplevel-ctx form env)
  (match form
    [(_ condition stmt ...)
     `("\n" |#reset-line|               ;make sure we start from the fresh line
       "#if !(" ,(cpp-condition->string condition) ")\n" |#reset-line|
       ,(intersperse "\n" (map (cut render-rec <> env) stmt)) "\n"
       "#endif /* ! " ,(cpp-condition->string condition) " */\n" |#reset-line|)]))

;; [cise toplevel/stmt] .cond CLAUSE [CLAUSE]
;;   c preprocessor if/elif/endif chain directive
(define-cise-macro (.cond form env)
  (ensure-stmt-or-toplevel-ctx form env)
  (match form
    [(_ (condition . stmts) ...)
     `("\n#if 0 /*dummy*/\n" |#reset-line|
       ,@(fold-right (lambda (c ss seed)
                       `(,(cond [(eq? c 'else) '("#else")]
                                [else `("#elif " ,(cpp-condition->string c))])
                         "\n" |#reset-line|
                         ,(map (cut render-rec <> env) ss) "\n"
                         ,@seed))
                     '("#endif\n" |#reset-line|)
                     condition stmts))]))

;; [cise toplevel/stmt] .define NAME [EXPR]
;; [cise toplevel/stmt] .define NAME (ARGS...) EXPR
;;   c preprocessor define directive

;; Note that "#define abc(a,b)" (i.e. no EXPR) cannot be generated
;; because it's ambiguous with "#define abc a(b)".
(define-cise-macro (.define form env)
  (ensure-stmt-or-toplevel-ctx form env)
  (match form
    [(_ name) `("#define " ,(x->string name) "\n" |#reset-line|)]
    [(_ name (args ...) expr) `("#define " ,(x->string name)
                                "(" ,(intersperse "," (map x->string args)) ")"
                                " (" ,(parameterize ([cise-emit-source-line #f])
                                        (render-rec expr (expr-env env))) ")"
                                "\n" |#reset-line|)]
    [(_ name expr) `("#define " ,(x->string name)
                     " (" ,(parameterize ([cise-emit-source-line #f])
                             (render-rec expr (expr-env env))) ")"
                     "\n" |#reset-line|)]))

;; [cise toplevel/stmt] .undef NAME
;;   c preprocessor undefine directive
(define-cise-macro (.undef form env)
  (ensure-stmt-or-toplevel-ctx form env)
  (match form
    [(_ name) `("#undef " ,(x->string name) "\n" |#reset-line|)]))

;; [cise toplevel/stmt] .include PATH
;;   c preprocessor include directive
(define-cise-macro (.include form env)
  (ensure-stmt-or-toplevel-ctx form env)
  (match form
    [(_ item ...)
     (map (^f `("#include "
                ,(cond [(string? f) (write-to-string f)]
                       [(symbol? f) (x->string f)]
                       [else (error "bad argument to .include:" f)])
                "\n" |#reset-line|))
          item)]
    [(_ . other) (error "malformed .include:" form)]))

(define-cise-macro |#if| .if)           ;backward compat.

;;------------------------------------------------------------
;; Operators
;;

;; [cise expr] + EXPR ...
;; [cise expr] - EXPR ...
;; [cise expr] * EXPR ...
;; [cise expr] / EXPR ...
;; [cise expr] % EXPR EXPR
;;   Same as C.
;;
;; [cise expr] and EXPR ...
;; [cise expr] or  EXPR ...
;; [cise expr] not EXPR
;;
;;   Boolean ops.  C's &&, ||, and !.
;;
;; [cise expr] logand EXPR EXPR ...
;; [cise expr] logior EXPR EXPR ...
;; [cise expr] logxor EXPR EXPR ...
;; [cise expr] lognot EXPR
;;
;;   Bitwise ops.
;;
;; [cise expr] * EXPR
;; [cise expr] & EXPR
;;
;;   Address ops.
;;
;; [cise expr] pre++ EXPR
;; [cise expr] post++ EXPR
;; [cise expr] pre-- EXPR
;; [cise expr] post-- EXPR
;;
;;   pre/post increment/decrement.
;;
;; [cise expr] <  EXPR EXPR
;; [cise expr] <= EXPR EXPR
;; [cise expr] >  EXPR EXPR
;; [cise expr] >= EXPR EXPR
;; [cise expr] == EXPR EXPR
;; [cise expr] != EXPR EXPR
;;
;;   comparison.
;;
;; [cise expr] << EXPR EXPR
;; [cise expr] >> EXPR EXPR
;;
;;   shift.
;;
;; [cise expr] set! LVALUE EXPR LVALUE EXPR ...
;; [cise expr] =    LVALUE EXPR LVALUE EXPR ...
;; [cise expr] +=   LVALUE EXPR
;; [cise expr] -=   LVALUE EXPR
;; [cise expr] *=   LVALUE EXPR
;; [cise expr] /=   LVALUE EXPR
;; [cise expr] %=   LVALUE EXPR
;; [cise expr] <<=  LVALUE EXPR
;; [cise expr] >>=  LVALUE EXPR
;; [cise expr] logand= LVALUE EXPR
;; [cise expr] logior= LVALUE EXPR
;; [cise expr] logxor= LVALUE EXPR
;;
;;   assignment.
;;
;; [cise expr] ->  EXPR EXPR ...
;; [cise expr] ref EXPR EXPR ...
;;
;;   reference.  (ref is C's '.')
;;
;; [cise expr] aref EXPR EXPR ...
;;
;;   array reference.
;;
;; [cise expr] cast TYPE EXPR
;;
;;   cast.
;;
;; [cise expr] .type TYPE
;;
;;   not a C expression, but useful to place a type name (e.g. an argument
;;   of sizeof etc.)
;;
;; [cise expr] ?: TEST-EXPR THEN-EXPR ELSE-EXPR
;;
;;   conditional.

(define-macro (define-nary op sop)
  `(define-cise-macro (,op form env)
     (let1 eenv (expr-env env)
       (wrap-expr
        (match form
          [(_ a)
           (list ,sop "("(render-rec a eenv)")")]
          [(_ a b)
           (list "("(render-rec a eenv)")",sop"("(render-rec b eenv)")")]
          [(_ a b . x)
           (list* ',op (list ',op a b) x)])
        env))))

(define-nary + "+")
(define-nary - "-")
(define-nary * "*")
(define-nary / "/")

(define-nary and "&&")
(define-nary or  "||")

(define-nary logior  "|")
(define-nary logxor  "^")
(define-nary logand  "&")

(define-macro (define-unary op sop)
  `(define-cise-macro (,op form env)
     (wrap-expr
      (match form
        [(_ a)   (list ,sop "("(render-rec a (expr-env env))")")])
      env)))

(define-unary not    "!")
(define-unary lognot "~")
(define-unary &      "&")               ; only unary op

(define-unary pre++  "++")
(define-unary pre--  "--")

(define-cise-macro inc! pre++)
(define-cise-macro dec! pre--)

(define-macro (define-post-unary op sop)
  `(define-cise-macro (,op form env)
     (wrap-expr
      (match form
        [(_ a)   (list "("(render-rec a (expr-env env))")" ,sop)])
      env)))

(define-post-unary post++ "++")
(define-post-unary post-- "--")

(define-macro (define-binary op sop)
  `(define-cise-macro (,op form env)
     (wrap-expr
      (match form
        [(_ a b)
         (list "("(render-rec a (expr-env env))")",sop
               "("(render-rec b (expr-env env))")")])
      env)))

(define-binary %       "%")
(define-binary <       "<")
(define-binary <=      "<=")
(define-binary >       ">")
(define-binary >=      ">=")
(define-binary ==      "==")
(define-binary !=      "!=")
(define-binary <<      "<<")
(define-binary >>      ">>")

(define-binary +=      "+=")
(define-binary -=      "-=")
(define-binary *=      "*=")
(define-binary /=      "/=")
(define-binary %=      "%=")
(define-binary <<=     "<<=")
(define-binary >>=     ">>=")

(define-binary logior= "|=")
(define-binary logxor= "^=")
(define-binary logand= "&=")

(define-macro (define-referencer op sop)
  `(define-cise-macro (,op form env)
     (let1 eenv (expr-env env)
       (wrap-expr
        (match form
          [(_ a b ...)
           (list "("(render-rec a eenv)")",sop
                 (intersperse ,sop (map (cut render-rec <> eenv) b)))])
        env))))

(define-referencer ->  "->")
(define-referencer ref ".")

(define-cise-macro (aref form env)
  (let1 eenv (expr-env env)
    (wrap-expr
     (match form
       [(_ a b ...)
        `("(",(render-rec a eenv)")"
          ,(append-map (^[ind] `("[",(render-rec ind eenv)"]")) b))])
     env)))

(define-cise-macro (?: form env)
  (let1 eenv (expr-env env)
    (wrap-expr
     (match form
       [(?: test then else)
        (list "(("(render-rec test eenv)")?("
              (render-rec then eenv)"):("
              (render-rec else eenv)"))")])
     env)))

(define-cise-macro (set! form env)
  (let1 eenv (expr-env env)
    (let loop ((args (cdr form)) (r '()))
      (match args
        [()  (wrap-expr (intersperse "," (reverse r)) env)]
        [(var val . more)
         (loop (cddr args)
               `((,(render-rec var eenv)
                  "=(",(render-rec val eenv)")") ,@r))]
        [_   (error "uneven args for set!:" form)]))))

(define-cise-macro = set!)              ;EXPERIMENTAL

;; [cise expr] funcall fn-expr arg-expr ...
;;   Generate fn-expr(arg-expr, ...)
;;   Needed if fn-expr isn't a simple identifier.
(define-cise-macro (funcall form env)
  (let1 eenv (expr-env env)
    (wrap-expr
     `("(" ,(render-rec (cadr form) eenv) ")"
       "(" ,@(intersperse "," (map (cut render-rec <> eenv) (cddr form)))
       ")")
     env)))

;;------------------------------------------------------------
;; Type-related expressions
;;

(define-cise-macro (cast form env)
  (let1 eenv (expr-env env)
    (wrap-expr
     (match form
       [(_ type expr)
        `("((",(cise-render-typed-var type "" env)")(",(render-rec expr eenv)"))")])
     env)))

(define-cise-macro (.type form env)
  (match form
    [(_ typenames ...)
     (when (null? typenames)
       (errorf "empty .type form is not allowed: ~s" form))
     `(,(cise-render-typed-var typenames "" env))]))

;;------------------------------------------------------------
;; Convenience expression macros
;;

;; Embed raw c code.  THIS IS A KLUDGE---SHOULDN'T BE USED.
;; Allowing raw C code prevents higher-level analysis of cise code.
;; This should be regarded as a compromise until cise support full C features.
(define-cise-expr C:
  [(_ stuff) (list (x->string stuff))])

;; DEPRECATED: cgen-stub-cise-ambient overrides 'return' macro.
;; Use it instead.
(define-cise-expr result
  [(_) (error "cise: result form needs at least one value'")]
  [(_ e) `(set! SCM_RESULT ,e)]
  [(_ e0 e1) `(set! SCM_RESULT0 ,e0 SCM_RESULT1 ,e1)]
  [(_ e0 e1 e2) `(set! SCM_RESULT0 ,e0 SCM_RESULT1 ,e1 SCM_RESULT2 ,e2)]
  [(_ xs ...) `(set!
                ,@(concatenate
                   (map-with-index
                    (^[i x] `(,(string->symbol #"SCM_RESULT~i") ,x))
                    xs)))])

(define-cise-expr list
  [(_)           '("SCM_NIL")]
  [(_ a)         `(SCM_LIST1 ,a)]
  [(_ a b)       `(SCM_LIST2 ,a ,b)]
  [(_ a b c)     `(SCM_LIST3 ,a ,b ,c)]
  [(_ a b c d)   `(SCM_LIST4 ,a ,b ,c ,d)]
  [(_ a b c d e) `(SCM_LIST5 ,a ,b ,c ,d ,e)]
  [(_ xs ...)     (fold-right (cut list 'Scm_Cons <> <>) 'SCM_NIL xs)])

(define-cise-expr values
  [(_)           '("Scm_Values(SCM_NIL)")]
  [(_ a)         a]
  [(_ a b)       `(Scm_Values2 ,a ,b)]
  [(_ a b c)     `(Scm_Values3 ,a ,b ,c)]
  [(_ a b c d)   `(Scm_Values4 ,a ,b ,c ,d)]
  [(_ a b c d e) `(Scm_Values5 ,a ,b ,c ,d ,e)]
  [(_ x ...)     `(Scm_Values ,(fold (^[elt r] `(Scm_cons ,elt ,r)) '() x))]
  )
;; Using quote is a convenient way to embed Scheme constant in C code.
(define-cise-expr quote
  [(_ cst)
   (unless (cgen-current-unit)
     (error "cise: quote can't be used unless cgen-current-unit is set: '"
            cst))
   (list (cgen-cexpr (cgen-literal cst)))])

;;=============================================================
;; Other utilities
;;

;; type-decl-initial? and type-decl-subsequent? are used to determine if
;; (sym sym2 ...) is a type spec or ordinary expression.  The way
;; to render it differs depending on whether it is a type spec.
;; The reason that we have two predicates are that '*' and '&' can
;; appear in the operator position of a valid expression.
(define (type-decl-initial? sym)
  (or (memq sym '(const class enum struct volatile unsigned long
                  char short int float double .array .struct .union .function))
      (and (symbol? sym)
           (#/.[*&]$/ (symbol->string sym)))))

(define (type-decl-subsequent? sym)
  (or (memq sym '(* &))
      (type-decl-initial? sym)))

(define (cise-render-typed-var typespec var env)
  (match typespec
    [('.array spec (dim ...))
     `(,(cise-render-typed-var spec var env)
       ,@(map (^.['* "[]"]
                 [x `("[" ,(render-rec x (expr-env env)) "]")])
              dim))]
    [('.struct (fields ...) . rest)
     (render-struct-or-union "struct" #f fields rest var env)]
    [('.struct tag (fields ...) . rest)
     (render-struct-or-union "struct" tag fields rest var env)]
    [('.struct tag . rest)
     (render-struct-or-union "struct" tag #f rest var env)]
    [('.union (fields ...) . rest)
     (render-struct-or-union "union" #f fields rest var env)]
    [('.union tag (fields ...) . rest)
     (render-struct-or-union "union" tag fields rest var env)]
    [('.union tag . rest)
     (render-struct-or-union "union" tag #f rest var env)]
    [('.function (args ...) rettype . rest)
     (let1 rt (let1 vv (canonicalize-vardecl `(_ ,rettype))
                 (unless (null? (cdr vv))
                  (errorf "Invalid return type in ~s" typespec))
                (caddar vv))
       `(,(cise-render-typed-var rt "" env)
         "("
         ,(if (null? rest)
            (cise-render-identifier var)
            (cise-render-typed-var rest var env))
         ")"
         "("
         ,@($ intersperse ", "
              $ map (^.[(arg ':: type) (cise-render-typed-var type arg env)])
              $ canonicalize-vardecl args)
         ")"))]
    [(x)
     `(,(x->string x) " " ,(cise-render-identifier var))]
    [(x xs ...) 
     `(,(x->string x) " " ,(cise-render-typed-var xs var env))]
    [x
     `(,(x->string x) " " ,(cise-render-identifier var))]))

(define (render-struct-or-union struct/union tag fields rest var env)
  `(,struct/union
    ,@(cond-list [tag `(" " ,tag)]
                 [fields 
                  `(" { "
                    ,@(map (^.[(member ':: type)
                               `(,(cise-render-typed-var type member env)
                                 "; ")])
                           (canonicalize-vardecl fields))
                    "} ")]
                 [(not fields) '(" ")])
    ,(if (null? rest)
       (cise-render-identifier var)
       (cise-render-typed-var rest var env))))

(define (cise-render-identifier sym)
  (cgen-safe-name-friendly (x->string sym)))

;; Allow var::type as (var :: type)
;; and (var::type init) as (var :: type init)
(define (canonicalize-vardecl vardecls)
  (define (expand-type elt seed)
    (cond
     [(keyword? elt)  ;; The case of (var ::type)
      (rxmatch-case (keyword->string elt)
        [#/^:(.+)$/ (_ t) `(:: ,(string->symbol t) ,@seed)]
        [else (cons elt seed)])]
     [(symbol? elt)
      (rxmatch-case (symbol->string elt)
        [#/^(.+)::$/ (_ v) `(,(string->symbol v) :: ,@seed)]
        [#/^(.+)::(.+)$/ (_ v t)
            `(,(string->symbol v) :: ,(string->symbol t) ,@seed)]
        [else (cons elt seed)])]
     [else (cons elt seed)]))

  (define (err decl) (error "invalid variable declaration:" decl))

  (define (scan in r)
    (match in
      [() (reverse r)]
      [([? keyword? xx] . rest) (err xx)]
      [([? symbol? var] ':: type . rest)
       (scan rest `((,var :: ,type) ,@r))]
      [([? symbol? var] . rest)
       (scan rest `((,var :: ScmObj) ,@r))]
      [(([? symbol? v] [? symbol? t] . args) . rest)
       (scan rest `(,(expand-type v (expand-type t args)) ,@r))]
      [(([? symbol? vt] . args) . rest)
       (scan rest `(,(expand-type vt args) ,@r))]
      [(xx . rest) (err xx)]))

  (scan (fold-right expand-type '() vardecls) '()))

;; Like canonicalize-vardecl, but for argument declarations.
;; (foo::type bar baz:: type bee :: type)
;; => ((foo . type) (bar . ScmObj) (baz . type) (bee . type))
(define (canonicalize-argdecl argdecls)
  (define (rec args)
    (match (canonicalize-vardecl args)
      [() '()]
      [((var ':: type) . rest) `((,var . ,type) ,@(rec rest))]
      [(var . rest) `((,var . ScmObj) ,@(rec rest))]))
  (rec argdecls))

;;=============================================================
;; Sealing the default environment
;;   This must come at the bottom of the module.

(cise-ambient (cise-default-ambient))

