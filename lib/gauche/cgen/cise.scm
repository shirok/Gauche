;;;
;;; gauche.cgen.cise - C in S expression
;;;  
;;;   Copyright (c) 2004-2008  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: cise.scm,v 1.9 2008-05-10 13:35:57 shirok Exp $
;;;

(define-module gauche.cgen.cise
  (use srfi-1)
  (use gauche.sequence)
  (use gauche.parameter)
  (use gauche.cgen.unit)
  (use gauche.cgen.literal)
  (use util.match)
  (use util.list)
  (export cise-render
          cise-emit-source-line
          define-cise-macro
          define-cise-stmt
          define-cise-expr
          )
  )
(select-module gauche.cgen.cise)

;; NB: a small experiment to see how I feel this...
;;  [@ a b c d] => (ref (ref (ref a b) c) d)
;; In string interpolations I have to use ,(@ ...) instead of ,[@ ...], for
;; the previous versions of interpolation code doesn't like #`",[...]".
;; Ideally this should be a compiler-macro (we can't make it a macro,
;; for we want to say (set! [@ x'y] val).
(define @
  (getter-with-setter
   (case-lambda
     ((obj selector) (ref obj selector))
     ((obj selector . more) (apply @ (ref obj selector) more)))
   (case-lambda
     ((obj selector val) ((setter ref) obj selector val))
     ((obj selector selector2 . rest)
      (apply (setter ref) (ref obj selector) selector2 rest)))))
;; end experiment

;;=============================================================
;; Parameters
;;

;; If true, include #line directive in the output.
(define cise-emit-source-line (make-parameter #t))

;;=============================================================
;; Environment
;;

;; Environment must be treated opaque from outside of CISE module.

(define-class <cise-env> ()
  ((context :init-keyword :context :init-value 'stmt) ; stmt or expr
   (decls   :init-keyword :decls   :init-value '())   ; list of extra decls
   ))

(define (make-env context decls)
  (make <cise-env> :context context :decls decls))
(define (env-ctx env)   [@ env'context])
(define (env-decls env) [@ env'decls])
(define (expr-ctx? env) (eq? (env-ctx env) 'expr))
(define (stmt-ctx? env) (eq? (env-ctx env) 'stmt))

(define (null-env)      (make-env 'stmt '()))

(define (expr-env env)
  (if (expr-ctx? env) env (make-env 'expr (env-decls env))))
(define (stmt-env env)
  (if (stmt-ctx? env) env (make-env 'stmt (env-decls env))))

(define (ensure-stmt-ctx form env)
  (unless (stmt-ctx? env)
    (error "cise: statment appears in an expression context:" form)))

(define (env-decl-add! env decl)
  (push! [@ env'decls] decl))

(define (wrap-expr form env)
  (if (stmt-ctx? env) `(,form ";") form))

(define (render-env-decls env)
  (map (match-lambda
         ((var type) `(,(cise-render-type type)" ",var";")))
       (env-decls env)))

;; Check source-info attribute of the input S-expr, and returns Stree
;; of "#line" line if necessary.
(define (source-info form env)
  (if (not (cise-emit-source-line))
    '()
    (match (debug-source-info form)
      (((? string? file) line)
       `((source-info ,file ,line)))
      (_ '()))))
   
;;=============================================================
;; Expander
;;
;;  Cgen expander knows little about C.  It handles literals
;;  (strings, numbers, booleans, and characters) and function calls.
;;  All other stuff is handled by "cise macros"
;;
;;  TBD: Currently some cise macros need to call render-rec recursively
;;  to finish expansion.  We don't export render-rec, though, which
;;  limits the ability of 

(define *cgen-macro* (make-hash-table 'eq?))

(define-syntax define-cise-macro
  (syntax-rules ()
    [(_ (op form env) . body)
     (hash-table-put! *cgen-macro* 'op (lambda (form env) . body))]))

(define-syntax define-cise-stmt
  (syntax-rules ()
    [(_ "clauses" op clauses (:where defs ...))
     (define-cise-macro (op form env)
       defs ...
       (ensure-stmt-ctx form env)
       (match form . clauses))]
    [(_ "clauses" op clauses ())
     (define-cise-macro (op form env)
       (ensure-stmt-ctx form env)
       (match form . clauses))]
    [(_ "clauses" op (clause ...) (x . y))
     (define-cise-stmt "clauses" op (clause ... x) y)]
    [(_ op . clauses)
     (define-cise-stmt "clauses" op () clauses)]))

(define-syntax define-cise-expr
  (syntax-rules ()
    [(_ "clauses" op clauses (:where defs ...))
     (define-cise-macro (op form env)
       defs ...
       (let1 expanded (match form . clauses)
         (if (and (pair? expanded) (symbol? (car expanded)))
           (render-rec expanded env)
           (wrap-expr expanded env))))]
    [(_ "clauses" op clauses ())
     (define-cise-expr "clauses" op clauses (:where))]
    [(_ "clauses" op (clause ...) (x . y))
     (define-cise-expr "clauses" op (clause ... x) y)]
    [(_ op . clauses)
     (define-cise-expr "clauses" op () clauses)]))

;; cise-render cise &optional port as-expr?
(define (cise-render form . opts)
  (let-optionals* opts ((port (current-output-port))
                        (expr #f))
    (define current-file #f)
    (define current-line 1)
    (define (render-finish stree)
      (match stree
        [('source-info (? string? file) line)
         (cond ((and (equal? file current-file) (eqv? line current-line)))
               ((and (equal? file current-file) (eqv? line (+ 1 current-line)))
                (inc! current-line)
                (format port "\n"))
               (else
                (set! current-file file)
                (set! current-line line)
                (format port "\n#line ~a ~s\n" line file)))]
        [(x . y) (render-finish x) (render-finish y)]
        [(? (any-pred string? symbol? number?) x) (display x port)]
        [_ #f]))
    
    (let* ((env ((if expr expr-env identity) (null-env)))
           (stree (render-rec form env)))
      (render-finish `(,@(render-env-decls env) ,stree)))))

;; render-rec :: Cise, Env -> Stree
(define (render-rec form env)
  (match form
    [((? symbol? key) . args)
     (cond ((hash-table-get *cgen-macro* key #f)
            => (lambda (expander)
                 `(,@(source-info form env)
                   ,@(render-rec (expander form env) env))))
           (else
            (let1 eenv (expr-env env)
              (wrap-expr
               `(,@(source-info form env)
                 ,(cise-render-identifier key) "("
                 ,@(intersperse "," (map (cut render-rec <> eenv) args))
                 ")")
               env))))]
    [(x . y)     form]   ; already stree
    [(? symbol?) (wrap-expr (cise-render-identifier form) env)]
    [(? identifier?) (wrap-expr (cise-render-identifier (unwrap-syntax form))
                                env)]
    [(? string?) (wrap-expr (write-to-string form) env)]
    [(? real?)   (wrap-expr form env)]
    [()          '()]
    [#\'         (wrap-expr "'\\''"  env)]
    [#\\         (wrap-expr "'\\\\'" env)]
    [#\newline   (wrap-expr "'\\n'"  env)]
    [#\return    (wrap-expr "'\\r'"  env)]
    [#\tab       (wrap-expr "'\\t'"  env)]
    [(? char?)   (wrap-expr `("'" ,(if (char-set-contains? #[[:alnum:]] form)
                                     (string form)
                                     (format "\\x~2'0x" (char->integer form)))
                              "'") env)]
    [_           (error "Invalid CISE form: " form)]))

;;=============================================================
;; Built-in macros
;;

;;------------------------------------------------------------
;; Syntax
;;
(define-cise-macro (begin form env)
  (ensure-stmt-ctx form env)
  (match form
    [(_ . forms)
     `("{" ,@(map (cut render-rec <> env) forms) "}")]))

(define-cise-macro (let* form env)
  (ensure-stmt-ctx form env)
  (match form
    [(_ ((var . spec) ...) . body)
     (let1 eenv (expr-env env)
       `(begin ,@(map (lambda (var spec)
                        (receive (type has-init? init)
                            (match spec
                              [()         (values 'ScmObj #f #f)]
                              [(init)     (values 'ScmObj #t init)]
                              [(':: type) (values type #f #f)]
                              [(':: type init) (values type #t init)])
                          `(,(cise-render-type type)" "
                            ,(cise-render-identifier var)
                            ,@(cond-list (has-init?
                                          `("=",(render-rec init eenv))))
                            ";")))
                      var spec)
               ,@(map (cut render-rec <> env) body)))]
    ))

(define-cise-macro (if form env)
  (ensure-stmt-ctx form env)
  (let1 eenv (expr-env env)
    (match form
      [(_ test then)
       `("if (",(render-rec test eenv)")"
         ,(render-rec then env))]
      [(_ test then else)
       `("if (",(render-rec test eenv)")"
         ,(render-rec then env)" else " ,(render-rec else env))]
      )))

(define-cise-stmt when
  [(_ test . forms) `(if ,test (begin ,@forms))])

(define-cise-stmt unless
  [(_ test . forms) `(if (not ,test) (begin ,@forms))])

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

(define-cise-stmt loop
  [form `(for () ,@(cdr form))])

(define-cise-macro (while form env)
  (ensure-stmt-ctx form env)
  (let1 eenv (expr-env env)
    (match form
      [(_ test . body)
       `("while"
         ,(render-rec test eenv)
         ,(render-rec `(begin ,@body) env))])))

(define-cise-macro (for-each form env)
  (ensure-stmt-ctx form env)
  (let ((eenv (expr-env env))
        (tmp  (gensym "cise__")))
    (match form
      [(_ ('lambda (var) . body) list-expr)
       (env-decl-add! env `(,tmp ScmObj))
       `("SCM_FOR_EACH(" ,(cise-render-identifier tmp) ","
         ,(render-rec list-expr eenv) ") {"
         ,(render-rec `(let* ((,var :: ScmObj (SCM_CAR ,tmp)))
                         ,@body) env)
         "}")])))

(define-cise-macro (pair-for-each form env)
  (ensure-stmt-ctx form env)
  (let ((eenv (expr-env env)))
    (match form
      [(_ ('lambda (var) . body) list-expr)
       (env-decl-add! env `(,var ScmObj))
       `("SCM_FOR_EACH(" ,(cise-render-identifier var) ","
         ,(render-rec list-expr eenv) ")"
         ,(render-rec `(begin ,@body) env)
         )])))

(define-cise-macro (return form env)
  (ensure-stmt-ctx form env)
  (match form
    [(_ expr) `("return (" ,(render-rec expr (expr-env env)) ");")]))

(define-cise-stmt break
  [(_) '("break;")])

(define-cise-stmt continue
  [(_) '("continue;")])

(define-cise-stmt label
  [(_ name) `(,(cise-render-identifier name) " : ")])

(define-cise-stmt goto
  [(_ name) `("goto " ,(cise-render-identifier name) ";")])

(define-cise-macro (cond form env)
  (ensure-stmt-ctx form env)
  (let1 eenv (expr-env env)
    (define (a-clause test rest)
      `("(" ,(render-rec test eenv) ")" ,(render-rec `(begin ,@rest) env)))
    (match form
      [(_ (test . rest) ...)
       (fold-right (lambda (test rest r)
                     (cond
                      [(and (null? r) (eq? test 'else))
                       `(" else ",(render-rec `(begin ,@rest) env))]
                      [(eq? test (caadr form)) ; first form
                       `("if ",(a-clause test rest) ,@r)]
                      [else
                       `("else if" ,(a-clause test rest) ,@r)]))
                   '() test rest)]
      )))

(define (case-generator form env fallthrough?)
  (let1 eenv (expr-env env)
    (match form
      [(_ expr (literalss . clauses) ...)
       `("switch (",(render-rec expr eenv)") {"
         ,@(map (lambda (literals clause)
                  `(,@(source-info literals env)
                    ,@(if (eq? literals 'else)
                        '("default: ")
                        (map (lambda (literal) `("case ",literal" : "))
                             literals))
                    ,@(render-rec `(begin ,@clause
                                          ,@(if fallthrough? '() '((break))))
                                  env)))
                literalss clauses)
         "}")]
      )))    

(define-cise-macro (case form env)
  (ensure-stmt-ctx form env)
  (case-generator form env #f))

(define-cise-macro (case/fallthrough form env)
  (ensure-stmt-ctx form env)
  (case-generator form env #t))

;;------------------------------------------------------------
;; Operators
;;

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
(define-binary logior  "|")
(define-binary logxor  "^")
(define-binary logand  "&")
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
                 (intersperse ,sop (map cise-render-identifier b)))])
        env))))

(define-referencer ->  "->")
(define-referencer ref ".")

(define-cise-macro (aref form env)
  (let1 eenv (expr-env env)
    (wrap-expr
     (match form
       [(_ a b ...)
        (list "("(render-rec a eenv)")"
              (append-map (lambda (ind) `("[",(render-rec ind eenv)"]")) b))])
     env)))

(define-cise-macro (cast form env)
  (let1 eenv (expr-env env)
    (wrap-expr
     (match form
       [(_ type expr)
        `("((",type")(",(render-rec expr eenv)"))")])
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
               `((,(render-rec var eenv)"=(",(render-rec val eenv)")") ,@r))]
        [_   (error "uneven args for set!:" form)]))))

;;------------------------------------------------------------
;; Convenience expression macros
;;

;; Embed raw c code.  NB: I'm not sure yet about the name.  It is
;; desirable to be consistent with genstub (currently it uses (c <expr>))
;; I'll think it a bit more.
(define-cise-expr C:
  [(_ stuff) (list (x->string stuff))])

(define-cise-expr result
  [(_ e) `(set! SCM_RESULT ,e)]
  [(_ e0 e1) `(begin (set! SCM_RESULT0 ,e0) (set! SCM_RESULT1 ,e1))]
  )

(define-cise-expr list
  [(_)           '("SCM_NIL")]
  [(_ a)         `(SCM_LIST1 ,a)]
  [(_ a b)       `(SCM_LIST2 ,a ,b)]
  [(_ a b c)     `(SCM_LIST3 ,a ,b ,c)]
  [(_ a b c d)   `(SCM_LIST4 ,a ,b ,c ,d)]
  [(_ a b c d e) `(SCM_LIST5 ,a ,b ,c ,d ,e)]
  [(_ x ...)     (fold (lambda (elt r) `(Scm_Cons ,elt ,r)) '() x)])

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

(define (cise-render-type typespec)
  (x->string typespec))                 ;for the time being

(define (cise-render-identifier sym)
  (cgen-safe-name-friendly (x->string sym)))

(provide "gauche/cgen/cise")

