;;;
;;; compile-1.scm - The compiler: Pass 1
;;;
;;;   Copyright (c) 2004-2018  Shiro Kawai  <shiro@acm.org>
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

;;===============================================================
;; Pass 1
;;
;;   Converts S-expr to IForm.  Macros are expanded.  Variable references
;;   are resolved and converted to either $lref or $gref.  The constant
;;   variable references (defined by define-constant) are converted to
;;   its values at this stage.

;; Common entry to handle procedure call
;; proc is IForm.  args is [Sexpr].
(define-inline (pass1/call program proc args cenv)
  (cond
   [(has-tag? proc $LAMBDA)        ; immediate lambda
    (expand-inlined-procedure program proc (imap (cut pass1 <> cenv) args))]
   [(null? args) ($call program proc '())] ; fast path
   [(> (length args) MAX_LITERAL_ARG_COUNT)
    (errorf "Too many arguments in the call of `~,,,,40s...'" program)]
   [else (let1 cenv (cenv-sans-name cenv)
           ($call program proc (imap (cut pass1 <> cenv) args)))]))

;; Check if the head of the list is a variable, and if so, lookup it.
;; Note that we need to detect the case ((with-module foo bar) arg ...)
;; NB: This isn't a proper fix, for we cannot deal with the situation
;; like nested or aliased with-modules.  The Right Thing is to run
;; `pass1 for syntax' on (car PROGRAM) and check the result to see if
;; we need to treat PROGRAM as a special form or an ordinary procedure.
;; It would be a large change, so this is a compromise...
(define-inline (pass1/lookup-head head cenv)
  (or (and (variable? head)
           (cenv-lookup-syntax cenv head))
      (and (pair? head)
           (module-qualified-variable? head cenv)
           (let1 mod (ensure-module (cadr head) 'with-module #f)
             (cenv-lookup-syntax (cenv-swap-module cenv mod) (caddr head))))))

;;--------------------------------------------------------------
;; pass1 :: Sexpr, Cenv -> IForm
;;
;;  The Pass 1 entry point.
;;  This is one of the most frequently called routine.  It is critical to
;;  make sure all internal functions are inlined, in case you
;;  change something.
(define (pass1 program cenv)

  ;; Handle a global call.  PROGRAM's car is resolved to an identifier, ID.
  ;; We know PROGRAM is a call to global procedure, macro, or syntax.
  (define (pass1/global-call id)
    (receive (gval type) (global-call-type id cenv)
      (if gval
        (case type
          [(macro)  (pass1 (call-macro-expander gval program cenv) cenv)]
          [(syntax) (call-syntax-handler gval program cenv)]
          [(inline) (or (pass1/expand-inliner program id gval cenv)
                        (pass1/call program ($gref id) (cdr program) cenv))])
        (pass1/call program ($gref id) (cdr program) cenv))))

  ;; main body of pass1
  (cond
   [(pair? program)                    ; (op . args)
    (unless (list? program)
      (error "proper list required for function application or macro use:" program))
    (cond
     [(pass1/lookup-head (car program) cenv)
      => (^h (cond
              [(identifier? h) (pass1/global-call h)]
              [(lvar? h) (pass1/call program ($lref h) (cdr program) cenv)]
              [(macro? h) ;; local macro
               (pass1 (call-macro-expander h program cenv) cenv)]
              [(syntax? h);; locally rebound syntax
               (call-syntax-handler h program cenv)]
              [else (error "[internal] unknown resolution of head:" h)]))]
     [(pass1/detect-constant-setter-call (car program) cenv)
      => (^[setter]
           (or (pass1/expand-inliner program `(setter ,(car program))
                                     setter cenv)
               (and-let* ([info (~ setter'info)]
                          [binfo (pair-attribute-get info 'bind-info #f)]
                          [mod (find-module (car binfo))]
                          [name (cadr binfo)])
                 (pass1/call program
                             (pass1 (make-identifier name mod '()) cenv)
                             (cdr program) cenv))
               (pass1/call program (pass1 (car program) (cenv-sans-name cenv))
                           (cdr program) cenv)))]
     [else (pass1/call program (pass1 (car program) (cenv-sans-name cenv))
                       (cdr program) cenv)])]
   [(variable? program)                 ; variable reference
    (let1 r (cenv-lookup-variable cenv program)
      (cond [(lvar? r) ($lref r)]
            [(identifier? r)
             (or (and-let* ([const (find-const-binding r)]) ($const const))
                 ($gref r))]
            [else (error "[internal] cenv-lookup returned weird obj:" r)]))]
   [else ($const program)]))

;; If op is (setter <var>), check if <var> has inlinable binding and
;; its setter is locked; if so, returns the setter.  
;; There are a bunch of hoops to go through to satisfy the condition.
(define (pass1/detect-constant-setter-call op cenv)
  (and (pair? op) (pair? (cdr op)) (null? (cddr op))
       (not (vm-compiler-flag-is-set? SCM_COMPILE_NOINLINE_SETTERS))
       (global-identifier=? (car op) setter.)
       (and-let* ([var (cadr op)]
                  [ (variable? var) ]   ;ok, <var> is variable
                  [hd (pass1/lookup-head var cenv)]
                  [ (identifier? hd) ]
                  [gloc (id->bound-gloc hd)] ; <var> has inlinable binding
                  [val (gloc-ref gloc)]
                  [ (procedure? val) ])
         (procedure-locked-setter val)))) ;and has locked setter

;; Expand inlinable procedure.  Returns Maybe IForm
;; NAME is a variable, used for the error message.
;; PROC is <procedure>.
;; NB: This may return #f if inlining is abandoned.
(define (pass1/expand-inliner src name proc cenv)
  ;; TODO: for inline asm, check validity of opcode.
  (let ([inliner (%procedure-inliner proc)]
        [args (cdr src)])
    (match inliner
      [#f #f]                          ;no inliner, fallback case
      [(? integer?)                    ;VM insn
       (let ([nargs (length args)]
             [opt?  (slot-ref proc 'optional)])
         (unless (argcount-ok? args (slot-ref proc 'required) opt?)
           (errorf "wrong number of arguments: ~a requires ~a, but got ~a"
                   (if (variable? name) (variable-name name) name)
                   (slot-ref proc 'required) nargs))
         ;; We might get away with this limit by transforming inline calls
         ;; to apply or something.  Maybe in future.
         (when (> nargs MAX_LITERAL_ARG_COUNT)
           (errorf "Too many arguments in the call of `~,,,,40s...'" src))
         ($asm src (if opt? `(,inliner ,nargs) `(,inliner))
               (imap (cut pass1 <> cenv) args)))]
      [(? vector?)                     ;inlinable lambda
       (expand-inlined-procedure src
                                 (unpack-iform inliner)
                                 (imap (cut pass1 <> cenv) args))]
      [(? macro?)
       (let1 expanded (call-macro-expander inliner src cenv)
         (if (eq? src expanded)    ;no expansion
           #f
           (pass1 expanded cenv)))]
      [(? procedure?)
       ;; Call procedural inliner: Src, [IForm] -> IForm
       ;; The second arg is IForms of arguments.
       (let1 iform (inliner src (imap (cut pass1 <> cenv) args))
         (if (undefined? iform)         ;no expansion
           #f
           iform))]
      [_ (errorf "[internal] Invalid inliner attached to ~s: ~s"
                 proc inliner)]
      )))

;; Returns #t iff exp is the form (with-module module VARIABLE)
;; We need to check the global value of with-module, for it might
;; be redefined.  We assume this function is called infrequently,
;; thus we can afford the time.
(define (module-qualified-variable? expr cenv)
  (match expr
    [((? variable? wm) mod (? variable? v))
     (and-let* ([var (cenv-lookup-syntax cenv wm)]
                [ (identifier? var) ])
       (global-identifier=? var with-module.))]
    [_ #f]))

;;--------------------------------------------------------------
;; pass1/body - Compiling body with internal definitions.
;;
;; For the letrec* semantics, we need to build the internal frame
;; as we go though the body, since internal macro definition need to
;; capture the internal environment.
;;
;;    (let ((x 1))
;;      (define x 2)
;;      (define-syntax foo
;;        (syntax-rules ()
;;          [(_ v) (define v x)]))   ;; must refer to inner x
;;      (foo y)
;;      y)   => 2
;;
;; To avoid unnecessary allocation, we adopt somewhat convoluted strategy
;; that delays frame allocation until needed, and once allocated, we
;; "grow" the frame as new definition appears.  This is an exception of
;; the general principle that cenv is immutable.

;; pass1/body :: [Sexpr], Cenv -> IForm
(define (pass1/body exprs cenv)
  ;; First, we pair up each expr with dummy source info '().  Some of expr
  ;; may be an 'include' form and expanded into the content of the file,
  ;; in which case we keep the source file info in each cdr of the pair.
  (pass1/body-rec (map list exprs) #f #f cenv))

;; If we find internal (syntax) definition, we extend cenv with
;; two frames, one for local macros and one for local variables,
;; so that the internal macros can capture the correct scope of
;; identifiers.
;; For the local variables, we insert dummy binding
;;    (<name> :rec <init-expr> . <src>)
;; as the placeholder.  Once we find the boundary of definitions and
;; expressions, we re-evaluate <init-expr> and replace the frame entry with
;;    (<name> . <lvar>)
(define (pass1/body-rec exprs mframe vframe cenv)
  (match exprs
    [(((op . args) . src) . rest)
     (or (and-let* ([ (or (not vframe) (not (assq op vframe))) ]
                    [head (pass1/lookup-head op cenv)])
           (unless (list? args)
             (error "proper list required for function application \
                     or macro use:" (caar exprs)))
           (cond
            [(lvar? head) (pass1/body-finish exprs mframe vframe cenv)]
            [(macro? head)  ; locally defined macro
             (pass1/body-macro-expand-rec head exprs mframe vframe cenv)]
            [(syntax? head) ; when (let-syntax ((xif if)) (xif ...)) etc.
             (pass1/body-finish exprs mframe vframe cenv)]
            [(and (pair? head) (eq? (car head) :rec))
             (pass1/body-finish exprs mframe vframe cenv)]
            [(not (identifier? head)) (error "[internal] pass1/body" head)]
            [(or (global-identifier=? head define.)
                 (global-identifier=? head define-inline.)
                 (global-identifier=? head r5rs-define.))
             (let1 def (match args
                         [((name . formals) . body)
                          `(,name :rec (,lambda. ,formals ,@body) . ,src)]
                         [(var init) `(,var :rec ,init . ,src)]
                         [_ (error "malformed internal define:" (caar exprs))])
               (if (not mframe)
                 (let* ([cenv (cenv-extend cenv '() SYNTAX)]
                        [mframe (car (cenv-frames cenv))]
                        [cenv (cenv-extend cenv `(,def) LEXICAL)]
                        [vframe (car (cenv-frames cenv))])
                   (pass1/body-rec rest mframe vframe cenv))
                 (begin
                   (push! (cdr vframe) def)
                   (pass1/body-rec rest mframe vframe cenv))))]
            [(global-identifier=? head define-syntax.) ; internal syntax definition
             (match args
               [(name trans-spec)
                (if (not mframe)
                 (let* ([cenv (cenv-extend cenv `((,name)) SYNTAX)]
                        [mframe (car (cenv-frames cenv))]
                        [cenv (cenv-extend cenv `() LEXICAL)]
                        [vframe (car (cenv-frames cenv))]
                        [trans (pass1/eval-macro-rhs
                                'define-syntax trans-spec
                                (cenv-add-name cenv (variable-name name)))])
                   (assq-set! (cdr mframe) name trans)
                   (pass1/body-rec rest mframe vframe cenv))
                 (begin
                   (push! (cdr mframe) `(,name))
                   (let1 trans (pass1/eval-macro-rhs
                                'define-syntax trans-spec
                                (cenv-add-name cenv (variable-name name)))
                     (assq-set! (cdr mframe) name trans)
                     (pass1/body-rec rest mframe vframe cenv))))]
               [_ (error "syntax-error: malformed internal define-syntax:"
                         `(,op ,@args))])]
            [(global-identifier=? head begin.) ;intersperse forms
             (pass1/body-rec (append (imap (cut cons <> src) args) rest)
                             mframe vframe cenv)]
            [(global-identifier=? head include.)
             (let1 sexpr&srcs (pass1/expand-include args cenv #f)
               (pass1/body-rec (append sexpr&srcs rest) mframe vframe cenv))]
            [(global-identifier=? head include-ci.)
             (let1 sexpr&srcs (pass1/expand-include args cenv #t)
               (pass1/body-rec (append sexpr&srcs rest) mframe vframe cenv))]
            [(identifier? head)
             (or (and-let* ([gloc (id->bound-gloc head)]
                            [gval (gloc-ref gloc)]
                            [ (macro? gval) ])
                   (pass1/body-macro-expand-rec gval exprs mframe vframe cenv))
                 (pass1/body-finish exprs mframe vframe cenv))]
            [else (error "[internal] pass1/body" head)]))
         (pass1/body-finish exprs mframe vframe cenv))]
    [_ (pass1/body-finish exprs mframe vframe cenv)]))

(define (pass1/body-macro-expand-rec mac exprs mframe vframe cenv)
  (pass1/body-rec
   (acons (call-macro-expander mac (caar exprs) cenv)
          (cdar exprs) ;src
          (cdr exprs)) ;rest
   mframe vframe cenv))

;; Finishing internal definitions.  If we have internal defs, we wrap
;; the rest by letrec.
(define (pass1/body-finish exprs mframe vframe cenv)
  (if (not mframe)
    (pass1/body-rest exprs cenv)
    ;; Replace dummy bindings to the real one
    (let* ([intdefs. (reverse (cdr vframe))]
           [vars  (map car intdefs.)]
           [lvars (imap make-lvar+ vars)])
      (set-cdr! vframe (%map-cons vars lvars))
      ($let #f 'rec* lvars
            (imap2 (cut pass1/body-init <> <> cenv) lvars (map cddr intdefs.))
            (pass1/body-rest exprs cenv)))))

(define (pass1/body-init lvar init&src newenv)
  (let1 e (if (null? (cdr init&src))
            (cenv-add-name newenv (lvar-name lvar))
            (cenv-add-name/source newenv (lvar-name lvar) (cdr init&src)))
    (rlet1 iexpr (pass1 (car init&src) e)
      (lvar-initval-set! lvar iexpr))))

(define (pass1/body-rest exprs cenv)
  (match exprs
    [() ($seq '())]
    [(expr&src) (pass1/body-1 expr&src cenv)]
    [_ (let1 stmtenv (cenv-sans-name cenv)
         ($seq (let loop ([exprs exprs] [r '()])
                 (if (null? (cdr exprs))
                   (reverse (cons (pass1/body-1 (car exprs) cenv) r))
                   (loop (cdr exprs)
                         (cons (pass1/body-1 (car exprs) stmtenv) r))))))]))

(define (pass1/body-1 expr&src cenv)
  (let1 src (cdr expr&src)
    (if (string? src)
      (pass1 (car expr&src) (cenv-swap-source cenv src))
      (pass1 (car expr&src) cenv))))

;;--------------------------------------------------------------
;; Pass1 utilities
;;

;; get symbol or id, and returns identiier.
(define (ensure-identifier sym-or-id cenv)
  (if (identifier? sym-or-id)
    sym-or-id
    (make-identifier sym-or-id (cenv-module cenv) (cenv-frames cenv))))

;; Does the given argument list satisfy procedure's reqargs/optarg?
(define (argcount-ok? args reqargs optarg?)
  (let1 nargs (length args)
    (or (and (not optarg?) (= nargs reqargs))
        (and optarg? (>= nargs reqargs)))))

;; signal an error if the form is not on the toplevel
(define-inline (check-toplevel form cenv)
  (unless (cenv-toplevel? cenv)
    (error "syntax-error: the form can appear only in the toplevel:" form)))

;; returns a module specified by THING.
(define (ensure-module thing name create?)
  (let1 mod (cond [(symbol? thing) (find-module thing)]
                  [(identifier? thing) (find-module (unwrap-syntax thing))]
                  [(module? thing) thing]
                  [else
                   (errorf "~a requires a module name or a module, but got: ~s"
                           name thing)])
    (or mod
        (if create?
          (make-module (if (identifier? thing) (unwrap-syntax thing) thing))
          (errorf "~a: no such module: ~s" name thing)))))

;; IFORM must be a $LAMBDA node.  This expands the application of IFORM
;; on IARGS (list of IForm) into a mere $LET node.
;; The nodes within IFORM will be reused in the resulting $LET structure,
;; so be careful not to share substructures of IFORM accidentally.
(define (expand-inlined-procedure src iform iargs)
  (let ([lvars ($lambda-lvars iform)]
        [args  (adjust-arglist ($lambda-reqargs iform) ($lambda-optarg iform)
                               iargs ($lambda-name iform))])
    (for-each (^[lv a] (lvar-initval-set! lv a)) lvars args)
    ($let src 'let lvars args ($lambda-body iform))))

;; Adjust argument list according to reqargs and optarg count.
;; Used in procedure inlining and local call optimization.
(define (adjust-arglist reqargs optarg iargs name)
  (unless (argcount-ok? iargs reqargs (> optarg 0))
    (errorf "wrong number of arguments: ~a requires ~a, but got ~a"
            name reqargs (length iargs)))
  (if (zero? optarg)
    iargs
    (receive (reqs opts) (split-at iargs reqargs)
      (append! reqs (list (if (null? opts) ($const '()) ($list #f opts)))))))

;;----------------------------------------------------------------
;; Pass1 syntaxes
;;

(define-macro (define-pass1-syntax formals module . body)
  (let ([mod (ecase module
               [(:null) 'null]
               [(:gauche) 'gauche]
               [(:internal) 'gauche.internal])]
        ;; a trick to assign comprehensive name to body:
        [name (string->symbol #"syntax/~(car formals)")])
    `(let ((,name (^ ,(cdr formals) ,@body)))
       (%insert-syntax-binding (find-module ',mod) ',(car formals)
                               (make-syntax ',(car formals) ,name)))))

(define (global-id id) (make-identifier id (find-module 'gauche) '()))
(define (global-id% id) (make-identifier id (find-module 'gauche.internal) '()))

(define define.         (global-id 'define))
(define define-inline.  (global-id 'define-inline))
(define define-syntax.  (global-id 'define-syntax))
(define lambda.         (global-id 'lambda))
(define r5rs-define.    (make-identifier 'define (find-module 'null) '()))
(define r5rs-lambda.    (make-identifier 'lambda (find-module 'null) '()))
(define setter.         (global-id 'setter))
(define lazy.           (global-id 'lazy))
(define eager.          (global-id 'eager))
(define values.         (global-id 'values))
(define begin.          (global-id 'begin))
(define include.        (global-id 'include))
(define include-ci.     (global-id 'include-ci))
(define else.           (global-id 'else))
(define =>.             (global-id '=>))
(define current-module. (global-id 'current-module))
(define with-module.    (global-id 'with-module))
(define quasiquote.     (global-id 'quasiquote))
(define unquote.        (global-id 'unquote))
(define unquote-splicing. (global-id 'unquote-splicing))

(define %make-er-transformer.          (global-id% '%make-er-transformer))
(define %make-er-transformer/toplevel. (global-id% '%make-er-transformer/toplevel))
(define %with-inline-transformer.      (global-id% '%with-inline-transformer))

;; Returns an IForm for (values) - useful for define-pass1-syntax that does
;; compile-time things and returns nothing.  The delay trick is to create
;; iform only once.  The module to call pass1 doesn't matter, for we directly
;; use identifier for gauche#values.
(define $values0 (let1 iform (delay (pass1 `(,values.) (make-bottom-cenv)))
                   (^[] (force iform))))

;; Definitions ........................................

;; Note on constant binding and inlinable binding:
;;   define-constant and define-inline both create a binding that
;;   is not supposed to be altered, but they have slightly different
;;   semantics.   Define-constant binds a global variable to a value that
;;   is computable at compile time, and serializable to a precompiled
;;   file.  When the compiler sees a global variable reference with
;;   a constant binding, it replaces the reference to the value itself
;;   at pass 1.  Define-inline can bind a variable to a value that is
;;   calculated at runtime.  The compiler does not replace the variable
;;   references with values, but it freely rearranges the references within
;;   the source code.  If an inlinable binding is used at the head position,
;;   the compiler looks at its value, and if it is known to be bound to
;;   an inlinable procedure, the procedure's body is inlined.

(define-pass1-syntax (define form cenv) :null
  (pass1/define form form '() #f (cenv-module cenv) cenv))

(define-pass1-syntax (define form cenv) :gauche
  (pass1/define form form '() #t (cenv-module cenv) cenv))

(define-pass1-syntax (define-constant form cenv) :gauche
  (pass1/define form form '(const) #t (cenv-module cenv) cenv))

(define-pass1-syntax (define-in-module form cenv) :gauche
  (match form
    [(_ module . rest)
     (pass1/define `(_ . ,rest) form '() #t
                   (ensure-module module 'define-in-module #f)
                   cenv)]
    [_ (error "syntax-error: malformed define-in-module:" form)]))

(define (pass1/define form oform flags extended? module cenv)
  (check-toplevel oform cenv)
  (match form
    [(_ (name . args) body ...)
     (pass1/define `(define ,name
                      ,(with-original-source
                        `(,(if extended? lambda. r5rs-lambda.) ,args ,@body)
                        oform))
                   oform flags extended? module cenv)]
    [(_ name expr)
     (unless (variable? name) (error "syntax-error:" oform))
     (let1 cenv (cenv-add-name cenv (variable-name name))
       ;; If NAME is an identifier, it is inserted by macro expander; we
       ;; can't simply place it in $define, since it would insert a toplevel
       ;; definition into the toplevel of macro-definition environment---
       ;; we don't want a mere macro call would modify different module.
       ;; We rename it to uninterned symbol, so, even the binding itself
       ;; is into the macro-definiting module, it won't be visible from
       ;; other code except the code generated in the same macro expansion.
       ;; A trick - we directly modify the identifier, so that other forms
       ;; referring to the same (eq?) identifier can keep referring it.
       (let1 id (if (identifier? name)
                  (%rename-toplevel-identifier! name)
                  (make-identifier name module '()))
         ($define oform flags id (pass1 expr cenv))))]
    [_ (error "syntax-error:" oform)]))

(define (%rename-toplevel-identifier! identifier)
  (slot-set! identifier 'name (gensym #"~(identifier->symbol identifier)."))
  identifier)

;; Inlinable procedure.
;;   Inlinable procedure has both properties of a macro and a procedure.
;;   It is a bit tricky since the inliner information has to exist
;;   both in compile time and execution time.
;;
;;   Processing define-inline involves two actions.
;;   (1) Process the lambda node to be inlined.  A packed IForm should be
;;       attached, and if the lambda node closes enviornment, some code
;;       transformation is required.
;;   (2) Bind the resulting node to the global name, and mark the binding
;;       'inlinable'.
;;
;;   These two are functionally orthogonal.  Especially, not all expressions
;;   can yield inlinable lambda node as (1).   However, to make procedure
;;   inlining work effectively, both of these actions are required; that's
;;   why we process them together.
;;
;;   Steps:
;;   1. Canonicalize the form to (define-inline NAME EXPR).
;;   2. See if EXPR ultimately returns $LAMBDA node.  If so, does the
;;      node closes local environment?  (pass1/check-inlinable-lambda)
;;   3. If EXPR does not yield a closure, we just create 'inlinable'
;;      binding (by pass1/make-inlinable-binding)
;;      but do not do anything further.
;;   4. If EXPR directly yields a closure without an environment,
;;      process the closure (pass1/mark-closure-inlinable!) and
;;      then make inlinable binding.
;;   5. If EXPR creates a local environment, we have to transform
;;      the closed variables into global variables.   See the comment
;;      of subst-lvars above for the details of transformation.
;;

(define-pass1-syntax (define-inline form cenv) :gauche
  (check-toplevel form cenv)
  (match form
    [(_ (name . args) . body)
     (pass1/define-inline form name `(,lambda. ,args ,@body) cenv)]
    [(_ name expr)
     (unless (variable? name) (error "syntax-error:" form))
     (pass1/define-inline form name expr cenv)]
    [_ (error "syntax-error: malformed define-inline:" form)]))

(define (pass1/define-inline form name expr cenv)
  (let1 iform (pass1 expr (cenv-add-name cenv (variable-name name)))
    (receive (closure closed) (pass1/check-inlinable-lambda iform)
      (cond
       [(and (not closure) (not closed)) ; too complex to inline
        (pass1/make-inlinable-binding form name iform cenv)]
       [(null? closed)               ; no closed env
        (pass1/mark-closure-inlinable! closure name cenv)
        (pass1/make-inlinable-binding form name closure cenv)]
       [else ; inlinable lambda has closed env.
        ;; See the comment in subst-lvars above on the transformation.
        ;; closed :: [(lvar . init-iform)]
        ;; gvars :: [(identifier . iform)]
        ;; subs :: [(lvar . iform)]  ; iform being $const or $gref
        (receive (gvars subs)
            (pass1/define-inline-classify-env name closed cenv)
          (let1 defs (pass1/define-inline-gen-closed-env gvars cenv)
            ($lambda-body-set! closure
                               (subst-lvars ($lambda-body closure) subs))
            (pass1/mark-closure-inlinable! closure name cenv)
            ($seq `(,@defs
                     ,(pass1/make-inlinable-binding form name closure cenv)))))]
       ))))

;; If IFORM generats a closure with local environment, returns
;; the closure itself ($lambda node) and the environment
;; ((lvar . init) ...).
;; Typical case is ($let ... ($lambda ...)).  In such case this
;; procedure effectively strips $let nodes.
;; If IFORM has more complicated structure, we just return (values #f #f)
;; to give up inlining.
(define (pass1/check-inlinable-lambda iform)
  (cond [(has-tag? iform $LAMBDA) (values iform '())]
        [(has-tag? iform $LET)
         (receive (closure closed)
             (pass1/check-inlinable-lambda ($let-body iform))
           (if (and (not closure) (not closed))
             (values #f #f) ; giveup
             (let loop ([lvars (reverse ($let-lvars iform))]
                        [inits (reverse ($let-inits iform))]
                        [closed closed])
               (if (null? lvars)
                 (values closure closed)
                 (loop (cdr lvars) (cdr inits)
                       (acons (car lvars) (car inits) closed))))))]
        [else (values #f #f)]))

(define (pass1/define-inline-classify-env name lv&inits cenv)
  (define gvars '())
  (define subs '())
  (let loop ([lv&inits lv&inits])
    (match lv&inits
      [() (values (reverse gvars) (reverse subs))]
      [((and (lv . (? $const?)) p) . lv&inits)
       (push! subs p) (loop lv&inits)]
      [((lv . init) . lv&inits)
       (let1 gvar (make-identifier (gensym #"~|name|$~(lvar-name lv).")
                                   (cenv-module cenv) '())
         (push! subs `(,lv . ,($gref gvar)))
         (push! gvars `(,gvar . ,(subst-lvars init subs)))
         (loop lv&inits))])))

;; gvars :: [(identifier . iform)]
(define (pass1/define-inline-gen-closed-env gvars cenv)
  (imap (^[gv] ($define #f '(inlinable) (car gv) (cdr gv))) gvars))

;; set up $LAMBDA node (closure) to be inlinable.  If NAME is given,
;; this also inserts the binding to the current compiling environment
;; so that inlining is effective for the rest of the compilation.
(define (pass1/mark-closure-inlinable! closure name cenv)
  (let* ([module  (cenv-module cenv)]
         ;; Dummy-proc is only a placeholder to record the inliner info
         ;; to be used during compilation of the current compiler unit.
         ;; Its body doesn't matter, but we need to make sure every dummy-proc
         ;; is a different instance.  If we make it a constant procedure,
         ;; Gauche's compiler optimizes it to refer to the singleton instance.
         [dummy-proc (^ _ name)]
         [packed (pack-iform closure)])
    ($lambda-flag-set! closure packed)
    (when name
      ;; record inliner function for compiler.  this is used only when
      ;; the procedure needs to be inlined in the same compiler unit.
      (%insert-binding module (unwrap-syntax name) dummy-proc)
      (set! (%procedure-inliner dummy-proc) (pass1/inliner-procedure packed)))))

(define (pass1/make-inlinable-binding form name iform cenv)
  ;; See the comment in pass1/define about renaming the toplevel identifier.
  (let1 id (if (identifier? name)
             (%rename-toplevel-identifier! name)
             (make-identifier name (cenv-module cenv) '()))
    ($define form '(inlinable) id iform)))

(define (pass1/inliner-procedure inline-info)
  (unless (vector? inline-info)
    (error "[internal] pass1/inliner-procedure got invalid info" inline-info))
  (^[form cenv]
    (expand-inlined-procedure form (unpack-iform inline-info)
                              (imap (cut pass1 <> cenv) (cdr form)))))

;; Toplevel macro definitions

(define-pass1-syntax (define-macro form cenv) :gauche
  (check-toplevel form cenv)
  (match form
    [(_ (name . formals) body ...)
     (pass1/define-macro form name `(,lambda. ,formals ,@body) cenv)]
    [(_ name expr)
     (pass1/define-macro form name expr cenv)]
    [_ (error "syntax-error:" form)]))

(define (pass1/define-macro src name expr cenv)
  (unless (variable? name) (error "syntax-error:" src))
  ;; TODO: macro autoload
  (let* ([proc (eval expr (cenv-module cenv))]
         [trans (%make-macro-transformer name
                                         (^[form env] (apply proc (cdr form)))
                                         expr #f)])
    ;; See the "Hygiene alert" in pass1/define.
    (%insert-syntax-binding (cenv-module cenv) (unwrap-syntax name) trans)
    ($const-undef)))

(define-pass1-syntax (define-syntax form cenv) :null
  (check-toplevel form cenv)
  ;; Temporary: we use the old compiler's syntax-rules implementation
  ;; for the time being.
  (match form
    [(_ name expr)
     (let* ([cenv (cenv-add-name cenv (variable-name name))]
            [transformer (pass1/eval-macro-rhs 'define-syntax expr cenv)])
       ;; See the "Hygiene alert" in pass1/define.
       (%insert-syntax-binding (cenv-module cenv) (unwrap-syntax name)
                               transformer)
       ($const-undef))]
    [_ (error "syntax-error: malformed define-syntax:" form)]))

;; Experimental
(define-pass1-syntax (define-inline/syntax form cenv) :gauche
  (check-toplevel form cenv)
  (match form
    [(_ name expr macro-expr)
     (let* ([cenv (cenv-add-name cenv (variable-name name))]
            [xformer (pass1/eval-macro-rhs 'define-inline/syntax
                                           macro-expr cenv)]
            [body (pass1/call expr ($gref %with-inline-transformer.)
                              (list expr xformer) cenv)])
       (pass1/make-inlinable-binding form name body cenv))]
    [_ (error "syntax-error: define-inline/syntax")]))
            
;; Returns either <syntax> or <macro>
(define (pass1/eval-macro-rhs who expr cenv)
  (rlet1 transformer ((make-toplevel-closure (compile expr cenv)))
    (unless (or (is-a? transformer <syntax>)
                (is-a? transformer <macro>))
      (errorf "syntax-error: rhs expression of ~a ~s \
               doesn't yield a syntactic transformer: ~s"
              who expr transformer))))

(inline-stub
 (define-cproc make-toplevel-closure (code::<compiled-code>)
  (return (Scm_MakeClosure (SCM_OBJ code) NULL)))
 )

;; Macros ...........................................

(define-pass1-syntax (er-macro-transformer form cenv) :gauche
  (match form
    [(_ xformer)
     ;; We need to capture the current CENV as the macro definition
     ;; environment.  There's a catch, though---if we're AOT compiling
     ;; a macro, the captured CENV must be serializable to a file,
     ;; which isn't generally the case.
     ;; So, if we're compiling toplevel, we call a special API that takes
     ;; the current module and cenv-exp-name, and reconstruct the cenv
     ;; at runtime.
     ;; If cenv has local environment, we don't bother that, for the macro
     ;; will be fully expanded during AOT compilation.  HOWEVER - we can't
     ;; embed cenv as a vector literal (e.g. `',cenv) since quoting will
     ;; strip all identifier information in cenv.  The right thing would be
     ;; to make cenv as a record.  For now, we take advantage that unquoted
     ;; vector evaluates to itself, and insert cenv without quoting.  This
     ;; has to change if we prohibit unquoted vector literals.
     (if (cenv-toplevel? cenv)
       (pass1 `(,%make-er-transformer/toplevel. ,xformer
                                                ,(cenv-module cenv)
                                                ',(cenv-exp-name cenv))
              cenv)
       (pass1 `(,%make-er-transformer. ,xformer ,cenv) cenv))]
    [_ (error "syntax-error: malformed er-macro-transformer:" form)]))

(define-pass1-syntax (%macroexpand form cenv) :gauche
  (match form
    [(_ expr) ($const (%internal-macro-expand expr cenv #f))]
    [_ (error "syntax-error: malformed %macroexpand:" form)]))

(define-pass1-syntax (%macroexpand-1 form cenv) :gauche
  (match form
    [(_ expr) ($const (%internal-macro-expand expr cenv #t))]
    [_ (error "syntax-error: malformed %macroexpand-1:" form)]))

(define (%internal-macro-expand expr cenv once?)
  (define (xpand expr)
    (match expr
      [((? variable? op) . args)
       (let1 var (cenv-lookup-syntax cenv op)
         (cond [(macro? var) (call-macro-expander var expr cenv)]
               [(identifier? var)
                (if-let1 gval (and-let* ([gloc (id->bound-gloc var)]
                                         [gval (gloc-ref gloc)]
                                         [ (macro? gval) ])
                                gval)
                  (call-macro-expander gval expr cenv)
                  expr)]
               [else expr]))]
      [((? macro? op) . args) (call-macro-expander op expr cenv)]
      [_ expr]))
  (if once?
    (xpand expr)
    (let loop ([expr expr])
      (let1 e2 (xpand expr)
        (if (eq? e2 expr)
          expr
          (loop e2))))))

(define-pass1-syntax (... form cenv) :null
  (error "invalid syntax:" form))

(define-pass1-syntax (let-syntax form cenv) :null
  (match form
    [(_ ((name trans-spec) ...) body ...)
     (let* ([trans (map (^[n x] (pass1/eval-macro-rhs
                                 'let-syntax x
                                 (cenv-add-name cenv (variable-name n))))
                        name trans-spec)]
            [newenv (cenv-extend cenv (%map-cons name trans) SYNTAX)])
       (pass1/body body newenv))]
    [_ (error "syntax-error: malformed let-syntax:" form)]))

(define-pass1-syntax (letrec-syntax form cenv) :null
  (match form
    [(_ ((name trans-spec) ...) body ...)
     (let* ([newenv (cenv-extend cenv (%map-cons name trans-spec) SYNTAX)]
            [trans (map (^[n x] (pass1/eval-macro-rhs
                                 'letrec-syntax x
                                 (cenv-add-name newenv (variable-name n))))
                        name trans-spec)])
       (for-each set-cdr! (cdar (cenv-frames newenv)) trans)
       (pass1/body body newenv))]
    [_ (error "syntax-error: malformed letrec-syntax:" form)]))

(define-pass1-syntax (syntax-rules form cenv) :null
  (match form
    [(_ (literal ...) rule ...)
     ($const (compile-syntax-rules (cenv-exp-name cenv) form #t literal rule
                                   (cenv-module cenv)
                                   (cenv-frames cenv)))]
    [(_ (? variable-or-keyword? elli) (literal ...) rule ...)
     ;; NB: We allow keyword for ellipsis, so that something like ::: can be
     ;; used.
     ($const (compile-syntax-rules (cenv-exp-name cenv) form elli literal rule
                                   (cenv-module cenv)
                                   (cenv-frames cenv)))]
    [_ (error "syntax-error: malformed syntax-rules:" form)]))

;; If family ........................................

(define-pass1-syntax (if form cenv) :null
  (match form
    [(_ test then else)
     ($if form (pass1 test (cenv-sans-name cenv))
          (pass1 then cenv) (pass1 else cenv))]
    [(_ test then)
     ($if form (pass1 test (cenv-sans-name cenv))
          (pass1 then cenv) ($const-undef))]
    [_ (error "syntax-error: malformed if:" form)]))

(define-pass1-syntax (and form cenv) :null
  (define (rec exprs)
    (match exprs
      [() `#(,$CONST #t)]
      [(expr) (pass1 expr cenv)]
      [(expr . more)
       ($if #f (pass1 expr (cenv-sans-name cenv)) (rec more) ($it))]
      [_ (error "syntax-error: malformed and:" form)]))
  (rec (cdr form)))

(define-pass1-syntax (or form cenv) :null
  (define (rec exprs)
    (match exprs
      [() ($const-f)]
      [(expr) (pass1 expr cenv)]
      [(expr . more)
       ($if #f (pass1 expr (cenv-sans-name cenv)) ($it) (rec more))]
      [_ (error "syntax-error: malformed or:" form)]))
  (rec (cdr form)))

(define-pass1-syntax (when form cenv) :gauche
  (match form
    [(_ test expr1 expr2 ...)
     (let1 cenv (cenv-sans-name cenv)
       ($if form (pass1 test cenv)
            ($seq (imap (cut pass1 <> cenv) (cons expr1 expr2)))
            ($const-undef)))]
    [_ (error "syntax-error: malformed when:" form)]))

(define-pass1-syntax (unless form cenv) :gauche
  (match form
    [(_ test expr1 expr2 ...)
     (let1 cenv (cenv-sans-name cenv)
       ($if form (pass1 test cenv)
            ($const-undef)
            ($seq (imap (cut pass1 <> cenv) (cons expr1 expr2)))))]
    [_ (error "syntax-error: malformed unless:" form)]))

(define-pass1-syntax (else form cenv) :null
  (error "invalid syntax:" form))
(define-pass1-syntax (=> form cenv) :null
  (error "invalid syntax:" form))

(define-pass1-syntax (cond form cenv) :null
  (define (process-clauses cls)
    (match cls
      [() ($const-undef)]
      ;; (else . exprs)
      [(([? (cut global-eq? <> else. cenv)] exprs ...) . rest)
       (unless (null? rest)
         (error "syntax-error: 'else' clause followed by more clauses:" form))
       ($seq (imap (cut pass1 <> cenv) exprs))]
      ;; (test => proc)
      [((test [? (cut global-eq? <> =>. cenv)] proc) . rest)
       (let ([test (pass1 test cenv)]
             [tmp (make-lvar 'tmp)])
         (lvar-initval-set! tmp test)
         ($let (car cls) 'let
               (list tmp)
               (list test)
               ($if (car cls)
                    ($lref tmp)
                    ($call (car cls)
                           (pass1 proc (cenv-sans-name cenv))
                           (list ($lref tmp)))
                    (process-clauses rest))))]
      ;; (generator guard => proc) -- SRFI-61 'general cond clause'
      [((generator guard [? (cut global-eq? <> =>. cenv)] receiver) . rest)
       (let1 tmp (make-lvar 'tmp)
         ($receive (car cls) 0 1 (list tmp)
                   (pass1 generator cenv)
                   ($if (car cls)
                        ($asm #f
                              `(,APPLY 2)
                              (list (pass1 guard (cenv-sans-name cenv))
                                    ($lref tmp)))
                        ($asm #f
                              `(,APPLY 2)
                              (list (pass1 receiver (cenv-sans-name cenv))
                                    ($lref tmp)))
                        (process-clauses rest))))]
      [((test) . rest)                  ; (test)
       ($if (car cls) (pass1 test (cenv-sans-name cenv))
            ($it)
            (process-clauses rest))]
      [((test exprs ...) . rest)          ; (test . exprs)
       ($if (car cls) (pass1 test (cenv-sans-name cenv))
            ($seq (imap (cut pass1 <> cenv) exprs))
            (process-clauses rest))]
      [_ (error "syntax-error: bad clause in cond:" form)]))

  (match form
    [(_) (error "syntax-error: at least one clause is required for cond:" form)]
    [(_ clause ...) (process-clauses clause)]
    [else (error "syntax-error: malformed cond:" form)]))

(define-pass1-syntax (case form cenv) :null
  (define (process-clauses tmpvar cls)
    (match cls
      [() ($const-undef)]
      [(([? (cut global-eq? <> else. cenv)] exprs ...) . rest)
       (unless (null? rest)
         (error "syntax-error: 'else' clause followed by more clauses:" form))
       (match exprs
         ;; (else => proc) -- SRFI-87 case clause
         [((? (cut global-eq? <> =>. cenv)) proc)
          ($call (car cls)
                 (pass1 proc (cenv-sans-name cenv))
                 (list ($lref tmpvar)))]
         ;; (else . exprs)
         [_ ($seq (imap (cut pass1 <> cenv) exprs))])]
      [((elts exprs ...) . rest)
       (let ([nelts (length elts)]
             [elts  (map unwrap-syntax elts)])
         ($if (car cls)
              (case nelts
                [(0)  ($const-f)]
                [(1)  (if (symbol? (car elts))
                        ($eq? #f  ($lref tmpvar) ($const (car elts)))
                        ($eqv? #f ($lref tmpvar) ($const (car elts))))]
                [else ($memv #f ($lref tmpvar) ($const elts))])
              (match exprs
                ;; (elts => proc) -- SRFI-87 case clause
                [((? (cut global-eq? <> =>. cenv)) proc)
                 ($call (car cls)
                        (pass1 proc (cenv-sans-name cenv))
                        (list ($lref tmpvar)))]
                ;; (elts . exprs)
                [_ ($seq (imap (cut pass1 <> cenv) exprs))])
              (process-clauses tmpvar (cdr cls))))]
      [_ (error "syntax-error: bad clause in case:" form)]))

  (match form
    [(_)
     (error "syntax-error: at least one clause is required for case:" form)]
    [(_ expr clause ...)
     (let* ([etree (pass1 expr cenv)]
            [tmp (make-lvar 'tmp)])
       (lvar-initval-set! tmp etree)
       ($let form 'let
             (list tmp)
             (list etree)
             (process-clauses tmp clause)))]
    [_ (error "syntax-error: malformed case:" form)]))

(define-pass1-syntax (and-let* form cenv) :gauche
  (define (process-binds binds body cenv)
    (match binds
      [() (pass1/body body cenv)]
      [((exp) . more)
       ($if form (pass1 exp (cenv-sans-name cenv))
            (process-binds more body cenv)
            ($it))]
      [([? variable? var] . more)
       ($if form (pass1 var (cenv-sans-name cenv))
            (process-binds more body cenv)
            ($it))]
      [(([? variable? var] init) . more)
       (let* ([lvar (make-lvar var)]
              [newenv (cenv-extend cenv `((,var . ,lvar)) LEXICAL)]
              [itree (pass1 init (cenv-add-name cenv var))])
         (lvar-initval-set! lvar itree)
         ($let form 'let
               (list lvar)
               (list itree)
               ($if form ($lref lvar)
                    (process-binds more body newenv)
                    ($it))))]
      [_ (error "syntax-error: malformed and-let*:" form)]))

  (match form
    [(_ binds . body) (process-binds binds body cenv)]
    [_ (error "syntax-error: malformed and-let*:" form)]))

;; Quote and quasiquote ................................

(define (pass1/quote obj)
  ($const (unwrap-syntax obj)))

(define-pass1-syntax (quote form cenv) :null
  (match form
    [(_ obj) (pass1/quote obj)]
    [else (error "syntax-error: malformed quote:" form)]))

(define-pass1-syntax (quasiquote form cenv) :null
  ;; We want to avoid unnecessary allocation as much as possible.
  ;; Current code generates constants not only the obvious constant
  ;; case, e.g. `(a b c), but also folds constant variable references,
  ;; e.g. (define-constant x 3) then `(,x) generate a constant list '(3).
  ;; This extends as far as the pass-1 constant folding goes, so `(,(+ x 1))
  ;; also becomes '(4).
  ;; NB: The current code allocates lots of intermediate $const node.

  (define (quasiquote? v)       (global-eq? v quasiquote. cenv))
  (define (unquote? v)          (global-eq? v unquote. cenv))
  (define (unquote-splicing? v) (global-eq? v unquote-splicing. cenv))

  ;; In the context where there's no outer list to which we intersperse to.
  (define (quasi obj level)
    (match obj
      [((? quasiquote?) x)
       (let1 xx (quasi x (+ level 1))
         (if ($const? xx)
           ($const (list 'quasiquote ($const-value xx)))
           ($list obj (list ($const 'quasiquote) xx))))]
      [((and (or (? unquote?) (? unquote-splicing?)) op) . xs)
       (if (zero? level)
         (if (and (unquote? op)
                  (pair? xs) (null? (cdr xs)))
           (pass1 (car xs) cenv)
           (errorf "invalid ~a form in this context: ~s" op obj))
         (let1 xx (quasi* xs (- level 1))
           (if ($const? xx)
             ($const (cons op ($const-value xx)))
             ($cons obj ($const op) xx))))]
      [(? pair?)       (quasi* obj level)]
      [(? vector?)     (quasi-vector obj level)]
      [(? identifier?) ($const (unwrap-syntax obj))]
      [() ($const-nil)]
      [_  ($const obj)]))

  ;; In the spliceable context.  objs is always a list.
  (define (quasi* objs level)
    ;; NB: we already excluded toplevel quasiquote and unquote
    (match objs
      [(((and (or (? unquote?) (? unquote-splicing?)) op) . xs) . ys)
       (let1 yy (quasi* ys level)
         (if (zero? level)
           ((if (unquote? op) build build@)
            (imap (cut pass1 <> cenv) xs) yy)
           (let1 xx (quasi* xs (- level 1))
             (if (and ($const? xx) ($const? yy))
               ($const (acons op ($const-value xx) ($const-value yy)))
               ($cons objs ($cons (car objs) ($const op) xx) yy)))))]
      [((or (? unquote?) (? unquote-splicing?)) . _) ;`(... . ,xs) `(... . ,@xs)
       (quasi objs level)]
      [((? vector? x) . ys) (quasi-cons objs quasi-vector x ys level)]
      [(x . ys)             (quasi-cons objs quasi x ys level)]
      [_                    (quasi objs level)]))

  ;; iforms :: [IForm]
  ;; rest   :: IForm
  (define (build iforms rest)
    (match iforms
      [() rest]
      [(x . xs) (let1 xx (build xs rest)
                  (if (and ($const? x) ($const? xx))
                    ($const (cons ($const-value x) ($const-value xx)))
                    ($cons #f x xx)))]))

  (define (build@ iforms rest)
    (match iforms
      [() rest]
      [(x . xs) (let1 xx (build@ xs rest)
                  (if ($const? xx)
                    (cond [(null? ($const-value xx)) x]
                          [($const? x) ($const (append ($const-value x)
                                                       ($const-value xx)))]
                          [else ($append #f x xx)])
                    ($append #f x xx)))]))

  (define (quasi-cons src quasi-car x ys level)
    (let ([xx (quasi-car x level)]
          [yy (quasi* ys level)])
      (if (and ($const? xx) ($const? yy))
        ($const (cons ($const-value xx) ($const-value yy)))
        ($cons src xx yy))))

  (define (quasi-vector obj level)
    (if (vector-has-splicing? obj)
      ($list->vector obj (quasi* (vector->list obj) level))
      (let* ([need-construct? #f]
             [elts (map (^[elt] (rlet1 ee (quasi elt level)
                                  (unless ($const? ee)
                                    (set! need-construct? #t))))
                        (vector->list obj))])
        (if need-construct?
          ($vector obj elts)
          ($const (list->vector (map (^[e] ($const-value e)) elts)))))))

  (define (vector-has-splicing? obj)
    (let loop ((i 0))
      (cond [(= i (vector-length obj)) #f]
            [(and (pair? (vector-ref obj i))
                  (unquote-splicing? (car (vector-ref obj i))))]
            [else (loop (+ i 1))])))

  (match form
    [(_ obj) (quasi obj 0)]
    [_ (error "syntax-error: malformed quasiquote:" form)]))

(define-pass1-syntax (unquote form cenv) :null
  (error "unquote appeared outside quasiquote:" form))

(define-pass1-syntax (unquote-splicing form cenv) :null
  (error "unquote-splicing appeared outside quasiquote:" form))

;; Lambda family (binding constructs) ...................

(define-pass1-syntax (lambda form cenv) :null
  (match form
    [(_ formals . body)
     (pass1/lambda (add-arg-info form formals) formals body cenv #f)]
    [_ (error "syntax-error: malformed lambda:" form)]))

(define-pass1-syntax (lambda form cenv) :gauche
  (match form
    [(_ formals . body)
     (pass1/lambda (add-arg-info form formals) formals body cenv #t)]
    [_ (error "syntax-error: malformed lambda:" form)]))

;; Add formals list as 'arg-info attributes of the source form
(define (add-arg-info form formals)
  (rlet1 xform (if (extended-pair? form)
                 form
                 (extended-cons (car form) (cdr form)))
    (pair-attribute-set! xform 'arg-info formals)))

(define (pass1/lambda form formals body cenv extended?)
  (receive (args reqargs optarg kargs) (parse-lambda-args formals)
    (cond [(null? kargs)
           (let* ([lvars (imap make-lvar+ args)]
                  [intform ($lambda form (cenv-exp-name cenv)
                                    reqargs optarg lvars #f #f)]
                  [newenv (cenv-extend/proc cenv (%map-cons args lvars)
                                            LEXICAL intform)])
             (vector-set! intform 6 (pass1/body body newenv))
             intform)]
          [(not extended?)
           (error "syntax-error: extended formals aren't allowed in R5RS \
                   lambda:" form)]
          [else
           (let1 g (gensym)
             (pass1/lambda form (append args g)
                           (pass1/extended-lambda form cenv g kargs body)
                           cenv #f))])))

(define-pass1-syntax (receive form cenv) :gauche
  (match form
    [(_ formals expr body ...)
     (receive (args reqargs optarg kargs) (parse-lambda-args formals)
       (unless (null? kargs)
         (error "syntax-error: extended lambda list isn't allowed in receive:"
                form))
       (let* ([lvars (imap make-lvar+ args)]
              [newenv (cenv-extend cenv (%map-cons args lvars) LEXICAL)])
         ($receive form reqargs optarg lvars (pass1 expr cenv)
                   (pass1/body body newenv))))]
    [_ (error "syntax-error: malformed receive:" form)]))

;; Returns <list of args>, <# of reqargs>, <has optarg?>, <kargs>
;; <kargs> is like (:optional (x #f) (y #f) :rest k) etc.
(define (parse-lambda-args formals)
  (let loop ([formals formals] [args '()] [n 0])
    (match formals
      [()      (values (reverse args) n 0 '())]
      [((? keyword-like?) . _) (values (reverse args) n 1 formals)]
      [(x . y) (loop (cdr formals) (cons (car formals) args) (+ n 1))]
      [x       (values (reverse (cons x args)) n 1 '())])))

;; Handles extended lambda list.  garg is a gensymed var that receives
;; restarg.
(define (pass1/extended-lambda form cenv garg kargs body)
  (define (collect-args xs r)
    (match xs
      [() (values (reverse r) '())]
      [((? keyword-like?) . _) (values (reverse r) xs)]
      [(var . rest) (collect-args rest (cons var r))]))
  (define (parse-kargs c xs os ks r a)
    (match xs
      [() (expand-opt os ks r a)]
      [(k . xs)
       (cond
        [(global-keyword-eq? k :optional c)
         (unless (null? os) (too-many :optional))
         (receive (os xs) (collect-args xs '()) (parse-kargs c xs os ks r a))]
        [(global-keyword-eq? k :key c)
         (unless (null? ks) (too-many :key))
         (receive (ks xs) (collect-args xs '()) (parse-kargs c xs os ks r a))]
        [(global-keyword-eq? k :rest c)
         (when r (too-many :rest))
         (receive (rs xs) (collect-args xs '())
           (match rs
             [(r) (parse-kargs c xs os ks r a)]
             [_ (error ":rest keyword in the extended lambda list must be \
                        followed by exactly one argument:" kargs)]))]
        [(global-keyword-eq? k :allow-other-keys c)
         (when a (too-many :allow-other-keys))
         (receive (a xs) (collect-args xs '())
           (match a
             [()   (parse-kargs c xs os ks r #t)]
             [(av) (parse-kargs c xs os ks r av)]
             [_ (error ":allow-other-keys keyword in the extended lambda list \
                        can be followed by zero or one argument:" kargs)]))]
        [else (error "invalid extended lambda list:" kargs)])]))
  (define (too-many key)
    (errorf "too many ~s keywords in the extended lambda list: ~s" key kargs))
  (define (expand-opt os ks r a)
    (if (null? os)
      (if r
        `(((with-module gauche let) ((,r ,garg)) ,@(expand-key ks garg a)))
        (expand-key ks garg a))
      (let ([binds (map (match-lambda
                          [[? symbol? o] o]
                          [[? identifier? o] o]
                          [(o init) `(,o ,init)]
                          [_ (error "illegal optional argument spec in " kargs)])
                        os)]
            [rest (or r (gensym))])
        `(((with-module gauche let-optionals*) ,garg ,(append binds rest)
           ,@(if (and (not r) (null? ks))
               ;; TODO: better error message!
               `((unless (null? ,rest)
                   (error "too many arguments for" ',form))
                 (let () ,@(expand-key ks rest a)))
               (expand-key ks rest a)))))))
  (define (expand-key ks garg a)
    (if (null? ks)
      body
      (let1 args (map (match-lambda
                        [[? symbol? o] o]
                        [[? identifier? o] o]
                        [(([? keyword-like? key] o) init)
                         (let1 k (if (identifier? key)
                                   (identifier->symbol key)
                                   key)
                           `(,o ,k ,init))]
                        [(o init) `(,o ,init)]
                        [_ (error "illegal keyword argument spec in " kargs)])
                      ks)
        `(((with-module gauche let-keywords*) ,garg
           ,(if a (append args a) args)
           ,@body)))))

  (parse-kargs cenv kargs '() '() #f #f))

;; case-lambda (srfi-16)
;;   we recognize it here so that we can do aggressive inlining.
(define-pass1-syntax (case-lambda form cenv) :gauche
  (match form
    [(_ (formals . body)) ; special case
     (pass1 `(,lambda. ,formals ,@body) cenv)]
    [(_) (error "syntax-error: malformed case-lambda:" form)]
    [(_ (formals . body) ...)
     (receive (min-req max-req) (find-argcount-minmax formals)
       (pass1 `(,(make-identifier 'make-case-lambda
                                  (find-module 'gauche.internal)
                                  '())
                ,min-req ,max-req ',formals
                (list ,@(map (^(f b) `(,lambda. ,f ,@b)) formals body))
                ',(cenv-exp-name cenv))
              cenv))]
    [_ (error "syntax-error: malformed case-lambda:" form)]))

(define (find-argcount-minmax formals)
  (define (length. xs k)
    (if (pair? xs) (length. (cdr xs) (+ k 1)) k))
  (let loop ([formals formals] [min-req #f] [max-req 0])
    (if (null? formals)
      (values min-req max-req)
      (let1 k (length. (car formals) 0)
        (loop (cdr formals) (if min-req (min min-req k) k) (max max-req k))))))

(define-pass1-syntax (let form cenv) :null
  (match form
    [(_ () body ...)
     (pass1/body body cenv)]
    [(_ ((var expr) ...) body ...)
     (let* ([lvars (imap make-lvar+ var)]
            [newenv (cenv-extend cenv (%map-cons var lvars) LEXICAL)])
       ($let form 'let lvars
             (map (^[init lvar]
                    (rlet1 iexpr
                        (pass1 init (cenv-add-name cenv (lvar-name lvar)))
                      (lvar-initval-set! lvar iexpr)))
                  expr lvars)
             (pass1/body body newenv)))]
    [(_ name ((var expr) ...) body ...)
     (unless (variable? name) (error "bad name for named let:" name))
     ;; Named let.  (let name ((var exp) ...) body ...)
     ;;
     ;;  We don't use the textbook expansion here
     ;;    ((letrec ((name (lambda (var ...) body ...))) name) exp ...)
     ;;
     ;;  Instead, we use the following expansion, except that we cheat
     ;;  environment during expanding {exp ...} so that the binding of
     ;;  name doesn't interfere with exp ....
     ;;
     ;;    (letrec ((name (lambda (var ...) body ...))) (name {exp ...}))
     ;;
     ;;  The reason is that this form can be more easily spotted by
     ;;  our simple-minded closure optimizer in Pass 2.
     (let ([lvar (make-lvar name)]
           [args (imap make-lvar+ var)]
           [argenv (cenv-sans-name cenv)])
       (let* ([env1 (cenv-extend cenv `((,name . ,lvar)) LEXICAL)]
              [env2 (cenv-extend/name env1 (%map-cons var args) LEXICAL name)]
              [lmda ($lambda form name (length args) 0 args
                             (pass1/body body env2))])
         (lvar-initval-set! lvar lmda)
         ($let form 'rec
               (list lvar)
               (list lmda)
               ($call #f ($lref lvar)
                      (imap (cut pass1 <> argenv) expr)))))]
    [_ (error "syntax-error: malformed let:" form)]))

(define-pass1-syntax (let* form cenv) :null
  (match form
    [(_ ((var expr) ...) body ...)
     (let loop ([vars var] [inits expr] [cenv cenv])
       (if (null? vars)
         (pass1/body body cenv)
         (let* ([lv (make-lvar (car vars))]
                [newenv (cenv-extend cenv `((,(car vars) . ,lv)) LEXICAL)]
                [iexpr (pass1 (car inits) (cenv-add-name cenv (car vars)))])
           (lvar-initval-set! lv iexpr)
           ($let #f 'let (list lv) (list iexpr)
                 (loop (cdr vars) (cdr inits) newenv)))))]
    [_ (error "syntax-error: malformed let*:" form)]))

(define-pass1-syntax (letrec form cenv) :null
  (pass1/letrec form cenv "letrec" 'rec))

(define-pass1-syntax (letrec* form cenv) :gauche
  (pass1/letrec form cenv "letrec*" 'rec*))

(define (pass1/letrec form cenv name type)
  (match form
    [(_ () body ...)
     (pass1/body body cenv)]
    [(_ ((var expr) ...) body ...)
     (let* ([lvars (imap make-lvar+ var)]
            [newenv (cenv-extend cenv (%map-cons var lvars) LEXICAL)])
       ($let form type lvars
             (map (^[lv init]
                    (rlet1 iexpr
                        (pass1 init (cenv-add-name newenv (lvar-name lv)))
                      (lvar-initval-set! lv iexpr)))
                  lvars expr)
             (pass1/body body newenv)))]
    [else (errorf "syntax-error: malformed ~a: ~s" name form)]))

(define-pass1-syntax (do form cenv) :null
  (match form
    [(_ ((var init . update) ...) (test expr ...) body ...)
     (let* ([tmp  (make-lvar 'do-proc)]
            [args (imap make-lvar+ var)]
            [newenv (cenv-extend/proc cenv (%map-cons var args)
                                      LEXICAL 'do-proc)]
            [clo ($lambda
                  form 'do-body (length var) 0 args
                  ($if #f
                       (pass1 test newenv)
                       (if (null? expr)
                         ($it)
                         ($seq (imap (cut pass1 <> newenv) expr)))
                       ($seq
                        (list
                         (pass1/body body newenv)
                         ($call form
                                ($lref tmp)
                                (map (match-lambda*
                                       [(() arg)   ($lref arg)]
                                       [((expr) _) (pass1 expr newenv)]
                                       [_ (error "bad update expr in do:" form)])
                                     update args)))))
                  #f)])
       (lvar-initval-set! tmp clo)
       ($let form 'rec
             (list tmp)
             (list clo)
             ($call form
                    ($lref tmp)
                    (map (cute pass1 <> (cenv-sans-name cenv)) init))))]
    [else (error "syntax-error: malformed do:" form)]))

;; Set! ......................................................

(define-pass1-syntax (set! form cenv) :null
  (match form
    [(_ (op . args) expr)
     ;; srfi-17.  We recurse to pass1 on expanded form, for (setter op) might
     ;; have a chance of optimization.
     (pass1 (with-original-source `((,setter. ,op) ,@args ,expr) form) cenv)]
    [(_ name expr)
     (unless (variable? name)
       (error "syntax-error: malformed set!:" form))
     (let ([var (cenv-lookup-variable cenv name)]
           [val (pass1 expr cenv)])
       (if (lvar? var)
         ($lset var val)
         ($gset (ensure-identifier var cenv) val)))]
    [_ (error "syntax-error: malformed set!:" form)]))

;; Begin .....................................................

(define-pass1-syntax (begin form cenv) :null
  ($seq (imap (cut pass1 <> cenv) (cdr form))))

;; Lazy & Delay ..............................................

(define-pass1-syntax (lazy form cenv) :gauche
  (match form
    [(_ expr) ($asm form `(,PROMISE)
                    (list (pass1 `(,lambda. () ,expr) cenv)))]
    [_ (error "syntax-error: malformed lazy:" form)]))

(define-pass1-syntax (delay form cenv) :null
  (match form
    [(_ expr) (pass1 `(,lazy. (,eager. ,expr)) cenv)]
    [_ (error "syntax-error: malformed delay:" form)]))

;; Module related ............................................

(define-pass1-syntax (define-module form cenv) :gauche
  (check-toplevel form cenv)
  (match form
    [(_ name body ...)
     (let* ([mod (ensure-module name 'define-module #t)]
            [newenv (make-bottom-cenv mod)])
       ($seq (imap (cut pass1 <> newenv) body)))]
    [_ (error "syntax-error: malformed define-module:" form)]))

(define-pass1-syntax (with-module form cenv) :gauche
  (match form
    [(_ name body ...)
     (let* ([mod (ensure-module name 'with-module #f)]
            [newenv (cenv-swap-module cenv mod)])
       ($seq (imap (cut pass1 <> newenv) body)))]
    [_ (error "syntax-error: malformed with-module:" form)]))

(define-pass1-syntax (select-module form cenv) :gauche
  (check-toplevel form cenv)
  (match form
    [(_ module)
     ;; This is the only construct that changes VM's current module.
     ;; We also modifies CENV's module, so that select-module has an
     ;; effect in the middle of sequence of expressions like
     ;;  (begin ... (select-module foo) ...)
     ;; It is yet debatable that how select-module should interact with EVAL.
     (let1 m (ensure-module module 'select-module #f)
       (vm-set-current-module m)
       (cenv-module-set! cenv m)
       ($values0))]
    [else (error "syntax-error: malformed select-module:" form)]))

(define-pass1-syntax (current-module form cenv) :gauche
  (unless (null? (cdr form))
    (error "syntax-error: malformed current-module:" form))
  ($const (cenv-module cenv)))

(define-pass1-syntax (export form cenv) :gauche
  (%export-symbols (cenv-module cenv) (cdr form))
  ($values0))

(define-pass1-syntax (export-all form cenv) :gauche
  (unless (null? (cdr form))
    (error "syntax-error: malformed export-all:" form))
  (%export-all (cenv-module cenv))
  ($values0))

(define-pass1-syntax (import form cenv) :gauche
  (define (ensure m) (or (find-module m) (error "unknown module" m)))
  (define (symbol-but-not-keyword? x)
    (and (symbol? x) (not (keyword? x))))
  (dolist [f (cdr form)]
    (match f
      [((? symbol-but-not-keyword? a) (? symbol-but-not-keyword? b) . r)
       ;;likely to be an r7rs-style import
       (error "This import form looks like R7RS `import', as opposed to \
               Gauche `import'.  If you're in REPL, type (use r7rs) \
               and (select-module r7rs.user) to enter the R7RS namespace.")]
      [(m . r) (process-import (cenv-module cenv) (ensure m) r)]
      [m       (process-import (cenv-module cenv) (ensure m) '())]))
  ($values0))

(define (process-import current imported args)
  (let loop ([imported imported]
             [args args]
             [prefix #f])
    (match args
      [() (%import-module current imported prefix)]
      [(':prefix p . rest)
       (loop imported rest (if prefix (string->symbol #"~|p|~prefix") p))]
      [(':only (ss ...) . rest)
       (let1 m (%make-wrapper-module imported prefix)
         (process-import:mapsym
          :only (unwrap-syntax ss) #f prefix
          (^[sym orig-sym] (unless (%alias-binding m orig-sym imported orig-sym)
                             (errorf "during processing :only clause: \
                                      binding of ~a isn't exported from ~a"
                                     orig-sym imported))))
         (%extend-module m '())
         (loop m rest #f))]
      [(':except (ss ...) . rest)
       (let1 m (%make-wrapper-module imported prefix)
         (process-import:mapsym
          :except (unwrap-syntax ss) #f prefix
          (^[sym orig-sym] (%hide-binding m orig-sym)))
         (loop m rest #f))]
      [(':rename ((ss ds) ...) . rest)
       (let* ([ss (unwrap-syntax ss)]
              [ds (unwrap-syntax ds)]
              [m0 (if prefix (%make-wrapper-module imported prefix) imported)]
              [m (%make-wrapper-module imported #f)])
         (process-import:mapsym
          :rename ds ss prefix
          (^[sym orig-sym] (unless (%alias-binding m sym imported orig-sym)
                             (errorf "during processing :rename clause: \
                                      binding of ~a isn't exported from ~a"
                                     orig-sym imported))))
         (dolist [s ss] (unless (find-binding m s #t) (%hide-binding m s)))
         (%extend-module m (list m0))
         (loop m rest #f))]
      [(other . rest) (error "invalid import spec:" args)])))

;; Common work to process new bindings in a trampoline module.
;; Calls PROCESS with each symbols in SYMS and OLD-SYMS, but
;; symbols in OLD-SYMS are prefix-stripped.  OLD-SYMS can be #f
;; then we assume it is the same as SYMS.
(define (process-import:mapsym who syms old-syms prefix process)
  (define (check s)
    (unless (symbol? s)
      (errorf "~a option of import must take list of symbols, but got: ~s"
              who s)))
  (for-each (^[sym osym]
              (check sym) (check osym)
              (process sym (process-import:strip-prefix who osym prefix)))
            syms (or old-syms syms)))

(define (process-import:strip-prefix who sym prefix)
  (if prefix
    (rlet1 sans (symbol-sans-prefix sym prefix)
      (unless sans (errorf "~a specifies nonexistent symbol: ~a" who sym)))
    sym))

(define-pass1-syntax (extend form cenv) :gauche
  (%extend-module (cenv-module cenv)
                  (imap (^[m] (or (find-module m)
                                  (begin
                                    (%require (module-name->path m))
                                    (find-module m))
                                  (error "undefined module" m)))
                        (cdr form)))
  ($values0))

(define-pass1-syntax (require form cenv) :gauche
  (match form
    [(_ feature) (%require feature) ($values0)]
    [_ (error "syntax-error: malformed require:" form)]))

;; Include .............................................

(define-pass1-syntax (include form cenv) :gauche
  ($seq (map (^p (pass1 (car p) (cenv-swap-source cenv (cdr p))))
             (pass1/expand-include (cdr form) cenv #f))))

(define-pass1-syntax (include-ci form cenv) :gauche
  ($seq (map (^p (pass1 (car p) (cenv-swap-source cenv (cdr p))))
             (pass1/expand-include (cdr form) cenv #t))))

;; Returns  ((Sexpr . Filename) ...)
(define (pass1/expand-include args cenv case-fold?)
  (define (do-include filename)
    (unless (string? filename)
      (error "include requires literal string, but got:" filename))
    (let1 iport (pass1/open-include-file filename (cenv-source-path cenv))
      (port-case-fold-set! iport case-fold?)
      (pass1/report-include iport #t)
      (unwind-protect
          ;; This could be written simpler using port->sexp-list, but it would
          ;; trigger autoload and reenters to the compiler.
          (let loop ([r (read iport)] [forms '()])
            (if (eof-object? r)
              `((,begin. ,@(reverse forms)) . ,(port-name iport))
              (loop (read iport) (cons r forms))))
        (pass1/report-include iport #f)
        (close-input-port iport))))
  (map do-include args))
  
;; If filename is relative, we try to resolve it with the source file.
;; whenever possible.
(define (pass1/open-include-file path includer-path)
  (let1 search-paths (if includer-path
                       (cons (sys-dirname includer-path) *load-path*)
                       *load-path*)
    ;; find-load-file returns either (<found-path> <rest-of-search-paths>)
    ;; or (<pseudo-path> <rest-of-search-paths> <thunk-to-open-content>)
    ;; see libeval.scm for the details.
    (if-let1 path&rest (find-load-file path search-paths
                                       (cons "" *load-suffixes*)
                                       :allow-archive #t
                                       :relative-dot-path #t)
      (if (pair? (cddr path&rest)) ; archive hook is in effect.
        ((caddr path&rest) (car path&rest))
        (open-input-file (car path&rest) :encoding #t))
      (error "include file is not readable: " path))))

;; Report including.
(define (pass1/report-include iport open?)
  (when (vm-compiler-flag-is-set? SCM_COMPILE_INCLUDE_VERBOSE)
    (format (current-error-port) ";;~a including ~s\n"
            (if open? "Begin" "End") (port-name iport))))

;; Class stuff ........................................

;; KLUDGES.  They should be implemented as macros, but the
;; current compiler doesn't preserve macro definitions.
;; These syntax handler merely expands the given form to
;; the call to internal procedures of objlib.scm, which
;; returns the macro expanded form.

(define-pass1-syntax (define-generic form cenv) :gauche
  (match form
    [(_ name . opts)
     (check-toplevel form cenv)
     (pass1 (with-module gauche.object (%expand-define-generic name opts)) cenv)]
    [_ (error "syntax-error: malformed define-generic:" form)]))

(define-pass1-syntax (define-method form cenv) :gauche
  (define (parse name rest quals)
    (match rest
      [((? keyword? q) . rest)
       (parse name rest (cons q quals))]
      [(specs . body)
       (pass1 (with-module gauche.object
                (%expand-define-method name quals specs body))
              cenv)]
      [_ (error "syntax-error: malformed define-method (empty body):" form)]))
  ;; Should we limit define-method only at the toplevel?  Doing so
  ;; is consistent with toplevel define and define-syntax.  Allowing
  ;; define-method in non-toplevel is rather CL-ish and not like Scheme.
  ;; (check-toplevel form cenv)
  (match form
    [(_ name . rest)
     (parse name rest '())]
    [_ (error "syntax-error: malformed define-method:" form)]))

(define-pass1-syntax (define-class form cenv) :gauche
  (match form
    [(_ name supers slots . options)
     (check-toplevel form cenv)
     (pass1 (with-module gauche.object
              (%expand-define-class name supers slots options))
            cenv)]
    [_ (error "syntax-error: malformed define-class:" form)]))

;; Black magic ........................................

(define-pass1-syntax (eval-when form cenv) :gauche
  (match form
    [(_ (w ...) expr ...)
     ;; check
     (let ([wlist
            (let loop ((w w) (r '()))
              (cond [(null? w) r]
                    [(memq (car w) '(:compile-toplevel :load-toplevel :execute))
                     (if (memq (car w) r)
                       (loop (cdr w) r)
                       (loop (cdr w) (cons (car w) r)))]
                    [else
                     (error "eval-when: situation must be a list of \
                             :compile-toplevel, :load-toplevel or :execute, \
                             but got:" (car w))]))]
           [situ (vm-eval-situation)])
       (when (and (eqv? situ SCM_VM_COMPILING)
                  (memq :compile-toplevel wlist)
                  (cenv-toplevel? cenv))
         (dolist [e expr] (eval e (cenv-module cenv))))
       (if (or (and (eqv? situ SCM_VM_LOADING)
                    (memq :load-toplevel wlist)
                    (cenv-toplevel? cenv))
               (and (eqv? situ SCM_VM_EXECUTING)
                    (memq :execute wlist)))
         ($seq (imap (cut pass1 <> cenv) expr))
         ($const-undef)))]
    [_ (error "syntax-error: malformed eval-when:" form)]))

#|
(define-pass1-syntax (with-meta form cenv) :gauche
  (match form
    [(_ (meta ...) expr)
     (let1 exp (pass1 expr cenv)
       exp)]
    [_ (error "syntax-error: malformed with-meta:" form)]))
|#

