;;
;; A compiler.
;;  $Id: comp.scm,v 1.1.2.9 2005-01-03 09:40:27 shirok Exp $

(define-module gauche.internal
  (use util.match)
  )
(select-module gauche.internal)

;; Entry
(define (compile program . opts)
  (let1 mod (get-optional opts #f)
    (if mod
      (let1 origmod (vm-current-module)
        (dynamic-wind
            (lambda () (vm-set-current-module mod))
            (lambda () (compile-int program '() 'tail))
            (lambda () (vm-set-current-module origmod))))
      (compile-int program '() 'tail))))

;; compile-int:: (Program, Env, Ctx) -> [Insn]
(define (compile-int program env ctx)
  (match program
    ((op . args)
     (cond
      ((variable? op)
       (let1 local (lookup-env op env #t)
         (cond
          ((vm-insn? local) ;; LREF
           (compile-call (add-srcinfo (extended-list local) op)
                         program env ctx))
          ((is-a? local <macro>) ;; local macro
           (compile-int (call-macro-expander local program env) env ctx))
          ((get-global-binding op env)
           => (lambda (gloc)
                (let1 val (gloc-ref gloc)
                  (cond
                   ((is-a? val <syntax>)
                    (call-syntax-compiler val program env ctx))
                   ((is-a? val <macro>)
                    (compile-int (call-macro-expander val program env)
                                 env ctx))
                   ((has-inliner? val)
                    (let1 inlined (call-procedure-inliner val program env)
                      (if inlined
                        (compile-int inlined env ctx)
                        (compile-call (compile-varref op '()) program env ctx))))
                   (else
                    (compile-call (compile-varref op '()) program env ctx))))))
          (else
           (compile-call (compile-varref op '()) program env ctx)))))
      ((is-a? op <syntax>)
       (call-syntax-compiler op program env ctx))
      (else
       (compile-call (compile-int op env 'normal) program env ctx))))
    ((? variable?)
     (compile-varref program env))
    (else
     (if (eq? ctx 'stmt) '() (list program)))
    ))

(define (compile-varref var env)
  (let1 loc (lookup-env var env #f)
    (add-srcinfo
     (if (variable? loc)
       (extended-list (vm-insn-make 'GREF) loc)
       (extended-list loc))
     var)))

;; Returns an insn list of function invocation 
;;  PROGRAM = (OP . ARGS) and HEAD is the compiled insn list for OP.
(define (compile-call head program env ctx)
  (compile-call-finish head (compile-args (cdr program) env)
                       (length (cdr program)) program ctx))

;; Finish compilation of calling sequence.  ARGCODE is an insn list
;; to push arguments into the stack.  HEADCODE is an insn list to
;; leave the procedure in the register.   PROGRAM is the source code
;; to be attached to the call instruction.
(define (compile-call-finish headcode argcode numargs program ctx)
  (define (srcinfo insn)
    (if program
      (add-srcinfo (extended-list insn) program)
      (list insn)))
  (if (eq? ctx 'tail)
    (append! (list (vm-insn-make 'PRE-TAIL numargs))
             argcode
             headcode
             (srcinfo (vm-insn-make 'TAIL-CALL numargs)))
    (list (vm-insn-make 'PRE-CALL numargs)
          (append! argcode headcode
                   (srcinfo (vm-insn-make 'CALL numargs))))))

;; Returns an insn list to push arguments ARGS into the stack.
(define (compile-args args env)
  (if (null? args)
    '()
    (append! (compile-int (car args) env 'normal)
             (list (vm-insn-make 'PUSH))
             (compile-args (cdr args) env))))

;; Returns an insn list of executing EXPRS in sequence.
(define (compile-seq exprs env ctx)
  (match exprs
    (() '())
    ((expr) (compile-int expr env ctx))
    ((expr . exprs)
     (append! (compile-int expr env 'stmt)
              (compile-seq exprs env ctx)))))

;; Look up local environment
;;
(define (lookup-env var env syntax?)
  (let outer ((env env)
              (depth 0))
    (if (pair? env)
      (let ((frame (car env)))
        (when (and (identifier? var)
                   (eq? (slot-ref var 'env) env))
          (set! var (slot-ref var 'name)))
        (if (pair? frame)
          (if (eq? (car frame) #t)
            ;; macro binding.
            (if syntax?
              (or (find-local-macro (cdr frame) var)
                  (outer (cdr env) depth))
              (outer (cdr env) depth))
            ;; look for variable binding.  there may be a case that
            ;; single frame contains more than one variable with the
            ;; same name (in the case like '(let* ((x 1) (x 2)) ...)'),
            ;; so we have to scan the frame until the end. */
            (let inner ((frame frame) (offset 0) (found #f))
              (cond ((null? frame)
                     (if found
                       (vm-insn-make 'LREF depth (- offset found 1))
                       (outer (cdr env) (+ depth 1))))
                    ((eq? (car frame) var)
                     (inner (cdr frame) (+ offset 1) offset))
                    (else
                     (inner (cdr frame) (+ offset 1) found))))
            )
          ;; don't count empty frames.  they are omitted at runtime.
          (outer (cdr env) depth)))
      ;; binding not found in local env.  returns an identifier.
      (if (and (symbol? var) (not syntax?))
        (make-identifier-old var '())
        var))))

(define (find-local-macro frame var)
  (let loop ((frame frame))
    (cond ((null? frame) #f)
          ((eq? (caar frame) var) (cdar frame))
          (else (loop (cdr frame))))))

(define (get-global-binding name env)
  (cond
   ((identifier? name)
    (find-binding (slot-ref name 'module) (slot-ref name 'name) #f))
   ((symbol? name)
    (find-binding (get-current-module env) name #f))
   (else #f)))

(define (get-current-module env)
  (vm-current-module))

(define (add-srcinfo form info)
  (pair-attribute-set! form 'source-info info)
  form)

;;============================================================
;; Special forms
;;

(define-macro (define-primitive-syntax formals . body)
  `(define ,(car formals)
     (make-syntax ',(car formals) (lambda ,(cdr formals) ,@body))))

;;------------------------------------------------------------
;; IF family  (if, when, unless, and, or)
;;

(define (compile-if-family test-code then-code else-code env ctx)
  (if (eq? ctx 'tail)
    (append! test-code
             (list (vm-insn-make 'IF) then-code)
             else-code)
    (let1 merger (list (vm-insn-make 'MNOP))
      (append! test-code
               (list (vm-insn-make 'IF) (append! then-code merger))
               (append! else-code merger)))))

(define-primitive-syntax (if@ form env ctx)
  (match form
    ((_ test then else)
     (compile-if-family (compile-int test env 'normal)
                        (compile-int then env ctx)
                        (compile-int else env ctx)
                        env ctx))
    ((_ test then)
     (compile-int `(,if@ ,test ,then ,(undefined)) env ctx))
    (else
     (error "malformed if:" form))))

(define-primitive-syntax (when@ form env ctx)
  (match form
    ((_ test . body)
     (compile-if-family (compile-int test env 'normal)
                        (compile-seq body env ctx)
                        (list (undefined))
                        env ctx))
    (else
     (error "malformed when:" form))))

(define-primitive-syntax (unless@ form env ctx)
  (match form
    ((_ test . body)
     (compile-if-family (compile-int test env 'normal)
                        (list (undefined))
                        (compile-seq body env ctx)
                        env ctx))
    (else
     (error "malformed unless:" form))))

(define-primitive-syntax (and@ form env ctx)
  (let1 merger (if (eq? ctx 'tail) '() (list (vm-insn-make 'MNOP)))
    (define (and-rec exprs)
      (match exprs
        (() (list #t))
        ((expr) (append! (compile-int expr env ctx) merger))
        ((expr . other)
         (append! (compile-int expr env 'normal)
                  (list (vm-insn-make 'IF)
                        (and-rec other))
                  merger))))
    (and-rec (cdr form))))

(define-primitive-syntax (or@ form env ctx)
  (let1 merger (if (eq? ctx 'tail) '() (list (vm-insn-make 'MNOP)))
    (define (or-rec exprs)
      (match exprs
        (() (list #f))
        ((expr) (append! (compile-int expr env ctx) merger))
        ((expr . other)
         (append! (compile-int expr env 'normal)
                  (list (vm-insn-make 'IF) merger)
                  (or-rec other)))))
    (or-rec (cdr form))))

;;------------------------------------------------------------
;; BEGIN
;;

;; NB: begins in the beginning of lambda bodies are handled
;; within lambda-family compiler.

(define-primitive-syntax (begin@ form env ctx)
  (compile-seq (cdr form) env ctx))

;;------------------------------------------------------------
;; LAMBDA family  (lambda, let, let*, letrec, receive)
;;


;;============================================================
;; Some experiment
;;

;; NB: for the time being, we use simple vector and manually
;; defined accessors/modifiers.  We can't use define-class stuff
;; here until we can compile gauche/object.scm into C.

;; Local variables (lvar)
;;
;;   Slots:
;;     name  - name of the variable (symbol)
;;     ref-count - in how many places this variable is referefnced?
;;     set-count - in how many places this variable is set!
;;

(define (make-lvar name) (vector 'lvar name 0 0))

(define (lvar? obj)      (and (vector? obj) (eq? (vector-ref obj 0) 'lvar)))
(define (lvar-name var)      (vector-ref var 1))
(define (lvar-ref-count var) (vector-ref var 2))
(define (lvar-set-count var) (vector-ref var 3))

(define (lvar-ref++! var)
  (vector-set! var 2 (+ 1 (vector-ref var 2))))
(define (lvar-set++! var)
  (vector-set! var 3 (+ 1 (vector-ref var 3))))

;; Compile-time environment (cenv)
;;
;;   Slots:
;;     module   - The 'current-module' to resolve global binding.
;;     frames   - List of local frames.  Each local frame has a form:
;;                (<syntax?> (<name> . <obj>) ...)
;;                where <syntax?> is #t for the local macro frames,
;;                and #f for the local binding frames.  <obj> is
;;                a <macro> object for the local macro frames, and
;;                lvar object for the local binding frames.

(define (make-cenv module frames) (vector 'cenv module frames))

(define (cenv? obj)      (and (vector? obj) (eq? (vector-ref obj 0) 'cenv)))
(define (cenv-module cenv)   (vector-ref cenv 1))
(define (cenv-frames cenv)   (vector-ref cenv 2))

(define (make-bottom-cenv)
  (make-cenv (vm-current-module) '()))

(define (cenv-extend cenv frame syntax?)
  (make-cenv (cenv-module cenv)
             (acons syntax? frame (cenv-frames cenv))))

;; Lookup compiler enviroment.  Returns either lvar, syntax, or identifier.
;; NB: the treatment of locally-bound identifier should be fixed.
(define (cenv-lookup cenv sym-or-id syntax?)
  (define (find-lvar frame)
    (if (car frame) ;; syntactic frame
      (cond ((and syntax? (assq sym-or-id (cdr frame))) => cdr)
            (else #f))
      (cond ((assq sym-or-id (cdr frame)) => cdr)
            (else #f))))

  (let ((frames (cenv-frames cenv)))
    (let loop ((frames frames))
      (cond ((null? frames)
             (if (identifier? sym-or-id)
               sym-or-id
               (make-identifier sym-or-id '() (cenv-module cenv))))
            ((find-lvar (car frames)))
            (else (loop (cdr frames)))))))

;; Intermediate form
;;
;; <top-expr> :=
;;    <expr>
;;    ($define <flags> <id> <expr>)
;;    ($define-macro <flags> <id> <expr>)
;;
;; <expr> :=
;;    ($lref <lvar>)        ;; local variable reference
;;    ($lset <lvar> <expr>) ;; local variable modification
;;    ($gref <id>)          ;; global variable reference
;;    ($gset <id> <expr>)   ;; global variable modification
;;    ($const <obj>)        ;; constant literal
;;    ($if <o> <expr> <expr+> <expr>) ;; branch
;;    ($let <o> (<lvar> ...) (<expr> ...) <expr>) ;; local binding
;;    ($receive <o> <reqarg> <optarg> (<lvar> ...) <expr> <expr>) ;; local binding (mv)
;;    ($lambda <o> <reqarg> <optarg> (<lvar> ...) <expr>)  ;; closure
;;    ($seq <expr> ...)     ;; sequencing
;;    ($call <o> <proc-expr> <arg-expr> ...) ;; procedure call
;;
;;    ($cons <o> <ca> <cd>)       ;; used in quasiquote
;;    ($append <o> <ca> <cd>)     ;; ditto
;;    ($vector <o> <elt> ...)     ;; ditto
;;    ($list->vector <o> <list>)  ;; ditto
;;
;; <expr+> :=
;;    <expr>
;;    ($it)                 ;; refer to the value in the last test clause.
;;
;;  NB: <o> is the original form, used to generate debug info.
;;      if the intermediate form doesn't have corresponding original
;;      form, it will be #f.
;;
;;  

;; Pass 1
;;   - Expand macros
;;   - Resolve variable reference
;;   - Convert special forms into a few number of primitive
;;     operators

(define (pass1 program cenv)
  (match program
    ((op . args)
     (if (variable? op)
       (let1 head (cenv-lookup cenv op #t)
         (cond
          ((lvar? head)
           (lvar-ref++! head)
           (pass1/call program `($lref ,head) args cenv))
          ((is-a? head <macro>)
           (error "local macro not supported yet"))
          ((identifier? head)
           (pass1/global-call head program cenv))
          (else
           (error "[internal] unknown resolution of head:" head))))
       (pass1/call program (pass1 op cenv) args cenv)))
    ((? variable?)
     (pass1/variable program cenv))
    (else
     `($const ,program))))

(define (pass1/variable var cenv)
  (let ((r (cenv-lookup cenv var #f)))
    (cond ((lvar? r)
           (begin (lvar-ref++! r) `($lref ,r)))
          ((identifier? r)
           `($gref ,r))
          ((symbol? r)
           `($gref ,(make-identifier r '() (cenv-module cenv))))
          (else
           (error "[internal] pass1/variable got weird object:" var)))))

(define (pass1/call program proc args cenv)
  `($call ,program ,proc ,@(map (lambda (arg) (pass1 arg cenv)) args)))

(define (pass1/global-call id program cenv)
  (let1 gloc (find-binding (slot-ref id 'module)
                           (slot-ref id 'name)
                           #f)
    (if (not gloc)
      (pass1/call program `($gref ,id) (cdr program) cenv)
      (let1 gval (gloc-ref gloc)
        (cond
         ((is-a? gval <macro>)
          (error "macro not supported yet:" program))
         ((is-a? gval <syntax>)
          ((get-pass1-syntax gval) program cenv))
         (else
          (pass1/call program `($gref ,id) (cdr program) cenv)))))))

(define (pass1/body forms cenv)
  ;; TODO: internal define
  `($seq ,@(map (cut pass1 <> cenv) forms)))

(define (ensure-identifier sym-or-id cenv)
  (if (identifier? sym-or-id)
    sym-or-id
    (make-identifier sym-or-id '() (cenv-module cenv))))

;; Returns <list of args>, <# of reqargs>, <has optarg?>
(define (parse-lambda-args formals)
  (let loop ((formals formals) (args '()))
    (cond ((null? formals) (values (reverse args) (length args) #f))
          ((pair? formals) (loop (cdr formals) (cons (car formals) args)))
          (else (values (reverse (cons formals args)) (length args) #t)))))

;; Strip syntactic info from form; unlike built-in unwrap-syntax,
;; this can handle circular structure.
(define (%unwrap-syntax form)
  (define (unwrap form history)
    (cond
     ((assq form history) form)
     ((pair? form)
      (let* ((h  (cons form history))
             (ca (unwrap (car form) h))
             (cd (unwrap (cdr form) h)))
        (if (and (eq? (car form) ca) (eq? (cdr form) cd))
          form
          (cons ca cd))))
     ((identifier? form)
      (slot-ref form 'name))
     ((vector? form)
      (let ((h (cons form history))
            (len (vector-length form)))
        (let loop ((i 0))
          (if (= i len)
            form
            (let* ((elt (vector-ref form i))
                   (nelt (unwrap elt h)))
              (if (eq? elt nelt)
                (loop (+ i 1))
                (let ((newvec (copy-vector form)))
                  (let loop ((i i))
                    (if (= i len)
                      newvec
                      (begin (vector-set! newvec i
                                          (unwrap (vector-ref form i) h))
                             (loop (+ i 1))))))
                ))))
        ))
     (else form)))
  (unwrap form '()))

;;----------------------------------------------------------------
;; Pass1 syntaxes
;;

(define *pass1-syntax-alist* '())

(define-macro (define-pass1-syntax formals . body)
  `(set! *pass1-syntax-alist*
         (acons ,(car formals)
                (lambda ,(cdr formals) ,@body)
                *pass1-syntax-alist*)))

(define (global-id id)
  (make-identifier id '() (find-module 'gauche)))

;; Definitions ........................................

(define (pass1/define form flags module cenv origform)
  (match form
    ((_ (name . args) body ...)
     (pass1/define-common `(define name
                             (,(global-id 'lambda) ,args ,@body))
                          flags module cenv origform))
    ((_ name expr)
     (unless (variable? name)
       (error "syntax-error:" origform))
     `($define ,flags
               ,(make-identifier (%unwrap-syntax name) '() module)
               ,(pass1 expr cenv)))
    (else (error "syntax-error:" origform))))

(define-pass1-syntax (define form cenv)
  (pass1/define form '() (cenv-module module) cenv form))

(define-pass1-syntax (define-constant form cenv)
  (pass1/define form '(const) (cenv-module module) cenv form))

;; If family ........................................

(define-pass1-syntax (if form cenv)
  (match form
    ((_ test then else)
     `($if ,form ,(pass1 test cenv) ,(pass1 then cenv) ,(pass1 else cenv)))
    ((_ test then)
     `($if ,form ,(pass1 test cenv) ,(pass1 then cenv) ($const ,(undefined))))
    (else
     (error "syntax-error: malformed if:" form))))

(define-pass1-syntax (and form cenv)
  (define (rec exprs)
    (match exprs
      (() '($const #t))
      ((expr) (pass1 expr cenv))
      ((expr . more)
       `($if #f ,(pass1 expr cenv) ,(rec more) ($it)))
      (else
       (error "syntax-error: malformed and:" form))))
  (rec (cdr form)))

(define-pass1-syntax (or form cenv)
  (define (rec exprs)
    (match exprs
      (() '($const #f))
      ((expr) (pass1 expr cenv))
      ((expr . more)
       `($if #f ,(pass1 expr cenv) ($it) ,(rec more)))
      (else
       (error "syntax-error: malformed or:" form))))
  (rec (cdr form)))

(define-pass1-syntax (when form cenv)
  (match form
    ((_ test body ...)
     `($if ,form ,(pass1 test cenv)
           ($seq ,@(map (cut pass1 <> cenv) body))
           ($const ,(undefined))))
    (else
     (error "syntax-error: malformed when:" form))))

(define-pass1-syntax (unless form cenv)
  (match form
    ((_ test body ...)
     `($if ,form ,(pass1 test cenv)
           ($const ,(undefined))
           ($seq ,@(map (cut pass1 <> cenv) body))))
    (else
     (error "syntax-error: malformed unless:" form))))

;; Quote and quasiquote ................................

(define (pass1/quote obj)
  `($const ,(%unwrap-syntax obj)))

(define-pass1-syntax (quote form cenv)
  (match form
    ((_ obj) (pass1/quote obj))
    (else (error "syntax-error: malformed quote:" form))))

(define-pass1-syntax (quasiquote form cenv)
  (define (wrap obj orig)
    (if (eq? obj orig) `($const ,obj) obj))
  (define (quasi obj)
    (match obj
      (('unquote x)
       (pass1 x cenv))
      ((x 'unquote-splicing y)            ;; `(x . ,@y)
       (error "unquote-splicing appeared in invalid context:" obj))
      ((('unquote-splicing x) 'unquote y) ;; `(,@x . ,y)
       `($append ,(car obj) ,(pass1 x cenv) ,(pass1 y cenv)))
      ((('unquote-splicing x) . y)        ;; `(,@x . rest)
       (let1 yy (quasi y)
         `($append ,(car obj) ,(pass1 x cenv) ,(wrap yy y))))
      ((x 'unquote y)                     ;; `(x . ,y)
       (let1 xx (quasi x)
         `($cons ,(wrap xx x) ,(pass1 y cenv))))
      ((x . y)                            ;; general case of pair
       (let ((ca (quasi (car obj)))
             (cd (quasi (cdr obj))))
         (if (and (eq? ca (car obj)) (eq? cd (cdr obj)))
           obj
           `($cons ,obj ,(wrap ca (car obj)) ,(wrap cd (cdr obj))))))
      ((? vector?)
       (quasi-vector obj))
      (else obj)))

  (define (quasi-vector obj)
    (case (scan-vector obj)
      ((has-unquote)
       `($vector ,obj
                 ,@(map (lambda (elt) (let1 v (quasi elt) (wrap v elt)))
                        (vector->list obj))))
      ((has-splicing)
       `($list->vector ,obj ,(quasi (vector->list obj))))
      (else obj)))

  (define (scan-vector obj)
    (let loop ((i 0) (r 'const))
      (if (= i (vector-length obj))
        r
        (match (vector-ref obj i)
          (('unquote-splicing _) 'has-splicing)
          (('unquote _) (loop (+ i 1) 'has-unquote))
          (else (loop (+ i 1) r))))))
  
  (match form
    ((_ obj) 
     (let1 v (quasi obj) (wrap v obj)))
    (else (error "syntax-error: malformed quasiquote:" form))))

;; Lambda family (binding constructs) ...................

(define-pass1-syntax (lambda form cenv)
  (match form
    ((_ formals . body)
     (receive (args reqargs has-optarg?) (parse-lambda-args formals)
       (let* ((lvars (map make-lvar args))
              (newenv (cenv-extend cenv (cons #f (map cons args lvars)) #f)))
         `($lambda ,form ,reqargs ,has-optarg?
                   ,lvars ,(pass1/body body newenv)))))
    (else
     (error "syntax-error: malformed lambda:" form))))

(define-pass1-syntax (receive form cenv)
  (match form
    ((_ formals expr body ...)
     (receive (args reqargs has-optarg?) (parse-lambda-args formals)
       (let* ((lvars (map make-lvar args))
              (newenv (cenv-extend cenv (cons #f (map cons args lvars)) #f)))
         `($receive ,form ,reqargs ,has-optarg?
                    ,lvars ,(pass1/body body newenv)))))
    (else
     (error "syntax-error: malformed receive:" form))))

(define-pass1-syntax (let form cenv)
  (match form
    ((_ ((var expr) ...) body ...)
     (let* ((lvars (map make-lvar var))
            (newenv (cenv-extend cenv (cons #f (map cons var lvars)) #f)))
       `($let ,form ,lvars ,(map (cut pass1 <> newenv) expr)
              ,(pass1/body body newenv))))
    ((_ name ((var expr) ...) body ...)
     (unless (variable? name)
       (error "bad name for named let:" name))
     ;; (let name ((var exp) ...) body ...)
     ;; == ((letrec ((name (lambda (var ...) body ...))) name) exp ...)
     (let* ((lvar (make-lvar name))
            (args (map make-lvar var))
            (env1 (cenv-extend cenv `(#f (,name . ,lvar)) #f))
            (env2 (cenv-extend env1 (cons #f (map cons var args)) #f)))
     `($call ,form
             ($let #f (,lvar) (($const ,(undefined)))
                   ($seq ($lset ,lvar
                                ($lambda ,form ,(length args) #f
                                         ,args ,(pass1/body body env2)))
                         ($lref ,lvar)))
             ,@(map (cut pass1 <> cenv) expr))))
    (else
     (error "syntax-error: malformed let:" form))))

(define-pass1-syntax (let* form cenv)
  (match form
    ((_ ((var expr) ...) body ...)
     (let loop ((vars var) (inits expr) (cenv cenv))
       (if (null? vars)
         (pass1/body body cenv)
         (let* ((lv (make-lvar (car vars)))
                (newenv (cenv-extend cenv (list #f (cons (car vars) lv)) #f)))
           `($let #f (,lv) (,(pass1 (car inits) cenv))
                  ,(loop (cdr vars) (cdr inits) newenv))))))
    (else
     (error "syntax-error: malformed let*:" form))))

(define-pass1-syntax (letrec form cenv)
  (match form
    ((_ ((var expr) ...) body ...)
     (let* ((lvars (map make-lvar var))
            (newenv (cenv-extend cenv (cons #f (map cons var lvars)) #f))
            (setup (map (lambda (lv init)
                          `($lset ,lv ,(pass1 init newenv)))
                        lvars expr)))
       `($let ,form ,lvars ,(map (lambda (_) `($const ,(undefined))) lvars)
              ($seq ,@setup ,(pass1/body body newenv)))))
    (else
     (error "syntax-error: malformed letrec:" form))))

;; Set! ......................................................

(define-pass1-syntax (set! form cenv)
  (match form
    ((_ (op . args) expr)
     `($call ,form
             ($gref ,(global-id 'setter))
             ,@(map (cut pass1 <> cenv) args)
             ,(pass1 expr cenv)))
    ((_ name expr)
     (unless (variable? name)
       (error "syntax-error: malformed set!:" form))
     (let ((var (cenv-lookup cenv name #f))
           (val (pass1 expr cenv)))
       (if (lvar? var)
         (begin (lvar-set++! var) `($lset ,var ,val))
         `($gset ,(ensure-identifier var cenv) ,val))))
    (else
     (error "syntax-error: malformed set!:" form))))

;; Begin .....................................................

(define-pass1-syntax (begin form cenv)
  `($seq ,@(map (cut pass1 <> cenv) (cdr form))))

;; Bridge to dispatch new compiler pass-1 syntax handler based on
;; original binding

(define (get-pass1-syntax val)
  (cond ((assq val *pass1-syntax-alist*) => cdr)
        (else (error "pass1 syntax not supported:" val))))

;;============================================================
;; Utilities
;;

(define (variable? arg)
  (or (symbol? arg) (identifier? arg)))

(define (global-eq? var sym env)
  (and (variable? var)
       (let1 v (lookup-env var env #t)
         (cond
          ((identifier? v) (eq? (slot-ref v 'name) sym))
          ((symbol? v) (eq? v sym))
          (else #f)))))

  
  

(define (find pred lis)
  (let loop ((lis lis))
    (cond ((null? lis) #f)
          ((pred (car lis)))
          (else (loop (cdr lis))))))

(define (fold proc seed lis)
  (let loop ((lis lis) (seed seed))
    (if (null? lis)
      seed
      (loop (cdr lis) (proc (car lis) seed)))))

(define (append-map proc lis)
  (apply append (map proc lis)))

;; return the last cdr of improper list.
;; when applied to compiler env, this returns a module.
(define (last-cdr lis)
  (let loop ((lis lis))
    (if (pair? lis) (loop (cdr lis)) lis)))

;; These used to be defined in autoloaded file.
;; The new compiler needs them, so we need to compile them as well.
(define (caaar x) (car (caar x)))
(define (caadr x) (car (cadr x)))
(define (cadar x) (car (cdar x)))
(define (caddr x) (car (cddr x)))
(define (cdaar x) (cdr (caar x)))
(define (cdadr x) (cdr (cadr x)))
(define (cddar x) (cdr (cdar x)))
(define (cdddr x) (cdr (cddr x)))

(define (caaaar x) (caar (caar x)))
(define (caaadr x) (caar (cadr x)))
(define (caadar x) (caar (cdar x)))
(define (caaddr x) (caar (cddr x)))
(define (cadaar x) (cadr (caar x)))
(define (cadadr x) (cadr (cadr x)))
(define (caddar x) (cadr (cdar x)))
(define (cadddr x) (cadr (cddr x)))

(define (cdaaar x) (cdar (caar x)))
(define (cdaadr x) (cdar (cadr x)))
(define (cdadar x) (cdar (cdar x)))
(define (cdaddr x) (cdar (cddr x)))
(define (cddaar x) (cddr (caar x)))
(define (cddadr x) (cddr (cadr x)))
(define (cdddar x) (cddr (cdar x)))
(define (cddddr x) (cddr (cddr x)))

;;============================================================
;; Initialization
;;

(define (init-compiler)
  ;; Injects syntax objects into basic modules.
  (define (inject module name comp)
    (%insert-binding module name comp))

  (let ((N (find-module 'null))
        (G (find-module 'gauche)))
    
    (inject N 'if              if@)
    (inject G 'when            when@)
    (inject G 'unless          unless@)
    (inject N 'and             and@)
    (inject N 'or              or@)

    (inject N 'begin           begin@)
    ))
