;;
;; A compiler.
;;  $Id: comp.scm,v 1.1.2.21 2005-01-14 02:14:16 shirok Exp $

(define-module gauche.internal
  (use util.match)
  )
(select-module gauche.internal)

;;============================================================
;; For testing
;;

(define (compile program . opts)
  (let1 mod (get-optional opts #f)
    (if mod
      (let1 origmod (vm-current-module)
        (dynamic-wind
            (lambda () (vm-set-current-module mod))
            (lambda () (compile-rec program))
            (lambda () (vm-set-current-module origmod))))
      (compile-rec program))))

(define (compile-rec program)
;  (display "Input: ") (write program) (newline)
  (let1 p1 (pass1 program (make-bottom-cenv))
;    (display "Pass1: ") (write p1) (newline)
    (let1 p3 (pass3 (pass2 p1))
;      (display "Pass3: ") (write p3) (newline)
;      (vm-dump-code (vm-pack-code p3))
      p3)))

;;============================================================
;; Data structures
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
;;                (<type> (<name> . <obj>) ...)
;;
;;                <type>     <obj>
;;                ----------------------------------------------
;;                'lexical   <lvar>     ;; lexical binding
;;                'syntax    <macro>    ;; syntactic binding
;;                'pattern   <pvar>     ;; pattern variable

(define (make-cenv module frames) (vector 'cenv module frames))

(define (cenv? obj)      (and (vector? obj) (eq? (vector-ref obj 0) 'cenv)))
(define (cenv-module cenv)   (vector-ref cenv 1))
(define (cenv-frames cenv)   (vector-ref cenv 2))

(define (make-bottom-cenv)
  (make-cenv (vm-current-module) '()))

(define (cenv-extend cenv frame type)
  (make-cenv (cenv-module cenv)
             (acons type frame (cenv-frames cenv))))

;; toplevel environment == cenv has only syntactic frames
(define (cenv-toplevel? cenv)
  (every (lambda (frame) (eq? (car frame) #t)) (cenv-frames cenv)))

;; Lookup compiler enviroment.
;; lookup-as argument
;;    'lexical - lookup only lexical bindings
;;    'syntax  - lookup lexical and syntactic bindings
;;    'pattern - lookup lexical, syntactic and pattern bindings

(define (cenv-lookup cenv sym-or-id lookup-as)

  (define search-types
    (case lookup-as
      ((lexical) '(lexical))
      ((syntax)  '(lexical syntax))
      ((pattern) '(lexical syntax pattern))))

  (define (find-lvar frame)
    (cond ((and (memq (car frame) search-types)
                (assq sym-or-id (cdr frame)))
           => cdr)
          (else #f)))

  (let ((frames (cenv-frames cenv)))
    (let loop ((frames frames))
      (cond ((null? frames)
             (if (identifier? sym-or-id)
               sym-or-id
               (make-identifier sym-or-id '() (cenv-module cenv))))
            ((find-lvar (car frames)))
            (else (loop (cdr frames)))))))

;; Pattern variable (pvar)
;;   Keeps binding info of pattern variables at the compile time.

(define (make-pvar name level value)  (vector 'pvar name level value))

(define (pvar-name pvar)   (vector-ref pvar 1))
(define (pvar-level pvar)  (vector-ref pvar 2))
(define (pvar-value pvar)  (vector-ref pvar 3))

;; Intermediate form
;;
;; <top-expr> :=
;;    <expr>
;;    ($define <o> <flags> <id> <expr>)
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
;;    ($memv <o> <obj> <list>)    ;; used in case
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
       (let1 head (cenv-lookup cenv op 'syntax)
         (cond
          ((lvar? head)
           (lvar-ref++! head)
           (pass1/call program `($lref ,head) args cenv))
          ((is-a? head <macro>)
           (pass1 (call-macro-expander head program cenv) cenv))
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
  (let ((r (cenv-lookup cenv var 'lexical)))
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
          (pass1 (call-macro-expander gval program cenv) cenv))
         ((is-a? gval <syntax>)
          ((get-pass1-syntax gval) program cenv))
         (else
          (pass1/call program `($gref ,id) (cdr program) cenv)))))))

(define (pass1/body forms origform cenv)
  ;; Scan internal defines.  We need to expand macros at this stage,
  ;; since the macro may produce more internal defines.  Note that the
  ;; previous internal definition in the same body may shadow the macro
  ;; binding, so we need to check idef_vars for that.
  ;;
  ;; Actually, this part touches the hole of R5RS---we can't determine
  ;; the scope of the identifiers of the body until we find the boundary
  ;; of internal define's, but in order to find all internal defines
  ;; we have to expand the macro and we need to detemine the scope
  ;; of the macro keyword.  Search "macro internal define" in
  ;; comp.lang.scheme for the details.
  ;;
  ;; I use the model that appears the same as Chez, which adopts
  ;; let*-like semantics for the purpose of determining macro binding
  ;; during expansion.
  (define (pick-intdefs exprs intdefs)
    (match exprs
      (()
       ;(error "empty-body" origform)
       ($const-undef))
      (((op . args) . rest)
       (if (or (not (variable? op)) (assq op intdefs))
         ;; This can't be an internal define.
         (wrap-intdefs intdefs exprs)
         (let1 var (cenv-lookup cenv op 'syntax)
           (cond
            ((lvar? var) (wrap-intdefs intdefs exprs))
            ((is-a? var <macro>)
             (pick-intdefs
              (append (call-macro-expander var (car exprs) cenv)
                      exprs)
              intdefs))
            ((identifier? var)
             (if (eq? (slot-ref var 'name) 'define)
               (handle-intdef (car exprs) rest intdefs)
               (wrap-intdefs intdefs exprs)))
            (else
             (error "[internal] pass1/body" var))))))
      (else
       (wrap-intdefs intdefs exprs))))

  (define (handle-intdef def exprs intdefs)
    (match def
      ((_ (name . args) body ...)
       (pick-intdefs exprs
                     (cons (list name `(,(global-id 'lambda) ,args ,@body))
                           intdefs)))
      ((_ name expr)
       (pick-intdefs exprs (cons (list name expr) intdefs)))
      (else
       (error "malformed internal define:" def))))

  (define (wrap-intdefs intdefs exprs)
    (if (null? intdefs)
      `($seq ,@(map (cut pass1 <> cenv) exprs))
      (pass1 `(,(global-id 'letrec) ,intdefs ,@exprs) cenv)))
  
  ;; TODO: internal define
  (pick-intdefs forms '()))

(define (ensure-identifier sym-or-id cenv)
  (if (identifier? sym-or-id)
    sym-or-id
    (make-identifier sym-or-id '() (cenv-module cenv))))

;; convenient procedure to produce ($const #<undef>)
(define ($const-undef) `($const ,(undefined)))

;; Returns <list of args>, <# of reqargs>, <has optarg?>
(define (parse-lambda-args formals)
  (let loop ((formals formals) (args '()))
    (cond ((null? formals) (values (reverse args) (length args) 0))
          ((pair? formals) (loop (cdr formals) (cons (car formals) args)))
          (else (values (reverse (cons formals args)) (length args) 1)))))

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

(define (ensure-module thing name create?)
  (let1 mod 
      (cond ((symbol? thing) (find-module thing))
            ((identifier? thing) (find-module (slot-ref thing 'name)))
            ((module? thing) thing)
            (else
             (errorf "~a requires a module name or a module, but got: ~s"
                     name thing)))
    (or mod
        (if create?
          (make-module (if (identifier? thing) (slot-ref thing 'name) thing))
          (errorf "~a: no such module: ~s" name thing)))))

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

(define (pass1/define form oform flags module cenv)
  (unless (cenv-toplevel? cenv)
    (error "syntax-error: non-toplevel define is not allowed:" oform))
  (match form
    ((_ (name . args) body ...)
     (pass1/define `(define ,name
                      (,(global-id 'lambda) ,args ,@body))
                   oform flags module cenv))
    ((_ name expr)
     (unless (variable? name)
       (error "syntax-error:" origform))
     `($define ,oform ,flags
               ,(make-identifier (%unwrap-syntax name) '() module)
               ,(pass1 expr cenv)))
    (else (error "syntax-error:" oform))))

(define-pass1-syntax (define form cenv)
  (pass1/define form form '() (cenv-module cenv) cenv))

(define-pass1-syntax (define-constant form cenv)
  (pass1/define form form '(const) (cenv-module cenv) cenv))

(define-pass1-syntax (define-in-module form cenv)
  (match form
    ((_ module . rest)
     (pass1/define `(_ . ,rest) form '()
                   (ensure-module module 'define-in-module #f)
                   cenv))
    (else (error "syntax-error: malformed define-in-module:" form))))

(define (pass1/define-macro form oform module cenv)
  (unless (cenv-toplevel? cenv)
    (error "syntax-error: non-toplevel define-macro is not allowed:" oform))
  (match form
    ((_ (name . args) body ...)
     (pass1/define-macro `(define-macro ,name
                            (,(global-id 'lambda) ,args ,@body))
                         oform module cenv))
    ((_ name expr)
     (unless (variable? name)
       (error "syntax-error:" origform))
     ;; TODO: macro autoload
     (let1 trans (make-macro-transformer name (eval expr module))
       (%insert-binding module name trans)
       ($const-undef)))
    (else (error "syntax-error:" oform))))

(define-pass1-syntax (define-macro form cenv)
  (unless (cenv-toplevel? cenv)
    (error "syntax-error: non-toplevel define-macro is not allowed:" form))
  (pass1/define-macro form form (cenv-module cenv) cenv))


(define-pass1-syntax (define-syntax form cenv)
  (unless (cenv-toplevel? cenv)
    (error "syntax-error: non-toplevel define-syntax is not allowed:" oform))
  ;; Temporary: we use the old compiler's syntax-rules implementation
  ;; for the time being.
  (match form
    ((_ name ('syntax-rules (literal ...) rule ...))
     (let1 transformer (compile-syntax-rules name literal rule cenv)
       (%insert-binding (cenv-module cenv) name transformer)
       ($const-undef)))
    (else
     (error "syntax-error: malformed define-syntax:" form))))

;; Macros ...........................................

(define-pass1-syntax (%macroexpand form cenv)
  (match form
    ((_ expr) `($const ,(%internal-macro-expand expr cenv #f)))
    (else "syntax-error: malformed %macroexpand:" form)))

(define-pass1-syntax (%macroexpand-1 form cenv)
  (match form
    ((_ expr) `($const ,(%internal-macro-expand expr cenv #t)))
    (else "syntax-error: malformed %macroexpand-1:" form)))

(define-pass1-syntax (let-syntax form cenv)
  (match form
    ((_ ((name trans-spec) ...) body ...)
     (let* ((trans (map (lambda (n spec)
                          (match spec
                            (('syntax-rules (lit ...) rule ...)
                             (compile-syntax-rules n lit rule cenv))
                            (else
                             (error "syntax-error: malformed transformer-spec:"
                                    spec))))
                        name trans-spec))
            (newenv (cenv-extend cenv (map cons name trans) 'syntax)))
       (pass1/body body body newenv)))
    (else "syntax-error: malformed let-syntax:" form)))

;; If family ........................................

(define-pass1-syntax (if form cenv)
  (match form
    ((_ test then else)
     `($if ,form ,(pass1 test cenv) ,(pass1 then cenv) ,(pass1 else cenv)))
    ((_ test then)
     `($if ,form ,(pass1 test cenv) ,(pass1 then cenv) ,($const-undef)))
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

(define-pass1-syntax (cond form cenv)
  (define (process-clauses cls)
    (match cls
      (() ($const-undef))
      ((((? (global-eq?? 'else cenv)) expr ...) . rest)
       (unless (null? rest)
         (error "syntax-error: 'else' clause followed by more clauses:" form))
       `($seq ,@(map (cut pass1 <> cenv) expr)))
      (((test (? (global-eq?? '=> cenv)) proc) . rest)
       (let1 tmp (make-lvar 'tmp)
         `($let ,(car cls)
                (,tmp)
                (,(pass1 test cenv))
                ($if ,(car cls)
                     ($lref ,tmp)
                     ($call ,(car cls)
                            ,(pass1 proc cenv)
                            ($lref ,tmp))
                     ,(process-clauses rest)))))
      (((test) . rest)
       `($if ,(car cls)
             ,(pass1 test cenv) ($it) ,(process-clauses rest)))
      (((test expr ...) . rest)
       `($if ,(car cls)
             ,(pass1 test cenv)
             ($seq ,@(map (cut pass1 <> cenv) expr))
             ,(process-clauses rest)))
      (else
       (error "syntax-error: bad clause in cond:" form))))
  (match form
    ((_)
     (error "syntax-error: at least one clause is required for cond:" form))
    ((_ clause ...)
     (process-clauses clause))
    (else
     (error "syntax-error: malformed cond:" form))))

(define-pass1-syntax (case form cenv)
  (define (process-clauses tmpvar cls)
    (match cls
      (() ($const-undef))
      ((('else expr ...) . rest) ;; NB: use global-id=?
       (unless (null? rest)
         (error "syntax-error: 'else' clause followed by more clauses:" form))
       `($seq ,@(map (cut pass1 <> cenv) expr)))
      ((((elt ...) expr ...) . rest)
       (let1 elts (map %unwrap-syntax elt)
         `($if ,(car cls)
               ($memv #f ($lref ,tmpvar) ($const ,elts))
               ($seq ,@(map (cut pass1 <> cenv) expr))
               ,(process-clauses tmpvar rest))))
      (else
       (error "syntax-error: bad clause in case:" form))))
  
  (match form
    ((_)
     (error "syntax-error: at least one clause is required for case:" form))
    ((_ expr clause ...)
     (let1 tmp (make-lvar 'tmp)
       `($let ,form (,tmp)
              (,(pass1 expr cenv))
              ,(process-clauses tmp clause))))
    (else
     (error "syntax-error: malformed case:" form))))

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
  (define (quasi obj level)
    (match obj
      (('unquote x)
       (if (zero? level)
         (pass1 x cenv)
         (let1 xx (quasi x (- level 1))
           (if (eq? x xx)
             obj
             (list '$list obj '($const unquote) xx)))))
      ((x 'unquote-splicing y)            ;; `(x . ,@y)
       (error "unquote-splicing appeared in invalid context:" obj))
      ((('unquote-splicing x) 'unquote y) ;; `(,@x . ,y)
       (if (zero? level)
         `($append ,(car obj) ,(pass1 x cenv) ,(pass1 y cenv))
         (let ((xx (quasi x (- level 1)))
               (yy (quasi y (- level 1))))
           (if (and (eq? xx x) (eq? yy y))
             obj
             (list '$list obj
                   (list '$list (car obj)
                         '($const unquote-splicing) (wrap xx x))
                   '($const unquote) (wrap yy y))))))
      ((('unquote-splicing x) . y)        ;; `(,@x . rest)
       (let1 yy (quasi y level)
         (if (zero? level)
           `($append ,(car obj) ,(pass1 x cenv) ,(wrap yy y))
           (let1 xx (quasi x (- level 1))
             (if (and (eq? xx x) (eq? yy y))
               obj
               (list '$list* obj
                     (list '$list (car obj)
                           '($const unquote-splicing) (wrap xx x))
                     (wrap yy y)))))))
      ((x 'unquote y)                     ;; `(x . ,y)
       (let1 xx (quasi x level)
         (if (zero? level)
           `($cons ,obj ,(wrap xx x) ,(pass1 y cenv))
           (let1 yy (quasi y (- level 1))
             (if (and (eq? xx x) (eq? yy y))
               obj
               (list '$list obj (wrap xx x) '($const unquote) (wrap yy y)))))))
      (('quasiquote x)
       (let1 xx (quasi x (+ level 1))
         (if (eq? xx x)
           obj
           (list '$list obj '($const quasiquote) (wrap xx x)))))
      ((x . y)                            ;; general case of pair
       (let ((ca (quasi (car obj) level))
             (cd (quasi (cdr obj) level)))
         (if (and (eq? ca (car obj)) (eq? cd (cdr obj)))
           obj
           `($cons ,obj ,(wrap ca (car obj)) ,(wrap cd (cdr obj))))))
      ((? vector?)
       (quasi-vector obj level))
      (else obj)))

  (define (quasi-vector obj level)
    (let* ((z  (vector->list obj))
           (zz (quasi z level)))
      (if (eq? z zz)
        obj
        `($list->vector ,obj ,zz))))

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
     (let1 v (quasi obj 0) (wrap v obj)))
    (else (error "syntax-error: malformed quasiquote:" form))))

(define-pass1-syntax (unquote form cenv)
  (error "unquote appeared outside quasiquote:" form))

(define-pass1-syntax (unquote-splicing form cenv)
  (error "unquote-splicing appeared outside quasiquote:" form))

;; Lambda family (binding constructs) ...................

(define-pass1-syntax (lambda form cenv)
  (match form
    ((_ formals . body)
     (receive (args reqargs optarg) (parse-lambda-args formals)
       (let* ((lvars (map make-lvar args))
              (newenv (cenv-extend cenv (map cons args lvars) 'lexical)))
         `($lambda ,form ,reqargs ,optarg
                   ,lvars ,(pass1/body body form newenv)))))
    (else
     (error "syntax-error: malformed lambda:" form))))

(define-pass1-syntax (receive form cenv)
  (match form
    ((_ formals expr body ...)
     (receive (args reqargs optarg) (parse-lambda-args formals)
       (let* ((lvars (map make-lvar args))
              (newenv (cenv-extend cenv (map cons args lvars) 'lexical)))
         `($receive ,form ,reqargs ,optarg
                    ,lvars ,(pass1 expr cenv)
                    ,(pass1/body body form newenv)))))
    (else
     (error "syntax-error: malformed receive:" form))))

(define-pass1-syntax (let form cenv)
  (match form
    ((_ () body ...)
     (pass1/body body form cenv))
    ((_ ((var expr) ...) body ...)
     (let* ((lvars (map make-lvar var))
            (newenv (cenv-extend cenv (map cons var lvars) 'lexical)))
       `($let ,form ,lvars ,(map (cut pass1 <> cenv) expr)
              ,(pass1/body body form newenv))))
    ((_ name ((var expr) ...) body ...)
     (unless (variable? name)
       (error "bad name for named let:" name))
     ;; (let name ((var exp) ...) body ...)
     ;; == ((letrec ((name (lambda (var ...) body ...))) name) exp ...)
     (let* ((lvar (make-lvar name))
            (args (map make-lvar var))
            (env1 (cenv-extend cenv `((,name . ,lvar)) 'lexical))
            (env2 (cenv-extend env1 (map cons var args) 'lexical)))
     `($call ,form
             ($let #f (,lvar) (($const ,(undefined)))
                   ($seq ($lset ,lvar
                                ($lambda ,form ,(length args) 0
                                         ,args ,(pass1/body body form env2)))
                         ($lref ,lvar)))
             ,@(map (cut pass1 <> cenv) expr))))
    (else
     (error "syntax-error: malformed let:" form))))

(define-pass1-syntax (let* form cenv)
  (match form
    ((_ ((var expr) ...) body ...)
     (let loop ((vars var) (inits expr) (cenv cenv))
       (if (null? vars)
         (pass1/body body form cenv)
         (let* ((lv (make-lvar (car vars)))
                (newenv (cenv-extend cenv `((,(car vars) . ,lv)) 'lexical)))
           `($let #f (,lv) (,(pass1 (car inits) cenv))
                  ,(loop (cdr vars) (cdr inits) newenv))))))
    (else
     (error "syntax-error: malformed let*:" form))))

(define-pass1-syntax (letrec form cenv)
  (match form
    ((_ () body ...)
     (pass1/body body form cenv))
    ((_ ((var expr) ...) body ...)
     (let* ((lvars (map make-lvar var))
            (newenv (cenv-extend cenv (map cons var lvars) 'lexical))
            (setup (map (lambda (lv init)
                          `($lset ,lv ,(pass1 init newenv)))
                        lvars expr)))
       `($let ,form ,lvars ,(map (lambda (_) `($const ,(undefined))) lvars)
              ($seq ,@setup ,(pass1/body body form newenv)))))
    (else
     (error "syntax-error: malformed letrec:" form))))

(define-pass1-syntax (do form cenv)
  (match form
    ((_ ((var init . update) ...) (test expr ...) body ...)
     (let* ((tmp  (make-lvar 'do-proc))
            (args (map make-lvar var))
            (newenv (cenv-extend cenv (map cons var args) 'lexical)))
       `($let ,form
              (,tmp)
              (,($const-undef))
              ($seq
               ($lset ,tmp
                      ($lambda
                       ,form ,(length var) 0 ,args
                       ($if #f
                            ,(pass1 test newenv)
                            ,(if (null? expr)
                               '($it)
                               `($seq ,@(map (cut pass1 <> newenv) expr)))
                            ($seq ,(pass1/body body form newenv)
                                  ($call ,form
                                         ($lref ,tmp)
                                         ,@(map (lambda (upd arg)
                                                  (match upd
                                                    (() `($lref ,arg))
                                                    ((expr)
                                                     (pass1 expr newenv))
                                                    (else
                                                     (error "bad update expr in do:"
                                                            form))))
                                                update args))))))
               ($call ,form
                      ($lref ,tmp)
                      ,@(map (cut pass1 <> cenv) init))))))
    (else
     (error "syntax-error: malformed do:" form))))

;; Set! ......................................................

(define-pass1-syntax (set! form cenv)
  (match form
    ((_ (op . args) expr)
     `($call ,form
             ($call #f
                    ($gref ,(global-id 'setter))
                    ,(pass1 op cenv))
             ,@(map (cut pass1 <> cenv) args)
             ,(pass1 expr cenv)))
    ((_ name expr)
     (unless (variable? name)
       (error "syntax-error: malformed set!:" form))
     (let ((var (cenv-lookup cenv name 'lexical))
           (val (pass1 expr cenv)))
       (if (lvar? var)
         (begin (lvar-set++! var) `($lset ,var ,val))
         `($gset ,(ensure-identifier var cenv) ,val))))
    (else
     (error "syntax-error: malformed set!:" form))))

;; Begin .....................................................

(define-pass1-syntax (begin form cenv)
  `($seq ,@(map (cut pass1 <> cenv) (cdr form))))

;; Delay .....................................................

(define-pass1-syntax (delay form cenv)
  (match form
    ((_ expr)
     `($promise ,form ,(pass1 `(,(global-id 'lambda) () ,expr) cenv)))
    (else (error "syntax-error: malformed delay:" form))))

;; Module related ............................................

(define-pass1-syntax (define-module form cenv)
  (unless (cenv-toplevel? cenv)
    (error "define-module should be used only in the toplevel"))
  (match form
    ((_ name body ...)
     (let* ((mod (ensure-module name 'define-module #t))
            (newenv (make-cenv mod '())))
       (dynamic-wind
           (lambda () (vm-set-current-module mod))
           (lambda () `($seq ,@(map (cut pass1 <> newenv) body)))
           (lambda () (vm-set-current-module (cenv-module cenv))))))
    (else
     (error "syntax-error: malformed define-module:" form))))

(define-pass1-syntax (with-module form cenv)
  (match form
    ((_ name body ...)
     (let* ((mod (ensure-module name 'with-module #f))
            (newenv (make-cenv mod (cenv-frames cenv))))
       (dynamic-wind
           (lambda () (vm-set-current-module mod))
           (lambda () `($seq ,@(map (cut pass1 <> newenv) body)))
           (lambda () (vm-set-current-module (cenv-module cenv))))))
    (else
     (error "syntax-error: malformed with-module:" form))))

(define-pass1-syntax (select-module form cenv)
  (unless (cenv-toplevel? cenv)
    (error "select-module should be used only in the toplevel"))
  (match form
    ((_ module)
     (vm-set-current-module (ensure-module module 'select-module #f))
     ($const-undef))
    (else (error "syntax-error: malformed select-module:" form))))

(define-pass1-syntax (current-module form cenv)
  (unless (null? (cdr form))
    (error "syntax-error: malformed current-module:" form))
  `($const ,(cenv-module cenv)))

(define-pass1-syntax (export form cenv)
  `($const ,(%export-symbols (cenv-module cenv) (cdr form))))

(define-pass1-syntax (import form cenv)
  `($const ,(%import-modules (cenv-module cenv) (cdr form))))

;; Bridge to dispatch new compiler pass-1 syntax handler based on
;; original binding

(define (get-pass1-syntax val)
  (cond ((assq val *pass1-syntax-alist*) => cdr)
        (else (error "pass1 syntax not supported:" val))))

;;------------------------------------------------------------
;; Pass 2.  Optimization
;;

(define (pass2 form)
  form)

;;------------------------------------------------------------
;; Pass 3.  Code generation
;;

;; This pass pushes down a runtime environment, renv.  It is
;; a list of lvars.  
;; 
;; The context, ctx, is either one of the following symbols.
;;
;;   normal/bottom : the FORM is evaluated in the context that the
;;            stack has no pending arguments (i.e. a continuation
;;            frame is just pushed).
;;   normal/top : the FORM is evaluated, while there are pending
;;            arguments in the stack top.  Such premature arguments
;;            should be protected if VM calls something that may
;;            capture the continuation.
;;   stmt/bottom : Like normal/bottom, but the result of FORM won't
;;            be used.
;;   stmt/top : Like normal/top, but the result of FORM won't be used.
;;   tail   : FORM is evaluated in the tail context.  It is always
;;            bottom.
;;

;; predicate
(define (normal-context? ctx) (memq ctx '(normal/bottm normal/top)))
(define (stmt-context? ctx)   (memq ctx '(stmt/bottm stmt/top)))
(define (tail-context? ctx)   (eq? ctx 'tail))
(define (bottom-context? ctx) (memq ctx '(normal/bottom stmt/bottom tail)))
(define (top-context? ctx)    (memq ctx '(normal/top stmt/top)))

;; context switch 
(define (normal-context prev-ctx)
  (if (bottom-context? prev-ctx) 'normal/bottom 'normal/top))

(define (stmt-context prev-ctx)
  (if (bottom-context? prev-ctx) 'stmt/bottom 'stmt/top))

(define (tail-context prev-ctx) 'tail)

;; Instruction accumulator (iacc)
;;
;;  Does local optimization (instruction combination)
;;
;;  The implementation will be revised once we replace
;;  the old compiler for the new one.
;;

;; For the time being, we use differential list, with extra pointer
;; to point the "last opcode".
(define (make-iacc)
  (let1 cell (list #f)
    (vector 'iacc cell cell #f)))

(define (iacc? obj)    (and (vector? obj) (eq? (vector-ref obj 0) 'iacc)))
(define (iacc-code iacc)  (cdr (vector-ref iacc 1)))

(define (iacc-current-insn iacc)
  (car (vector-ref iacc 3)))
(define (iacc-current-operands iacc)
  (cdr (vector-ref iacc 3)))

;; emit one insn, with combining instructons.
(define (iacc-emit! iacc info insn . operands)

  (define (emit! insn . operand)
    (let1 code (cons/info info (apply vm-insn-make insn) operand)
      (set-cdr! (vector-ref iacc 2) code)
      (vector-set! iacc 2 (last-pair code))
      (vector-set! iacc 3 code)))

  (define (replace! insn)
    (let1 cur (vector-ref iacc 3)
      (set-car! cur (apply vm-insn-make insn))))

  (define (replace/operand! insn . operand)
    (let1 cur (vector-ref iacc 3)
      (set-car! cur (apply vm-insn-make insn))
      (set-cdr! cur operand)
      (vector-set! iacc 2 (last-pair cur))))

  (match insn
    (('LREF depth off)
     (case depth
       ((0) (case off
              ((0) (emit! '(LREF0)))
              ((1) (emit! '(LREF1)))
              ((2) (emit! '(LREF2)))
              ((3) (emit! '(LREF3)))
              ((4) (emit! '(LREF4)))))
       ((1) (case off
              ((0) (emit! '(LREF10)))
              ((1) (emit! '(LREF11)))
              ((2) (emit! '(LREF12)))
              ((3) (emit! '(LREF13)))
              ((4) (emit! '(LREF14)))))
       (else (emit! insn))))
    (('LSET depth off)
     (case depth
       ((0) (case off
              ((0) (emit! '(LSET0)))
              ((1) (emit! '(LSET1)))
              ((2) (emit! '(LSET2)))
              ((3) (emit! '(LSET3)))
              ((4) (emit! '(LSET4)))))
       (else (emit! insn))))
    (('PUSH)
     (let1 ci (iacc-current-insn iacc)
       (if (vm-insn? ci)
         (let1 prev (vm-insn-inspect ci)
           (case (string->symbol (car prev))
             ((LREF0)  (replace! '(LREF0-PUSH)))
             ((LREF1)  (replace! '(LREF1-PUSH)))
             ((LREF2)  (replace! '(LREF2-PUSH)))
             ((LREF3)  (replace! '(LREF3-PUSH)))
             ((LREF4)  (replace! '(LREF4-PUSH)))
             ((LREF10) (replace! '(LREF10-PUSH)))
             ((LREF11) (replace! '(LREF11-PUSH)))
             ((LREF12) (replace! '(LREF12-PUSH)))
             ((LREF13) (replace! '(LREF13-PUSH)))
             ((LREF14) (replace! '(LREF14-PUSH)))
             ((LREF)   (replace! (cons 'LREF-PUSH (cdr prev))))
             ((GREF)   (replace! '(GREF-PUSH)))
             ((CONST)
              (let1 operand (car (iacc-current-operands iacc))
                (cond
                 ((null? operand)
                  (replace/operand! '(PUSHNIL)))
                 ((not operand)
                  (replace/operand! '(PUSHFALSE)))
                 ((and (integer? operand)
                       (<= #x-fffff operand #xfffff)) ;;kludge!
                  (replace/operand! `(PUSHI ,operand)))
                 (else
                  (replace! '(CONST-PUSH))))))
             (else (emit! insn))))
         (emit! insn))))
    (else (apply emit! insn operands))
    ))

(define (iacc-emit-raw! iacc lis)
  (when (pair? lis)
    (set-cdr! (vector-ref iacc 2) lis)
    (vector-set! iacc 2 (last-pair lis))
    (vector-set! iacc 3 lis)))

;;
;; Pass 3 main entry
;;
(define (pass3 form)

  (define (rec form iacc renv ctx)
    (match form
      (('$define info flags id expr)
       (rec expr iacc '() 'normal/bottom)
       (iacc-emit! iacc info 
                   (if (memq 'const flags) '(DEFINE-CONST) '(DEFINE))
                   id))
      (('$lref lvar)
       (receive (depth offset) (lookup-lvar lvar renv ctx)
         (iacc-emit! iacc (lvar-name lvar) (list 'LREF depth offset))))
      (('$lset lvar expr)
       (receive (depth offset) (lookup-lvar lvar renv ctx)
         (rec expr iacc renv (normal-context ctx))
         (iacc-emit! iacc (lvar-name lvar) (list 'LSET depth offset))))
      (('$gref id)
       (iacc-emit! iacc id '(GREF) id))
      (('$gset id expr)
       (rec expr iacc renv (normal-context ctx))
       (iacc-emit! iacc id '(GSET) id))
      (('$const value)
       (unless (stmt-context? ctx)
         (iacc-emit! iacc #f '(CONST) value)))
      (('$if info test then else)
       (let ((then-iacc (make-iacc))
             (merger (if (tail-context? ctx) '() (list (vm-insn-make 'MNOP)))))
         (rec test iacc renv (normal-context ctx))
         (rec then then-iacc renv ctx)
         (iacc-emit-raw! then-iacc merger)
         (iacc-emit! iacc info '(IF) (iacc-code then-iacc))
         (rec else iacc renv ctx)
         (iacc-emit-raw! iacc merger)))
      (('$it) #f)
      (('$let info lvars inits expr)
       (cond
        ((bottom-context? ctx)
         (prepare-args inits iacc renv ctx)
         (iacc-emit! iacc info `(LOCAL-ENV ,(length lvars)))
         (rec expr iacc (cons lvars renv) ctx)
         (unless (tail-context? ctx)
           (iacc-emit! iacc #f '(POP-LOCAL-ENV))))
        (else
         (let1 branch-iacc (make-iacc)
           (prepare-args inits branch-iacc renv ctx)
           (iacc-emit! branch-iacc info `(LOCAL-ENV ,(length lvars)))
           (rec expr branch-iacc (cons lvars renv) ctx)
           (iacc-emit! iacc #f `(PRE-CALL ,(length lvars))
                       (iacc-code branch-iacc))))))
      (('$receive info nargs optarg lvars expr body)
       (let1 branch-iacc (make-iacc)
         (rec body branch-iacc (cons lvars renv) ctx)
         (rec expr iacc renv (normal-context ctx))
         (iacc-emit! iacc info `(RECEIVE ,nargs ,optarg)
                     (iacc-code branch-iacc))))
      (('$lambda info nargs optarg lvars expr)
       (let1 body-iacc (make-iacc)
         (rec expr body-iacc (if (null? lvars) renv (cons lvars renv))
              (tail-context ctx))
         (iacc-emit! iacc info `(LAMBDA ,nargs ,optarg)
                     (iacc-code body-iacc))))
      (('$seq . exprs)
       (unless (null? exprs)
         (let loop ((exprs exprs))
           (match exprs
             ((expr) (rec expr iacc renv ctx))
             ((expr . rest)
              (rec expr iacc renv (stmt-context ctx))
              (loop rest))))))
      (('$call info proc . args)
       (let1 numargs (length args)
         (cond
          ((tail-context? ctx)
           (iacc-emit! iacc #f `(PRE-TAIL ,numargs))
           (prepare-args args iacc renv ctx)
           (rec proc iacc renv 'normal/top)
           (iacc-emit! iacc info `(TAIL-CALL ,numargs)))
          (else
           (let1 call-iacc (make-iacc)
             (prepare-args args call-iacc renv ctx)
             (rec proc call-iacc renv 'normal/top)
             (iacc-emit! call-iacc info `(CALL ,numargs))
             (iacc-emit! iacc #f `(PRE-CALL ,numargs) (iacc-code call-iacc))
             )))))
      (('$promise info thunk)
       (rec thunk iacc renv (normal-context ctx))
       (iacc-emit! iacc info '(PROMISE)))
      (('$cons info x y)
       (builtin-twoargs iacc info '(CONS) x y renv ctx))
      (('$append info x y)
       (builtin-twoargs iacc info '(APPEND 2) x y renv ctx))
      (('$list info . elts)
       (builtin-nargs iacc info 'LIST elts renv ctx))
      (('$list* info . elts)
       (builtin-nargs iacc info 'LIST-STAR elts renv ctx))
      (('$vector info . elts)
       (builtin-nargs iacc info 'VEC elts renv ctx))
      (('$list->vector info exp)
       (rec exp iacc renv (normal-context ctx))
       (iacc-emit! iacc info '(LIST2VEC)))
      (('$memv info obj lis)
       (builtin-twoargs iacc info '(MEMV) obj lis renv ctx))
      (else
       (error "[internal error] broken intermediate form:" form))))

  ;; Returns depth and offset of local variable reference.
  (define (lookup-lvar lvar renv ctx)
    (let outer ((renv renv)
                (depth 0))
      (if (null? renv)
        (error "[internal error] stray local variable:" lvar)
        (let inner ((frame (car renv))
                    (count 1))
          (cond ((null? frame) (outer (cdr renv) (+ depth 1)))
                ((eq? (car frame) lvar)
                 (values depth (- (length (car renv)) count)))
                (else (inner (cdr frame) (+ count 1))))))))
  
  (define (prepare-args args iacc renv ctx)
    (dolist (arg args)
      (rec arg iacc renv (if (eq? arg (car args))
                           (normal-context ctx)
                           'normal/top))
      (iacc-emit! iacc #f '(PUSH))))

  (define (builtin-twoargs iacc info insn arg0 arg1 renv ctx)
    (rec arg0 iacc renv (normal-context ctx))
    (iacc-emit! iacc #f '(PUSH))
    (rec arg1 iacc renv 'normal/top)
    (iacc-emit! iacc info insn))

  (define (builtin-nargs iacc info insn args renv ctx)
    (if (null? args)
      (iacc-emit! iacc info (list insn 0))
      (let loop ((as args))
        (cond ((null? (cdr as))
               (rec (car as) iacc renv 'normal/top)
               (iacc-emit! iacc info (list insn (length args))))
              (else
               (rec (car as) iacc renv 'normal/top)
               (iacc-emit! iacc #f '(PUSH))
               (loop (cdr as)))))))

  ;; Main body of pass3.
  (let1 iacc (make-iacc)
    (rec form iacc '() 'tail)
    (iacc-code iacc)
    ))
         
;;============================================================
;; Utilities
;;

(define (cons/info info ca cd)
  (if info
    (let1 p (extended-cons ca cd)
      (pair-attribute-set! p 'source-info info)
      p)
    (cons ca cd)))

(define (list/info info arg0 . args)
  (if info
    (let1 p (extended-cons arg0 args)
      (pair-attribute-set! p 'source-info info)
      p)
    (cons arg0 args)))

(define (variable? arg)
  (or (symbol? arg) (identifier? arg)))

(define (global-eq? var sym cenv)
  (and (variable? var)
       (let1 v (cenv-lookup cenv var 'lexical)
         (cond
          ((identifier? v) (eq? (slot-ref v 'name) sym))
          ((symbol? v) (eq? v sym))
          (else #f)))))

(define (global-eq?? sym cenv)
  (cut global-eq? <> sym cenv))

(define (find pred lis)
  (let loop ((lis lis))
    (cond ((null? lis) #f)
          ((pred (car lis)))
          (else (loop (cdr lis))))))

(define (every pred lis)
  (if (null? lis) #t (and (pref (car lis)) (every pred (cdr lis)))))

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
;  (define (inject module name comp)
;    (%insert-binding module name comp))

;  (let ((N (find-module 'null))
;        (G (find-module 'gauche)))
    
;    (inject N 'if              if@)
;    (inject G 'when            when@)
;    (inject G 'unless          unless@)
;    (inject N 'and             and@)
;    (inject N 'or              or@)

;    (inject N 'begin           begin@)
  #f)
