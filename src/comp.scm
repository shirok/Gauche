;;
;; A compiler.
;;  $Id: comp.scm,v 1.1.2.7 2005-01-02 12:20:13 shirok Exp $

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
     (if (variable? op)
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
           (compile-call (compile-varref op '()) program env ctx))))
       (compile-call (compile-int op env 'normal) program env ctx)))
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
    (() (list (undefined)))
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
        (make-identifier var '())
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

(define-primitive-syntax (@if form env ctx)
  (match form
    ((_ test then else)
     (compile-if-family (compile-int test env 'normal)
                        (compile-int then env ctx)
                        (compile-int else env ctx)
                        env ctx))
    ((_ test then)
     (compile-if-family (compile-int test env 'normal)
                        (compile-int then env ctx)
                        (list (undefined))
                        env ctx))
    (else
     (error "malformed if:" form))))

(define-primitive-syntax (@when form env ctx)
  (match form
    ((_ test . body)
     (compile-if-family (compile-int test env 'normal)
                        (compile-seq body env ctx)
                        (list (undefined))
                        env ctx))
    (else
     (error "malformed when:" form))))

(define-primitive-syntax (@unless form env ctx)
  (match form
    ((_ test . body)
     (compile-if-family (compile-int test env 'normal)
                        (list (undefined))
                        (compile-seq body env ctx)
                        env ctx))
    (else
     (error "malformed unless:" form))))

(define-primitive-syntax (@and form env ctx)
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

(define-primitive-syntax (@or form env ctx)
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

                                                     
                                      

         

;; BEGIN



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
    
    (inject N 'if              @if)
    (inject G 'when            @when)
    (inject G 'unless          @unless)
    (inject N 'and             @and)
    (inject N 'or              @or)
    ))
