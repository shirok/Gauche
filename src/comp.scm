;;
;; A compiler.
;;  $Id: comp.scm,v 1.1.2.3 2004-12-31 01:03:10 shirok Exp $

(define-module gauche.compile
  (use util.match)
  )
(select-module gauche.compile)

;; Entry
(define (compile program . opts)
  (compile-int program (get-optional opts #f) 'tail))

;; compile-int:: (Program, Env, Ctx) -> [Insn]
(define (compile-int program env ctx)
  (match program
    ((op . args)
     (let ((head 
            (if (variable? op)
              (match (lookup-env op env #t)
                (('LREF detph offset) `((LREF ,depth ,offset)))
                ((? (cut is-a? <> <macro>))
                 (error "local macro not supported"))
                (else
                 ;; global variable reference.
                 (compile-varref op '())))
              (compile-int head env 'normal)))
           (argcode (compile-args args env))
           (nargs  (length args)))
       (case ctx
         ((tail)
          (append `((PRE-TAIL ,nargs))
                  argcode
                  head
                  `((TAIL-CALL ,nargs))))
         (else
          (append `((PRE-CALL ,nargs))
                  (append argcode head `((CALL ,nargs))))))))
    ((? variable?)
     (compile-varref program env))
    (else
     (if (eq? ctx 'stmt) '() (list program)))
    ))

(define (compile-varref var env)
  (let1 loc (lookup-env var env #f)
    (if (variable? loc)
      `((GREF ,loc))
      (list loc))))

(define (compile-args args env)
  (if (null? args)
    '()
    (append (compile-int (car args) env 'normal)
            '((PUSH))
            (compile-args (cdr args) env))))

;; Look up local environment
;;
(define (lookup-env var env syntax?)
  (let outer ((env env)
              (depth 0))
    (if (pair? env)
      (let ((var (if (and (identifier? var)
                          (eq? (ref var 'env) env))
                   (ref var 'name)
                   var))
            (frame (car env)))
        (if (eq? (car frame) #t)
          ;; macro binding.
          (if syntax?
            (or (find (lambda (p) (eq? var (car p))) (cdr frame))
                (outer (cdr env) (+ depth 1)))
            (outer (cdr env) (+ depth 1)))
          ;; look for variable binding.  there may be a case that
          ;; single frame contains more than one variable with the
          ;; same name (in the case like '(let* ((x 1) (x 2)) ...)'),
          ;; so we have to scan the frame until the end. */
          (let inner ((frame frame) (offset 0) (found #f))
            (cond ((null? frame)
                   (if found
                     `(LREF ,depth ,(- offset found 1))
                     (outer (cdr env) (+ depth 1))))
                  ((eq? (car frame) var)
                   (inner (cdr frame) (+ offset 1) offset))
                  (else
                   (inner (cdr frame) (+ offset 1) found))))
          ))
      ;; binding not found in local env.  returns an identifier.
      (if (and (symbol? var) (not syntax?))
        (make-identifier var '())
        var))))

;;============================================================
;; Special forms
;;

(define (syntax-if form env ctx)
  (match form
    ((if test then else)
     (let ((test-code (compile-int test env 'normal))
           (then-code (compile-int then env ctx))
           (else-code (compile-int else env ctx)))
       (if (eq? ctx tail)
         (append test-code
                 (cons '(IF) test-code)
                 else-code)
         (let1 merger (list (list 'MNOP))
           (append test-code
                   (cons '(IF) (append test-code merger))
                   (append else-code merger))))))
    ((if test then)
     (syntax-if `(if ,test ,then (undefined))))
    (else
     (error "syntax error:" form))))
       


        
;;============================================================
;; Utilities
;;

(define (variable? arg)
  (or (symbol? arg) (identifier? arg)))

(define (find pred lis)
  (let loop ((lis lis))
    (cond ((null? lis) #f)
          ((pred (car lis)))
          (else (loop (cdr lis))))))

(define (append-map proc lis)
  (apply append (map proc lis)))

