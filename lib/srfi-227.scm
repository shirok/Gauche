;;;
;;; srfi-227 - Optional arguments
;;;

;; Its let-optionals* differ from Gauche's builtin.

(define-module srfi-227
  (use util.match)
  (export opt-lambda opt*-lambda
          let-optionals
          (rename srfi-227:let-optionals* let-optionals*))
  )
(select-module srfi-227)

;; Returns list of vars, list of bindings, and rest arg.
(define (%classify-args formals who)
  (define (scan-vars fs vars)
    (match fs
      [() (values (reverse vars) '() #f)]
      [((v init) . rest) (scan-binds fs (reverse vars) '())]
      [((v . _) . _) (err)]
      [(v . rest) (scan-vars rest (cons v vars))]
      [v (values (reverse vars) '() v)]))
  (define (scan-binds fs vars binds)
    (match fs
      [() (values vars (reverse binds) #f)]
      [((v init) . rest) (scan-binds rest vars `((,v ,init) ,@binds))]
      [(v . rest) (err)]
      [v (values vars (reverse binds) v)]))
  (define (err)
    (errorf "Invalid ~a formals: ~s" who formals))
  (scan-vars formals '()))

(define-syntax opt-lambda
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ opt-formals body . bodies)
        (receive (vars binds restarg) (%classify-args opt-formals 'opt-lambda)
          (let ([tmps (map (^_ (gensym)) binds)]
                [restvar (or restarg (gensym))])
            (quasirename r
              `(let ,(map (^[bind tmp]
                            (quasirename r
                              `(,tmp (lambda () ,(cadr bind)))))
                          binds tmps)
                 (lambda (,@vars . rest)
                   ;; this is Gauche's built-in let-optionals*
                   (let-optionals* rest
                       (,@(map (^[bind tmp] `(,(car bind) (,tmp))) binds tmps)
                        . ,restvar)
                     ,@(if restarg
                         '()
                         (quasirename r
                           `((unless (null? ,restvar)
                               (error "Too many arguments for opt-lambda")))))
                     ,body ,@bodies))))))]
       [_ (error "Malformed opt-lambda:" f)]))))

(define-syntax opt*-lambda
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ opt-formals body . bodies)
        (receive (vars binds restarg) (%classify-args opt-formals 'opt*-lambda)
          (let1 restvar (or restarg (gensym))
            (quasirename r
              `(lambda (,@vars . rest)
                 ;; this is Gauche's built-in let-optionals*
                 (let-optionals* rest (,@binds . ,restvar)
                   ,@(if restarg
                       '()
                       (quasirename r
                         `((unless (null? ,restvar)
                             (error "Too many arguments for opt*-lambda")))))
                   ,body ,@bodies)))))]
       [_ (error "Malformed opt*-lambda:" f)]))))

(define-syntax let-optionals
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ expr opt-formals body . bodies)
        (quasirename r
          `(apply (opt-lambda ,opt-formals ,body ,@bodies) ,expr))]
       [_ (error "Malformed let-optionals:" f)]))))

(define-syntax srfi-227:let-optionals*
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ expr opt-formals body . bodies)
        (quasirename r
          `(apply (opt*-lambda ,opt-formals ,body ,@bodies) ,expr))]
       [_ (error "Malformed let-optionals*:" f)]))))
