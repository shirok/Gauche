;;;
;;; (srfi 226 fluid)
;;;

(define-module srfi.226.fluid
  (use util.match)
  (export set!
          define-fluid
          define-thread-fluid
          define-fluidified
          fluid-let
          fluid-let*
          fluid-parameter
          ))
(select-module srfi.226.fluid)

;; NB: We use experimental id-macro-dispatcher.
;;   https://github.com/shirok/Gauche/issues/1295

(define-syntax fluid-parameter (make-id-macro-dispatcher))

(define (%gen r id param-expr)
  (assume (identifier? id)
    "define-fluid requires an identifier, but got: " (unwrap-syntax id))
  (let1 pname (gensym (x->string id))
    (quasirename r
      `(begin
         (define ,pname ,param-expr)
         (define-syntax ,id
           (make-id-transformer
            (er-macro-transformer
             (^[ff rr cc]
               (define (set!? x) (cc (rr'set!) x))
               (define (fluid-parameter? x) (cc (rr'fluid-parameter) x))
               (match ff
                 [((? set!?) _ e)
                  (quasirename rr
                    `(independently (,',pname ,e)))] ;to void result value
                 [((? fluid-parameter?) _) ',pname]
                 [(? identifier?) `(,',pname)]
                 [_ (error "invalud use of fluid:" ff)])))))))))

(define-syntax define-fluid
  (er-macro-transformer
   (^[f r c]
     (define (gen id make-param-args)
       (%gen r id (quasirename r `(make-shared-parameter ,@make-param-args))))
     (match f
       [(_ id expr) (gen id `(,expr))]
       [(_ id expr conv) (gen id `(,expr ,conv))]))))

(define-syntax define-thread-fluid
  (er-macro-transformer
   (^[f r c]
     (define (gen id make-param-args)
       (%gen r id (quasirename r `(make-thread-parameter ,@make-param-args))))
     (match f
       [(_ id expr) (gen id `(,expr))]
       [(_ id expr conv) (gen id `(,expr ,conv))]))))

(define-syntax define-fluidified
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ id param-expr) (%gen r id param-expr)]))))

(define-syntax fluid-let
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ (((? identifier? id) init) ...) body ...)
        (let1 binds (map (^[id init] (quasirename r
                                       `((fluid-parameter ,id) ,init)))
                         id init)
          (quasirename r
            `(parameterize ,binds ,@body)))]
       [_ (error "malformed fluid-let:" f)]))))


(define-syntax fluid-let*
  (syntax-rules ()
    [(_ () body ...) (begin body ...)]
    [(_ ((var init) bind ...) body ...)
     (fluid-let ((var init)) (fluid-let* (bind ...) body ...))]))
