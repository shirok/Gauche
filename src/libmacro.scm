;;;
;;; libmacro.scm - built-in macros
;;;
;;;   Copyright (c) 2000-2018  Shiro Kawai  <shiro@acm.org>
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
(use util.match)
(declare (keep-private-macro cond-expand quasirename 
                             syntax-error syntax-errorf
                             ^ ^_ ^a ^b ^c ^d ^e ^f ^g ^h ^i ^j ^k ^l ^m ^n
                             ^o ^p ^q ^r ^s ^t ^u ^v ^w ^x ^y ^z rec
                             push! pop! inc! dec! update!
                             let1 if-let1 and-let1 let/cc begin0 rlet1
                             let-values let*-values values-ref values->list
                             assume assume-type cond-list
                             define-compiler-macro))

;; This file defines built-in macros.
;; We need the compiler to be initialized at this stage.

;;; cond-expand

(define-syntax cond-expand
  (er-macro-transformer
   (^[f r c]
     (define features ((with-module gauche.internal cond-features)))
     (define use. (r 'use))
     (define begin. (r 'begin))
     (define else. (r 'else))

     ;; Check feature requirement.  Returns #f if requirement is not
     ;; satisfied.  Returns a list of features to be use'd if requirement
     ;; is satisfied (it can be an emptylist, if the requirement is fulfilled
     ;; by Gauche built-in features).
     (define (fulfill? req seed)
       (cond
        [(identifier? req) (fulfill? (identifier->symbol req) seed)]
        [(symbol? req)
         (and-let1 p (assq req features)
           (if (null? (cdr p)) seed (cons (cadr p) seed)))]
        [(not (pair? req)) (error "Invalid cond-expand feature-id:" req)]
        [else
         (case (unwrap-syntax (car req))
           [(and) (fulfill-and (cdr req) seed)]
           [(or)  (fulfill-or  (cdr req) seed)]
           [(not) (fulfill-not (cadr req) seed)]
           [(library) (fulfill-library (cdr req) seed)]
           [else (error "Invalid cond-expand feature expression:" req)])]))

     (define (fulfill-and reqs seed)
       (if (null? reqs)
         seed
         (and-let1 c1 (fulfill? (car reqs) seed)
           (fulfill-and (cdr reqs) c1))))

     (define (fulfill-or reqs seed)
       (if (null? reqs)
         #f
         (or (fulfill? (car reqs) seed)
             (fulfill-or (cdr reqs) seed))))

     (define (fulfill-not req seed)
       (if (fulfill? req '()) #f seed))

     (define (fulfill-library rest seed)
       (unless (null? (cdr rest))
         (error "Invalid feature requirement:" `(library ,@rest)))
       (let1 modname (library-name->module-name (car rest))
         (and (library-exists? modname) seed)))

     (define (rec cls)
       (cond
        [(null? cls) (error "Unfulfilled cond-expand:" cls)]
        [(not (pair? (car cls)))
         (error "Bad clause in cond-expand:" (car cls))]
        [(c (r (caar cls)) else.)
         (if (null? (cdr cls))
           `(,begin. ,@(cdar cls))
           (error "Misplaced else clause in cond-expand:" (car cls)))]
        [(fulfill? (caar cls) '())
         => (^[uses]
              `(,begin. ,@(map (^[mod] `(,use. ,mod)) uses)
                        ,@(cdar cls)))]
        [else (rec (cdr cls))]))

     (rec (cdr f)))))

;;; quasirename

;; NB: The walk code has the same structure as quasiquote expander; the
;; output is different, however, and it's not so straightforward to refactor
;; them.  We'll eventually make a generic walker, but for now we have
;; two separately.

(define-syntax quasirename
  (er-macro-transformer
   (^[f r c]
     (define unquote. (r'unquote))
     (define (unquote? x)
       (and (or (symbol? x) (identifier? x))
            (c (r x) unquote.)))
     (define unquote-splicing. (r'unquote-splicing))
     (define (unquote-splicing? x)
       (and (or (symbol? x) (identifier? x))
            (c (r x) unquote-splicing.)))
     (define (unquote*? x)
       (and (or (symbol? x) (identifier? x))
            (or (c (r x) unquote.)
                (c (r x) unquote-splicing.))))
     (define quasiquote. (r'quasiquote))
     (define (quasiquote? x)
       (and (or (symbol? x) (identifier? x))
            (c (r x) quasiquote.)))
     (define cons. (r'cons))
     (define list. (r'list))
     (define append. (r'append))
     (define vector. (r'vector))
     (define list->vector. (r'list->vector))
     (define let. (r'let))
     (define rename. (r'rename))

     ;; In the context where there's no outer list to which we intersperse to.
     (define (quasi obj level)
       (match obj
         [((? quasiquote?) x)
          (let1 xx (quasi x (+ level 1))
            (if (eq? x xx)
              obj
              `(,list. 'quasiquote ,xx)))]
         [((? unquote?) x)
          (if (zero? level)
            x
            (let1 xx (quasi x (- level 1))
              (if (eq? x xx)
                obj
                `(,list. ','unquote ,xx))))]
         [((? unquote*? op) . xs) ;valid unquote is already handled
          (if (zero? level)
            (errorf "invalid ~a form in this context: ~s" op obj)
            (let1 xxs (quasi* xs (- level 1))
              (if (eq? xs xxs)
                obj
                `(,cons. ',op ,xxs))))]
         [(? pair?)       (quasi* obj level)]
         [(? vector?)     (quasi-vector obj level)]
         [(or (? symbol?) (? identifier?)) `(,rename. ',obj)]
         [_  `',obj]))

     ;; In the spliceable context.  objs is always a list.
     ;; NB: we already excluded toplevel quasiquote and unquote
     (define (quasi* objs level)
       (match objs
         [(((? unquote*? op) . xs) . ys)
          (let1 yys (quasi* ys level)
            (if (zero? level)
              ((if (unquote? op) build build@) xs yys)
              (let1 xxs (quasi* xs (- level 1))
                (if (and (eq? xs xxs) (eq? ys yys))
                  obj
                  `(,cons. (,cons. ',op ,xxs) ,yys)))))]
         [((? unquote*?) . _) ;`(... . ,xs) `(... . ,@xs)
          (quasi objs level)]
         [((? vector? x) . ys) (quasi-cons objs quasi-vector x ys level)]
         [(x . ys)             (quasi-cons objs quasi x ys level)]
         [_                    (quasi objs level)]))

     (define (build objs rest)
       (match objs
         [() rest]
         [(x . xs) `(,cons. ,x ,(build xs rest))]))

     (define (build@ objs rest)
       (match objs
         [() rest]
         [(x . xs) `(,append. ,x ,(build@ xs rest))]))

     (define (quasi-cons objs quasi-car x ys level)
       (let ([xx (quasi-car x level)]
             [yys (quasi* ys level)])
         (if (and (eq? x xx) (eq? ys yys))
           objs
           `(,cons. ,xx ,yys))))

     (define (quasi-vector obj level)
       (if (vector-has-splicing? obj)
         `(,list->vector. ,(quasi* (vector->list obj) level))
         (let* ([need-construct? #f]
                [elts (map (^[elt] (rlet1 ee (quasi elt level)
                                     (unless (eq? ee elt)
                                       (set! need-construct? #t))))
                           (vector->list obj))])
           (if need-construct?
             `(,vector. ,@elts)
             obj))))

     (define (vector-has-splicing? obj)
       (let loop ((i 0))
         (cond [(= i (vector-length obj)) #f]
               [(and (pair? (vector-ref obj i))
                     (unquote-splicing? (car (vector-ref obj i))))]
               [else (loop (+ i 1))])))

     ;; TRANSIENT: For the backward compatibility with legacy quasirename form.
     ;; *quasirename-mode* is set according to GAUCHE_QUASIRENAME_MODE env var.
     ;; see below.  We use global-variable-ref, since while compiling
     ;; this with 0.9.7 *quasirename-mode* isn't defined.
     (define qmode
       (global-variable-ref 'gauche.internal '*quasirename-mode* #f))
     (define (legacy-message f)
       (if-let1 srcinfo (debug-source-info f)
         (format "Legacy quasirename form (~a:~a): ~s" 
                 (car srcinfo) (cadr srcinfo) f)
         (format "Legacy quasirename form: ~s" f)))
  
     (match f
       [(_ rr ((? quasiquote? qq) ff))
        (if (eq? qmode 'legacy)
          `(,let. ((,rename. ,rr))
              ,(quasi `(,qq ,ff) 0))
          `(,let. ((,rename. ,rr))
              ,(quasi ff 0)))]
       [(_ rr ff)                       ; old format
        (case qmode
          [(warn) (let1 srcinfo (debug-source-info f)
                    (warn "~a\n" (legacy-message f)))]
          [(strict) (let1 srcinfo (debug-source-info f)
                      (error (legacy-message f)))])
        `(,let. ((,rename. ,rr))
           ,(quasi ff 0))]
       [_ (error "malformed quasirename:" f)]))))

(with-module gauche.internal
  (define *quasirename-mode*
    (and-let1 m (sys-getenv "GAUCHE_QUASIRENAME_MODE")
      (cond [(equal? m "legacy") 'legacy]
            [(equal? m "compatible") 'compatible]
            [(equal? m "strict") 'strict]
            [(equal? m "warn") 'warn]
            [else (warn "Invalid GAUCHE_QUASIRENAME_MODE value; must be one \
                         of compatible, legacy, warn or strict, but got: ~s\n"
                        m)]))))

;;; syntax-error msg arg ...
;;; syntax-errorf fmtstr arg ...
;;;   Signal an error at compile time.
;;;   These are typically used as a result of expansion of syntax-rules
;;;   macro; er-macro or legacy macro can directly call error/errorf so
;;;   there's no point to use syntax-error.  Then, the 'original attribute
;;;   of the form contains the macro input that caused syntax error.
;;;   We extract that and throw a compound condition, so that the
;;;   error message will include the macro input that directly caused
;;;   this error.

(define-syntax syntax-error
  (er-macro-transformer
   (^[f r c]
     (let ([args (map unwrap-syntax (cdr f))]
           [original ((with-module gauche.internal pair-attribute-get)
                      f 'original #f)])
       (if original
         (raise (make-compound-condition
                 (apply make-error (car args) (cdr args))
                 (make <compile-error-mixin> :expr original)))
         (apply error args))))))

(define-syntax syntax-errorf
  (er-macro-transformer
   (^[f r c]
     (let ([args (map unwrap-syntax (cdr f))]
           [original ((with-module gauche.internal pair-attribute-get)
                      f 'original #f)])
       (if original
         (raise (make-compound-condition
                 (make-error (apply format/ss (car args) (cdr args)))
                 (make <compile-error-mixin> :expr original)))
         (apply errorf args))))))

;;; ^ == lambda
(define-syntax ^
  (er-macro-transformer
   (^[f r c] (quasirename r (lambda ,@(cdr f))))))

;; (^x . body) == (lambda (x) . body) where x in #[a-z_]
;; TODO: need to make 'lambda's hygineic!
(define-macro (^-generator var)
  (let ([name (string->symbol (string-append "^" (symbol->string var)))])
    `(define-syntax ,name
       (er-macro-transformer
        (^[f r c]
          `(,,'(r'lambda) (,',var) ,@,'(cdr f)))))))
(define-macro (define-^x . vars)
  `(begin ,@(map (lambda (x) `(^-generator ,x)) vars)))
(define-^x _ a b c d e f g h i j k l m n o p q r s t u v w x y z)

;;; rec (srfi-31)

(define-syntax rec
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ (name . formals) . body)
        (quasirename r
          (letrec ((,name (lambda ,formals ,@body))) ,name))]
       [(_ name expr)
        (quasirename r
          (letrec ((,name ,expr)) ,name))]
       [_ (error "malformed rec:" f)]))))

;;; bind construct

(define-syntax let1                     ;single variable bind
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ var exp . body) (quasirename r
                             (let ((,var ,exp)) ,@body))]
       [_ (error "malformed let1:" f)]))))

(define-syntax if-let1                  ;like aif in On Lisp, but explicit var
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ var exp then . else) (quasirename r
                                  (let ((,var ,exp)) (if ,var ,then ,@else)))]
       [_ (error "malformed if-let1:" f)]))))

(define-syntax and-let1                 ;returns #f if test evaluates #f
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ var test exp . more) (quasirename r
                                  (let ((,var ,test)) 
                                    (and ,var (begin ,exp ,@more))))]
       [_ (error "malformed and-let1:" f)]))))

(define-syntax let/cc                   ;as in PLT
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ var . body) (quasirename r
                         (call/cc (lambda (,var) ,@body)))]
       [_ (error "malformed let/cc:" f)]))))

(define-syntax begin0                   ;prog1 in Lisp
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ exp . more) (quasirename r
                         (receive res ,exp ,@more (apply values res)))]
       [_ (error "malformed begin0:" f)]))))

(define-syntax rlet1                    ;begin0 + let1
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ var exp . body) (quasirename r
                             (let ((,var ,exp)) ,@body ,var))]
       [_ (error "malformed rlet1:" r)]))))

(define-syntax let-values
  (er-macro-transformer
   (^[f r c]
     ;; we rename all the formals and rebind
     (match f
       [(_ ((formals init) ...) . body)
        (let* ([formals* (map (^f (map* (^x (gensym (x->string x)))
                                     (^t (if (null? t) t (gensym (x->string t))))
                                     f))
                              formals)]
               [rebinds (append-map
                         (^[f f*]
                           (map* list
                                 (^[a b] (if (null? a) '() (list (list a b))))
                                 f f*))
                         formals formals*)])
          (quasirename r
            (let*-values ,(map list formals* init)
              (let ,rebinds
                ,@body))))]))))

(define-syntax let*-values
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ () . body) (quasirename r
                        (let () ,@body))]
       [(_ ((formals init) . rest) . body)
        (quasirename r
          (receive ,formals ,init
            (let*-values ,rest ,@body)))]))))

(define-syntax values-ref
  (er-macro-transformer
   (^[f r c]
     ;; we provide shortcut for common cases
     (match f
       [(_ mv-expr n)
        (if (and (exact-integer? n) (<= n 5))
          (let1 vars (map (^_ (gensym)) (iota (+ n 1)))
            (quasirename r
              (receive (,@vars . _) ,mv-expr ,(last vars))))
          (quasirename r
            (receive vals ,mv-expr (list-ref vals ,n))))]
       [(_ mv-expr n m)
        (if (and (exact-integer? n) (<= n 5)
                 (exact-integer? m) (<= m 5))
          (let* ([vars (map (^_ (gensym)) (iota (+ (max n m) 1)))]
                 [vn (list-ref vars n)]
                 [vm (list-ref vars m)])
            (quasirename r
              (receive (,@vars . _) ,mv-expr (values ,vn ,vm))))
          (quasirename r
            (receive vals ,mv-expr (values (list-ref vals ,n)
                                           (list-ref vals ,m)))))]
       [(_ mv-expr ns ...)
        (if (null? ns)
          (error "malformed values-ref:" f)
          (if (and (every exact-integer? ns)
                   (every (cut <= <> 5) ns))
            (let* ([vars (map (^_ (gensym)) (iota (+ (apply max ns) 1)))]
                   [rvars (map (^k (list-ref vars k)) ns)])
              (quasirename r
                (receive (,@vars . _) ,mv-expr (values ,@rvars))))
            (quasirename r
              (receive vals ,mv-expr
                (apply values (map (^[i] (list-ref vals i)) 
                                   (list ,@(cons n ns))))))))]))))

(define-syntax values->list
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ mv-expr) (quasirename r
                      (receive x ,mv-expr x))]))))
        
;;; generalized set! family

(define-syntax push!
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ (proc arg ...) val)
        (let1 vars (map (^_ (gensym)) arg)
          (quasirename r
            (let [(getter ,proc) ,@(map list vars arg)]
              ((setter getter) ,@vars (cons ,val (getter ,@vars))))))]
       [(_ loc val)
        (quasirename r
          (set! ,loc (cons ,val ,loc)))]
       [_ (error "malformed push!:" f)]))))

(define-syntax pop!
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ (proc arg ...))
        (let1 vars (map (^_ (gensym)) arg)
          (quasirename r
            (let ([getter ,proc] ,@(map list vars arg))
              (let1 val (getter ,@vars)
                ((setter getter) ,@vars (cdr val))
                (car val)))))]
       [(_ loc)
        (quasirename r
          (let1 val ,loc
            (set! ,loc (cdr val))
            (car val)))]
       [_ (error "malformed pop!:" f)]))))

(define-syntax inc!
  (er-macro-transformer
   (^[f r c]
     (define (gen proc arg delta)
       (let1 vars (map (^_ (gensym)) arg)
         (quasirename r
           (let ([getter ,proc] ,@(map list vars arg))
             ((setter getter) ,@vars (+ (getter ,@vars) ,delta))))))
     (match f
       [(_ (proc arg ...) delta) (gen proc arg delta)]
       [(_ (proc arg ...))       (gen proc arg 1)]
       [(_ loc delta) (quasirename r
                        (set! ,loc (+ ,loc ,delta)))]
       [(_ loc)       (quasirename r
                        (set! ,loc (+ ,loc 1)))]
       [_ (error "malformed inc!:" f)]))))

(define-syntax dec!
  (er-macro-transformer
   (^[f r c]
     (define (gen proc arg delta)
       (let1 vars (map (^_ (gensym)) arg)
         (quasirename r
           (let ([getter ,proc] ,@(map list vars arg))
             ((setter getter) ,@vars (- (getter ,@vars) ,delta))))))
     (match f
       [(_ (proc arg ...) delta) (gen proc arg delta)]
       [(_ (proc arg ...))       (gen proc arg 1)]
       [(_ loc delta) (quasirename r
                        (set! ,loc (- ,loc ,delta)))]
       [(_ loc)       (quasirename r
                        (set! ,loc (- ,loc 1)))]
       [_ (error "malformed dec!:" f)]))))

(define-syntax update!
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ (proc arg ...) updater val ...)
        (let1 vars (map (^_ (gensym)) arg)
          (quasirename r
            (let ([getter ,proc] ,@(map list vars arg))
              ((setter getter) ,@vars (,updater ,@val (getter ,@vars))))))]
       [(_ loc updater val ...)
        (quasirename r
          (set! ,loc (,updater ,@val ,loc)))]
       [_ (error "malformed update!:" f)]))))

;;; assume (srfi-145) and co.

;; We might add run-time optimization switch to expand assume to nothing.
(define-syntax assume
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ expr . objs)
        (quasirename r
          (unless ,expr
            (error (format "Invalid assumption: ~s" ',expr) ,@objs)))]))))

(define-syntax assume-type
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ expr type)
        (quasirename r
          (let1 v ,expr
            (unless (is-a? v ,type)
              (type-error 'expr ,type v))))]))))

;;; cond-list - a syntax to construct a list

;;   (cond-list clause clause2 ...)
;;
;;   clause : (test expr ...)
;;          | (test => proc)
;;          | (test @ expr ...) ;; splice
;;          | (test => @ proc)  ;; splice

(define-syntax cond-list
  (er-macro-transformer
   (^[f r c]
     (let ([=>. (r '=>)]
           (@.  (r '@)))
       (match f
         [(_) '()]
         [(_ (test) . rest)
          (quasirename r
            (let* ([tmp ,test]
                   [r (cond-list ,@rest)])
              (if tmp (cons tmp r) r)))]
         [(_ (test (? (cut c <> =>.)) proc) . rest)
          (quasirename r
            (let* ([tmp ,test]
                   [r (cond-list ,@rest)])
              (if tmp (cons (,proc tmp) r) r)))]
         [(_ (test (? (cut c <> =>.)) (? (cut c <> @.)) proc) . rest)
          (quasirename r
            (let* ([tmp ,test]
                   [r (cond-list ,@rest)])
              (if tmp (append (,proc tmp) r) r)))]
         [(_ (test (? (cut c <> @.)) expr ...) . rest)
          (quasirename r
            (let* ([tmp ,test]
                   [r (cond-list ,@rest)])
              (if tmp (append (begin ,@expr) r) r)))]
         [(_ (test expr ...) . rest)
          (quasirename r
            (let* ([tmp ,test]
                   [r (cond-list ,@rest)])
              (if tmp (cons (begin ,@expr) r) r)))]
         )))))


;;;
;;; OBSOLETED - Tentative compiler macro 
;;;


;; TRANSIENT: Remove by 1.0
(define-macro (define-compiler-macro name xformer-spec)
  (error "define-compiler-macro is obsoleted.  Use define-inline/syntax."))
