;;;
;;; libmacro.scm - built-in macros
;;;
;;;   Copyright (c) 2000-2022  Shiro Kawai  <shiro@acm.org>
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
                             ^o ^p ^q ^r ^s ^t ^u ^v ^w ^x ^y ^z $ cut cute rec
                             guard check-arg
                             push! push-unique! pop! inc! dec! update!
                             let1 if-let1 and-let1 let/cc begin0 rlet1
                             let-values let*-values define-values set!-values
                             values-ref values->list
                             assume assume-type
                             dotimes dolist do-plist doplist
                             ecase cond-list define-condition-type condition
                             unwind-protect
                             let-keywords let-keywords* let-optionals*
                             lcons lcons* llist*
                             rxmatch-let rxmatch-if rxmatch-cond rxmatch-case
                             define-compiler-macro))

;; This file defines built-in macros.
;; We need the compiler to be initialized at this stage.

;;; cond-expand

(autoload gauche.version valid-version-spec? version-satisfy?)
(autoload gauche.package find-gauche-package-description)

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
        [(identifier? req)
         (and-let1 p (assq (identifier->symbol req) features)
           ((with-module gauche.internal %check-feature-id) (car p))
           (if (null? (cdr p)) seed (cons (cadr p) seed)))]
        [(not (pair? req)) (error "Invalid cond-expand feature-id:" req)]
        [else
         (case (unwrap-syntax (car req))
           [(and) (fulfill-and (cdr req) seed)]
           [(or)  (fulfill-or  (cdr req) seed)]
           [(not) (fulfill-not (cadr req) seed)]
           [(library) (fulfill-library (cdr req) seed)]
           [(package) (fulfill-package (cdr req) seed)]
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
       (let* ([libname (car rest)]
              [modname (cond
                        [(identifier? libname) (identifier->symbol libname)]
                        [(list? libname) (library-name->module-name libname)]
                        [else (error "Invalid library name in 'library' clause \
                                      of cond-expand:" libname)])])
         ;; NB: R7RS doesn't say we load the library implicitly.
         (and (library-exists? modname)
              seed)))

     (define (fulfill-package rest seed)
       (match rest
         [(package)
          (or (equal? package 'gauche)
              (find-gauche-package-description package))]
         [(package version-spec)
          (unless (valid-version-spec? version-spec)
            (error "Invalid version spec in package clause:" `(package ,@rest)))
          (if (equal? package 'gauche)
            (and (version-satisfy? version-spec (gauche-version)) seed)
            (and-let1 gpd (find-gauche-package-description package)
              (and (version-satisfy? version-spec (~ gpd'version)) seed)))]
         [_ (error "Malformed package clause:" `(package ,@rest))]))

     (define (rec cls)
       (cond
        [(null? cls) (error "Unfulfilled cond-expand (likely missing feature to run this code)")]
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

;; transitional
(with-module gauche.internal
  (define (%check-feature-id sym)
    (when (and (#/^srfi-\d+$/ (symbol->string sym))
               (not (vm-compiler-flag-is-set? SCM_COMPILE_SRFI_FEATURE_ID)))
      (warn "Feature identifier ~a is deprecated.  Use (library ~a)\n"
            sym sym))))

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
       (and (identifier? x)
            (c (r x) unquote.)))
     (define unquote-splicing. (r'unquote-splicing))
     (define (unquote-splicing? x)
       (and (identifier? x)
            (c (r x) unquote-splicing.)))
     (define (unquote*? x)
       (and (identifier? x)
            (or (c (r x) unquote.)
                (c (r x) unquote-splicing.))))
     (define quasiquote. (r'quasiquote))
     (define (quasiquote? x)
       (and (identifier? x)
            (c (r x) quasiquote.)))
     (define cons. (r'cons))
     (define extended-cons. (r'extended-cons))
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
         [(? identifier?) `(,rename. ',obj)]
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
           (if-let1 si (pair-attribute-get objs 'source-info #f)
             (let1 orig (assoc-ref ((with-module gauche.internal %procedure-tags-alist) r)
                                   'macro-input)
               `(,extended-cons. ,xx ,yys '((source-info ,@si)
                                            ,@(cond-list
                                               [orig `(original . ,orig)]))))
             `(,cons. ,xx ,yys)))))

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
           [original (pair-attribute-get f 'original #f)])
       (if original
         (raise (make-compound-condition
                 (apply make-error (car args) (cdr args))
                 (make <compile-error-mixin> :expr original)))
         (apply error args))))))

(define-syntax syntax-errorf
  (er-macro-transformer
   (^[f r c]
     (let ([args (map unwrap-syntax (cdr f))]
           [original (pair-attribute-get f 'original #f)])
       (if original
         (raise (make-compound-condition
                 (make-error (apply format/ss (car args) (cdr args)))
                 (make <compile-error-mixin> :expr original)))
         (apply errorf args))))))

;;; ^ == lambda
(define-syntax ^
  (er-macro-transformer
   (^[f r c] (quasirename r `(lambda ,@(cdr f))))))

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

;;; $

;; Haskell-ish application.
;; The starting '$' introduces the macro.
;; Subsequent '$' delimits "one more arguments"
;; Subsequent '$*' delimits "zero or more arguments".
;;
;;  ($ f a b c)         => (f a b c)
;;  ($ f a b c $)       => (lambda (arg) (f a b c arg))
;;  ($ f $ g a b c)     => (f (g a b c))
;;  ($ f $ g a b c $)   => (lambda (arg) (f (g a b c arg)))
;;  ($ f $ g $ h a b c) => (f (g (h a b c)))
;;  ($ f a $ g b $ h c) => (f a (g b (h c)))
;;  ($ f a $ g b $ h $) => (lambda (arg) (f a (g b (h arg))))
;;
;;  ($ f a b c $*)      => (lambda args (apply f a b c args))
;;                         == (pa$ f a b c)
;;  ($ f a b $* g c d)  => (apply f a b (g c d))
;;  ($ f a b $* g c d $) => (lambda (arg) (apply f a b (g c d arg)))
;;  ($ f a b $* g c d $*) => (lambda args (apply f a b (apply g c d args)))
;;  ($ f a b $ g c d $*) => (lambda args (f a b (apply g c d args)))

(define-syntax $
  (er-macro-transformer
   (^[f r c]
     (define $.  (r '$))
     (define $*. (r '$*))

     ;; ($ x y $ ...) => (x y ($-fold ...))
     ;; last-arg has gensym'd symbol used as the outermost lambda arg.
     ;; it is passed only when lis ends with $ or $*.
     (define ($-fold lis rargs last-arg)
       (cond
        [(null? lis) (reverse rargs)]
        [(c (car lis) $.)
         (if (null? (cdr lis))
           (reverse rargs `(,last-arg))
           (reverse rargs `(,($-fold (cdr lis) '() last-arg))))]
        [(c (car lis) $*.)
         (if (null? (cdr lis))
           (cons (r'apply) (reverse rargs `(,last-arg)))
           (cons (r'apply) (reverse rargs `(,($-fold (cdr lis) '() last-arg)))))]
        [else ($-fold (cdr lis) (cons (car lis) rargs) last-arg)]))

     (if (null? (cdr f))
       (error "malformed $ form:" f)
       (let1 x (last f)
         (cond [(c x $.)
                (let1 arg (gensym)
                  `(,(r'lambda) (,arg) ,($-fold (cdr f) '() arg)))]
               [(c x $*.)
                (let1 arg (gensym)
                  `(,(r'lambda) ,arg ,($-fold (cdr f) '() arg)))]
               [else ($-fold (cdr f) '() #f)]))))))

;;; cut, cute (srfi-26)

(define-syntax cut
  (er-macro-transformer
   (^[f r c]
     (define <>. (r '<>))
     (define <...>. (r '<...>))
     ;; returns list of tmp args and list of original items in which
     ;; placeholders are replaced with tmp vars
     (define (scan args tmps elts)
       (cond [(null? args) (values (reverse tmps) (reverse elts))]
             [(not (pair? args)) (error "Malformed cut:" f)]
             [(c (car args) <...>.)
              (if (null? (cdr args))
                (let1 restarg (gensym)
                  (values (reverse tmps restarg)
                          (cons (r'apply) (reverse elts (list restarg)))))
                (error "Malformed cut:" f))]
             [(c (car args) <>.)
              (let1 t (gensym)
                (scan (cdr args) (cons t tmps) (cons t elts)))]
             [else
              (scan (cdr args) tmps (cons (car args) elts))]))
     (receive (tmps elts) (scan (cdr f) '() '())
       `(,(r'lambda) ,tmps ,elts)))))

(define-syntax cute
  (er-macro-transformer
   (^[f r c]
     (define <>. (r '<>))
     (define <...>. (r '<...>))
     ;; returns three lists: list of tmp args, list of bindings
     ;; and list of all tmp vars
     (define (scan args tmps binds elts)
       (cond [(null? args)
              (values (reverse tmps) (reverse binds) (reverse elts))]
             [(not (pair? args)) (error "Malformed cute:" f)]
             [(c (car args) <...>.)
              (if (null? (cdr args))
                (let1 restarg (gensym)
                  (values (reverse tmps restarg)
                          (reverse binds)
                          (cons (r'apply) (reverse elts (list restarg)))))
                (error "Malformed cute:" f))]
             [(c (car args) <>.)
              (let1 t (gensym)
                (scan (cdr args) (cons t tmps) binds (cons t elts)))]
             [else
              (let1 t (gensym)
                (scan (cdr args) tmps `((,t ,(car args)) ,@binds) (cons t elts)))]))
     (receive (tmps binds elts) (scan (cdr f) '() '() '())
       `(,(r'letrec) ,binds
         (,(r'lambda) ,tmps ,elts))))))

;;; rec (srfi-31)

(define-syntax rec
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ (name . formals) . body)
        (quasirename r
          `(letrec ((,name (lambda ,formals ,@body))) ,name))]
       [(_ name expr)
        (quasirename r
          `(letrec ((,name ,expr)) ,name))]
       [_ (error "malformed rec:" f)]))))

;;; guard (srfi-34)

(define-syntax guard
  (er-macro-transformer
   (^[f r c]
     (define %reraise. ((with-module gauche.internal make-identifier)
                        '%reraise
                        (find-module 'gauche.internal)
                        '()))
     (match f
       [(_ (var clause ...) body ...)
        (unless (every list? clause)
          (error "malformed guard clauses:" f))
        (if (and (pair? clause)
                 (c (r'else) (car (last clause))))
          (quasirename r
            `(with-error-handler
                 (lambda (,var)
                   (cond ,@clause))
               (lambda () ,@body)
               :rewind-before #t))
          (quasirename r
            `(with-error-handler
                 (lambda (,var)
                   (cond ,@clause
                         (else (,%reraise.))))
               (lambda () ,@body)
               :rewind-before #t)))]
       [_ (error "malformed guard:" f)]))))

;;; check-arg

(define-syntax check-arg
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ test arg)
        (quasirename r
          `(let ((tmp ,arg))
             (unless (,test tmp)
               (errorf "bad type of argument for ~s: ~s" 'arg tmp))))]
       [_ (error "malformed check-arg:" f)]))))

;;; bind construct

(define-syntax let1                     ;single variable bind
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ var exp . body) (quasirename r
                             `(let ((,var ,exp)) ,@body))]
       [_ (error "malformed let1:" f)]))))

(define-syntax if-let1                  ;like aif in On Lisp, but explicit var
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ var exp then . else) (quasirename r
                                  `(let ((,var ,exp)) (if ,var ,then ,@else)))]
       [_ (error "malformed if-let1:" f)]))))

(define-syntax and-let1                 ;returns #f if test evaluates #f
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ var test exp . more) (quasirename r
                                  `(let ((,var ,test))
                                     (and ,var (begin ,exp ,@more))))]
       [_ (error "malformed and-let1:" f)]))))

(define-syntax let/cc                   ;as in PLT
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ var . body) (quasirename r
                         `(call/cc (lambda (,var) ,@body)))]
       [_ (error "malformed let/cc:" f)]))))

(define-syntax begin0                   ;prog1 in Lisp
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ exp . more) (quasirename r
                         `(receive res ,exp ,@more (apply values res)))]
       [_ (error "malformed begin0:" f)]))))

(define-syntax rlet1                    ;begin0 + let1
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ var exp . body) (quasirename r
                             `(let ((,var ,exp)) ,@body ,var))]
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
            `(let*-values ,(map list formals* init)
               (let ,rebinds
                 ,@body))))]))))

(define-syntax let*-values
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ () . body) (quasirename r
                        `(let () ,@body))]
       [(_ ((formals init) . rest) . body)
        (quasirename r
          `(receive ,formals ,init
             (let*-values ,rest ,@body)))]))))

(define-syntax define-values
  (er-macro-transformer
   (^[f r c]
     ;; (define-values (a b ... z) expr)
     ;; => (begin (define a (undefined))
     ;;           ...
     ;;           (define z (receive (a' ... z') expr
     ;;             (set! a a') ...
     ;;             z')
     (match f
       [(_ formals expr)
        (match formals
          [()  ; allowed in r7rs
           (quasirename r
             `(define ,(gensym) (receive ,(gensym) ,expr #f)))]
          [(v) ; trivial case
           (quasirename r
             `(define ,v ,expr))]
          [(_ ...)
           (let ([vs (drop-right formals 1)]
                 [tmps (map (^_ (gensym)) formals)])
             `(,(r'begin)
               ,@(map (^v `(,(r'define) ,v (,(r'undefined)))) vs)
               (,(r'define) ,(last formals)
                (,(r'receive) ,tmps ,expr
                 ,@(map (^[v t] `(,(r'set!) ,v ,t)) vs tmps)
                 ,(last tmps)))))]
          [(? pair?) ; improper list
           (let* ([vs (drop-right formals 0)]
                  [tmps (map* (^_ (gensym)) (^_ (gensym)) formals)])
             `(,(r'begin)
               ,@(map (^v `(,(r'define) ,v (,(r'undefined)))) vs)
               (,(r'define) ,(cdr (last-pair formals))
                (,(r'receive) ,tmps ,expr
                 ,@(map (^[v t] `(,(r'set!) ,v ,t)) vs tmps)
                 ,(cdr (last-pair tmps))))))]
          [v   ; single variable
           (let1 tmp (gensym)
             (quasirename r
               `(define ,v (receive ,tmp ,expr ,tmp))))])]))))

(define-syntax set!-values
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ (var ...) expr)
        (let1 tmps (map (^_ (gensym)) var)
          (quasirename r
            `(receive ,tmps ,expr
               ,@(map (^[v t] (quasirename r `(set! ,v ,t))) var tmps)
               (undefined))))]
       [_ (error "Malformed set!-values:" f)]))))

(define-syntax values-ref
  (er-macro-transformer
   (^[f r c]
     ;; we provide shortcut for common cases
     (match f
       [(_ mv-expr n)
        (if (and (exact-integer? n) (<= n 5))
          (let1 vars (map (^_ (gensym)) (iota (+ n 1)))
            (quasirename r
              `(receive (,@vars . _) ,mv-expr ,(last vars))))
          (quasirename r
            `(receive vals ,mv-expr (list-ref vals ,n))))]
       [(_ mv-expr n m)
        (if (and (exact-integer? n) (<= n 5)
                 (exact-integer? m) (<= m 5))
          (let* ([vars (map (^_ (gensym)) (iota (+ (max n m) 1)))]
                 [vn (list-ref vars n)]
                 [vm (list-ref vars m)])
            (quasirename r
              `(receive (,@vars . _) ,mv-expr (values ,vn ,vm))))
          (quasirename r
            `(receive vals ,mv-expr (values (list-ref vals ,n)
                                           (list-ref vals ,m)))))]
       [(_ mv-expr ns ...)
        (if (null? ns)
          (error "malformed values-ref:" f)
          (if (and (every exact-integer? ns)
                   (every (cut <= <> 5) ns))
            (let* ([vars (map (^_ (gensym)) (iota (+ (apply max ns) 1)))]
                   [rvars (map (^k (list-ref vars k)) ns)])
              (quasirename r
                `(receive (,@vars . _) ,mv-expr (values ,@rvars))))
            (quasirename r
              `(receive vals ,mv-expr
                 (apply values (map (^[i] (list-ref vals i))
                                    (list ,@(cons n ns))))))))]))))

(define-syntax values->list
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ mv-expr) (quasirename r
                      `(receive x ,mv-expr x))]))))

;;; generalized set! family

(define-syntax push!
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ (proc arg ...) val)
        (let1 vars (map (^_ (gensym)) arg)
          (quasirename r
            `(let [(getter ,proc) ,@(map list vars arg)]
               ((setter getter) ,@vars (cons ,val (getter ,@vars))))))]
       [(_ loc val)
        (quasirename r
          `(set! ,loc (cons ,val ,loc)))]
       [_ (error "malformed push!:" f)]))))

(define-syntax push-unique!
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ p val) (quasirename r `(push-unique! ,p ,val eqv?))]
       [(_ (proc arg ...) val equal)
        (let1 vars (map (^_ (gensym)) arg)
          (quasirename r
            `(let ([val ,val]
                   [getter ,proc]
                   ,@(map list vars arg))
               (let1 vs (getter ,@vars)
                 (unless (member val vs ,equal)
                   ((setter getter) ,@vars (cons val vs)))))))]
       [(_ loc val equal)
        (quasirename r
          `(let ([val ,val]
                 [vs ,loc])
             (unless (member val vs ,equal)
               (set! ,loc (cons val vs)))))]
       [_ (error "malformed push-unique!:" f)]))))

(define-syntax pop!
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ (proc arg ...))
        (let1 vars (map (^_ (gensym)) arg)
          (quasirename r
            `(let ([getter ,proc] ,@(map list vars arg))
               (let1 val (getter ,@vars)
                 ((setter getter) ,@vars (cdr val))
                 (car val)))))]
       [(_ loc)
        (quasirename r
          `(let1 val ,loc
             (set! ,loc (cdr val))
             (car val)))]
       [_ (error "malformed pop!:" f)]))))

(define-syntax inc!
  (er-macro-transformer
   (^[f r c]
     (define (gen proc arg delta)
       (let1 vars (map (^_ (gensym)) arg)
         (quasirename r
           `(let ([getter ,proc] ,@(map list vars arg))
              ((setter getter) ,@vars (+ (getter ,@vars) ,delta))))))
     (match f
       [(_ (proc arg ...) delta) (gen proc arg delta)]
       [(_ (proc arg ...))       (gen proc arg 1)]
       [(_ loc delta) (quasirename r
                        `(set! ,loc (+ ,loc ,delta)))]
       [(_ loc)       (quasirename r
                        `(set! ,loc (+ ,loc 1)))]
       [_ (error "malformed inc!:" f)]))))

(define-syntax dec!
  (er-macro-transformer
   (^[f r c]
     (define (gen proc arg delta)
       (let1 vars (map (^_ (gensym)) arg)
         (quasirename r
           `(let ([getter ,proc] ,@(map list vars arg))
              ((setter getter) ,@vars (- (getter ,@vars) ,delta))))))
     (match f
       [(_ (proc arg ...) delta) (gen proc arg delta)]
       [(_ (proc arg ...))       (gen proc arg 1)]
       [(_ loc delta) (quasirename r
                        `(set! ,loc (- ,loc ,delta)))]
       [(_ loc)       (quasirename r
                        `(set! ,loc (- ,loc 1)))]
       [_ (error "malformed dec!:" f)]))))

(define-syntax update!
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ (proc arg ...) updater val ...)
        (let1 vars (map (^_ (gensym)) arg)
          (quasirename r
            `(let ([getter ,proc] ,@(map list vars arg))
               ((setter getter) ,@vars (,updater ,@val (getter ,@vars))))))]
       [(_ loc updater val ...)
        (quasirename r
          `(set! ,loc (,updater ,@val ,loc)))]
       [_ (error "malformed update!:" f)]))))

;;; assume (srfi-145) and co.

;; We might add run-time optimization switch to expand assume to nothing.
(define-syntax assume
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ expr)
        (quasirename r
          `(or ,expr
               (error (format "Invalid assumption: ~s" ',expr))))]
       [(_ expr msg . objs)
        (quasirename r
          `(or ,expr
               (error ,msg ,@objs)))]))))

;; This will eventually folded into the compiler.  The argumet must be
;; a literal <type>.
(define-syntax assume-type
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ expr type)
        (quasirename r
          `(let1 v ,expr
             (if (of-type? v ,type)
               v
               (type-error ',expr ,type v))))]
       [(_ expr type msg . objs)
        (quasirename r
          `(let1 v ,expr
             (if (of-type? v ,type)
               v
               (error ,msg ,@objs))))]))))

;;; repeat construct

(define-syntax dotimes
  (er-macro-transformer
   (^[f r c]
     (define (expand var n res body)
       (let1 limit (gensym "limit")
         (quasirename r
           `(do ([,limit ,n]
                 [,var 0 (+ ,var 1)])
                [(>= ,var ,limit) ,res]
            ,@body))))
     (match f
       [(_ (var n res) . body) (expand var n res body)]
       [(_ (var n) . body)     (expand var n (undefined) body)]
       [(_ (n) . body)
        ;; gauche extension.  we special-case when n is inf.0 to avoid
        ;; unnecessary flonum calculation
        (quasirename r
          `(let1 i ,n
             (cond [(<= i 0) (undefined)]
                   [(infinite? i) (do () (#f) ,@body)]
                   [else
                    ,(expand (gensym) n (undefined) body)])))]
       [_ (error "Malformed dotimes:" f)]))))

(define-syntax dolist
  (er-macro-transformer
   (^[f r c]
     (define (expand var lis res body)
       (let1 p (gensym "p")
         (quasirename r
           `(do ([,p ,lis (cdr ,p)])
                [(null? ,p)
                 (let1 ,var '() ,res)] ;bound var for CL compatibility
              (let1 ,var (car ,p) ,@body)))))
     (match f
       [(_ (var lis res) . body) (expand var lis res body)]
       [(_ (var lis) . body)     (expand var lis (undefined) body)]
       [(_ (lis) . body)         (expand (gensym) lis (undefined) body)]
       [_ (error "Malformed dolist:" f)]))))

(define-syntax do-plist
  (er-macro-transformer
   (^[f r c]
     (define (expand k v plis body default)
       (quasirename r
         `(do ([e (^[,k ,v] ,@body)]
               [p ,plis (cddr p)])
              [(cond [(null? p) #t]
                     [(null? (cdr p)) (e (car p) ,default)]
                     [else #f])]
            (e (car p) (cadr p)))))
     (match f
       [(_ ((k v) plis default) . body)
        (expand k v plis body default)]
       [(_ ((k v) plis) . body)
        (expand k v plis body
                (quasirename r `(error "plist is not even:" ,plis)))]
       [_ (error "Malformed do-plist:" f)]))))

(define-syntax doplist do-plist)        ;for backward compatibility

;;; ecase, a la CL

(define-syntax ecase
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ expr clause ...)
        (or (and-let* ([ (pair? clause) ]
                       [last-clause (last clause)]
                       [ (pair? last-clause) ]
                       [ (c (r'else) (car last-clause)) ])
              ;; If there's an else, ecase is the same as case.
              (quasirename r
                `(case ,expr ,@clause)))
            (let1 choices (append-map car clause)
              (quasirename r
                `(let ([v ,expr])
                   (case v
                     ,@clause
                     (else (errorf "ecase test fell through: got ~s, \
                                    expecting one of ~s" v ',choices)))))))]
       [_ (error "Malformed ecase:" f)]))))


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
            `(let* ([tmp ,test]
                    [r (cond-list ,@rest)])
               (if tmp (cons tmp r) r)))]
         [(_ (test (? (cut c <> =>.)) proc) . rest)
          (quasirename r
            `(let* ([tmp ,test]
                    [r (cond-list ,@rest)])
               (if tmp (cons (,proc tmp) r) r)))]
         [(_ (test (? (cut c <> =>.)) (? (cut c <> @.)) proc) . rest)
          (quasirename r
            `(let* ([tmp ,test]
                    [r (cond-list ,@rest)])
               (if tmp (append (,proc tmp) r) r)))]
         [(_ (test (? (cut c <> @.)) expr ...) . rest)
          (quasirename r
            `(let* ([tmp ,test]
                    [r (cond-list ,@rest)])
               (if tmp (append (begin ,@expr) r) r)))]
         [(_ (test expr ...) . rest)
          (quasirename r
            `(let* ([tmp ,test]
                    [r (cond-list ,@rest)])
               (if tmp (cons (begin ,@expr) r) r)))]
         )))))

;;; srfi-35 condition macros

;; we extend srfi-35 to allow #f as predicate and accessors, as well as
;; omitting accessors.

(define-syntax define-condition-type
  (er-macro-transformer
   (^[f r c]
     (match (cdr f)
       [(name super pred . field-specs)
        (define (badfield-error field)
          (error "bad field spec for define-condition-type:" field))
        (define (scan-specs specs slots readers)
          (match specs
            [() (emit-defs slots readers)]
            [((field #f) . rest)
             (scan-specs rest (cons field slots) readers)]
            [((field) . rest)
             (scan-specs rest (cons field slots) readers)]
            [((field reader) . rest)
             (scan-specs rest (cons field slots)
                         (cons (quasirename r
                                 `(define (,reader obj)
                                    (condition-ref obj ',field)))
                               readers))]
            [_ (badfield-error (car specs))]))
        (define (emit-defs slots readers)
          (quasirename r
            `(begin
               (define-class ,name (,super)
                 ,(map (^s (quasirename r
                             `(,s :init-keyword ',(make-keyword s))))
                       slots)
                 :metaclass <condition-meta>)
               ,@readers
               ,@(if pred
                   (quasirename r
                     `((define (,pred obj)
                        (condition-has-type? obj ,name))))
                   '()))))
        (scan-specs field-specs '() '())]
       ))))

(define-syntax condition
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ type-field-binding ...)
        (quasirename r
          `(make-compound-condition
            ,@(map (match-lambda
                     [(type (field expr) ...)
                      (quasirename r
                        `(make-condition
                          ,type
                          ,@(append-map (^[f e] `(',f ,e)) field expr)))]
                     [_ (error "malformed condition:" f)])
                   type-field-binding)))]
       [_ (error "malformed condition:" f)]))))


;;; unwind-protect

;; HANDLERS are called when BODY exits, either normally or abnormally.  This
;; is convenient if you have cleanup tasks you want to execute after BODY.
;;
;; Note that we don't call HANDLERS if control jumps out from BODY by
;; calling a continuation captured outside of unwind-protect; in such case
;; the control may return into BODY later.
;;
;; Once HANDLERS are executed, the resources needed in BODY may no longer
;; available.  However, we don't prohibit reentering BODY again, since
;; there may be a delimited continuation that can be reexecuted within
;; the dynamic environment of BODY.   If the control falls out from BODY
;; (either normally or abnormally) second time or later, HANDLERS will
;; no longer be executed.
;;
;; We also tweak exit-handler parameter inside the dyanmic scope of BODY
;; to ensure HANDLERS are executed even if BODY calls exit.
;; (An alternative idea is to treat exit as if it's another kind of a
;; condition, so that guard clauses are invoked.  We tried it, but the
;; problem is how to deal with "ignore-errors" idiom,
;; e.g. (guard (e [else #f]) body).  The exit condition shouldn't be
;; stopped in such a way.)
;;
;; TODO: Current definition doesn't work when unwind-protect is used
;; within a thread that is terminated; thread termination isn't a condition
;; either.

;; The source-info argument isn't used now, but could be useful for
;; troubleshooting.  We use keyword arguments so that we can enhance
;; %unwind-protect later without breaking existing precompiled code
;; that alreay has call to %unwind-protect embedded as the result of
;; expansion.
(define-syntax unwind-protect
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ body handlers ...)
        ;; Some trick to hide %unwind-protect in gauche.internal
        (let* ([sinfo (debug-source-info f)]
               [lambda. (r 'lambda)]
               [%unwind-protect. ((with-module gauche.internal make-identifier)
                                  '%unwind-protect
                                  (find-module 'gauche.internal)
                                  '())])
          (if sinfo
            `(,%unwind-protect. (,lambda. () ,body) (,lambda. () ,@handlers)
                                ':source-info ',sinfo)
            `(,%unwind-protect. (,lambda. () ,body) (,lambda. () ,@handlers))))]
       [_ (error "Malformed unwind-protect:" f)]))))

(select-module gauche.internal)
(define (%unwind-protect thunk handlers :key (source-info #f))
  (let ([x (exit-handler)]
        [done #f])
    (define (cleanup)
      (unless done
        (set! done #t)
        (handlers)))
    (with-error-handler
        (lambda (e)
          (exit-handler x)
          (cond
           [(condition-has-type? e <serious-condition>)
            (cleanup)
            ;; NB: We don't know E is thrown by r7rs#raise or
            ;; r7rs#raise-continuable, but gauche#raise can handle both
            ;; case.
            (raise e)]
           [else
            ;; exception handler can return to the caller
            ((with-module gauche.internal %reraise))]))
      (lambda ()
        (receive r
            (dynamic-wind
              (lambda ()
                (exit-handler (lambda (code fmt args)
                                (cleanup)
                                (x code fmt args))))
              thunk
              (lambda () (exit-handler x)))
          (cleanup)
          (apply values r)))
      :rewind-before #t)))

;;; Extended argument parsing

;; Extended lambda formals (:optional, :key, :rest etc) are
;; expanded into the call of let-optionals* and let-keywords*
;; macros within the compiler.  Eventually the handling of the
;; optional and keyword arguments will be built in the VM.

(select-module gauche)
(define-syntax let-optionals*
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ arg specs . body)
        (define (rec arg vars&inits rest)
          (cond
           [(null? (cdr vars&inits))
            (quasirename r
              `((let ((,(caar vars&inits)
                       (if (null? ,arg) ,(cdar vars&inits) (car ,arg)))
                      ,@(if (null? rest)
                          '()
                          `((,rest (if (null? ,arg) '() (cdr ,arg))))))
                  ,@body)))]
           [else
            (let ([g (gensym)]
                  [v (caar vars&inits)]
                  [i (cdar vars&inits)])
              ;; NB: if the compiler were more clever, we could use receive
              ;; or apply to make (null? ,arg) test once.  For now, testing it
              ;; twice is faster.
              (quasirename r
                `((let ((,v (if (null? ,arg) ,i (car ,arg)))
                        (,g (if (null? ,arg) '() (cdr ,arg))))
                    ,@(rec g (cdr vars&inits) rest)))))]))
        (let1 g (gensym)
          (quasirename r
            `(let ((,g ,arg))
               ,@(rec g (map* (^s
                               (cond [(and (pair? s) (pair? (cdr s)) (null? (cddr s)))
                                      (cons (car s) (cadr s))]
                                     [(or (symbol? s) (identifier? s))
                                      (cons s `(,(r'undefined)))]
                                     [else (error "malformed let-optionals* bindings:"
                                                  specs)]))
                              (^_ '()) ; ignore last cdr of dotted list
                              specs)
                      (cdr (last-pair specs))))))]
       [_ (error "Malformed let-optionals*:" f)]))))

(select-module gauche)
(define-syntax let-keywords
  (er-macro-transformer
   (^[f r c] ((with-module gauche.internal %let-keywords-rec) f r 'let))))

(define-syntax let-keywords*
  (er-macro-transformer
   (^[f r c] ((with-module gauche.internal %let-keywords-rec) f r 'let*))))

(select-module gauche.internal)
(use util.match)
(define (%let-keywords-rec form rename %let)
  ;; arg is either:
  ;;   identidfir
  ;;   (identifier default-expr)
  ;;   (identifier keyword default-expr)
  ;; returns (values identifier keyword default-expr)
  (define (triplet var&default)
    (or (and-let* ([ (list? var&default) ]
                   [var (unwrap-syntax (car var&default))]
                   [ (symbol? var) ])
          (case (length var&default)
            [(2) (values (car var&default)
                         (make-keyword var)
                         (cadr var&default))]
            [(3) (values (car var&default)
                         (unwrap-syntax (cadr var&default))
                         (caddr var&default))]
            [else #f]))
        (and-let* ([var (unwrap-syntax var&default)]
                   [ (symbol? var) ])
          (values var (make-keyword var) (undefined)))
        (error "bad binding form in let-keywords" var&default)))
  ;; Loop over var-specs, returns
  ;;  (values vars        ; list of local variables to bound
  ;;          keys        ; list of keywords to detect
  ;;          defaults    ; list of expressions for the default
  ;;          tmps        ; list of temporary variables
  ;;          restvar)    ; #f, #t or identifier
  (define (process-specs specs)
    (let loop ((specs specs)
               (vars '()) (keys '()) (defaults '()) (tmps '()))
      (define (finish restvar)
        (values (reverse! vars)
                (reverse! keys)
                (reverse! defaults)
                (reverse! tmps)
                restvar))
      (cond [(null? specs) (finish #f)]
            [(pair? specs)
             (receive (var key default) (triplet (car specs))
               (loop (cdr specs)
                     (cons var vars)
                     (cons key keys)
                     (cons default defaults)
                     (cons (gensym) tmps)))]
            [else (finish (or specs #t))])))
  (match form
    [(_ arg specs . body)
     (let ([argvar (gensym "args")]
           [loop (gensym "loop")]
           [let. (rename %let)])
       (receive (vars keys defaults tmps restvar) (process-specs specs)
         (quasirename rename
           `(let
             ,loop ((,argvar ,arg)
                    ,@(if (boolean? restvar) '() `((,restvar '())))
                    ,@(map (cut list <> (undefined)) tmps))
             (cond
              [(null? ,argvar)
               (,let. ,(map (^[var tmp default]
                              (quasirename rename
                                `(,var (if (undefined? ,tmp) ,default ,tmp))))
                            vars tmps defaults)
                      ,@body)]
              [(null? (cdr ,argvar))
               (error "keyword list not even" ,argvar)]
              [else
               (case (unwrap-syntax-1 (car ,argvar))
                 ,@(map (^[key]
                          (quasirename rename
                            `((,key)
                              (,loop (cddr ,argvar)
                                     ,@(if (boolean? restvar)
                                         '()
                                         `(,restvar))
                                     ,@(map (^[k t] (if (eq? key k)
                                                      (quasirename rename
                                                        `(cadr ,argvar))
                                                      t))
                                            keys tmps)))))
                        keys)
                 (else
                  ,(cond [(eq? restvar #t)
                          (quasirename rename
                            `(,loop (cddr ,argvar) ,@tmps))]
                         [(eq? restvar #f)
                          (quasirename rename
                            `(begin
                               (errorf "unknown keyword ~S" (car ,argvar))
                               (,loop (cddr ,argvar) ,@tmps)))]
                         [else
                          (quasirename rename
                            `(,loop
                              (cddr ,argvar)
                              (list* (car ,argvar) (cadr ,argvar) ,restvar)
                              ,@tmps))])))])))))]
    [_ (errorf "Malformed ~a: ~S"
               (if (eq? %let 'let) 'let-keywords 'let-keywords*)
               form)]))

;;; lseq
(select-module gauche)
(define-syntax lcons
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ x y)
        (quasirename r
          `(,(with-module gauche.internal %lcons) ,x (lambda () ,y)))]
       [(_ x y attrs)
        (quasirename r
          `(,(with-module gauche.internal %lcons) ,x (lambda () ,y) ,attrs))]))))

(define-syntax lcons*
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ x) x]
       [(_ x y) (quasirename r `(lcons ,x ,y))]
       [(_ x y z ...) (quasirename r
                        `(cons ,x (lcons* ,y ,@z)))]))))

(define-syntax llist* lcons*)

;;; rxmatch-* macros

;; rxmatch-let expr (var ...) form ...
(define-syntax rxmatch-let
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ expr (var ...) form ...)
        (quasirename r
          `(if-let1 m ,expr
             (let ,(map (^[v i] (quasirename r
                                  `(,v (rxmatch-substring m ,i))))
                        var (iota (length var)))
               ,@form)
             (error "rxmatch-let: match failed:" ',expr)))]
       [_ (error "malformed rxmatch-let:" f)]))))

;; rxmatch-if expr (var ...) then else
(define-syntax rxmatch-if
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ expr (var ...) then else)
        (quasirename r
          `(if-let1 m ,expr
             (let ,(append-map (^[v i]
                                 (if v
                                   (quasirename r
                                     `((,v (rxmatch-substring m ,i))))
                                   '()))
                               var (iota (length var)))
               ,then)
             ,else))]
       [_ (error "malformed rxmatch-if:" f)]))))

;; rxmatch-cond (expr bind form ...) ... [(else form ...)]
(define-syntax rxmatch-cond
  (er-macro-transformer
   (^[f r c]
     (define (test.? x) (c (r'test) x))
     (define (else.? x) (c (r'else) x))
     (define (=>.? x)   (c (r'=>) x))
     (let loop ([clauses (cdr f)])
       (match clauses
         [() `(,(r'undefined))]
         [(((? else.?) form ...) . rest)
          (if (null? rest)
            (quasirename r `(begin ,@form))
            (error "extra stuff after else clause in rxmatch-cond:" f))]
         [(((? test.?) expr (? =>.?) proc) . rest)
          (quasirename r
            `(if-let1 x ,expr
               (,proc x)
               ,(loop rest)))]
         [(((? test.?) expr form ...) . rest)
          (quasirename r
            `(if ,expr
               (begin ,@form)
               ,(loop rest)))]
         [((matchexp (var ...) form ...) . rest)
          (quasirename r
            `(rxmatch-if ,matchexp ,var
               (begin ,@form)
               ,(loop rest)))]
         [_ (error "bad clause in rxmatch-cond:" (car clause))])))))

;; rxmatch-case exp clause ...
;;   clause: (re bind form ...)
;;           (test pred form ...)
;;           (test pred => proc)
;;           (else form ...)
;;           (else => proc)
(define-syntax rxmatch-case
  (er-macro-transformer
   (^[f r c]
     (define (test.? x) (c (r'test) x))
     (define (else.? x) (c (r'else) x))
     (define (=>.? x)   (c (r'=>) x))
     (define tmp. (r'tmp))
     (define strp. (r'strp))
     (define (loop clauses)
       (match clauses
         [() `(,(r'undefined))]
         [(((? else.?) (? =>.?) proc) . rest)
          (if (null? rest)
            `(,proc ,tmp.)
            (error "extra stuff after else clause in rxmatch-case:" f))]
         [(((? else.?) form ...) . rest)
          (if (null? rest)
            (quasirename r `(begin ,@form))
            (error "extra stuff after else clause in rxmatch-case:" f))]
         [(((? test.?) pred (? =>.?) proc) . rest)
          (quasirename r
            `(if-let1 x (,pred ,tmp.)
               (,proc x)
               ,(loop rest)))]
         [(((? test.?) pred form ...) . rest)
          (quasirename r
            `(if (,pred ,tmp.)
               (begin ,@form)
               ,(loop rest)))]
         [((re (var ...) form ...) . rest)
          (quasirename r
            `(rxmatch-if (and ,strp. (rxmatch ,re ,tmp.)) ,var
               (begin ,@form)
               ,(loop rest)))]
         [_ (error "bad clause in rxmatch-case:" (car clause))]))
     (match f
       [(_ exp . clauses)
        (quasirename r
          `(let* ([,tmp. ,exp]
                  [,strp. (string? ,tmp.)])
             ,(loop clauses)))]
       [_ (error "malformed rxmatch-case:" f)]))))


;;;
;;; OBSOLETED - Tentative compiler macro
;;;

(select-module gauche)
;; TRANSIENT: Remove by 1.0
(define-macro (define-compiler-macro name xformer-spec)
  (error "define-compiler-macro is obsoleted.  Use define-hybrid-syntax."))
