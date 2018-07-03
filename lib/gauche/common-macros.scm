;;;
;;; common-macros.scm - common macros
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

;;; Defines number of useful macros.  This file is to be autoloaded.

;; Note: This file is so fundamental that most other autoloaded files
;; depend on it.  If you modify this file, be very careful not to depend
;; on other autoloaded files, since it is easy to create circular
;; dependency.

(define-module gauche.common-macros
  (export syntax-error syntax-errorf
          push! pop! inc! dec! update!
          check-arg get-optional get-keyword*
          ^ ^_ ^a ^b ^c ^d ^e ^f ^g ^h ^i ^j ^k ^l ^m ^n ^o ^p ^q
          ^r ^s ^t ^u ^v ^w ^x ^y ^z $
          let1 if-let1 and-let1 rlet1
          let/cc begin0 fluid-let
          let-values let*-values
          values-ref values->list
          ecase
          dotimes doilst doplist while until
          guard unwind-protect
          cond-list
          assume assume-type))
(select-module gauche.common-macros)

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
;;;   TODO: Move this to core after 0.9.6.

(define-syntax syntax-error
  (er-macro-transformer
   (lambda (f r c)
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
   (lambda (f r c)
     (let ([args (map unwrap-syntax (cdr f))]
           [original ((with-module gauche.internal pair-attribute-get)
                      f 'original #f)])
       (if original
         (raise (make-compound-condition
                 (make-error (apply format/ss (car args) (cdr args)))
                 (make <compile-error-mixin> :expr original)))
         (apply errorf args))))))

;;;-------------------------------------------------------------
;;; generalized set! family

(define-syntax update!
  (syntax-rules ()
    [(_ "vars" ((var arg) ...) () proc updater val ...)
     (let ((getter proc)
           (var arg) ...)
       ((setter getter) var ... (updater val ... (getter var ...))))]
    [(_ "vars" ((var arg) ...) (arg0 arg1 ...) proc updater val ...)
     (update! "vars"
              ((var arg) ... (newvar arg0))
              (arg1 ...)
              proc updater val ...)]
    [(_ (proc arg ...) updater val ...)
     (update! "vars"
              ()
              (arg ...)
              proc updater val ...)]
    [(_ loc updater val ...)
     (set! loc (updater val ... loc))]
    [(_ . other)
     (syntax-error "malformed update!" (update! . other))]))

(define-syntax push!
  (syntax-rules ()
    [(_ "vars" ((var arg) ...) () proc val)
     (let ((getter proc)
           (var arg) ...)
       ((setter getter) var ... (cons val (getter var ...))))]
    [(_ "vars" ((var arg) ...) (arg0 arg1 ...) proc val)
     (push! "vars" ((var arg) ... (newvar arg0)) (arg1 ...) proc val)]
    [(_ (proc arg ...) val)
     (push! "vars" () (arg ...) proc val)]
    [(_ loc val)
     (set! loc (cons val loc))]
    [(_ . other)
     (syntax-error "malformed push!" (push! . other))]))

(define-syntax pop!
  (syntax-rules ()
    [(_ "vars" ((var arg) ...) () proc)
     (let ((getter proc)
           (var arg) ...)
       (let ((val (getter var ...)))
         ((setter getter) var ... (cdr val))
         (car val)))]
    [(_ "vars" ((var arg) ...) (arg0 arg1 ...) proc)
     (pop! "vars" ((var arg) ... (newvar arg0)) (arg1 ...) proc)]
    [(_ (proc arg ...))
     (pop! "vars" () (arg ...) proc)]
    [(_ loc)
     (let ((val loc))
       (set! loc (cdr val))
       (car val))]
    [(_ . other)
     (syntax-error "malformed pop!" (pop! . other))]))

(define-syntax inc!
  (syntax-rules ()
    [(_ "vars" ((var arg) ...) () proc num)
     (let ((getter proc)
           (delta num)
           (var arg) ...)
       (let ((val (getter var ...)))
         ((setter getter) var ... (+ val delta))))]
    [(_ "vars" ((var arg) ...) (arg0 arg1 ...) proc num)
     (inc! "vars" ((var arg) ... (newvar arg0)) (arg1 ...) proc num)]
    [(_ (proc arg ...) num)
     (inc! "vars" () (arg ...) proc num)]
    [(_ (proc arg ...))
     (inc! "vars" () (arg ...) proc 1)]
    [(_ loc num)
     (let ((val loc))
       (set! loc (+ val num)))]
    [(_ loc)
     (inc! loc 1)]
    [(_ . other)
     (syntax-error "malformed inc!" (inc! . other))]))

(define-syntax dec!
  (syntax-rules ()
    [(_ "vars" ((var arg) ...) () proc num)
     (let ((getter proc)
           (delta num)
           (var arg) ...)
       (let ((val (getter var ...)))
         ((setter getter) var ... (- val delta))))]
    [(_ "vars" ((var arg) ...) (arg0 arg1 ...) proc num)
     (dec! "vars" ((var arg) ... (newvar arg0)) (arg1 ...) proc num)]
    [(_ (proc arg ...) num)
     (dec! "vars" () (arg ...) proc num)]
    [(_ (proc arg ...))
     (dec! "vars" () (arg ...) proc 1)]
    [(_ loc num)
     (let ((val loc))
       (set! loc (- val num)))]
    [(_ loc)
     (dec! loc 1)]
    [(_ . other)
     (syntax-error "malformed dec!" (dec! . other))]))

;;;-------------------------------------------------------------
;;; bind construct

;; ^ == lambda
(define-syntax ^
  (syntax-rules ()
    [(^ formals . body) (lambda formals . body)]))

;; (^x . body) == (lambda (x) . body) where x in #[a-z_]
;; TODO: need to make 'lambda's hygineic!
(define-macro (^-generator var)
  (let ([name (string->symbol (string-append "^" (symbol->string var)))])
    `(define-macro (,name . body)
       `(lambda (,',var) ,@body))))
(define-macro (define-^x . vars)
  `(begin ,@(map (lambda (x) `(^-generator ,x)) vars)))
(define-^x _ a b c d e f g h i j k l m n o p q r s t u v w x y z)

(define-syntax let1                     ;single variable bind
  (syntax-rules ()
    [(let1 var exp . body)
     (let ((var exp)) . body)]))

(define-syntax if-let1                  ;like aif in On Lisp, but explicit var
  (syntax-rules ()
    [(if-let1 var exp then . else)
     (let ((var exp)) (if var then . else))]))

(define-syntax and-let1                 ;returns #f if test evaluates to #f
  (syntax-rules ()
    [(and-let1 var test exp exp2 ...)
     (let ((var test)) (and var (begin exp exp2 ...)))]))

(define-syntax let/cc                   ;as in PLT
  (syntax-rules ()
    [(let/cc var . body)
     (call/cc (lambda (var) . body))]))

(define-syntax begin0                   ;prog1 in Lisp.
  (syntax-rules ()
    [(begin0 exp exp2 ...)
     (receive r exp exp2 ... (apply values r))]))

(define-syntax rlet1                    ;begin0 + let1.  name is arguable.
  (syntax-rules ()
    [(rlet1 var exp body ...)
     (let ((var exp)) body ... var)]))

(define-syntax values-ref               ;nth-value in Lisp
  (syntax-rules ()
    ;; provide shortcut for common cases
    [(_ mv-expr 0)
     (receive (v . ignore) mv-expr v)]
    [(_ mv-expr 1)
     (receive (v0 v1 . ignore) mv-expr v1)]
    [(_ mv-expr 2)
     (receive (v0 v1 v2 . ignore) mv-expr v2)]
    [(_ mv-expr n)
     (receive v mv-expr (list-ref v n))]
    ;; The following extension is experimental; do not rely on them.
    [(_ mv-expr n m)
     (receive v mv-expr (values (list-ref v n) (list-ref v m)))]
    [(_ mv-expr n m l)
     (receive v mv-expr (values (list-ref v n) (list-ref v m) (list-ref v l)))]
    [(_ mv-expr n m l k ...)
     (receive v mv-expr
       (apply values
              (map (lambda (i) (list-ref v i)) (list n m l k ...))))]
    ))

(define-syntax values->list             ;multilple-value-list in CL
  (syntax-rules ()
    [(_ mv-expr) (receive x mv-expr x)]))

;; fluid-let written by Dorai Sitaram
;; NB: all threads shares the state of fluid global vers.
;; this is mainly for the comatibility of existing third-party code.
(define-macro fluid-let
  (lambda (varvals . body)
    (let ((vars (map car varvals))
          (vars-twins (map (lambda (ig) (gensym)) varvals))
          (swap (gensym))
          (temp (gensym)))
      `(let (,@(map list vars-twins (map cadr varvals)))
         (let ((,swap
                (lambda ()
                  ,@(map (lambda (var twin)
                           `(let ((,temp ,var))
                              (set! ,var ,twin)
                              (set! ,twin ,temp)))
                         vars vars-twins))))
           (dynamic-wind
               ,swap
               (lambda () ,@body)
               ,swap))))))

;; srfi-11, r7rs
(define-syntax let*-values
  (syntax-rules ()
    [(_ () . body)
     (let () . body)]
    [(_ ((formals init) . rest) . body)
     (receive formals init
       (let*-values rest . body))]))

(define-syntax let-values
  (er-macro-transformer
   (^[f r c]
     ;; we rename all the formals and rebind
     (let* ([bindings (cadr f)]
            [body (cddr f)]
            [formals  (map car bindings)]
            [inits    (map cadr bindings)]
            [formals* (map (^f (map* (^x (gensym (x->string x)))
                                     (^t (if (null? t) t (gensym (x->string t))))
                                     f))
                           formals)]
            [rebinds (append-map
                      (^[f f*]
                        (map* list
                              (^[a b] (if (null? a) '() (list (list a b))))
                              f f*))
                      formals formals*)])
       ;; TRANSIENT: Avoid using quasirename until 0.9.6 release, for
       ;; it autoloads macroutil.scm -> util/match.scm and create
       ;; circular dependency.
       `(,(r'let*-values) ,(map list formals* inits)
         (,(r'let) ,rebinds
          ,@body))))))

;;;-------------------------------------------------------------
;;; applications

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

;; Kludge: We already have binding of '$' in gauche module, created for
;; autoload.  Using (define-syntax $ ...) here makes a separate binding in
;; gauche.common-macros, and that makes the literal comparison of '$'
;; fail, since '$' in here refers to gauche.common-macros#$, while
;; the macro use environment will refer to gauche#$.  We use very ugly
;; hack here to workaround the issue; needs fundamental fix later.
(define-syntax %$
  (syntax-rules ()
    [($ x . xs) (%$-split (x . xs) () ())]
    [($) (syntax-error "invalid $ form" ($))]))
(set! $ %$) ; DON'T DO THIS.  Only works in the current version.

;; ($ a b $ c d $*) => ($* (c d) $ (a b))
;; ($ a b $ c d $* e f) => ((e f) $* (c d) $ (a b))
(define-syntax %$-split
  (syntax-rules ($ $*)
    ;; terminal condition
    [(_ ()   segs (e ...)) (%$-gen #f    ((e ...)           . segs))]
    [(_ ($)  segs (e ...)) (%$-gen (arg) ((e ... arg)       . segs))]
    [(_ ($*) segs (e ...)) (%$-gen arg   ((apply e ... arg) . segs))]
    ;; recurse
    [(_ ($ t ...)  segs (e ...)) (%$-split (t ...) ($  (e ...) . segs) ())]
    [(_ ($* t ...) segs (e ...)) (%$-split (t ...) ($* (e ...) . segs) ())]
    [(_ (t0 t ...) segs (e ...)) (%$-split (t ...) segs (e ... t0))]
    ))

(define-syntax %$-gen
  (syntax-rules ($ $*)
    ;; terminal condition
    [(_ #f     (seg))  seg]
    [(_ formal (seg))  (lambda formal seg)]
    ;; recurse
    [(_ type (seg0 $ (s ...) . segs))  (%$-gen type ((s ... seg0) . segs))]
    [(_ type (seg0 $* (s ...) . segs)) (%$-gen type ((apply s ... seg0) . segs))]
    ))

;;;-------------------------------------------------------------
;;; useful argument utility

(define-syntax check-arg
  (syntax-rules ()
    [(_ test arg)
     (let ((tmp arg))
       (unless (test tmp)
         (errorf "bad type of argument for ~s: ~s" 'arg tmp)))]
    ))

(define-syntax get-keyword*
  (syntax-rules ()
    [(_ key lis default)
     (let ((li lis))
       (let loop ((l li))
         (cond ((null? l) default)
               ((null? (cdr l)) (error "keyword list not even" li))
               ((eq? key (car l)) (cadr l))
               (else (loop (cddr l))))))]
    [(_ key lis) (get-keyword key lis)]))

(define-syntax get-optional
  (syntax-rules ()
    [(_ args default)
     (let ((a args))
       (if (pair? a) (car a) default))]
    [(_ . other)
     (syntax-error "badly formed get-optional" (get-optional . other))]
    ))

;;;-------------------------------------------------------------
;;; repeat construct

(define-syntax dotimes
  (syntax-rules ()
    [(_ (var n res) . body)
     (do ([limit n]
          [var 0 (+ var 1)])
         [(>= var limit) res]
       . body)]
    [(_ (var n) . body)
     (do ([limit n]
          [var 0 (+ var 1)])
         [(>= var limit) (undefined)]
       . body)]
    [(_ (n) . body)
     (let1 i n
       (cond [(<= i 0) (undefined)]
             [(infinite? i)
              (do () (#f) . body)] ;avoid unnecessary flonum calculation
             [else
              (do ([i i (- i 1)])
                  [(<= i 0) (undefined)]
                . body)]))]
    [(_ . other)
     (syntax-error "malformed dotimes" (dotimes . other))]))

(define-syntax dolist
  (syntax-rules ()
    [(_ (var lis res) . body)
     (do ([p lis (cdr p)])
         [(null? p)
          (let1 var '() res)]      ;bound var for CL compatibility
       (let1 var (car p) . body))]
    [(_ (var lis) . body)
     (do ([p lis (cdr p)])
         [(null? p) '()]
       (let1 var (car p) . body))]
    [(_ (lis) . body)
     (dolist (tmp lis) . body)]
    [(_ . other)
     (syntax-error "malformed dolist" (dolist . other))]))

(define-syntax doplist
  (syntax-rules ()
    [(_ ((k v) plis default) . body)
     (do ([p plis (cddr p)])
         [(cond [(null? p) #t]
                [(null? (cdr p))
                 (let ([k (car p)]
                       [v default])
                   . body)]
                [else #f])]
       (let ([k (car p)]
             [v (cadr p)])
         . body))]
    [(_ ((k v) plis) . body)
     (do ([p plis (cddr p)])
         [(cond [(null? p) #t]
                [(null? (cdr p)) (error "plist is not even:" plis)]
                [else #f])]
       (let ([k (car p)]
             [v (cadr p)])
         . body))]
    [(_ . other)
     (syntax-error "malformed doplist" (doplist . other))]))

(define-syntax while
  (syntax-rules (=>)
    [(_ expr guard => var . body)
     (do ((var expr expr))
         ((not (guard var)))
       . body)]
    [(_ expr => var . body)
     (do ((var expr expr))
         ((not var))
       . body)]
    [(_ expr . body)
     (do ()
         ((not expr))
       . body)]
    [(_ . other)
     (syntax-error "malformed while" (while . other))]))

(define-syntax until
  (syntax-rules (=>)
    [(_ expr guard => var . body)
     (do ((var expr expr))
         ((guard var))
       . body)]
    [(_ expr => var . body)
     (do ((var expr expr))
         (var)
       . body)]
    [(_ expr . body)
     (do ()
         (expr)
       . body)]
    [(_ . other)
     (syntax-error "malformed until" (until . other))]))

;;;-------------------------------------------------------------
;;; ecase, a la CL

(define-syntax ecase
  (syntax-rules ()
    [(ecase expr clause ...) (ecase-helper expr () () clause ...)]))

(define-syntax ecase-helper
  (syntax-rules (else =>)
    [(ecase-helper expr choices (clause ...))
     (let ([v expr])
       (case v
         clause ...
         (else (errorf "ecase test fell through: got ~s, \
                        expecting one of ~s" v 'choices))))]
    [(ecase-helper expr choices (clause ...) (else . rest))
     ;; If there's an else, ecase is the same as case.
     (case expr clause ... (else . rest))]
    [(ecase-helper expr (choice ...) (clause ...) ((v ...) . rest) more-clause ...)
     (ecase-helper expr
                   (choice ... v ...)
                   (clause ... ((v ...) . rest))
                   more-clause ...)]))

;;;-------------------------------------------------------------
;;; guard (srfi-34)

(define %reraise (with-module gauche.internal %reraise))

(define-syntax guard
  (syntax-rules ()
    [(guard (var . clauses) . body)
     (with-error-handler
         (lambda (e)
           (let ((var e))
             (%guard-rec var e . clauses)))
       (lambda () . body)
       :rewind-before #t)]))

(define-syntax %guard-rec
  (syntax-rules (else =>)
    [(%guard-rec var exc)
     ;; exception handler can return to the caller
     (%reraise exc)]
    [(%guard-rec var exc (else . exprs))
     (begin . exprs)]
    [(%guard-rec var exc (test => proc) . more)
     (let ((tmp test))
       (if tmp
         (proc tmp)
         (%guard-rec var exc . more)))]
    [(%guard-rec var exc (test . exprs) . more)
     (if test
       (begin . exprs)
       (%guard-rec var exc . more))]
    [(%guard-rec var exc other . more)
     (syntax-error "malformed guard clause" other)]))

;;;-------------------------------------------------------------
;;; unwind-protect
;;;

;; We set up exit-handler in the dynamic extent of BODY (but not in HANDLER),
;; since if BODY calls exit, the error handlers won't be called---the dynamic
;; environment is rewound upon exit, but that merely reset the error handlers.
;;
;; An alternative idea is to treat exit as if it's another kind of a condition,
;; so that guard clauses are invoked.  We tried it, but the problem is how to
;; deal with "ignore-errors" idiom, e.g. (guard (e [else #f]) body).  The exit
;; condition shouldn't be stopped in such a way.
;;
;; TODO: Current definition doesn't work when unwind-protect is used
;; within a thread that is terminated; thread termination isn't a condition
;; either.
(define-syntax unwind-protect
  (syntax-rules ()
    [(unwind-protect body handler ...)
     (let ([x (exit-handler)]
           [h (lambda () handler ...)]
           [done #f])
       (with-error-handler
           (lambda (e)
             (exit-handler x)
             (cond
              [(condition-has-type? e <serious-condition>)
               (unless done (set! done #t) (h))
               ;; NB: We don't know E is thrown by r7rs#raise or
               ;; r7rs#raise-continuable, but gauche#raise can handle both
               ;; case.
               (raise e)]
              [else
               ;; exception handler can return to the caller
               (%reraise e)]))
         (lambda ()
           (receive r
               (dynamic-wind
                 (lambda ()
                   (when done
                     (error "Attempt to reenter obsoleted dynamic environment"))
                   (exit-handler (lambda (code fmt args)
                                   (set! done #t)
                                   (h)
                                   (x code fmt args))))
                 (lambda () body)
                 (lambda () (exit-handler x)))
             (set! done #t)
             (h)
             (apply values r)))
         :rewind-before #t))]
    [(unwind-protect . other)
     (syntax-error "malformed unwind-protect" (unwind-protect . other))]))

;;; ------------------------------------------------------------
;;; conditionals
;;;

;; cond-list - a syntax to construct a list
;;
;;   (cond-list clause clause2 ...)
;;
;;   clause : (test expr ...)
;;          | (test => proc)
;;          | (test @ expr ...) ;; splice
;;          | (test => @ proc)  ;; splice

(define-syntax cond-list
  (syntax-rules (=> @)
    [(_) '()]
    [(_ (test) . rest)
     (let* ([tmp test]
            [r (cond-list . rest)])
       (if tmp (cons tmp r) r))]
    [(_ (test => proc) . rest)
     (let* ([tmp test]
            [r (cond-list . rest)])
       (if tmp (cons (proc tmp) r) r))]
    [(_ (test => @ proc) . rest)
     (let* ([tmp test]
            [r (cond-list . rest)])
       (if tmp (append (proc tmp) r) r))]
    [(_ (test @ . expr) . rest)
     (let* ([tmp test]
            [r (cond-list . rest)])
       (if tmp (append (begin . expr) r) r))]
    [(_ (test . expr) . rest)
     (let* ([tmp test]
            [r (cond-list . rest)])
       (if tmp (cons (begin . expr) r) r))]
    ))

;;;----------------------------------------------------------------
;;; assume (srfi-145) and co.
;;;

;; We might add run-time optimization switch to expand assume to nothing.
(define-syntax assume
  (syntax-rules ()
    [(_ expr)
     (unless expr
       (error (format "Invalid assumption: ~s" 'expr)))]
    [(_ expr message ...)
     (unless expr
       (error (format "Invalid assumption: ~s:" 'expr) message ...))]))

(define-syntax assume-type
  (syntax-rules ()
    [(_ expr type)
     (let1 v expr
       (unless (is-a? v type)
         (type-error 'expr type v)))]
    ))
