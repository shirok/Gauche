;;
;; util.match - Andrew Wright's pattern matching macro.
;;
;;   Ported to Gauche by Shiro Kawai.  Public domain.
;;   Modified to work with Gauche's object system instead of the original
;;   structure model.
;;

(define-module util.match
  (export match
          match-lambda
          match-lambda*
          match-let
          match-let*
          match-letrec
          match-let1
          match-define
          match:$-ref
          |setter of match:$-ref|
          match:every
          match:error))
(select-module util.match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern Matching Syntactic Extensions for Scheme
;;
(define match:version "Version 1.18, July 17, 1995")
;;
;; Report bugs to wright@research.nj.nec.com.  The most recent version of
;; this software can be obtained by anonymous FTP from ftp.nj.nec.com
;; in file pub/wright/match.tar.Z.  Be sure to set "type binary" when
;; transferring this file.
;;
;; Written by Andrew K. Wright, 1993 (wright@research.nj.nec.com).
;; Adapted from code originally written by Bruce F. Duba, 1991.
;; This package also includes a modified version of Kent Dybvig's
;; define-structure (see Dybvig, R.K., The Scheme Programming Language,
;; Prentice-Hall, NJ, 1987).
;;
;; This software is in the public domain.  Feel free to copy,
;; distribute, and modify this software as desired.  No warranties
;; nor guarantees of any kind apply.  Please return any improvements
;; or bug fixes to wright@research.nj.nec.com so that they may be included
;; in future releases.
;;
;; This macro package extends Scheme with several new expression forms.
;; Following is a brief summary of the new forms.  See the associated
;; LaTeX documentation for a full description of their functionality.
;;
;; [SK]: check out Gauche texinfo manual for modified spec.
;;
;;         match expressions:
;;
;; exp ::= ...
;;       | (match exp clause ...)
;;       | (match-lambda clause ...)
;;       | (match-lambda* clause ...)
;;       | (match-let ((pat exp) ...) body)
;;       | (match-let* ((pat exp) ...) body)
;;       | (match-letrec ((pat exp) ...) body)
;;       | (match-define pat exp)
;;
;; clause ::= (pat body) | (pat => exp)
;;
;;         patterns:                       matches:
;;
;; pat ::= identifier                      anything, and binds identifier
;;       | _                               anything
;;       | ()                              the empty list
;;       | #t                              #t
;;       | #f                              #f
;;       | string                          a string
;;       | number                          a number
;;       | character                       a character
;;       | keyword                         a keyword
;;       | 'sexp                           an s-expression
;;       | 'symbol                         a symbol (special case of s-expr)
;;       | (pat_1 ... pat_n)               list of n elements
;;       | (pat_1 ... pat_n . pat_{n+1})   list of n or more
;;       | (pat_1 ... pat_n pat_n+1 ooo)   list of n or more, each element
;;                                           of remainder must match pat_n+1
;;       | #(pat_1 ... pat_n)              vector of n elements
;;       | #(pat_1 ... pat_n pat_n+1 ooo)  vector of n or more, each element
;;                                           of remainder must match pat_n+1
;;       | #&pat                           box
;;       | ($ struct-name pat_1 ... pat_n) a structure
;;       | (= field pat)                   a field of a structure
;;       | (and pat_1 ... pat_n)           if all of pat_1 thru pat_n match
;;       | (or pat_1 ... pat_n)            if any of pat_1 thru pat_n match
;;       | (not pat_1 ... pat_n)           if all pat_1 thru pat_n don't match
;;       | (? predicate pat_1 ... pat_n)   if predicate true and all of
;;                                           pat_1 thru pat_n match
;;       | (set! identifier)               anything, and binds setter
;;       | (get! identifier)               anything, and binds getter
;;       | `qp                             a quasi-pattern
;;
;; ooo ::= ...                             zero or more
;;       | ___                             zero or more
;;       | ..k                             k or more
;;       | __k                             k or more
;;
;;         quasi-patterns:                 matches:
;;
;; qp  ::= ()                              the empty list
;;       | #t                              #t
;;       | #f                              #f
;;       | string                          a string
;;       | number                          a number
;;       | character                       a character
;;       | identifier                      a symbol
;;       | (qp_1 ... qp_n)                 list of n elements
;;       | (qp_1 ... qp_n . qp_{n+1})      list of n or more
;;       | (qp_1 ... qp_n qp_n+1 ooo)      list of n or more, each element
;;                                           of remainder must match qp_n+1
;;       | #(qp_1 ... qp_n)                vector of n elements
;;       | #(qp_1 ... qp_n qp_n+1 ooo)     vector of n or more, each element
;;                                           of remainder must match qp_n+1
;;       | #&qp                            box
;;       | ,pat                            a pattern
;;       | ,@pat                           a pattern
;;
;; The names (quote, quasiquote, unquote, unquote-splicing, ?, _, $,
;; and, or, not, set!, get!, ..., ___) cannot be used as pattern variables.
;;
;;
;; match:error-control controls what code is generated for failed matches.
;; Possible values:
;;  'unspecified - do nothing, ie., evaluate (cond [#f #f])
;;  'fail - call match:error, or die at car or cdr
;;  'error - call match:error with the unmatched value
;;  'match - call match:error with the unmatched value _and_
;;             the quoted match expression
;; match:error-control is set by calling match:set-error-control with
;; the new value.
;;
;; match:error is called for a failed match.
;; match:error is set by calling match:set-error with the new value.
;;
;; End of user visible/modifiable stuff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; [SK] NOTE NOTE NOTE
;;; match macro is unhygienic, and it is possible that it has undesired
;;; interference with hygienic maros.  I put certain ad-hoc hack in it
;;; to avoid some unexpected behavior.  It is still a hack, and ultimately
;;; this should be rewritten in hygienic macro.  Here's some points:
;;;
;;; - In patterns, treat symbols and identifiers the same as pattern vars.
;;; - When inserting references to global variables defined in this module,
;;;   use identifiers instead of symbols.
;;;
;;; For the latter I needed to use make-identifier which is a private
;;; procedure in gauche.internal.  If you're reading this code out of
;;; curiousity, DO NOT USE IT in your code.  The identifier stuff may be
;;; changed greatly when I implement proper low-level hygienic stuff.
;;;
;;; The plan is that we revise the code to use er-macro in a couple of
;;; releases (we need to release er-macro support first, for util.match
;;; is used to build HEAD version of source so in this module we can only
;;; depend on the features available in the current release of Gauche.
;;;

(define (%match-id sym)
  ((with-module gauche.internal make-identifier) sym (current-module) '()))

(define match.         (%match-id 'match))
(define match-let*.    (%match-id 'match-let*))
(define match:error.   (%match-id 'match:error))
(define match-lambda*. (%match-id 'match-lambda*))
(define match-letrec.  (%match-id 'match-letrec))
(define match-define.  (%match-id 'match-define))
(define match:length>=?. (%match-id 'match:length>=?))

(define lambda.        ((with-module gauche.internal make-identifier) 'lambda
                        (find-module 'null) '()))

(define-inline (match:length>=? n) (cut length>=? <> n))

;;; [SK] End of black magic

(define match:error
  (lambda (val . args)
    (errorf "~a: no matching clause for ~a"
            (string-join (map x->string args) " ")
            val)))

(define match:every every) ; alias gauche's every, in case the user of
                           ; util.match isn't inheriting gauche.

(define match:syntax-err
  (lambda (obj msg) (error msg obj)))

(define match:error-control 'error)
(define match:set-error-control
  (lambda (v) (set! match:error-control v)))

(define-inline (symid? x) (or (symbol? x) (identifier? x)))

(define match:disjoint-predicates
  (cons 'null
        '(pair?
          symid?
          boolean?
          number?
          string?
          char?
          procedure?
          vector?)))

;; These two methods are used for positional match of objects using $.
;; The default is to take the order of class-slots, which is suffice for
;; most cases.  A metaclass can override these if desired.
(define-method match:$-ref ((class <class>) fnum obj)
  (and-let1 slot (list-ref (class-slots class) fnum #f)
    (slot-ref obj (slot-definition-name slot))))
(define-method (setter match:$-ref) ((class <class>) fnum obj val)
  (and-let1 slot (list-ref (class-slots class) fnum #f)
    (slot-set! obj (slot-definition-name slot) val)))

(define (genmatch x clauses match-expr)
  (let* ((eb-errf (error-maker match-expr))
         (blist (car eb-errf))
         (plist (map (lambda (c)
                       (let* ((x (bound (validate-pattern (car c))))
                              (p (car x))
                              (bv (cadr x))
                              (bindings (caddr x))
                              (code (gensym))
                              (fail (and (pair? (cdr c))
                                         (pair? (cadr c))
                                         (equal? (caadr c) '=>)
                                         (pair? (cdadr c))
                                         (symid? (cadadr c))
                                         (null? (cddadr c))
                                         (pair? (cddr c))
                                         (cadadr c)))
                              (bv2 (if fail (cons fail bv) bv))
                              (body (if fail (cddr c) (cdr c))))
                         (set! blist
                               (cons `(,code (,lambda. ,bv2 ,@body))
                                     (append bindings blist)))
                         (list p code bv (and fail (gensym)) #f)))
                     clauses))
         (code (gen x '() plist (cdr eb-errf) (gensym))))
    (unreachable plist match-expr)
    `(let ,blist
       ,code)))

(define (genletrec pat exp body match-expr)
  (let* ((eb-errf (error-maker match-expr))
         (x (bound (validate-pattern pat)))
         (p (car x))
         (bv (cadr x))
         (bindings (caddr x))
         (code (gensym))
         (plist (list (list p code bv #f #f)))
         (x (gensym))
         (m (gen x '() plist (cdr eb-errf) (gensym)))
         (gs (map (lambda (_) (gensym)) bv)))
    (unreachable plist match-expr)
    `(letrec (,@(map (lambda (v) `(,v #f)) bv)
              (,x ,exp)
              (,code (,lambda. ,gs
                       ,@(map (lambda (v g) `(set! ,v ,g)) bv gs)
                       ,@body))
              ,@bindings
              ,@(car eb-errf))
       ,m)))

(define (gendefine pat exp match-expr)
  (let* ((eb-errf (error-maker match-expr))
         (x (bound (validate-pattern pat)))
         (p (car x))
         (bv (cadr x))
         (bindings (caddr x))
         (code (gensym))
         (plist (list (list p code bv #f #f)))
         (x (gensym))
         (m (gen x '() plist (cdr eb-errf) (gensym)))
         (gs (map (lambda (_) (gensym)) bv)))
    (unreachable plist match-expr)
    `(begin ,@(map (lambda (v) `(define ,v #f)) bv)
            (let ((,x ,exp)
                  (,code (,lambda. ,gs
                           ,@(map (lambda (v g) `(set! ,v ,g)) bv gs)
                           (cond (#f #f))))
                  ,@bindings
                  ,@(car eb-errf))
              ,m))))

(define (symbolize x)
  (cond [(symbol? x) x]
        [(identifier? x) (unwrap-syntax x)]
        [else x]))

(define (pattern-var? x)
  (and-let* ([x (cond [(symbol? x) x]
                      [(identifier? x) (unwrap-syntax x)]
                      [else #f])]
             [ (not (dot-dot-k? x)) ])
    (not (memq x '(quasiquote quote unquote unquote-splicing
                   ? _ $ struct @ object = and or not set! get! ... ___)))))

(define (dot-dot-k? s)
  (and (symid? s)
       (if (member s '(... ___))
         0
         (and-let* ([s (if (symbol? s) s (unwrap-syntax s))]
                    [m (#/^(?:\.\.|__)(\d+)$/ (symbol->string s))])
           (string->number (m 1))))))

(define (error-maker match-expr)
  (cond
   ((eq? match:error-control 'unspecified)
    (cons '() (lambda (x) `(cond (#f #f)))))
   ((memq match:error-control '(error fail))
    (cons '() (lambda (x) `(,match:error. ,x))))
   ((eq? match:error-control 'match)
    (let ((errf (gensym))
          (arg (gensym)))
      (cons `((,errf (,lambda. (,arg)
                       (,match:error. ,arg ',match-expr))))
            (lambda (x) `(,errf ,x)))))
   (else (match:syntax-err
          '(unspecified error fail match)
          "invalid value for match:error-control, legal values are"))))

(define (unreachable plist match-expr)
  (for-each
   (lambda (x)
     (if (not (car (cddddr x)))
       (warn "unreachable pattern ~a in ~a~%" (car x) match-expr)))
   plist))

(define (validate-pattern pattern)
  (define (simple? x)
    (or (string? x) (boolean? x) (char? x) (number? x) (null? x)
        ;; This last term is to support both disjoint-keyword and
        ;; keyword-is-symbol runtime.  After we fully migrated to
        ;; keyword-is-symbol, just remove this term.
        (and (not (symbol? :x)) (keyword? x))))
  (define (ordinary p)
    (let ((cons-ordinary (lambda (x y) (cons (ordinary x) (ordinary y)))))
      (cond
       ((simple? p) p)
       ((equal? p '_) '_)
       ((pattern-var? p) p)
       ((pair? p)
        (case (symbolize (car p))
          ((quasiquote)
           (if (and (pair? (cdr p)) (null? (cddr p)))
             (quasi (cadr p))
             (cons-ordinary (car p) (cdr p))))
          ((quote)
           (if (and (pair? (cdr p)) (null? (cddr p)))
             p
             (cons-ordinary (car p) (cdr p))))
          ((?)
           (if (and (pair? (cdr p)) (list? (cddr p)))
             `(? ,(cadr p) ,@(map ordinary (cddr p)))
             (cons-ordinary (car p) (cdr p))))
          ((=)
           (if (and (pair? (cdr p)) (pair? (cddr p)) (null? (cdddr p)))
             `(= ,(cadr p) ,(ordinary (caddr p)))
             (cons-ordinary (car p) (cdr p))))
          ((and)
           (if (and (list? (cdr p)) (pair? (cdr p)))
             `(and ,@(map ordinary (cdr p)))
             (cons-ordinary (car p) (cdr p))))
          ((or)
           (if (and (list? (cdr p)) (pair? (cdr p)))
             `(or ,@(map ordinary (cdr p)))
             (cons-ordinary (car p) (cdr p))))
          ((not)
           (if (and (list? (cdr p)) (pair? (cdr p)))
             `(not ,@(map ordinary (cdr p)))
             (cons-ordinary (car p) (cdr p))))
          (($ struct)
           (if (and (pair? (cdr p)) (symid? (cadr p)) (list? (cddr p)))
             `($ ,(cadr p) ,@(map ordinary (cddr p)))
             (cons-ordinary (car p) (cdr p))))
          ((@ object)
           (if (and (pair? (cdr p)) (symid? (cadr p)) (list? (cddr p))
                    (every (lambda (p) (and (list? p) (= (length p) 2)))
                           (cddr p)))
             `(object ,(cadr p) ,@(map (lambda (p)
                                         (list (car p) (ordinary (cadr p))))
                                       (cddr p)))
             (cons-ordinary (car p) (cdr p))))
          ((set!)
           (if (and (pair? (cdr p)) (pattern-var? (cadr p)) (null? (cddr p)))
             p
             (cons-ordinary (car p) (cdr p))))
          ((get!)
           (if (and (pair? (cdr p)) (pattern-var? (cadr p)) (null? (cddr p)))
             p
             (cons-ordinary (car p) (cdr p))))
          ((unquote unquote-splicing)
           (cons-ordinary (car p) (cdr p)))
          (else
           (if (and (pair? (cdr p))
                    (dot-dot-k? (cadr p))
                    (null? (cddr p)))
             `(,(ordinary (car p)) ,(cadr p))
             (cons-ordinary (car p) (cdr p))))))
       ((vector? p)
        (let* ((pl (vector->list p))
               (rpl (reverse pl)))
          (list->vector
           (if (and (not (null? rpl)) (dot-dot-k? (car rpl)))
             (reverse (cons (car rpl) (map ordinary (cdr rpl))))
             (map ordinary pl)))))
       (else (match:syntax-err
              pattern
              "syntax error in pattern")))))
  (define (quasi p)
    (let ((cons-quasi (lambda (x y) (cons (quasi x) (quasi y)))))
      (cond
       ((simple? p) p)
       ((symid? p) `',p)
       ((pair? p)
        (cond
         ((eq? (car p) 'unquote)
          (if (and (pair? (cdr p)) (null? (cddr p)))
            (ordinary (cadr p))
            (cons-quasi (car p) (cdr p))))
         ((and (pair? (car p))
               (equal? (caar p) 'unquote-splicing)
               (pair? (cdar p))
               (null? (cddar p)))
          (if (null? (cdr p))
            (ordinary (cadar p))
            (append (ordlist (cadar p)) (quasi (cdr p)))))
         ((and (pair? (cdr p))
               (dot-dot-k? (cadr p))
               (null? (cddr p)))
          `(,(quasi (car p)) ,(cadr p)))
         (else (cons-quasi (car p) (cdr p)))))
       ((vector? p)
        (let* ((pl (vector->list p))
               (rpl (reverse pl)))
          (list->vector
           (if (and (not (null? rpl))
                    (dot-dot-k? (car rpl)))
             (reverse (cons (car rpl) (map quasi (cdr rpl))))
             (map quasi pl)))))
       (else
        (match:syntax-err pattern "syntax error in pattern")))))
  (define (ordlist p)
    (cond
     ((null? p) '())
     ((pair? p) (cons (ordinary (car p)) (ordlist (cdr p))))
     (else (match:syntax-err pattern
                             "invalid use of unquote-splicing in pattern"))))
  (ordinary pattern))

(define (bound pattern)
  (define pred-bodies '())

  (define (bound p a k)
    (cond
     ((eq? '_ p) (k p a))
     ((symid? p)
      (when (memq p a)
        (match:syntax-err pattern "duplicate variable in pattern"))
      (k p (cons p a)))
     ((and (pair? p) (equal? 'quote (car p)))
      (k p a))
     ((and (pair? p) (equal? '? (car p)))
      (cond
       ((not (null? (cddr p)))
        (bound `(and (? ,(cadr p)) ,@(cddr p)) a k))
       ((or (not (symid? (cadr p)))
            (memq (cadr p) a))
        (let ((g (gensym)))
          (push! pred-bodies `(,g ,(cadr p)))
          (k `(? ,g) a)))
       (else (k p a))))
     ((and (pair? p) (eq? '= (car p)))
      (cond
       ((or (not (symid? (cadr p)))
            (memq (cadr p) a))
        (let ((g (gensym)))
          (push! pred-bodies `(,g ,(cadr p)))
          (bound `(= ,g ,(caddr p)) a k)))
       (else (bound (caddr p) a (lambda (p2 a) (k `(= ,(cadr p) ,p2) a))))))
     ((and (pair? p) (eq? 'and (car p)))
      (bound* (cdr p) a (lambda (p a) (k `(and ,@p) a))))
     ((and (pair? p) (eq? 'or (car p)))
      (bound (cadr p) a
             (lambda (first-p first-a)
               (let or* ((plist (cddr p))
                         (k (lambda (plist) (k `(or ,first-p ,@plist)
                                               first-a))))
                 (if (null? plist)
                   (k plist)
                   (bound
                    (car plist)
                    a
                    (lambda (car-p car-a)
                      (if (not (permutation car-a first-a))
                        (match:syntax-err
                         pattern
                         "variables of or-pattern differ in"))
                      (or* (cdr plist)
                           (lambda (cdr-p)
                             (k (cons car-p cdr-p)))))))))))
     ((and (pair? p) (eq? 'not (car p)))
      (cond
       ((not (null? (cddr p)))
        (bound `(not (or ,@(cdr p))) a k))
       (else
        (bound (cadr p)
               a
               (lambda (p2 a2)
                 (if (not (permutation a a2))
                   (match:syntax-err p "no variables allowed in"))
                 (k `(not ,p2) a))))))
     ((and (pair? p)
           (pair? (cdr p))
           (dot-dot-k? (cadr p)))
      (bound (car p)
             a
             (lambda (q b)
               (let ((bvars (find-prefix b a)))
                 (k `(,q ,(cadr p)
                         ,bvars
                         ,(gensym)
                         ,(gensym)
                         ,(map (lambda (_) (gensym)) bvars))
                    b)))))
     ((and (pair? p) (memq (car p) '($ struct)))
      (bound* (cddr p)
              a
              (lambda (p1 a)
                (k `($ ,(cadr p) ,@p1) a))))
     ((and (pair? p) (memq (car p) '(@ object)))  ;; Gauche extension
      (unless (and (pair? (cdr p))
                   (every (lambda (p) (and (list? p) (= (length p) 2)))
                          (cddr p)))
        (match:syntax-err p "syntax error in pattern"))
      (bound* (map cadr (cddr p))
              a
              (lambda (p1 a)
                (k `(object ,(cadr p)
                            ,@(map (lambda (p q) (list (car p) q))
                                   (cddr p) p1))
                   a))))
     ((and (pair? p) (eq? 'set! (car p)))
      (if (memq (cadr p) a)
        (k p a)
        (k p (cons (cadr p) a))))
     ((and (pair? p) (eq? 'get! (car p)))
      (if (memq (cadr p) a)
        (k p a)
        (k p (cons (cadr p) a))))
     ((pair? p)
      (bound (car p)
             a
             (lambda (car-p a)
               (bound (cdr p)
                      a
                      (lambda (cdr-p a) (k (cons car-p cdr-p) a))))))
     ((vector? p)
      (boundv (vector->list p)
              a
              (lambda (pl a) (k (list->vector pl) a))))
     (else (k p a))))

  (define (boundv plist a k)
    (let ((g115 (lambda () (k plist a))))
      (if (pair? plist)
        (if (and (pair? (cdr plist))
                 (dot-dot-k? (cadr plist))
                 (null? (cddr plist)))
          (bound plist a k)
          (if (null? plist)
            (g115)
            (bound (car plist)
                   a
                   (lambda (car-p a)
                     (boundv (cdr plist)
                             a
                             (lambda (cdr-p a)
                               (k (cons car-p cdr-p) a)))))))
        (if (null? plist)
          (g115)
          (match:error plist)))))

  (define (bound* plist a k)
    (if (null? plist)
      (k plist a)
      (bound (car plist)
             a
             (lambda (car-p a)
               (bound* (cdr plist)
                       a
                       (lambda (cdr-p a)
                         (k (cons car-p cdr-p) a)))))))

  (define (find-prefix b a)
    (if (eq? b a)
      '()
      (cons (car b) (find-prefix (cdr b) a))))

  (define (permutation p1 p2)
    (and (= (length p1) (length p2))
         (every (lambda (x1) (memq x1 p2)) p1)))

  (bound pattern
         '()
         (lambda (p a) (list p (reverse a) pred-bodies))))

(define (gen x sf plist erract eta)
  (if (null? plist)
    (erract x)
    (let* ((v '())
           (val (lambda (x) (cdr (assq x v))))
           (fail (lambda (sf)
                   (gen x sf (cdr plist) erract eta)))
           (success (lambda (sf)
                      (set-car! (cddddr (car plist)) #t)
                      (let* ((code (cadr (car plist)))
                             (bv (caddr (car plist)))
                             (fail-sym (cadddr (car plist))))
                        (if fail-sym
                          (let ((ap `(,code ,fail-sym ,@(map val bv))))
                            `(call-with-current-continuation
                              (,lambda. (,fail-sym)
                                (let ((,fail-sym
                                       (,lambda. ()
                                         (call-with-values
                                             (,lambda. () ,(fail sf))
                                           ,fail-sym))))
                                  ,ap))))
                          `(,code ,@(map val bv)))))))
      (let next ((p (caar plist))
                 (e x)
                 (sf sf)
                 (kf fail)
                 (ks success))
        (cond
         ((eq? '_ p) (ks sf))
         ;; The check of (symbol? :x) is to support both disjoint-keyword and
         ;; keyword-is-symbol runtime.  Remove this clause once we fully
         ;; migrated to keyword-is-symbol.
         ((and (not (symbol? :x)) (keyword? p))
          (warn "Unquoted keyword `~s' in match pattern: ~s.  \
                 This would likely break in future versions of Gauche.  \
                 See the ``Keyword and symbol integration'' section \
                 of the manual for the details.\n" p x)
          (emit `(equal? ,e ,p) sf kf ks))
         ((symid? p) (set! v (cons (cons p e) v))
          (ks sf))
         ((null? p) (emit `(null? ,e) sf kf ks))
         ((equal? p ''()) (emit `(null? ,e) sf kf ks))
         ((string? p) (emit `(equal? ,e ,p) sf kf ks))
         ((boolean? p) (emit `(equal? ,e ,p) sf kf ks))
         ((char? p) (emit `(equal? ,e ,p) sf kf ks))
         ((number? p) (emit `(equal? ,e ,p) sf kf ks))
         ((and (pair? p) (equal? 'quote (car p)))
          (emit `(equal? ,e ,p) sf kf ks))
         ((and (pair? p) (eq? '? (car p)))
          (let ((tst `(,(cadr p) ,e)))
            (emit tst sf kf ks)))
         ((and (pair? p) (eq? '= (car p)))
          (if (and (pair? (cadr p))
                   (equal? (caadr p) 'quote))
            (next (caddr p) `(ref ,(cadr p) ,e) sf kf ks)
            (next (caddr p) `(,(cadr p) ,e) sf kf ks)))
         ((and (pair? p) (eq? 'and (car p)))
          (let loop ((p (cdr p))
                     (sf sf))
            (if (null? p)
              (ks sf)
              (next (car p) e sf kf (lambda (sf) (loop (cdr p) sf))))))
         ((and (pair? p) (eq? 'or (car p)))
          (let ((or-v v))
            (let loop ((p (cdr p))
                       (sf sf))
              (if (null? p)
                (kf sf)
                (begin (set! v or-v)
                       (next (car p) e sf (lambda (sf) (loop (cdr p) sf))
                             ks))))))
         ((and (pair? p) (eq? 'not (car p)))
          (next (cadr p) e sf ks kf))
         ((and (pair? p) (eq? '$ (car p)))
          (let* ((tag (cadr p))
                 (fields (cdr p))
                 (rlen (length fields))
                 (tst `(is-a? ,e ,tag)))
            (emit tst sf kf
                  (let rloop ((n 1))
                    (lambda (sf)
                      (if (= n rlen)
                        (ks sf)
                        (next (list-ref fields n)
                              `(match:$-ref ,tag ,(- n 1) ,e)
                              sf kf (rloop (+ 1 n)))))))))
         ((and (pair? p) (eq? 'object (car p)))  ;; Gauche extension
          (let* ((tag (cadr p))
                 (fields (cddr p))
                 (tst `(is-a? ,e ,tag)))
            (emit tst sf kf
                  (let rloop ((fields fields))
                    (lambda (sf)
                      (if (null? fields)
                        (ks sf)
                        (next (cadar fields)
                              `(ref ,e ',(caar fields))
                              sf kf (rloop (cdr fields)))))))))
         ((and (pair? p) (eq? 'set! (car p)))
          (set! v (cons (cons (cadr p) (get-setter e p)) v))
          (ks sf))
         ((and (pair? p) (eq? 'get! (car p)))
          (set! v (cons (cons (cadr p) (get-getter e p)) v))
          (ks sf))
         ((and (pair? p)
               (pair? (cdr p))
               (dot-dot-k? (cadr p)))
          (emit `(list? ,e) sf kf
                (lambda (sf)
                  (let* ((k (dot-dot-k? (cadr p)))
                         (ks (lambda (sf)
                               (let ((bound (list-ref p 2)))
                                 (cond
                                  ((eq? (car p) '_) (ks sf))
                                  ((null? bound)
                                   (let* ((eta (gensym))
                                          (ptst (next (car p) eta sf
                                                      (lambda (sf) #f)
                                                      (lambda (sf) #t)))
                                          (tst (if (and (pair? ptst)
                                                        (symid? (car ptst))
                                                        (pair? (cdr ptst))
                                                        (eq? eta (cadr ptst))
                                                        (null? (cddr ptst)))
                                                 (car ptst)
                                                 `(,lambda. (,eta) ,ptst))))
                                     (assm `(match:every ,tst ,e)
                                           (kf sf)
                                           (ks sf))))
                                  ((and (symid? (car p))
                                        (equal? (list (car p)) bound))
                                   (next (car p) e sf kf ks))
                                  (else
                                   (let* ((gloop (list-ref p 3))
                                          (ge (list-ref p 4))
                                          (fresh (list-ref p 5))
                                          (p1 (next (car p)
                                                    `(car ,ge)
                                                    sf
                                                    kf
                                                    (lambda (sf)
                                                      `(,gloop
                                                        (cdr ,ge)
                                                        ,@(map (lambda (b f)
                                                                 `(cons ,(val b)
                                                                        ,f))
                                                               bound
                                                               fresh))))))
                                     (set! v
                                           (append
                                            (map cons
                                                 bound
                                                 (map (lambda (x)
                                                        `(reverse ,x))
                                                      fresh))
                                            v))
                                     `(let ,gloop ((,ge ,e)
                                                   ,@(map (lambda (x)
                                                            `(,x '()))
                                                          fresh))
                                        (if (null? ,ge)
                                          ,(ks sf)
                                          ,p1)))))))))
                    (case k
                      ((0) (ks sf))
                      ((1) (emit `(pair? ,e) sf kf ks))
                      (else (emit `((,match:length>=?. ,k) ,e) sf kf ks)))))))
         ((pair? p) (emit `(pair? ,e)
                          sf
                          kf
                          (lambda (sf)
                            (next (car p)
                                  (add-a e)
                                  sf
                                  kf
                                  (lambda (sf)
                                    (next (cdr p) (add-d e) sf kf ks))))))
         ((and (vector? p)
               (>= (vector-length p) 6)
               (dot-dot-k? (vector-ref p (- (vector-length p) 5))))
          (let* ((vlen (- (vector-length p) 6))
                 (k (dot-dot-k? (vector-ref p (+ vlen 1))))
                 (minlen (+ vlen k))
                 (bound (vector-ref p (+ vlen 2))))
            (emit `(vector? ,e)
                  sf
                  kf
                  (lambda (sf)
                    (assm `(>= (vector-length ,e) ,minlen)
                          (kf sf)
                          ((let vloop ((n 0))
                             (lambda (sf)
                               (cond
                                ((not (= n vlen))
                                 (next (vector-ref p n)
                                       `(vector-ref ,e ,n)
                                       sf
                                       kf
                                       (vloop (+ 1 n))))
                                ((eq? (vector-ref p vlen) '_)
                                 (ks sf))
                                (else
                                 (let* ((gloop (vector-ref p (+ vlen 3)))
                                        (ind (vector-ref p (+ vlen 4)))
                                        (fresh (vector-ref p (+ vlen 5)))
                                        (p1 (next (vector-ref p vlen)
                                                  `(vector-ref ,e ,ind)
                                                  sf
                                                  kf
                                                  (lambda (sf)
                                                    `(,gloop
                                                      (- ,ind 1)
                                                      ,@(map (lambda (b f)
                                                               `(cons ,(val b)
                                                                      ,f))
                                                             bound
                                                             fresh))))))
                                        (set! v
                                              (append
                                               (map cons bound fresh)
                                               v))
                                        `(let ,gloop
                                           ((,ind (- (vector-length ,e) 1))
                                            ,@(map (lambda (x) `(,x '()))
                                                   fresh))
                                           (if (> ,minlen ,ind)
                                             ,(ks sf)
                                             ,p1)))))))
                           sf))))))
         ((vector? p)
          (let ((vlen (vector-length p)))
            (emit `(vector? ,e)
                  sf
                  kf
                  (lambda (sf)
                    (emit `(equal? (vector-length ,e) ,vlen)
                          sf
                          kf
                          (let vloop ((n 0))
                            (lambda (sf)
                              (if (= n vlen)
                                (ks sf)
                                (next (vector-ref p n)
                                      `(vector-ref ,e ,n)
                                      sf
                                      kf
                                      (vloop (+ 1 n)))))))))))
         (else (display "FATAL ERROR IN PATTERN MATCHER")
               (newline)
               (error #f "THIS NEVER HAPPENS")))))))

(define (emit tst sf kf ks)
  (cond
   ((in tst sf) (ks sf))
   ((in `(not ,tst) sf) (kf sf))
   (else
    (let* ((e (cadr tst))
           (implied (cond
                     ((eq? (car tst) 'equal?)
                      (let ((p (caddr tst)))
                        (cond
                         ((string? p) `((string? ,e)))
                         ((boolean? p) `((boolean? ,e)))
                         ((char? p) `((char? ,e)))
                         ((number? p) `((number? ,e)))
                         ((and (pair? p)
                               (equal? 'quote (car p)))
                          `((symbol? ,e)))
                         (else '()))))
                     ((eq? (car tst) 'null?)
                      `((list? ,e)))
                     (else '())))
           (not-imp (case (car tst)
                      ((list?) `((not (null? ,e))))
                      (else '())))
           (s (ks (cons tst (append implied sf))))
           (k (kf (cons `(not ,tst)
                        (append not-imp sf)))))
      (assm tst k s)))))

(define (assm tst f s)
  (cond
   ((equal? s f) s)
   ((and (eq? s #t) (eq? f #f)) tst)
   ((and (eq? (car tst) 'pair?)
         (memq match:error-control '(unspecified fail))
         (memq (car f) '(cond match:error))
         (guarantees s (cadr tst)))
    s)
   (;; (if (and X ...) Y f) => (if (and tst X ...) Y f)
    ;; (if X Y f)           => (if (and tst X) Y  f)
    (and (pair? s)
         (eq? (car s) 'if)
         (equal? (cadddr s) f))
    (if (eq? (car (cadr s)) 'and)
      `(if (and ,tst ,@(cdr (cadr s)))
         ,(caddr s)
         ,f)
      `(if (and ,tst ,(cadr s))
         ,(caddr s)
         ,f)))
   (;; (call/cc (lambda (X)
    ;;            (let ((Y (lambda () (call-with-values (lambda () f) X))))
    ;;              BODY)))
    ;; =>
    ;; (call/cc (lambda (X)
    ;;            (let ((Y (lambda () (call-with-values (lambda () f) X))))
    ;;              (assm tst (Y) BODY))))
    (and (pair? s)
         (equal? (car s) 'call-with-current-continuation)
         (pair? (cdr s))
         (pair? (cadr s))
         (equal? (caadr s) lambda.)
         (pair? (cdadr s))
         (pair? (cadadr s))
         (null? (cdr (cadadr s)))
         (pair? (cddadr s))
         (pair? (car (cddadr s)))
         (equal? (caar (cddadr s)) 'let)
         (pair? (cdar (cddadr s)))
         (pair? (cadar (cddadr s)))
         (pair? (caadar (cddadr s)))
         (pair? (cdr (caadar (cddadr s))))
         (pair? (cadr (caadar (cddadr s))))
         (equal? (caadr (caadar (cddadr s))) lambda.)
         (pair? (cdadr (caadar (cddadr s))))
         (null? (cadadr (caadar (cddadr s))))
         (pair? (cddadr (caadar (cddadr s))))
         (pair? (car (cddadr (caadar (cddadr s)))))
         (equal? (caar (cddadr (caadar (cddadr s)))) 'call-with-values)
         (pair? (cdar (cddadr (caadar (cddadr s)))))
         (pair? (cadar (cddadr (caadar (cddadr s)))))
         (equal? (caadar (cddadr (caadar (cddadr s)))) lambda.)
         (pair? (cdadar (cddadr (caadar (cddadr s)))))
         (null? (car (cdadar (cddadr (caadar (cddadr s))))))
         (pair? (cdr (cdadar (cddadr (caadar (cddadr s))))))
         (null? (cddr (cdadar (cddadr (caadar (cddadr s))))))
         (pair? (cddar (cddadr (caadar (cddadr s)))))
         (null? (cdddar (cddadr (caadar (cddadr s)))))
         (null? (cdr (cddadr (caadar (cddadr s)))))
         (null? (cddr (caadar (cddadr s))))
         (null? (cdadar (cddadr s)))
         (pair? (cddar (cddadr s)))
         (null? (cdddar (cddadr s)))
         (null? (cdr (cddadr s)))
         (null? (cddr s))
         (equal? f (cadr (cdadar (cddadr (caadar (cddadr s)))))))
    (let ((k (car (cadadr s)))
          (fail (car (caadar (cddadr s))))
          (s2 (caddar (cddadr s))))
      `(call-with-current-continuation
        (lambda (,k)
          (let ((,fail (,lambda. () (call-with-values (,lambda. () ,f) ,k))))
            ,(assm tst `(,fail) s2))))))
   ((and #f
         (pair? s)
         (equal? (car s) 'let)
         (pair? (cdr s))
         (pair? (cadr s))
         (pair? (caadr s))
         (pair? (cdaadr s))
         (pair? (car (cdaadr s)))
         (equal? (caar (cdaadr s)) 'lambda)
         (pair? (cdar (cdaadr s)))
         (null? (cadar (cdaadr s)))
         (pair? (cddar (cdaadr s)))
         (null? (cdddar (cdaadr s)))
         (null? (cdr (cdaadr s)))
         (null? (cdadr s))
         (pair? (cddr s))
         (null? (cdddr s))
         (equal? (caddar (cdaadr s)) f))
    (let ((fail (caaadr s))
          (s2 (caddr s)))
      `(let ((,fail (,lambda. () ,f)))
         ,(assm tst `(,fail) s2))))
   (else `(if ,tst ,s ,f))))

(define (guarantees code x)
  (let ((a (add-a x)) (d (add-d x)))
    (let loop ((code code))
      (cond
       ((not (pair? code)) #f)
       ((member (car code) '(cond match:error)) #t)
       ((or (equal? code a) (equal? code d)) #t)
       ((eq? (car code) 'if)
        (or (loop (cadr code))
            (and (loop (caddr code))
                 (loop (cadddr code)))))
       ((equal? (car code) 'lambda) #f)
       ((and (eq? (car code) 'let)
             (symid? (cadr code)))
        #f)
       (else (or (loop (car code))
                 (loop (cdr code))))))))

(define (in e l)
  (or (member e l)
      (and (eq? (car e) 'list?)
           ;; NB: Original Wright's code allows `(pair? ,(cadr e)) here as well,
           ;; but they are not the same condition - pair? allows improper list
           ;; but list? doesn't.  Ref: https://github.com/shirok/Gauche/issues/47
           (member `(null? ,(cadr e)) l))
      (and (eq? (car e) 'not)
           (let* ((srch (cadr e))
                  (const-class (equal-test? srch)))
             (cond
              (const-class
               (let mem ((l l))
                 (if (null? l)
                   #f
                   (let ((x (car l)))
                     (or (and (equal? (cadr x) (cadr srch))
                              (disjoint? x)
                              (not (equal? const-class (car x))))
                         (equal? x `(not (,const-class ,(cadr srch))))
                         (and (equal? (cadr x) (cadr srch))
                              (equal-test? x)
                              (not (equal? (caddr srch) (caddr x))))
                         (mem (cdr l)))))))
              ((disjoint? srch)
               (let mem ((l l))
                 (if (null? l)
                   #f
                   (let ((x (car l)))
                     (or (and (equal? (cadr x) (cadr srch))
                              (disjoint? x)
                              (not (equal? (car x) (car srch))))
                         (mem (cdr l)))))))
              ((eq? (car srch) 'list?)
               (let mem ((l l))
                 (if (null? l)
                   #f
                   (let ((x (car l)))
                     (or (and (equal? (cadr x) (cadr srch))
                              (disjoint? x)
                              (not (memq (car x) '(list? pair? null?))))
                         (mem (cdr l)))))))
              (else #f))))))

(define (equal-test? tst)
  (and (equal? (car tst) 'equal?)
       (let ((p (caddr tst)))
         (cond
          ((string? p) 'string?)
          ((boolean? p) 'boolean?)
          ((char? p) 'char?)
          ((number? p) 'number?)
          ((and (pair? p)
                (pair? (cdr p))
                (null? (cddr p))
                (equal? 'quote (car p))
                (symid? (cadr p)))
           'symbol?)
          (else #f)))))

(define (disjoint? tst)
  (memq (car tst) match:disjoint-predicates))

(define (add-a a)
  (let ((new (and (pair? a) (assq (car a) c---rs))))
    (if new (cons (cadr new) (cdr a)) `(car ,a))))

(define (add-d a)
  (let ((new (and (pair? a) (assq (car a) c---rs))))
    (if new (cons (cddr new) (cdr a)) `(cdr ,a))))

(define c---rs '((car caar . cdar)
                 (cdr cadr . cddr)
                 (caar caaar . cdaar)
                 (cadr caadr . cdadr)
                 (cdar cadar . cddar)
                 (cddr caddr . cdddr)
                 (caaar caaaar . cdaaar)
                 (caadr caaadr . cdaadr)
                 (cadar caadar . cdadar)
                 (caddr caaddr . cdaddr)
                 (cdaar cadaar . cddaar)
                 (cdadr cadadr . cddadr)
                 (cddar caddar . cdddar)
                 (cdddr cadddr . cddddr)))

(define (get-setter e p)
  (unless (pair? e)
    (match:syntax-err p "unnested set! pattern"))
  `(cute (or (setter ,(car e)) (error "no setter defined to bind " ',p))
         ,@(cdr e) <>))

(define (get-getter e p)
  (unless (pair? e)
    (match:syntax-err p "unnested get! pattern"))
  `(cute ,@e))

(define match:expanders
  (list genmatch genletrec gendefine pattern-var?))

(define-macro (match . args)
  (cond
    ((and (list? args)
          (<= 1 (length args))
          (match:every
            (lambda (y) (and (list? y) (<= 2 (length y))))
            (cdr args))) (let* ((exp (car args))
                                (clauses (cdr args))
                                (e (if (symid? exp) exp (gensym))))
                           (if (symid? exp)
                               ((car match:expanders)
                                e
                                clauses
                                `(,match. ,@args))
                               `(let ((,e ,exp))
                                  ,((car match:expanders)
                                    e
                                    clauses
                                    `(,match. ,@args))))))
    (else (match:syntax-err `(match ,@args) "syntax error in"))))

(define-macro (match-lambda . args)
  (if (and (list? args)
           (match:every
             (lambda (g126)
               (if (and (pair? g126) (list? (cdr g126)))
                   (pair? (cdr g126))
                   #f))
             args))
      ((lambda ()
         (let ((e (gensym))) `(,lambda. (,e) (,match. ,e ,@args)))))
      ((lambda ()
         (match:syntax-err
           `(match-lambda ,@args)
           "syntax error in")))))

(define-macro (match-lambda* . args)
  (if (and (list? args)
           (match:every
             (lambda (g134)
               (if (and (pair? g134) (list? (cdr g134)))
                   (pair? (cdr g134))
                   #f))
             args))
      ((lambda ()
         (let ((e (gensym))) `(,lambda. ,e (,match. ,e ,@args)))))
      ((lambda ()
         (match:syntax-err
           `(match-lambda* ,@args)
           "syntax error in")))))

(define-macro (match-let . args)
  (let ((g158 (lambda (pat exp body)
                `(,match. ,exp (,pat ,@body))))
        (g154 (lambda (pat exp body)
                (let ((g (map (lambda (x) (gensym)) pat))
                      (vpattern (list->vector pat)))
                  `(let ,(map list g exp)
                     (,match. (vector ,@g) (,vpattern ,@body))))))
        (g146 (lambda ()
                (match:syntax-err `(match-let ,@args) "syntax error in")))
        (g145 (lambda (p1 e1 p2 e2 body)
                (let ((g1 (gensym)) (g2 (gensym)))
                  `(let ((,g1 ,e1) (,g2 ,e2))
                     (,match. (cons ,g1 ,g2) ((,p1 . ,p2) ,@body))))))
        (g136 (cadddr match:expanders)))
    (if (pair? args)
        (if (symid? (car args))
            (if (and (pair? (cdr args)) (list? (cadr args)))
                (let g161 ((g162 (cadr args)) (g160 '()) (g159 '()))
                  (if (null? g162)
                      (if (and (list? (cddr args)) (pair? (cddr args)))
                          ((lambda (name pat exp body)
                             (if (match:every
                                   (cadddr match:expanders)
                                   pat)
                                 `(let ,@args)
                                 `(letrec ((,name (,match-lambda*.
                                                    (,pat ,@body))))
                                    (,name ,@exp))))
                           (car args)
                           (reverse g159)
                           (reverse g160)
                           (cddr args))
                          (g146))
                      (if (and (pair? (car g162))
                               (pair? (cdar g162))
                               (null? (cddar g162)))
                          (g161 (cdr g162)
                                (cons (cadar g162) g160)
                                (cons (caar g162) g159))
                          (g146))))
                (g146))
            (if (list? (car args))
                (if (match:every
                      (lambda (g167)
                        (if (and (pair? g167)
                                 (g136 (car g167))
                                 (pair? (cdr g167)))
                            (null? (cddr g167))
                            #f))
                      (car args))
                    (if (and (list? (cdr args)) (pair? (cdr args)))
                        ((lambda () `(let ,@args)))
                        (let g149 ((g150 (car args)) (g148 '()) (g147 '()))
                          (if (null? g150)
                              (g146)
                              (if (and (pair? (car g150))
                                       (pair? (cdar g150))
                                       (null? (cddar g150)))
                                  (g149 (cdr g150)
                                        (cons (cadar g150) g148)
                                        (cons (caar g150) g147))
                                  (g146)))))
                    (if (and (pair? (car args))
                             (pair? (caar args))
                             (pair? (cdaar args))
                             (null? (cddaar args)))
                        (if (null? (cdar args))
                            (if (and (list? (cdr args)) (pair? (cdr args)))
                                (g158 (caaar args)
                                      (cadaar args)
                                      (cdr args))
                                (let g149 ((g150 (car args))
                                           (g148 '())
                                           (g147 '()))
                                  (if (null? g150)
                                      (g146)
                                      (if (and (pair? (car g150))
                                               (pair? (cdar g150))
                                               (null? (cddar g150)))
                                          (g149 (cdr g150)
                                                (cons (cadar g150) g148)
                                                (cons (caar g150) g147))
                                          (g146)))))
                            (if (and (pair? (cdar args))
                                     (pair? (cadar args))
                                     (pair? (cdadar args))
                                     (null? (cdr (cdadar args)))
                                     (null? (cddar args)))
                                (if (and (list? (cdr args))
                                         (pair? (cdr args)))
                                    (g145 (caaar args)
                                          (cadaar args)
                                          (caadar args)
                                          (car (cdadar args))
                                          (cdr args))
                                    (let g149 ((g150 (car args))
                                               (g148 '())
                                               (g147 '()))
                                      (if (null? g150)
                                          (g146)
                                          (if (and (pair? (car g150))
                                                   (pair? (cdar g150))
                                                   (null? (cddar g150)))
                                              (g149 (cdr g150)
                                                    (cons (cadar g150)
                                                          g148)
                                                    (cons (caar g150)
                                                          g147))
                                              (g146)))))
                                (let g149 ((g150 (car args))
                                           (g148 '())
                                           (g147 '()))
                                  (if (null? g150)
                                      (if (and (list? (cdr args))
                                               (pair? (cdr args)))
                                          (g154 (reverse g147)
                                                (reverse g148)
                                                (cdr args))
                                          (g146))
                                      (if (and (pair? (car g150))
                                               (pair? (cdar g150))
                                               (null? (cddar g150)))
                                          (g149 (cdr g150)
                                                (cons (cadar g150) g148)
                                                (cons (caar g150) g147))
                                          (g146))))))
                        (let g149 ((g150 (car args)) (g148 '()) (g147 '()))
                          (if (null? g150)
                              (if (and (list? (cdr args))
                                       (pair? (cdr args)))
                                  (g154 (reverse g147)
                                        (reverse g148)
                                        (cdr args))
                                  (g146))
                              (if (and (pair? (car g150))
                                       (pair? (cdar g150))
                                       (null? (cddar g150)))
                                  (g149 (cdr g150)
                                        (cons (cadar g150) g148)
                                        (cons (caar g150) g147))
                                  (g146))))))
                (if (pair? (car args))
                    (if (and (pair? (caar args))
                             (pair? (cdaar args))
                             (null? (cddaar args)))
                        (if (null? (cdar args))
                            (if (and (list? (cdr args)) (pair? (cdr args)))
                                (g158 (caaar args)
                                      (cadaar args)
                                      (cdr args))
                                (let g149 ((g150 (car args))
                                           (g148 '())
                                           (g147 '()))
                                  (if (null? g150)
                                      (g146)
                                      (if (and (pair? (car g150))
                                               (pair? (cdar g150))
                                               (null? (cddar g150)))
                                          (g149 (cdr g150)
                                                (cons (cadar g150) g148)
                                                (cons (caar g150) g147))
                                          (g146)))))
                            (if (and (pair? (cdar args))
                                     (pair? (cadar args))
                                     (pair? (cdadar args))
                                     (null? (cdr (cdadar args)))
                                     (null? (cddar args)))
                                (if (and (list? (cdr args))
                                         (pair? (cdr args)))
                                    (g145 (caaar args)
                                          (cadaar args)
                                          (caadar args)
                                          (car (cdadar args))
                                          (cdr args))
                                    (let g149 ((g150 (car args))
                                               (g148 '())
                                               (g147 '()))
                                      (if (null? g150)
                                          (g146)
                                          (if (and (pair? (car g150))
                                                   (pair? (cdar g150))
                                                   (null? (cddar g150)))
                                              (g149 (cdr g150)
                                                    (cons (cadar g150)
                                                          g148)
                                                    (cons (caar g150)
                                                          g147))
                                              (g146)))))
                                (let g149 ((g150 (car args))
                                           (g148 '())
                                           (g147 '()))
                                  (if (null? g150)
                                      (if (and (list? (cdr args))
                                               (pair? (cdr args)))
                                          (g154 (reverse g147)
                                                (reverse g148)
                                                (cdr args))
                                          (g146))
                                      (if (and (pair? (car g150))
                                               (pair? (cdar g150))
                                               (null? (cddar g150)))
                                          (g149 (cdr g150)
                                                (cons (cadar g150) g148)
                                                (cons (caar g150) g147))
                                          (g146))))))
                        (let g149 ((g150 (car args)) (g148 '()) (g147 '()))
                          (if (null? g150)
                              (if (and (list? (cdr args))
                                       (pair? (cdr args)))
                                  (g154 (reverse g147)
                                        (reverse g148)
                                        (cdr args))
                                  (g146))
                              (if (and (pair? (car g150))
                                       (pair? (cdar g150))
                                       (null? (cddar g150)))
                                  (g149 (cdr g150)
                                        (cons (cadar g150) g148)
                                        (cons (caar g150) g147))
                                  (g146)))))
                    (g146))))
        (g146))))

(define-macro (match-let1 pat exp . body) ;; Gauche extension
  `(,match. ,exp (,pat ,@body)))

(define-macro (match-let* . args)
  (let ((g176 (lambda ()
                (match:syntax-err `(match-let* ,@args) "syntax error in"))))
    (if (pair? args)
        (if (null? (car args))
            (if (and (list? (cdr args)) (pair? (cdr args)))
                ((lambda (body) `(let* ,@args)) (cdr args))
                (g176))
            (if (and (pair? (car args))
                     (pair? (caar args))
                     (pair? (cdaar args))
                     (null? (cddaar args))
                     (list? (cdar args))
                     (list? (cdr args))
                     (pair? (cdr args)))
                ((lambda (pat exp rest body)
                   (if ((cadddr match:expanders) pat)
                       `(let ((,pat ,exp)) (,match-let*. ,rest ,@body))
                       `(,match. ,exp (,pat (,match-let*. ,rest ,@body)))))
                 (caaar args)
                 (cadaar args)
                 (cdar args)
                 (cdr args))
                (g176)))
        (g176))))

(define-macro (match-letrec . args)
  (let ((g200 (cadddr match:expanders))
        (g199 (lambda (p1 e1 p2 e2 body)
                `(,match-letrec. (((,p1 . ,p2) (cons ,e1 ,e2))) ,@body)))
        (g195 (lambda ()
                (match:syntax-err
                  `(match-letrec ,@args)
                  "syntax error in")))
        (g194 (lambda (pat exp body)
                `(,match-letrec.
                   ((,(list->vector pat) (vector ,@exp)))
                   ,@body)))
        (g186 (lambda (pat exp body)
                ((cadr match:expanders)
                 pat
                 exp
                 body
                 `(,match-letrec. ((,pat ,exp)) ,@body)))))
    (if (pair? args)
        (if (list? (car args))
            (if (match:every
                  (lambda (g206)
                    (if (and (pair? g206)
                             (g200 (car g206))
                             (pair? (cdr g206)))
                        (null? (cddr g206))
                        #f))
                  (car args))
                (if (and (list? (cdr args)) (pair? (cdr args)))
                    ((lambda () `(letrec ,@args)))
                    (let g189 ((g190 (car args)) (g188 '()) (g187 '()))
                      (if (null? g190)
                          (g195)
                          (if (and (pair? (car g190))
                                   (pair? (cdar g190))
                                   (null? (cddar g190)))
                              (g189 (cdr g190)
                                    (cons (cadar g190) g188)
                                    (cons (caar g190) g187))
                              (g195)))))
                (if (and (pair? (car args))
                         (pair? (caar args))
                         (pair? (cdaar args))
                         (null? (cddaar args)))
                    (if (null? (cdar args))
                        (if (and (list? (cdr args)) (pair? (cdr args)))
                            (g186 (caaar args) (cadaar args) (cdr args))
                            (let g189 ((g190 (car args))
                                       (g188 '())
                                       (g187 '()))
                              (if (null? g190)
                                  (g195)
                                  (if (and (pair? (car g190))
                                           (pair? (cdar g190))
                                           (null? (cddar g190)))
                                      (g189 (cdr g190)
                                            (cons (cadar g190) g188)
                                            (cons (caar g190) g187))
                                      (g195)))))
                        (if (and (pair? (cdar args))
                                 (pair? (cadar args))
                                 (pair? (cdadar args))
                                 (null? (cdr (cdadar args)))
                                 (null? (cddar args)))
                            (if (and (list? (cdr args)) (pair? (cdr args)))
                                (g199 (caaar args)
                                      (cadaar args)
                                      (caadar args)
                                      (car (cdadar args))
                                      (cdr args))
                                (let g189 ((g190 (car args))
                                           (g188 '())
                                           (g187 '()))
                                  (if (null? g190)
                                      (g195)
                                      (if (and (pair? (car g190))
                                               (pair? (cdar g190))
                                               (null? (cddar g190)))
                                          (g189 (cdr g190)
                                                (cons (cadar g190) g188)
                                                (cons (caar g190) g187))
                                          (g195)))))
                            (let g189 ((g190 (car args))
                                       (g188 '())
                                       (g187 '()))
                              (if (null? g190)
                                  (if (and (list? (cdr args))
                                           (pair? (cdr args)))
                                      (g194 (reverse g187)
                                            (reverse g188)
                                            (cdr args))
                                      (g195))
                                  (if (and (pair? (car g190))
                                           (pair? (cdar g190))
                                           (null? (cddar g190)))
                                      (g189 (cdr g190)
                                            (cons (cadar g190) g188)
                                            (cons (caar g190) g187))
                                      (g195))))))
                    (let g189 ((g190 (car args)) (g188 '()) (g187 '()))
                      (if (null? g190)
                          (if (and (list? (cdr args)) (pair? (cdr args)))
                              (g194 (reverse g187)
                                    (reverse g188)
                                    (cdr args))
                              (g195))
                          (if (and (pair? (car g190))
                                   (pair? (cdar g190))
                                   (null? (cddar g190)))
                              (g189 (cdr g190)
                                    (cons (cadar g190) g188)
                                    (cons (caar g190) g187))
                              (g195))))))
            (if (pair? (car args))
                (if (and (pair? (caar args))
                         (pair? (cdaar args))
                         (null? (cddaar args)))
                    (if (null? (cdar args))
                        (if (and (list? (cdr args)) (pair? (cdr args)))
                            (g186 (caaar args) (cadaar args) (cdr args))
                            (let g189 ((g190 (car args))
                                       (g188 '())
                                       (g187 '()))
                              (if (null? g190)
                                  (g195)
                                  (if (and (pair? (car g190))
                                           (pair? (cdar g190))
                                           (null? (cddar g190)))
                                      (g189 (cdr g190)
                                            (cons (cadar g190) g188)
                                            (cons (caar g190) g187))
                                      (g195)))))
                        (if (and (pair? (cdar args))
                                 (pair? (cadar args))
                                 (pair? (cdadar args))
                                 (null? (cdr (cdadar args)))
                                 (null? (cddar args)))
                            (if (and (list? (cdr args)) (pair? (cdr args)))
                                (g199 (caaar args)
                                      (cadaar args)
                                      (caadar args)
                                      (car (cdadar args))
                                      (cdr args))
                                (let g189 ((g190 (car args))
                                           (g188 '())
                                           (g187 '()))
                                  (if (null? g190)
                                      (g195)
                                      (if (and (pair? (car g190))
                                               (pair? (cdar g190))
                                               (null? (cddar g190)))
                                          (g189 (cdr g190)
                                                (cons (cadar g190) g188)
                                                (cons (caar g190) g187))
                                          (g195)))))
                            (let g189 ((g190 (car args))
                                       (g188 '())
                                       (g187 '()))
                              (if (null? g190)
                                  (if (and (list? (cdr args))
                                           (pair? (cdr args)))
                                      (g194 (reverse g187)
                                            (reverse g188)
                                            (cdr args))
                                      (g195))
                                  (if (and (pair? (car g190))
                                           (pair? (cdar g190))
                                           (null? (cddar g190)))
                                      (g189 (cdr g190)
                                            (cons (cadar g190) g188)
                                            (cons (caar g190) g187))
                                      (g195))))))
                    (let g189 ((g190 (car args)) (g188 '()) (g187 '()))
                      (if (null? g190)
                          (if (and (list? (cdr args)) (pair? (cdr args)))
                              (g194 (reverse g187)
                                    (reverse g188)
                                    (cdr args))
                              (g195))
                          (if (and (pair? (car g190))
                                   (pair? (cdar g190))
                                   (null? (cddar g190)))
                              (g189 (cdr g190)
                                    (cons (cadar g190) g188)
                                    (cons (caar g190) g187))
                              (g195)))))
                (g195)))
        (g195))))

(define-macro (match-define . args)
  (let ((g210 (cadddr match:expanders))
        (g209 (lambda ()
                (match:syntax-err
                  `(match-define ,@args)
                  "syntax error in"))))
    (if (pair? args)
        (if (g210 (car args))
            (if (and (pair? (cdr args)) (null? (cddr args)))
                ((lambda () `(begin (define ,@args))))
                (g209))
            (if (and (pair? (cdr args)) (null? (cddr args)))
                ((lambda (pat exp)
                   ((caddr match:expanders)
                    pat
                    exp
                    `(,match-define. ,@args)))
                 (car args)
                 (cadr args))
                (g209)))
        (g209))))

