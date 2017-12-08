;;;
;;; SRFI-149 Basic Syntax-rules Template Extensions
;;;
;;; modified for Gauche ( EXPERIMENTAL )
;;;

(define-module srfi-149-mod
  (export syntax-rules))
(select-module srfi-149-mod)

(define (identifier?-149 x)
  (or (symbol? x)
      (keyword? x)
      (identifier? x)))

(define (identifier->symbol-149 x)
  (cond
   ((symbol? x) x)
   ((keyword? x) x)
   ;((keyword? x) (string->symbol (string-append ":" (keyword->string x))))
   (else
    (identifier->symbol x))))

;; (from Sagittarius Scheme's comments)
;; Chibi allow 'ls' to be non pair as its extension.
;; syntax-rules depends on this behaviour so we need to allow it
(define (any-149 pred ls)
  (cond
   ((list? ls)
    (any pred ls))
   (else
    (let loop ((ls ls))
      (if (not (pair? ls))
        #f
        (or (pred (car ls))
            (loop (cdr ls))))))))

;; (from Sagittarius Scheme's comments)
;; Chibi's length* returns element count of car parts of inproper list
;; e.g) (length* '(1 2 3 . 4)) ;; => 3
;; And syntax-rules depends on this behaviour. So provide it.
(define (length*-149 ls)
  (cond
   ((list? ls)
    (length ls))
   (else
    (let loop ((i 0) (ls ls))
      (if (not (pair? ls))
        i
        (loop (+ i 1) (cdr ls)))))))

;(define (cons-source-149 kar kdr source) (cons kar kdr))
(define (cons-source-149 kar kdr source)
  (with-module gauche.internal (with-original-source (cons kar kdr) source)))

(define %number->string-149 number->string)

;(define (strip-syntactic-closures-149 x) x)
(define strip-syntactic-closures-149 unwrap-syntax)

(define (syntax-rules-transformer expr rename compare)
  (let ((ellipsis-specified? (identifier?-149 (cadr expr)))
        (count 0)
        (_er-macro-transformer (rename 'er-macro-transformer))
        (_lambda (rename 'lambda))      (_let (rename 'let))
        (_begin (rename 'begin))        (_if (rename 'if))
        (_and (rename 'and))            (_or (rename 'or))
        (_eq? (rename 'eq?))            (_equal? (rename 'equal?))
        (_car (rename 'car))            (_cdr (rename 'cdr))
        (_cons (rename 'cons))          (_pair? (rename 'pair?))
        (_null? (rename 'null?))        (_expr (rename 'expr))
        (_rename (rename 'rename))      (_compare (rename 'compare))
        (_quote (rename 'syntax-quote)) (_apply (rename 'apply))
        (_append (rename 'append))      (_map (rename 'map))
        (_vector? (rename 'vector?))    (_list? (rename 'list?))
        (_len (rename'len))             (_length (rename 'length*-149))
        (_- (rename '-))   (_>= (rename '>=))   (_error (rename 'error))
        (_ls (rename 'ls)) (_res (rename 'res)) (_i (rename 'i))
        (_reverse (rename 'reverse))
        (_vector->list (rename 'vector->list))
        (_list->vector (rename 'list->vector))
        (_cons3 (rename 'cons-source-149))
        (_underscore (rename '_)))
    (define ellipsis (if ellipsis-specified? (cadr expr) (rename '...)))
    (define lits (if ellipsis-specified? (car (cddr expr)) (cadr expr)))
    (define forms (if ellipsis-specified? (cdr (cddr expr)) (cddr expr)))
    (define (next-symbol s)
      (set! count (+ count 1))
      (rename (string->symbol (string-append s (%number->string-149 count)))))
    (define (expand-pattern pat tmpl)
      (let lp ((p (cdr pat))
               (x (list _cdr _expr))
               (dim 0)
               (vars '())
               (k (lambda (vars)
                    (list _cons (expand-template tmpl vars) #f))))
        (let ((v (next-symbol "v.")))
          (list
           _let (list (list v x))
           (cond
            ((identifier?-149 p)
             (if (any-149 (lambda (l) (compare p l)) lits)
                 (list _and
                       (list _compare v (list _rename (list _quote p)))
                       (k vars))
                 (if (compare p _underscore)
                     (k vars)
                     (list _let (list (list p v)) (k (cons (cons p dim) vars))))))
            ((ellipsis? p)
             (cond
              ((not (null? (cdr (cdr p))))
               (cond
                ((any-149 (lambda (x) (and (identifier?-149 x) (compare x ellipsis)))
                          (cddr p))
                 (error "multiple ellipses" p))
                (else
                 (let ((len (length*-149 (cdr (cdr p))))
                       (_lp (next-symbol "lp.")))
                   `(,_let ((,_len (,_length ,v)))
                      (,_and (,_>= ,_len ,len)
                             (,_let ,_lp ((,_ls ,v)
                                          (,_i (,_- ,_len ,len))
                                          (,_res (,_quote ())))
                                    (,_if (,_>= 0 ,_i)
                                        ,(lp `(,(cddr p)
                                               (,(car p) ,(car (cdr p))))
                                             `(,_cons ,_ls
                                                      (,_cons (,_reverse ,_res)
                                                              (,_quote ())))
                                             dim
                                             vars
                                             k)
                                        (,_lp (,_cdr ,_ls)
                                              (,_- ,_i 1)
                                              (,_cons3 (,_car ,_ls)
                                                       ,_res
                                                       ,_ls))))))))))
              ((identifier?-149 (car p))
               (list _and (list _list? v)
                     (list _let (list (list (car p) v))
                           (k (cons (cons (car p) (+ 1 dim)) vars)))))
              (else
               (let* ((w (next-symbol "w."))
                      (_lp (next-symbol "lp."))
                      (new-vars (all-vars (car p) (+ dim 1)))
                      (ls-vars (map (lambda (x)
                                      (next-symbol
                                       (string-append
                                        (symbol->string
                                         (identifier->symbol-149 (car x)))
                                        "-ls")))
                                    new-vars))
                      (once
                       (lp (car p) (list _car w) (+ dim 1) '()
                           (lambda (_)
                             (cons
                              _lp
                              (cons
                               (list _cdr w)
                               (map (lambda (x l)
                                      (list _cons (car x) l))
                                    new-vars
                                    ls-vars)))))))
                 (list
                  _let
                  _lp (cons (list w v)
                            (map (lambda (x) (list x (list _quote '()))) ls-vars))
                  (list _if (list _null? w)
                        (list _let (map (lambda (x l)
                                          (list (car x) (list _reverse l)))
                                        new-vars
                                        ls-vars)
                              (k (append new-vars vars)))
                        (list _and (list _pair? w) once)))))))
            ((pair? p)
             (list _and (list _pair? v)
                   (lp (car p)
                       (list _car v)
                       dim
                       vars
                       (lambda (vars)
                         (lp (cdr p) (list _cdr v) dim vars k)))))
            ((vector? p)
             (list _and
                   (list _vector? v)
                   (lp (vector->list p) (list _vector->list v) dim vars k)))
            ((null? p) (list _and (list _null? v) (k vars)))
            (else (list _and (list _equal? v p) (k vars))))))))
    (define (ellipsis-escape? x) (and (pair? x) (compare ellipsis (car x))))
    (define (ellipsis? x)
      (and (pair? x) (pair? (cdr x)) (compare ellipsis (cadr x))))
    (define (ellipsis-depth x)
      (if (ellipsis? x)
          (+ 1 (ellipsis-depth (cdr x)))
          0))
    (define (ellipsis-tail x)
      (if (ellipsis? x)
          (ellipsis-tail (cdr x))
          (cdr x)))
    (define (all-vars x dim)
      (let lp ((x x) (dim dim) (vars '()))
        (cond ((identifier?-149 x)
               (if (any-149 (lambda (lit) (compare x lit)) lits)
                   vars
                   (cons (cons x dim) vars)))
              ((ellipsis? x) (lp (car x) (+ dim 1) (lp (cddr x) dim vars)))
              ((pair? x) (lp (car x) dim (lp (cdr x) dim vars)))
              ((vector? x) (lp (vector->list x) dim vars))
              (else vars))))
    (define (free-vars x vars dim)
      (let lp ((x x) (free '()))
        (cond
         ((identifier?-149 x)
          (if (and (not (memq x free))
                   (cond ((assq x vars) => (lambda (cell) (>= (cdr cell) dim)))
                         (else #f)))
              (cons x free)
              free))
         ((pair? x) (lp (car x) (lp (cdr x) free)))
         ((vector? x) (lp (vector->list x) free))
         (else free))))
    (define (expand-template tmpl vars)
      (let lp ((t tmpl) (dim 0))
        (cond
         ((identifier?-149 t)
          (cond
           ((find (lambda (v) (eq? t (car v))) vars)
            => (lambda (cell)
                 (if (<= (cdr cell) dim)
                     t
                     (error "too few ...'s"))))
           (else
            (list _rename (list _quote t)))))
         ((pair? t)
          (cond
           ((ellipsis-escape? t)
            (list _quote
                  (if (pair? (cdr t))
                      (if (pair? (cddr t)) (cddr t) (cadr t))
                      (cdr t))))
           ((ellipsis? t)
            (let* ((depth (ellipsis-depth t))
                   (ell-dim (+ dim depth))
                   (ell-vars (free-vars (car t) vars ell-dim)))
              (cond
               ((null? ell-vars)
                (error "too many ...'s"))
               ((and (null? (cdr (cdr t))) (identifier?-149 (car t)))
                ;; shortcut for (var ...)
                (lp (car t) ell-dim))
               (else
                (let* ((once (lp (car t) ell-dim))
                       (nest (if (and (null? (cdr ell-vars))
                                      (identifier?-149 once)
                                      (eq? once (car vars)))
                                 once ;; shortcut
                                 (cons _map
                                       (cons (list _lambda ell-vars once)
                                             ell-vars))))
                       (many (do ((d depth (- d 1))
                                  (many nest
                                        (list _apply _append many)))
                                 ((= d 1) many))))
                  (if (null? (ellipsis-tail t))
                      many ;; shortcut
                      (list _append many (lp (ellipsis-tail t) dim))))))))
           (else (list _cons3 (lp (car t) dim) (lp (cdr t) dim) (list _quote t)))))
         ((vector? t) (list _list->vector (lp (vector->list t) dim)))
         ((null? t) (list _quote '()))
         (else t))))
    (list
     _er-macro-transformer
     (list _lambda (list _expr _rename _compare)
           (list
            _car
            (cons
             _or
             (append
              (map
               (lambda (clause) (expand-pattern (car clause) (cadr clause)))
               forms)
              (list
               (list _cons
                     (list _error "no expansion for"
                           (list (rename 'strip-syntactic-closures-149) _expr))
                     #f)))))))))

(define-syntax syntax-rules/aux
  ;; modified to avoid unbound variable error of syntax-rules-transformer
  ;(er-macro-transformer syntax-rules-transformer))
  (er-macro-transformer
   (lambda (expr rename compare)
     (syntax-rules-transformer expr rename compare))))

(define-syntax syntax-rules
  (er-macro-transformer
   (lambda (expr rename compare)
     (if (identifier?-149 (cadr expr))
         (list (rename 'let) (list (list (cadr expr) #t))
               (cons (rename 'syntax-rules/aux) (cdr expr)))
         (syntax-rules-transformer expr rename compare)))))

