;;;
;;; SRFI-0   feature based conditional expansion construct
;;;

(define-module srfi-0
  (export cond-expand))
(select-module srfi-0)

(define cond-features (with-module gauche.internal cond-features))

;;; Rewritten with a legacy macro, instead of r5rs syntax-rules,
;;; to enable adding features at runtime.  Such capability is
;;; for system management, and not supposed to be used freely
;;; by user programs.

;; Note: If you modify this, try to avoid relying on other autoloaded
;; modules as much as possible; cond-expand can be used extensively, and
;; it's easy to introduce a circular dependency.

(define-macro (cond-expand . clauses)

  ;; Kludge - must be replaced once we have low-level hygienic macro.
  (define use. ((with-module gauche.internal make-identifier)
                'use (find-module 'gauche) '()))
  (define begin. ((with-module gauche.internal make-identifier)
                  'begin (find-module 'gauche) '()))
  

  ;; Check feature requirement.  Returns #f if requirement is not
  ;; satisfied.  Returns a list of features to be use'd if requirement
  ;; is satisfied (it can be an emptylist, if the requirement is fulfilled
  ;; by Gauche built-in features).
  (define (fulfill? req seed)
    (cond
     [(identifier? req) (fulfill? (identifier->symbol req) seed)]
     [(symbol? req)
      (let ((p (assq req (cond-features))))
        (and p (if (null? (cdr p)) seed (cons (cadr p) seed))))]
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
      (let ((c1 (fulfill? (car reqs) seed)))
        (and c1 (fulfill-and (cdr reqs) c1)))))

  (define (fulfill-or reqs seed)
    (if (null? reqs)
      #f
      (let ((c1 (fulfill? (car reqs) seed)))
        (or c1 (fulfill-or (cdr reqs) seed)))))

  (define (fulfill-not req seed)
    (if (fulfill? req '()) #f seed))

  (define (fulfill-library rest seed)
    (unless (null? (cdr rest))
      (error "Invalid feature requirement:" `(library ,@rest)))
    (let ((modname (library-name->module-name (car rest))))
      (and (library-exists? modname) seed)))

  (define (rec cls)
    (cond
     [(null? cls) (error "Unfulfilled cond-expand:" cls)]
     [(not (pair? (car cls)))
      (error "Bad clause in cond-expand:" (car cls))]
     [(equal? (caar cls) 'else)
      (if (null? (cdr cls))
        `(,begin. . ,(cdar cls))
        (error "Misplaced else clause in cond-expand:" (car cls)))]
     [(fulfill? (caar cls) '())
      => (lambda (uses)
           `(,begin. ,@(map (lambda (mod) `(,use. ,mod)) uses)
                     ,@(cdar cls)))]
     [else
      (rec (cdr cls))]))

  (rec clauses))

