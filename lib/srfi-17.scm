;;;
;;; SRFI-17 : generalized set!
;;;

;;; This is just a quick implementation.  There is a lot of room of
;;; optimization (esp. using "lock" nature of getter-with-setter) -SK

;;; $Id: srfi-17.scm,v 1.2 2001-04-01 07:27:46 shiro Exp $

;; Generalized set! will eventually be integrated into core interpreter,
;; so we define things in the gauche module.   
(select-module gauche)

;; NB: this trick is only possible in the current architecture.
(define %set! (with-module scheme set!))

(define-syntax set!
  (syntax-rules ()
    ((set! (?proc ?arg ...) ?value)
     ((setter ?proc) ?arg ... ?value))
    ((set! ?var ?value)
     (%set! ?var ?value))))

(define *setter-table*  (make-hash-table))
(define *setter-locked* (make-hash-table))

(define (setter proc)
  (or (hash-table-get *setter-table* proc #f)
      (error "no setter defined for procedure ~s" proc)))

(define (%setter-set! proc setter . lock?)
  (when (hash-table-get *setter-locked* proc #f)
    (error "cannot change setter of ~s" proc))
  (hash-table-put! *setter-table* proc setter)
  (when (and (pair? lock?) (car lock?))
    (hash-table-put! *setter-locked* proc #t)))

(define (getter-with-setter get set)
  (%setter-set! get set)
  (hash-table-put! *setter-locked* get #t)
  get)

;; predefined setters
(let ()
  (%setter-set! setter %setter-set! #t)
  (%setter-set! car    set-car! #t)
  (%setter-set! cdr    set-cdr! #t)
  (%setter-set! caar   (lambda (p v) (set-car! (car p) v)) #t)
  (%setter-set! cadr   (lambda (p v) (set-car! (cdr p) v)) #t)
  (%setter-set! cdar   (lambda (p v) (set-cdr! (car p) v)) #t)
  (%setter-set! cddr   (lambda (p v) (set-cdr! (cdr p) v)) #t)
  (%setter-set! caaar  (lambda (p v) (set-car! (caar p) v)) #t)
  (%setter-set! caadr  (lambda (p v) (set-car! (cadr p) v)) #t)
  (%setter-set! cadar  (lambda (p v) (set-car! (cdar p) v)) #t)
  (%setter-set! caddr  (lambda (p v) (set-car! (cddr p) v)) #t)
  (%setter-set! cdaar  (lambda (p v) (set-cdr! (caar p) v)) #t)
  (%setter-set! cdadr  (lambda (p v) (set-cdr! (cadr p) v)) #t)
  (%setter-set! cddar  (lambda (p v) (set-cdr! (cdar p) v)) #t)
  (%setter-set! cdddr  (lambda (p v) (set-cdr! (cddr p) v)) #t)
  (%setter-set! caaaar (lambda (p v) (set-car! (caaar p) v)) #t)
  (%setter-set! caaadr (lambda (p v) (set-car! (caadr p) v)) #t)
  (%setter-set! caadar (lambda (p v) (set-car! (cadar p) v)) #t)
  (%setter-set! caaddr (lambda (p v) (set-car! (caddr p) v)) #t)
  (%setter-set! cadaar (lambda (p v) (set-car! (cdaar p) v)) #t)
  (%setter-set! cadadr (lambda (p v) (set-car! (cdadr p) v)) #t)
  (%setter-set! caddar (lambda (p v) (set-car! (cddar p) v)) #t)
  (%setter-set! cadddr (lambda (p v) (set-car! (cdddr p) v)) #t)
  (%setter-set! cdaaar (lambda (p v) (set-cdr! (caaar p) v)) #t)
  (%setter-set! cdaadr (lambda (p v) (set-cdr! (caadr p) v)) #t)
  (%setter-set! cdadar (lambda (p v) (set-cdr! (cadar p) v)) #t)
  (%setter-set! cdaddr (lambda (p v) (set-cdr! (caddr p) v)) #t)
  (%setter-set! cddaar (lambda (p v) (set-cdr! (cdaar p) v)) #t)
  (%setter-set! cddadr (lambda (p v) (set-cdr! (cdadr p) v)) #t)
  (%setter-set! cdddar (lambda (p v) (set-cdr! (cddar p) v)) #t)
  (%setter-set! cddddr (lambda (p v) (set-cdr! (cdddr p) v)) #t)
  (%setter-set! string-ref string-set! #t)
  (%setter-set! vector-ref vector-set! #t)
  )

(provide "srfi-17")

