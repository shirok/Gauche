;;;
;;; Adapt SSAX to Gauche
;;;
;;; $Id: adaptor.scm,v 1.2 2003-07-20 12:37:52 shirok Exp $
;;;

(define-module sxml.adaptor
  (use srfi-1)
  (export assert |--| begin0 let-values*
          *SSAX:warn-handler* SSAX:warn parser-error))
(select-module sxml.adaptor)

;; Derived from Oleg's myenv.scm -----------------------------

; assert the truth of an expression (or of a sequence of expressions)
;
; syntax: assert ?expr ?expr ... [report: ?r-exp ?r-exp ...]
;
; If (and ?expr ?expr ...) evaluates to anything but #f, the result
; is the value of that expression.
; If (and ?expr ?expr ...) evaluates to #f, an error is reported.
; The error message will show the failed expressions, as well
; as the values of selected variables (or expressions, in general).
; The user may explicitly specify the expressions whose
; values are to be printed upon assertion failure -- as ?r-exp that
; follow the identifier 'report:'
; Typically, ?r-exp is either a variable or a string constant.
; If the user specified no ?r-exp, the values of variables that are
; referenced in ?expr will be printed upon the assertion failure.

(define-macro (assert expr . others)
			; given the list of expressions or vars,
			; make the list appropriate for cerr
  (define (make-print-list prefix lst)
    (cond
     ((null? lst) '())
     ((symbol? (car lst))
      (cons #\newline
	(cons (list 'quote (car lst))
	  (cons ": " (cons (car lst) (make-print-list #\newline (cdr lst)))))))
     (else 
      (cons prefix (cons (car lst) (make-print-list "" (cdr lst)))))))

			; return the list of all unique "interesting"
			; variables in the expr. Variables that are certain
			; to be bound to procedures are not interesting.
  (define (vars-of expr)
    (let loop ((expr expr) (vars '()))
      (cond
       ((not (pair? expr)) vars)	; not an application -- ignore
       ((memq (car expr) 
	      '(quote let let* letrec let-values* lambda cond quasiquote
		      case define do assert))
	vars)				; won't go there
       (else				; ignore the head of the application
	(let inner ((expr (cdr expr)) (vars vars))
	  (cond 
	   ((null? expr) vars)
	   ((symbol? (car expr))
	    (inner (cdr expr)
		   (if (memq (car expr) vars) vars (cons (car expr) vars))))
	   (else
	    (inner (cdr expr) (loop (car expr) vars)))))))))

  (cond
   ((null? others)		; the most common case
    `(or ,expr (begin (cerr "failed assertion: " ',expr nl "bindings"
			    ,@(make-print-list #\newline (vars-of expr)) nl)
		      (error "assertion failure"))))
   ((eq? (car others) 'report:) ; another common case
    `(or ,expr (begin (cerr "failed assertion: " ',expr
			    ,@(make-print-list #\newline (cdr others)) nl)
		      (error "assertion failure"))))
   ((not (memq 'report: others))
    `(or (and ,expr ,@others)
	 (begin (cerr "failed assertion: " '(,expr ,@others) nl "bindings"
		      ,@(make-print-list #\newline
			 (vars-of (cons 'and (cons expr others)))) nl)
		      (error "assertion failure"))))
   (else			; report: occurs somewhere in 'others'
    (let loop ((exprs (list expr)) (reported others))
      (cond
       ((eq? (car reported) 'report:)
	`(or (and ,@(reverse exprs))
	     (begin (cerr "failed assertion: " ',(reverse exprs)
			  ,@(make-print-list #\newline (cdr reported)) nl)
		    (error "assertion failure"))))
       (else (loop (cons (car reported) exprs) (cdr reported)))))))
)

;; Macros used in sxpath.scm (increment/decrement operators) ------

; Read-only decrement
(define-macro (|--| x) `(- ,x 1))


;; Macros used in ssax main body ----------------------------------

(define-syntax begin0
  (syntax-rules ()
    ((_ form forms ...)
     (let ((var form)) forms ... var))))


; Multiple values are supported natively in Gauche.
; NB. we can't simply alias let-values* to SRFI-11 let*-values,
; for let-values* differs in the handling of the variables that
; receives a single value.
;
;   (let-values* ((v 1)) v)  => 1
;   (let*-values ((v 1)) v)  => (1)   ; SRFI-11 behavior

(define-syntax let-values*
  (syntax-rules ()
    ((_ () . body)
     (begin . body))
    ((_ (((vars ...) init) bindings ...) . body)
     (receive (vars ...) init
       (let-values* (bindings ...) . body)))
    ((_ ((var init) bindings ...) . body)
     (let ((var init))
       (let-values* (bindings ...) . body)))))

;; Warn and error procedures. ------------------------------------
(define *SSAX:warn-handler* #f)

(define (SSAX:warn port msg . args)
  (when (procedure? *SSAX:warn-handler*)
    (let ((err (open-output-string)))
      (display (port-position-prefix port) err)
      (display "Warning: " err)
      (if (and (string? msg) (string-prefix? "\n" msg))
          (display (string-drop msg 1) err)
          (display msg err))
      (for-each (lambda (m)
                  (cond ((equal? m "\n")) ;ignore
                        ((string? m) (display m err))
                        (else (write m err))))
                args)
      (newline err)
      (*SSAX:warn-handler* (get-output-string err)))))

(define (parser-error  port msg . args)
  (let ((err (open-output-string)))
    (display (port-position-prefix port) err)
    (for-each (lambda (m) ((if (string? m) display write) m err))
              args)
    (newline err)
    (error (get-output-string err))))


(provide "ssax/adaptor")
