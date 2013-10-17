;;;
;;; Adapt SSAX to Gauche
;;;

(define-module sxml.adaptor
  (use srfi-1)
  (use srfi-13)
  (export ascii->char ucscode->char char-return char-tab char-newline
          make-char-quotator assert |--| parser-error cout cerr nl
          string-rindex pp substring?))
(select-module sxml.adaptor)

;; Charcode related stuff, used in ssax.scm
(define ascii->char integer->char)
(define ucscode->char ucs->char)

(define-constant char-return  #\return)
(define-constant char-tab     #\tab)
(define-constant char-newline #\newline)

;; make-char-quotator, used in sxml.to-html and sxml.tools
(define (make-char-quotator rules)
  (lambda (s)
    (with-string-io s
      (lambda ()
        (let loop ((ch (read-char)))
          (cond ((eof-object? ch))
                ((assv ch rules)
                 => (lambda (p) (display (cdr p)) (loop (read-char))))
                (else (display ch) (loop (read-char)))))))
    ))

;; string-rindex is used in sxml-tools
(define string-rindex string-index-right)

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
       ((not (pair? expr)) vars)        ; not an application -- ignore
       ((memq (car expr)
              '(quote let let* letrec let-values* lambda cond quasiquote
                      case define do assert))
        vars)                           ; won't go there
       (else                            ; ignore the head of the application
        (let inner ((expr (cdr expr)) (vars vars))
          (cond
           ((null? expr) vars)
           ((symbol? (car expr))
            (inner (cdr expr)
                   (if (memq (car expr) vars) vars (cons (car expr) vars))))
           (else
            (inner (cdr expr) (loop (car expr) vars)))))))))

  (cond
   ((null? others)              ; the most common case
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
   (else                        ; report: occurs somewhere in 'others'
    (let loop ((exprs (list expr)) (reported others))
      (cond
       ((eq? (car reported) 'report:)
        `(or (and ,@(reverse exprs))
             (begin (cerr "failed assertion: " ',(reverse exprs)
                          ,@(make-print-list #\newline (cdr reported)) nl)
                    (error "assertion failure"))))
       (else (loop (cons (car reported) exprs) (cdr reported)))))))
)

;; Macro used in sxpath.scm

; Read-only decrement
(define-macro (|--| x) `(- ,x 1))

;; Error handler called in SSAX
(define (parser-error  port msg . args)
  (let1 err (open-output-string)
    (display (port-position-prefix port) err)
    (display msg err)
    (dolist [m args] ((if (string? m) display write) m err))
    (newline err)
    (error (get-output-string err))))

;; error reporting

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

(define (cerr . args)
  (for-each (lambda (x)
              (if (procedure? x)
                (x (current-error-port))
                (display x (current-error-port))))
            args))

(define-constant nl "\n")

;; pretty-printer called in sxpathlib.scm (node-trace).  it is used for
;; debugging code, so for the time being we use 'write' instead.
;; we might replace it once Gauche supports pretty-printer natively.
(define (pp arg) (write arg) (newline))

;; small function used in txpath.scm
(define (substring? pat str) (string-contains str pat))

