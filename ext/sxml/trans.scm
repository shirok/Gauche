;;;
;;; convert the original SSAX.scm into Gauche's preferable format.
;;;


(use srfi-13)

;; The conversion procedure:
;;
;;  - Comments are deleted.
;;  - Run-test macro definition and uses are eliminated.
;;  - Some macro and function definitions are replaced for
;;    the ones that uses Gauche's native method for efficiency.

(define *trans-table*
  '(;; remove run-test stuff
    ((define-macro run-test ...))
    ((define-macro (run-test ...) ...))
    ((run-test ...))
    ((include ...))
    ((cerr ...))
    ((let (...) (run-test ...) ...))
    ((let* (...) (run-test ...) ...))
    ;; optimizing xml-token construction stuff
    ((define (make-xml-token ...) ...)
     (define make-xml-token cons))
    ((define-macro xml-token-kind ...)
     (define xml-token-kind car))
    ((define-macro xml-token-head ...)
     (define xml-token-head cdr))
    ;; Gauche has those procedures natively.
    ((define (cons* ...) ...))
    ((define (string-whitespace? ...) ...)
     (define (string-whitespace? str) (string-every #[\s] str)))
    ((define (fold-right ...) ...))
    ((define (fold ...) ...))
    ;; We have constant definition instead.
    ((define ssax:S-chars ...))
    ;; These forms are in sxml-tools.
    ;; We have Gauche-specific versions for them
    ((define-macro (sxml:find-name-separator ...) ...))
    ((define (sxml:error ...) ...))
    ))

(define (prelude file)
  (format #t ";;; Generated from ~s\n" (sys-basename file)))

;; *very limited* pattern matcher
(define (match? rule sexp)
  (cond
   [(null? rule) (null? sexp)]
   [(eq? rule '...) #t]
   [(not (pair? rule)) (eq? rule sexp)]
   [else (let loop ([eltr rule] [elts sexp])
           (cond [(null? eltr) (null? elts)]
                 [(eq? (car eltr) '...) #t]
                 [(not (pair? elts)) #f]
                 [(match? (car eltr) (car elts)) (loop (cdr eltr) (cdr elts))]
                 [else #f]))]))

(define (replace rules sexp)
  (let loop ([rules rules])
    (cond [(null? rules) (list sexp)]
          [(match? (caar rules) sexp) (cdar rules)]
          [else (loop (cdr rules))])))

(define (include-translating file process)
  (with-input-from-file file
    (^[]
      (prelude file)
      (generator-for-each process read))))

(define (process-body file)
  (include-translating file
                       (^[sexp]
                         (for-each write (replace *trans-table* sexp))
                         (newline))))

(define (process-test file)
  (include-translating file
                       (^[sexp]
                         (when (and (pair? sexp) (eq? (car sexp) 'run-test))
                           (unless (and (pair? (cadr sexp))
                                        (eq? 'define (caadr sexp))
                                        (pair? (cadadr sexp))
                                        (memq (car (cadadr sexp))
                                              '(ssax:warn parser-error)))
                             (write `(%run-test ,@(cdr sexp)))
                             (newline))))))

(define (adjpath templ file) #"~(sys-dirname templ)/~file")

;; entry point
(define (main args)
  (unless (= (length args) 2)
    (error "Usage: gosh trans.scm <template-file>"))
  (let* ([templ (cadr args)]
         [dest  (sys-basename (if (string-suffix? ".scm.in" templ)
                                (string-drop-right templ 3)
                                (string-append templ ".out.scm")))])
    (with-input-from-file templ
      (^[] (with-output-to-file dest
             (^[] (generator-for-each
                   (^[line] (rxmatch-case line
                              [#/^;#include-body "(.*)"/ (#f file)
                               (process-body (adjpath templ file))]
                              [#/^;#include-test "(.*)"/ (#f file)
                               (process-test (adjpath templ file))]
                              [else (display line) (newline)]))
                   read-line)))))
    0))
