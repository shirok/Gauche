;;;
;;; parseopt.scm - yet another command-line argument parser
;;;
;;;  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: parseopt.scm,v 1.1 2001-07-02 00:25:32 shirok Exp $
;;;

(define-module gauche.parseopt
  (use gauche.regexp)
  (use srfi-2)
  (export make-option-parser parse-options))
(select-module gauche.parseopt)

;; Help functions

;; parse optspec clause
(define (compose-entry a-spec)
  (unless (string? (car a-spec))
    (error "option spec must be a string, but got" (car a-spec)))
  (unless (procedure? (cadr a-spec))
    (error "action spec must be a procedure, but got" (cadr a-spec)))
  (rxmatch-if (rxmatch #/^-*([-\w_+]+)(=(.+))?$/ (car a-spec))
      (#f opt #f argspec)
    (list opt (if argspec (string->list argspec) '()) (cadr a-spec))
    (error "unrecognized option spec:" (car a-spec))))

;; From the args given at the command line, get a next option.
;; Returns option string and rest args.
(define (next-option args)
  (if (null? args)
      (values #f '())
      (rxmatch-case (car args)
        (test (lambda (opt) (string=? opt "--"))
              (values #f (cdr args)))
        (#/^--?(\w[-\w_+]*)(=(.*))?$/
          (#f opt #f maybe-arg)
          (values opt (if maybe-arg (cons maybe-arg (cdr args)) (cdr args))))
        (else
         (values #f args)))))

;; From the list of optarg spec and given command line arguments,
;; get a list of optargs.  Returns optargs and unconsumed args.
(define (get-optargs option args argspec)
  (define (get-number arg)
    (or (string->number arg)
        (errorf "a number is required for option ~a, but got ~a"
                option arg)))
  (define (get-real arg)
    (or (and-let* ((num (string->number arg))
                   ((real? num)))
          num)
        (errorf "a real number is required for option ~a, but got ~a"
                option arg)))
  (define (get-integer arg)
    (or (and-let* ((num (string->number arg))
                   ((exact? num)))
          num)
        (errorf "an integer is required for option ~a, but got ~a"
                option arg)))
  
  (let loop ((spec argspec)
             (args args)
             (optargs '()))
    (cond ((null? spec) (values (reverse! optargs) args))
          ((null? args) (error "running out the arguments for option"
                               option))
          (else
           (case (car spec)
             ((#\s) (loop (cdr spec) (cdr args) (cons (car args) optargs)))
             ((#\n) (loop (cdr spec) (cdr args)
                          (cons (get-number (car args)) optargs)))
             ((#\f) (loop (cdr spec) (cdr args)
                          (cons (get-real (car args)) optargs)))
             ((#\i) (loop (cdr spec) (cdr args)
                          (cons (get-integer (car args)) optargs)))
             (else (error "unknown option argument spec:" (car spec))))))
    )
  )

;; Now, this is the argument parser body.
(define (parse-cmdargs args speclist fallback)
  (let loop ((args args))
    (receive (option nextargs) (next-option args)
      (if option
          (cond ((assoc option speclist)
                 => (lambda (entry)
                      (receive (optargs nextargs)
                          (get-optargs option nextargs (cadr entry))
                        (apply (caddr entry) optargs)
                        (loop nextargs))))
                (else (fallback option nextargs loop)))
          nextargs))))

;; Build 
(define (build-option-parser spec fallback)
  (let ((speclist (map compose-entry spec)))
    (lambda (args . maybe-fallback)
      (let ((fallback
             (if (pair? maybe-fallback)
                 (if (procedure? (car maybe-fallback))
                     (car maybe-fallback)
                     (error "fallback needs to be a procedure:"
                            (car maybe-fallback)))
                 fallback)))
        (parse-cmdargs args speclist fallback)))))

;;;
;;; The main body of the macros
;;;

(define-syntax make-option-parser
  (syntax-rules ()
    ((_ clauses)
     (make-option-parser-int clauses ()))))

(define-syntax make-option-parser-int
  (syntax-rules (else =>)
    ((_ () specs)
     (build-option-parser (list . specs)
                          (lambda (option args looper)
                            (error "unrecognized option:" option))))
    ((_ ((else args . body)) specs)
     (build-option-parser (list . specs) (lambda args . body)))
    ((_ ((else => proc)) specs)
     (build-option-parser (list . specs) proc))
    ((_ ((optspec => proc) . clause) (spec ...))
     (make-option-parser-int clause (spec ... (list optspec proc))))
    ((_ ((optspec vars . body) . clause) (spec ...))
     (make-option-parser-int clause
                                (spec ... (list optspec (lambda vars . body)))))
    ))

(define-syntax parse-options
  (syntax-rules ()
    ((_ args clauses)
     ((make-option-parser clauses) args))))

(provide "gauche/parseopt")
