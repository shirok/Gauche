;;;
;;; regexp.scm - regexp-related utilities
;;;
;;;  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: regexp.scm,v 1.10 2002-12-20 07:32:27 shirok Exp $
;;;

(define-module gauche.regexp
  (export rxmatch-let rxmatch-if rxmatch-cond rxmatch-case
          regexp-replace regexp-replace-all regexp-quote))
(select-module gauche.regexp)

(define-syntax rxmatch-bind*
  (syntax-rules ()
    ((rxmatch-bind* ?n ?match () ?form ...)
     (begin ?form ...))
    ((rxmatch-bind* ?n ?match (#f ?vars ...) ?form ...)
     (rxmatch-bind* (+ ?n 1) ?match (?vars ...) ?form ...))
    ((rxmatch-bind* ?n ?match (?var ?vars ...) ?form ...)
     (let ((?var (rxmatch-substring ?match ?n)))
       (rxmatch-bind* (+ ?n 1) ?match (?vars ...) ?form ...)))
    ))

(define-syntax rxmatch-let
  (syntax-rules ()
    ((rxmatch-let ?expr (?var ...) ?form ...)
     (cond (?expr
            => (lambda (match)
                 (rxmatch-bind* 0 match (?var ...) ?form ...)))
           (else (error "rxmatch-let: match failed:" '?expr))))))

(define-syntax rxmatch-if
  (syntax-rules ()
    ((rxmatch-if ?expr (?var ...) ?then ?else)
     (cond (?expr
            => (lambda (match)
                 (rxmatch-bind* 0 match (?var ...) ?then)))
           (else ?else)))))

(define-syntax rxmatch-cond
  (syntax-rules (test else =>)
    ((rxmatch-cond)
     #f)
    ((rxmatch-cond (else ?form ...))
     (begin ?form ...))
    ((rxmatch-cond (test ?expr => ?obj) ?clause ...)
     (cond (?expr => ?obj) (else (rxmatch-cond ?clause ...))))
    ((rxmatch-cond (test ?expr ?form ...) ?clause ...)
     (if ?expr (begin ?form ...) (rxmatch-cond ?clause ...)))
    ((rxmatch-cond (?matchexp ?bind ?form ...) ?clause ...)
     (rxmatch-if ?matchexp ?bind
               (begin ?form ...)
               (rxmatch-cond ?clause ...)))))

(define-syntax rxmatch-case
  (syntax-rules (test else =>)
    ((rxmatch-case #t ?temp ?strp)
     #f)
    ((rxmatch-case #t ?temp ?strp (else ?form ...))
     (begin ?form ...))
    ((rxmatch-case #t ?temp ?strp (test ?proc => ?obj) ?clause ...)
     (cond ((?proc ?temp) => ?obj)
           (else (rxmatch-case #t ?temp ?strp ?clause ...))))
    ((rxmatch-case #t ?temp ?strp (test ?proc ?form ...) ?clause ...)
     (if (?proc ?temp)
         (begin ?form ...)
         (rxmatch-case #t ?temp ?strp ?clause ...)))
    ((rxmatch-case #t ?temp ?strp (?re ?bind ?form ...) ?clause ...)
     (rxmatch-if (and ?strp (rxmatch ?re ?temp))
          ?bind
       (begin ?form ...)
       (rxmatch-case #t ?temp ?strp ?clause ...)))
    ((rxmatch-case #t ?temp ?strip ?clause ...)
     (syntax-error "malformed rxmatch-case"))
    ;; main entry
    ((rxmatch-case ?str ?clause ...)
     (let* ((temp ?str)
            (strp (string? temp)))
       (rxmatch-case #t temp strp ?clause ...)))
    ))

;;---------------------------------------------------------
;; regexp replace

;; aux routine for regexp-replace[-all]
;; "abc\\1de\\3" => '("abc" 1 "de" 3)
(define (regexp-parse-subpattern sub)
  (cond
   ((string? sub)
    (let loop ((sub sub) (r '()))
      (cond ((rxmatch #/\\((\d+)|(.))/ sub)
             => (lambda (m)
                  (cond ((rxmatch-substring m 2)
                         => (lambda (d)
                              (loop (rxmatch-after m)
                                    (list* (string->number d)
                                           (rxmatch-before m)
                                           r))))
                        ((rxmatch-substring m 3)
                         => (lambda (c)
                              (loop (rxmatch-after m)
                                    (list* c (rxmatch-before m) r)))))))
            (else (reverse (cons sub r))))))
   ((procedure? sub) sub)
   (else (error "string or procedure required, but got" sub))))

;; internal loop
(define (regexp-replace-rec match subpat out rec)
  (display (rxmatch-before match) out)
  (if (procedure? subpat)
      (display (subpat match) out)
      (for-each (lambda (pat)
                  (display (if (number? pat)
                               (rxmatch-substring match pat)
                               pat)
                           out))
                subpat))
  (rec (rxmatch-after match)))

(define (regexp-replace rx string sub)
  (let ((subpat (regexp-parse-subpattern sub))
        (match  (rxmatch rx string)))
    (if match
        (call-with-output-string
          (lambda (out)
            (regexp-replace-rec match subpat out
                                (lambda (str) (display str out)))))
        string)))

;; The inner call is awkward to avoid creation of output string
;; when no match at all.
(define (regexp-replace-all rx string sub)
  (let ((subpat (regexp-parse-subpattern sub))
        (match  (rxmatch rx string)))
    (if match
        (call-with-output-string
          (lambda (out)
            (define (loop str)
              (unless (equal? str "")
                (cond ((rxmatch rx str)
                       => (lambda (match)
                            (regexp-replace-rec match subpat out loop)))
                      (else (display str out)))))
            (regexp-replace-rec match subpat out loop)))
        string)))

;; Contributed from Alex Shinn; modified a bit by shiro
(define (regexp-quote str)
  (with-string-io
   str
   (lambda ()
     (let loop ((c (read-char)))
       (unless (eof-object? c)
         (when (char-set-contains? #[\\|\[\](){}.*+?^$] c) (write-char #\\))
         (write-char c)
         (loop (read-char)))))))
       
;;; scsh compatibility

;(define regexp-search rxmatch)
;(define match:start rxmatch-start)
;(define match:end   rxmatch-end)
;(define match:substring rxmatch-substring)
;
;(define-syntax let-match
;  (syntax-rules ()
;    ((let-match . ?body) (rxmatch-let . ?body))))
;
;(define-syntax if-match
;  (syntax-rules ()
;    ((if-match . ?body) (rxmatch-if . ?body))))
;
;(define-syntax match-cond
;  (syntax-rules ()
;    ((match-cond . ?body) (rxmatch-cond . ?body))))

(provide "gauche/regexp")
