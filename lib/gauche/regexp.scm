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
;;;  $Id: regexp.scm,v 1.6 2001-09-23 11:34:17 shirok Exp $
;;;

(define-module gauche.regexp
  (use srfi-13)
  (export rxmatch-let rxmatch-if rxmatch-cond rxmatch-case
          regexp-replace regexp-replace-all))
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
    ;; main entry
    ((rxmatch-case ?str ?clause ...)
     (let* ((temp ?str)
            (strp (string? temp)))
       (rxmatch-case #t temp strp ?clause ...)))
    ))

;; aux routine for regexp-replace[-all]
;; "abc\\1de\\3" => '("abc" 1 "de" 3)
(define (regexp-parse-subpattern sub)
  (let loop ((sub sub) (r '()))
    (receive (head rest) (string-scan sub #\\ 'both)
      (if (not head)
          (reverse (cons sub r))
          (let ((i (string-skip rest #[\d])))
            (cond ((not i) (reverse (list* (string->number rest) head r)))
                  ((= i 0) (loop rest (list* head r)))
                  (else
                   (loop (string-drop rest i)
                         (list* (string->number (string-take rest i)) head r)))
                  ))))
    ))

(define (regexp-replace-rec match subpat out rec)
  (display (rxmatch-before match) out)
  (for-each (lambda (pat)
              (display (if (number? pat)
                           (rxmatch-substring match pat)
                           pat)
                       out))
            subpat)
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

;; The inner call is redundant to avoid creation of output string
;; when no match at all.
(define (regexp-replace-all rx string sub)
  (let ((subpat (regexp-parse-subpattern sub))
        (match  (rxmatch rx string)))
    (if match
        (call-with-output-string
          (lambda (out)
            (define (loop str)
              (unless (string-null? str)
                (cond ((rxmatch rx str)
                       => (lambda (match)
                            (regexp-replace-rec match subpat out loop)))
                      (else (display str out)))))
            (regexp-replace-rec match subpat out loop)))
        string)))

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
