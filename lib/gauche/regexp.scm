;;;
;;; regexp.scm - regexp-related utilities
;;;  
;;;   Copyright (c) 2000-2007  Shiro Kawai  <shiro@acm.org>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  
;;;  $Id: regexp.scm,v 1.15 2007-03-02 07:39:08 shirok Exp $
;;;

(define-module gauche.regexp
  (export rxmatch-let rxmatch-if rxmatch-cond rxmatch-case
          regexp-replace regexp-replace-all
          regexp-replace* regexp-replace-all*
          regexp-quote))
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
                          (when (= (rxmatch-start match) (rxmatch-end match))
                            (error "regexp-replace-all: matching zero-length string causes infinite loop:" rx))
                          (regexp-replace-rec match subpat out loop)))
                    (else (display str out)))))
          (regexp-replace-rec match subpat out loop)))
      string)))

;; Multiple replacement 
(define (regexp-replace-driver name func-1)
  (lambda (string rx sub . more)
    (cond ((null? more)
           (func-1 rx string sub))
          (else
           (unless (zero? (modulo (length more) 2))
             (errorf "~a: regexp and subsitution don't pair up" name))
           (let loop ((s (func-1 rx string sub))
                      (args more))
             (if (null? args)
               s
               (loop (func-1 (car args) s (cadr args))
                     (cddr args))))))))

(define regexp-replace*
  (regexp-replace-driver 'regexp-replace* regexp-replace))
(define regexp-replace-all*
  (regexp-replace-driver 'regexp-replace-all* regexp-replace-all))

;; Contributed from Alex Shinn; modified a bit by shiro
(define (regexp-quote str)
  (with-string-io str
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
