;;;
;;; regexp.scm - regexp-related macros
;;;  
;;;   Copyright (c) 2000-2008  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: regexp.scm,v 1.18 2008-05-10 13:35:56 shirok Exp $
;;;

(define-module gauche.regexp
  (export rxmatch-let rxmatch-if rxmatch-cond rxmatch-case))
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

(provide "gauche/regexp")
