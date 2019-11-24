;;;
;;; common-macros.scm - common macros
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
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

;;; Defines number of useful macros.  This file is to be autoloaded.

;; Note: This file is so fundamental that most other autoloaded files
;; depend on it.  If you modify this file, be very careful not to depend
;; on other autoloaded files, since it is easy to create circular
;; dependency.

(define-module gauche.common-macros
  (export check-arg get-optional get-keyword*
          fluid-let
          doplist while until
          ))
(select-module gauche.common-macros)

;;;-------------------------------------------------------------
;;; bind construct

;; fluid-let written by Dorai Sitaram
;; NB: all threads shares the state of fluid global vers.
;; this is mainly for the comatibility of existing third-party code.
(define-macro fluid-let
  (lambda (varvals . body)
    (let ((vars (map car varvals))
          (vars-twins (map (lambda (ig) (gensym)) varvals))
          (swap (gensym))
          (temp (gensym)))
      `(let (,@(map list vars-twins (map cadr varvals)))
         (let ((,swap
                (lambda ()
                  ,@(map (lambda (var twin)
                           `(let ((,temp ,var))
                              (set! ,var ,twin)
                              (set! ,twin ,temp)))
                         vars vars-twins))))
           (dynamic-wind
               ,swap
               (lambda () ,@body)
               ,swap))))))

;;;-------------------------------------------------------------
;;; useful argument utility

(define-syntax check-arg
  (syntax-rules ()
    [(_ test arg)
     (let ((tmp arg))
       (unless (test tmp)
         (errorf "bad type of argument for ~s: ~s" 'arg tmp)))]
    ))

(define-syntax get-keyword*
  (syntax-rules ()
    [(_ key lis default)
     (let ((li lis))
       (let loop ((l li))
         (cond ((null? l) default)
               ((null? (cdr l)) (error "keyword list not even" li))
               ((eq? key (car l)) (cadr l))
               (else (loop (cddr l))))))]
    [(_ key lis) (get-keyword key lis)]))

(define-syntax get-optional
  (syntax-rules ()
    [(_ args default)
     (let ((a args))
       (if (pair? a) (car a) default))]
    [(_ . other)
     (syntax-error "badly formed get-optional" (get-optional . other))]
    ))

;;;-------------------------------------------------------------
;;; repeat construct

(define-syntax doplist
  (syntax-rules ()
    [(_ ((k v) plis default) . body)
     (do ([p plis (cddr p)])
         [(cond [(null? p) #t]
                [(null? (cdr p))
                 (let ([k (car p)]
                       [v default])
                   . body)]
                [else #f])]
       (let ([k (car p)]
             [v (cadr p)])
         . body))]
    [(_ ((k v) plis) . body)
     (do ([p plis (cddr p)])
         [(cond [(null? p) #t]
                [(null? (cdr p)) (error "plist is not even:" plis)]
                [else #f])]
       (let ([k (car p)]
             [v (cadr p)])
         . body))]
    [(_ . other)
     (syntax-error "malformed doplist" (doplist . other))]))

(define-syntax while
  (syntax-rules (=>)
    [(_ expr guard => var . body)
     (do ((var expr expr))
         ((not (guard var)))
       . body)]
    [(_ expr => var . body)
     (do ((var expr expr))
         ((not var))
       . body)]
    [(_ expr . body)
     (do ()
         ((not expr))
       . body)]
    [(_ . other)
     (syntax-error "malformed while" (while . other))]))

(define-syntax until
  (syntax-rules (=>)
    [(_ expr guard => var . body)
     (do ((var expr expr))
         ((guard var))
       . body)]
    [(_ expr => var . body)
     (do ((var expr expr))
         (var)
       . body)]
    [(_ expr . body)
     (do ()
         (expr)
       . body)]
    [(_ . other)
     (syntax-error "malformed until" (until . other))]))
