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
          $
          fluid-let
          ecase
          dotimes doilst doplist while until
          guard
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
;;; applications

;; Haskell-ish application.
;; The starting '$' introduces the macro.
;; Subsequent '$' delimits "one more arguments"
;; Subsequent '$*' delimits "zero or more arguments".
;;
;;  ($ f a b c)         => (f a b c)
;;  ($ f a b c $)       => (lambda (arg) (f a b c arg))
;;  ($ f $ g a b c)     => (f (g a b c))
;;  ($ f $ g a b c $)   => (lambda (arg) (f (g a b c arg)))
;;  ($ f $ g $ h a b c) => (f (g (h a b c)))
;;  ($ f a $ g b $ h c) => (f a (g b (h c)))
;;  ($ f a $ g b $ h $) => (lambda (arg) (f a (g b (h arg))))
;;
;;  ($ f a b c $*)      => (lambda args (apply f a b c args))
;;                         == (pa$ f a b c)
;;  ($ f a b $* g c d)  => (apply f a b (g c d))
;;  ($ f a b $* g c d $) => (lambda (arg) (apply f a b (g c d arg)))
;;  ($ f a b $* g c d $*) => (lambda args (apply f a b (apply g c d args)))
;;  ($ f a b $ g c d $*) => (lambda args (f a b (apply g c d args)))

;; Kludge: We already have binding of '$' in gauche module, created for
;; autoload.  Using (define-syntax $ ...) here makes a separate binding in
;; gauche.common-macros, and that makes the literal comparison of '$'
;; fail, since '$' in here refers to gauche.common-macros#$, while
;; the macro use environment will refer to gauche#$.  We use very ugly
;; hack here to workaround the issue; needs fundamental fix later.
(define-syntax %$
  (syntax-rules ()
    [($ x . xs) (%$-split (x . xs) () ())]
    [($) (syntax-error "invalid $ form" ($))]))
(set! $ %$) ; DON'T DO THIS.  Only works in the current version.

;; ($ a b $ c d $*) => ($* (c d) $ (a b))
;; ($ a b $ c d $* e f) => ((e f) $* (c d) $ (a b))
(define-syntax %$-split
  (syntax-rules ($ $*)
    ;; terminal condition
    [(_ ()   segs (e ...)) (%$-gen #f    ((e ...)           . segs))]
    [(_ ($)  segs (e ...)) (%$-gen (arg) ((e ... arg)       . segs))]
    [(_ ($*) segs (e ...)) (%$-gen arg   ((apply e ... arg) . segs))]
    ;; recurse
    [(_ ($ t ...)  segs (e ...)) (%$-split (t ...) ($  (e ...) . segs) ())]
    [(_ ($* t ...) segs (e ...)) (%$-split (t ...) ($* (e ...) . segs) ())]
    [(_ (t0 t ...) segs (e ...)) (%$-split (t ...) segs (e ... t0))]
    ))

(define-syntax %$-gen
  (syntax-rules ($ $*)
    ;; terminal condition
    [(_ #f     (seg))  seg]
    [(_ formal (seg))  (lambda formal seg)]
    ;; recurse
    [(_ type (seg0 $ (s ...) . segs))  (%$-gen type ((s ... seg0) . segs))]
    [(_ type (seg0 $* (s ...) . segs)) (%$-gen type ((apply s ... seg0) . segs))]
    ))

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

(define-syntax dotimes
  (syntax-rules ()
    [(_ (var n res) . body)
     (do ([limit n]
          [var 0 (+ var 1)])
         [(>= var limit) res]
       . body)]
    [(_ (var n) . body)
     (do ([limit n]
          [var 0 (+ var 1)])
         [(>= var limit) (undefined)]
       . body)]
    [(_ (n) . body)
     (let1 i n
       (cond [(<= i 0) (undefined)]
             [(infinite? i)
              (do () (#f) . body)] ;avoid unnecessary flonum calculation
             [else
              (do ([i i (- i 1)])
                  [(<= i 0) (undefined)]
                . body)]))]
    [(_ . other)
     (syntax-error "malformed dotimes" (dotimes . other))]))

(define-syntax dolist
  (syntax-rules ()
    [(_ (var lis res) . body)
     (do ([p lis (cdr p)])
         [(null? p)
          (let1 var '() res)]      ;bound var for CL compatibility
       (let1 var (car p) . body))]
    [(_ (var lis) . body)
     (do ([p lis (cdr p)])
         [(null? p) '()]
       (let1 var (car p) . body))]
    [(_ (lis) . body)
     (dolist (tmp lis) . body)]
    [(_ . other)
     (syntax-error "malformed dolist" (dolist . other))]))

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

;;;-------------------------------------------------------------
;;; guard (srfi-34)

(define %reraise (with-module gauche.internal %reraise))

(define-syntax guard
  (syntax-rules ()
    [(guard (var . clauses) . body)
     (with-error-handler
         (lambda (e)
           (let ((var e))
             (%guard-rec var e . clauses)))
       (lambda () . body)
       :rewind-before #t)]))

(define-syntax %guard-rec
  (syntax-rules (else =>)
    [(%guard-rec var exc)
     ;; exception handler can return to the caller
     (%reraise)]
    [(%guard-rec var exc (else . exprs))
     (begin . exprs)]
    [(%guard-rec var exc (test => proc) . more)
     (let ((tmp test))
       (if tmp
         (proc tmp)
         (%guard-rec var exc . more)))]
    [(%guard-rec var exc (test . exprs) . more)
     (if test
       (begin . exprs)
       (%guard-rec var exc . more))]
    [(%guard-rec var exc other . more)
     (syntax-error "malformed guard clause" other)]))

