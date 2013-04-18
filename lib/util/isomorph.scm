;;;
;;; isomorph.scm - check isomorphism
;;;
;;;   Copyright (c) 2000-2013  Shiro Kawai  <shiro@acm.org>
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

(define-module util.isomorph
  (use srfi-1)
  (export isomorphic? object-isomorphic?)
  )
(select-module util.isomorph)

(define (isomorphic? a b . args)
  (define ctx (if (pair? args)
                  (if (hash-table? (car args))
                      (car args)
                      (error "hash table required, but got" (car args)))
                  (make-hash-table)))

  (define (iso? a b)
    (cond ((or (boolean? a) (char? a) (number? a) (undefined? a)
               (eof-object? a))
           (eqv? a b))
          ((eq? a '()) (eq? b '()))
          ((hash-table-get ctx a #f)   ;node has been appeared
           => (lambda (bb) (eq? bb b)))
          (else
           (hash-table-put! ctx a b)
           (cond ((pair? a) (and (pair? b)
                                 (iso? (car a) (car b))
                                 (iso? (cdr a) (cdr b))))
                 ((string? a) (and (string? b) (string=? a b)))
                 ((symbol? a) (eq? a b))
                 ((keyword? a) (eqv? a b))
                 ((vector? a) (vector-iso? a b))
                 (else (object-isomorphic? a b ctx))
                 ))))

  (define (vector-iso? a b)
    (and (vector? b)
         (let loop ((la (vector->list a)) (lb (vector->list b)))
           (cond ((null? la) (null? lb))
                 ((null? lb) #f)
                 ((iso? (car la) (car lb)) (loop (cdr la) (cdr lb)))
                 (else #f)))))

  (define (hash-iso? a b)
    (and (hash-table? b)
         (let loop ((la (hash-table-map a cons))
                    (lb (hash-table-map b cons)))
           (cond ((null? a) (null? b))
                 ((null? b) #f)
                 ((assq (caar la) lb)
                  => (lambda (p)
                       (and (iso? (cdar la) (cdr p))
                            (loop (cdr la) (alist-delete (caar la) lb)))))
                 (else #f)))))

  (iso? a b)
  )

(define-method object-isomorphic? (a b context)
  (equal? a b))                         ;default

