;;;
;;; gauche.uvector - uniform vectors
;;;  
;;;   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
;;;  $Id: uvector.scm,v 1.7 2003-07-05 03:29:11 shirok Exp $
;;;

;; This module defines the superset of SRFI-4, homogeneous numeric vector
;; types.   Most of basic operations are defined in the DSO module libuvector.
;; Besides defining functions, the DSO sets up a reader hook to enable
;; extended syntax such as #s8(1 2 3).
;; This module also defines methods for collection and sequence frameworks.

(define-module gauche.uvector
  (use gauche.collection)
  (use gauche.sequence)
  (use util.queue)
  (export-all)
  )
(select-module gauche.uvector)
(dynamic-load "libgauche-uvector")

;; collection protocol implementation
(define-macro (%define-srfi-4-collection-interface tag)
  (let* ((tagvector (string->symbol #`",|tag|vector"))
         (class     (string->symbol #`"<,|tagvector|>"))
         (meta      (string->symbol #`"<,|tagvector|-meta>"))
         (len       (string->symbol #`",|tagvector|-length"))
         (ref       (string->symbol #`",|tagvector|-ref"))
         (set       (string->symbol #`",|tagvector|-set!"))
         (copy      (string->symbol #`",|tagvector|-copy"))
         (->list    (string->symbol #`",|tagvector|->list"))
         (list->    (string->symbol #`"list->,|tagvector|"))
         (->vec     (string->symbol #`",|tagvector|->vector"))
         (vec->     (string->symbol #`"vector->,|tagvector|"))
         )
    `(begin
       (define-method call-with-iterator ((v ,class) proc . opts)
         (let* ((start (get-keyword :start opts #f))
                (len   (,len v))
                (i     (or start 0)))
           (proc (lambda () (>= i len))
                 (lambda () (let ((r (,ref v i))) (inc! i) r)))))
       (define-method call-with-builder ((c ,meta) proc . opts)
         (let ((size  (get-keyword :start opts #f)))
           (if size
               (let ((v (,make size))
                     (i 0))
                 (proc (lambda (item) (,set v i item) (inc! i))
                       (lambda () v)))
               (let ((q (make-queue)))
                 (proc (lambda (item) (enqueue! q item))
                       (lambda () (,list-> (dequeue-all! q)))))
               )))
       (define-method referencer ((v ,class)) ,ref)
       (define-method modifier   ((v ,class)) ,set)
       (define-method size-of ((v ,class)) (,len v))
       (define-method coerce-to ((c <list-meta>) (v ,class))
         (,->list v))
       (define-method coerce-to ((c ,meta) (v <list>))
         (,list-> v))
       (define-method coerce-to ((c <vector-meta>) (v ,class))
         (,->vec v))
       (define-method coerce-to ((c ,meta) (v <vector>))
         (,vec-> v))
       (define-method coerce-to ((c ,meta) (v ,class))
         (,copy v))
       (define-method subseq ((v ,class) . args)
         (apply ,copy v args))
       )))

(%define-srfi-4-collection-interface s8)
(%define-srfi-4-collection-interface u8)
(%define-srfi-4-collection-interface s16)
(%define-srfi-4-collection-interface u16)
(%define-srfi-4-collection-interface s32)
(%define-srfi-4-collection-interface u32)
(%define-srfi-4-collection-interface s64)
(%define-srfi-4-collection-interface u64)
(%define-srfi-4-collection-interface f32)
(%define-srfi-4-collection-interface f64)

;; some special cases
(define-method coerce-to ((dst <string-meta>) (src <u8vector>))
  (u8vector->string src))
(define-method coerce-to ((dst <string-meta>) (src <s8vector>))
  (s8vector->string src))
(define-method coerce-to ((dst <u8vector-meta>) (src <string>))
  (string->u8vector src))
(define-method coerce-to ((dst <s8vector-meta>) (src <string>))
  (string->s8vector src))
(define-method coerce-to ((dst <string-meta>) (src <u32vector>))
  (u32vector->string src))
(define-method coerce-to ((dst <string-meta>) (src <s32vector>))
  (s32vector->string src))
(define-method coerce-to ((dst <u32vector-meta>) (src <string>))
  (string->u32vector src))
(define-method coerce-to ((dst <s32vector-meta>) (src <string>))
  (string->s32vector src))

(provide "gauche/uvector")
