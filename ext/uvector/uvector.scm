;;;
;;; gauche.uvector - uniform vectors
;;;
;;;  Copyright(C) 2001-2002 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: uvector.scm,v 1.5 2003-05-28 13:17:37 shirok Exp $
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
(dynamic-load "libuvector")

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
