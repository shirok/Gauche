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
;;;  $Id: uvector.scm,v 1.2 2002-08-10 01:06:18 shirok Exp $
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
(dynamic-load "libuvector" :export-symbols #t)

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
         ;; arithmetic ops
         (add       (string->symbol #`",|tagvector|-add"))
         (sub       (string->symbol #`",|tagvector|-sub"))
         (mul       (string->symbol #`",|tagvector|-mul"))
         (div       (string->symbol #`",|tagvector|-div"))
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
       ;; Arithmetic operation override
       (define-method object-+ ((a ,class) (b ,class)) (,add a b))
       (define-method object-+ ((a ,class) (b <list>)) (,add a b))
       (define-method object-+ ((a ,class) (b <vector>)) (,add a b))
       (define-method object-+ ((a ,class) (b <number>)) (,add a b))
       (define-method object-+ ((a <number>) (b ,class)) (,add b a))
       (define-method object-- ((a ,class) (b ,class)) (,sub a b))
       (define-method object-- ((a ,class) (b <list>)) (,sub a b))
       (define-method object-- ((a ,class) (b <vector>)) (,sub a b))
       (define-method object-- ((a ,class) (b <number>)) (,sub a b))
       (define-method object-- ((a <number>) (b ,class)) (,sub b a))
       (define-method object-* ((a ,class) (b ,class)) (,mul a b))
       (define-method object-* ((a ,class) (b <list>)) (,mul a b))
       (define-method object-* ((a ,class) (b <vector>)) (,mul a b))
       (define-method object-* ((a ,class) (b <number>)) (,mul a b))
       (define-method object-* ((a <number>) (b ,class)) (,mul b a))
       ,@(if (member tag '(f32 f64))
             `((define-method object-/ ((a ,class) (b ,class)) (,div a b))
               (define-method object-/ ((a ,class) (b <list>)) (,div a b))
               (define-method object-/ ((a ,class) (b <vector>)) (,div a b))
               (define-method object-/ ((a ,class) (b <number>)) (,div a b))
               (define-method object-/ ((a <number>) (b ,class)) (,div b a))
               )
             '())
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

(provide "gauche/uvector")
