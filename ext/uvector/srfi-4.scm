;;;
;;; SRFI-4  homogeneous numeric vectors
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
;;;  $Id: srfi-4.scm,v 1.3 2001-12-02 09:06:33 shirok Exp $
;;;

;; This is a wrapper of libuvector.so
;; Besides defining functions, the DSO sets up a reader hook to enable
;; extended syntax such as #s8(1 2 3).
;; This module also defines methods for collection and sequence frameworks.

(define-module srfi-4
  (use gauche.collection)
  (use gauche.sequence)
  (use util.queue)
  (export-all)
  )
(select-module srfi-4)
(dynamic-load "libuvector" :export-symbols #t)

;; collection protocol implementation
(define-syntax %define-srfi-4-collection-interface
  (syntax-rules ()
    ((_ ?class ?meta ?make ?len ?ref ?set! ?copy ?->list ?list-> ?->vec ?vec->)
     (begin
       (define-method call-with-iterator ((v ?class) proc . opts)
         (let* ((start (get-keyword :start opts #f))
                (len   (?len v))
                (i     (or start 0)))
           (proc (lambda () (>= i len))
                 (lambda () (let ((r (?ref v i))) (inc! i) r)))))
       (define-method call-with-builder ((c ?meta) proc . opts)
         (let ((size  (get-keyword :start opts #f)))
           (if size
               (let ((v (?make size))
                     (i 0))
                 (proc (lambda (item) (?set! v i item) (inc! i))
                       (lambda () v)))
               (let ((q (make-queue)))
                 (proc (lambda (item) (enqueue! q item))
                       (lambda () (?list-> (dequeue-all! q)))))
               )))
       (define-method referencer ((v ?class)) ?ref)
       (define-method modifier   ((v ?class)) ?set!)
       (define-method size-of ((v ?class)) (?len v))
       (define-method coerce-to ((c <list-meta>) (v ?class))
         (?->list v))
       (define-method coerce-to ((c ?meta) (v <list>))
         (?list-> v))
       (define-method coerce-to ((c <vector-meta>) (v ?class))
         (?->vec v))
       (define-method coerce-to ((c ?meta) (v <vector>))
         (?vec-> v))
       (define-method coerce-to ((c ?meta) (v ?class))
         (?copy v))
       ))))

(%define-srfi-4-collection-interface
  <s8vector> <s8vector-meta> make-s8vector s8vector-length
  s8vector-ref s8vector-set! s8vector-copy s8vector->list
  list->s8vector s8vector->vector vector->s8vector)
(%define-srfi-4-collection-interface
  <u8vector> <u8vector-meta> make-u8vector u8vector-length
  u8vector-ref u8vector-set! u8vector-copy u8vector->list
  list->u8vector u8vector->vector vector->u8vector)
(%define-srfi-4-collection-interface
  <s16vector> <s16vector-meta> make-s16vector s16vector-length
  s16vector-ref s16vector-set! s16vector-copy s16vector->list
  list->s16vector s16vector->vector vector->s16vector)
(%define-srfi-4-collection-interface
  <u16vector> <u16vector-meta> make-u16vector u16vector-length
  u16vector-ref u16vector-set! u16vector-copy u16vector->list
  list->u16vector u16vector->vector vector->u16vector)
(%define-srfi-4-collection-interface
  <s32vector> <s32vector-meta> make-s32vector s32vector-length
  s32vector-ref s32vector-set! s32vector-copy s32vector->list
  list->s32vector s32vector->vector vector->s32vector)
(%define-srfi-4-collection-interface
  <u32vector> <u32vector-meta> make-u32vector u32vector-length
  u32vector-ref u32vector-set! u32vector-copy u32vector->list
  list->u32vector u32vector->vector vector->u32vector)
(%define-srfi-4-collection-interface
  <s64vector> <s64vector-meta> make-s64vector s64vector-length
  s64vector-ref s64vector-set! s64vector-copy s64vector->list
  list->s64vector s64vector->vector vector->s64vector)
(%define-srfi-4-collection-interface
  <u64vector> <u64vector-meta> make-u64vector u64vector-length
  u64vector-ref u64vector-set! u64vector-copy u64vector->list
  list->u64vector u64vector->vector vector->u64vector)
(%define-srfi-4-collection-interface
  <f32vector> <f32vector-meta> make-f32vector f32vector-length
  f32vector-ref f32vector-set! f32vector-copy f32vector->list
  list->f32vector f32vector->vector vector->f32vector)
(%define-srfi-4-collection-interface
  <f64vector> <f64vector-meta> make-f64vector f64vector-length
  f64vector-ref f64vector-set! f64vector-copy f64vector->list
  list->f64vector f64vector->vector vector->f64vector)

(provide "srfi-4")
