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
;;;  $Id: srfi-4.scm,v 1.2 2001-07-12 20:38:59 shirok Exp $
;;;

;; This is a wrapper of libuvector.so
;; Besides defining functions, the DSO sets up a reader hook to enable
;; extended syntax such as #s8(1 2 3)

(define-module srfi-4
  (export <s8vector> make-s8vector s8vector s8vector?
          s8vector-ref s8vector-set! s8vector-copy s8vector-copy!
          s8vector->list list->s8vector s8vector->vector vector->s8vector
          <u8vector> make-u8vector u8vector u8vector?
          u8vector-ref u8vector-set! u8vector-copy u8vector-copy!
          u8vector->list list->u8vector u8vector->vector vector->u8vector
          <s16vector> make-s16vector s16vector s16vector?
          s16vector-ref s16vector-set! s16vector-copy s16vector-copy!
          s16vector->list list->s16vector s16vector->vector vector->s16vector
          <u16vector> make-u16vector u16vector u16vector?
          u16vector-ref u16vector-set! u16vector-copy u16vector-copy!
          u16vector->list list->u16vector u16vector->vector vector->u16vector
          <s32vector> make-s32vector s32vector s32vector?
          s32vector-ref s32vector-set! s32vector-copy s32vector-copy!
          s32vector->list list->s32vector s32vector->vector vector->s32vector
          <u32vector> make-u32vector u32vector u32vector?
          u32vector-ref u32vector-set! u32vector-copy u32vector-copy!
          u32vector->list list->u32vector u32vector->vector vector->u32vector
          <f32vector> make-f32vector f32vector f32vector?
          f32vector-ref f32vector-set! f32vector-copy f32vector-copy!
          f32vector->list list->f32vector f32vector->vector vector->f32vector
          <f64vector> make-f64vector f64vector f64vector?
          f64vector-ref f64vector-set! f64vector-copy f64vector-copy!
          f64vector->list list->f64vector f64vector->vector vector->f64vector
          )

  (dynamic-load "libuvector" :export-symbols #t)

  )

(provide "srfi-4")
