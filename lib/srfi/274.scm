;;;
;;; SRFI-274 Extended List Conversion Procedures
;;;

;; Gauche-provided list conersion procedures below are all enhanced
;; to support start/end optional arguments.  This module is merely
;; to provide importable namespace.

(define-module srfi.274
  (use util.stream)
  (use scheme.ideque)
  (use gauche.generator)
  (use gauche.uvector)
  (export list-copy                     ;builtin
          list->string                  ;builtin
          list->vector                  ;builtin
          list->stream                  ;util.stream
          list->ideque                  ;scheme.ideque
          list->generator               ;gauche.generator
          list->u8vector                ;gauche.uvector
          list->s8vector
          list->u16vector
          list->s16vector
          list->u32vector
          list->s32vector
          list->u64vector
          list->s64vector
          list->f16vector
          list->f32vector
          list->f64vector
          list->c32vector
          list->c64vector
          list->c128vector))
