;; srfi-160 - Homogeneous numeric vector libraries

(define-module srfi-160
  (extend gauche.uvector.u8
          gauche.uvector.s8
          gauche.uvector.u16
          gauche.uvector.s16
          gauche.uvector.u32
          gauche.uvector.s32
          gauche.uvector.u64
          gauche.uvector.s64
          gauche.uvector.f16
          gauche.uvector.f32
          gauche.uvector.f64
          gauche.uvector.c32
          gauche.uvector.c64
          gauche.uvector.c128))

