;;;
;;; mt-random - Mersenne Twister interface
;;;
;;;  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: mt-random.scm,v 1.1 2002-05-10 10:57:48 shirok Exp $
;;;

(define-module math.mt-random
  (export <mersenne-twister>
          mt-random-real
          mt-random-integer)
  )
(select-module math.mt-random)

(dynamic-load "mt-random")

(provide "math/mt-random")
