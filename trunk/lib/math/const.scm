;;;
;;; math.const - useful constants
;;;
;;; public domain
;;;

(define-module math.const
  (export pi e 1/pi 180/pi pi/2 pi/4 pi/180 2pi))

(select-module math.const)

(define-constant pi     3.141592653589793)
(define-constant 2pi    6.283185307179586)
(define-constant pi/2   1.5707963267948966)
(define-constant pi/4   0.7853981633974483)
(define-constant pi/180 0.017453292519943295)
(define-constant 1/pi   0.3183098861837907)
(define-constant 180/pi 57.29577951308232)
(define-constant e      2.718281828459045)

