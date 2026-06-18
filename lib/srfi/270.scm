;;;
;;;  SRFI=270 - hexadecimal floating-point numbers
;;;

;; SRFI-270 reader/writer is built-in.  This module provides
;; single public procedure, write-hexadecimal-float.
;; Gauche programs doesn't need it, but this is useful for portable programs.

(define-module srfi.270
  (use srfi-13)
  (export write-hexadecimal-float))
(select-module srfi.270)

(define (write-hexadecimal-float z :optional (oport (current-output-port)))
  (assume z <complex>)
  (display (number->string (inexact z) 16) oport))
