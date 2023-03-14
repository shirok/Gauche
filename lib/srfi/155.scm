; -*- coding:utf-8 -*-
;;;
;;; SRFI-155
;;;

(define-module srfi.155
  (use srfi.154)                        ;dynamic-extent?

  ;; Gauche's built-in delay/force are SRFI-155 compatible.
  (export delay delay-force force
          make-promise promise?
          forcing-extent dynamic-extent?))
(select-module srfi.155)

(define (forcing-extent)
  (error "forcing-extent is not supported (SRFI-155 optional procedure)"))

(define (make-promise obj)
  (if (promise? obj) obj (delay obj)))

(define-syntax delay-force lazy)
