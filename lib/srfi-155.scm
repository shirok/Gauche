; -*- coding:utf-8 -*-
;;;
;;; srfi-155
;;;

(define-module srfi-155
  (use gauche.parameter)
  (use scheme.lazy :rename ((delay scheme-delay)
                            (delay-force scheme-delay-force)))
  (use srfi-154)
  (export delay delay-force force
	  make-promise promise?
	  forcing-extent dynamic-extent?))
(select-module srfi-155)

;; Reference implementation:

;; Copyright (C) Marc Nieper-Wißkirchen (2017).  All Rights Reserved. 

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define current-forcing-extent (make-parameter #f))

(define (forcing-extent)
  (unless (current-forcing-extent)
    (error "forcing-extent: there is no promise being forced"))
  (current-forcing-extent))

(define-syntax delay
  (syntax-rules (force)
    ((delay (force expression))
     (delay-force expression))
    ((delay expression)
     (let ((dynamic-extent (current-dynamic-extent)))
       (scheme-delay
	(let ((forcing-extent (current-dynamic-extent)))
	  (with-dynamic-extent dynamic-extent (lambda ()
						(parameterize
						    ((current-forcing-extent forcing-extent))
						  expression)))))))))

(define-syntax delay-force
  (syntax-rules ()
    ((delay-force expression)
     (let ((dynamic-extent (current-dynamic-extent)))
       (scheme-delay-force
	(with-dynamic-extent dynamic-extent (lambda ()
					      expression)))))))


