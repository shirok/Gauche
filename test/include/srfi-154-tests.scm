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

(define-library (srfi 154 test)
  (export run-tests)
  (import (scheme base)
	  (srfi 64)
	  (srfi 154))
  (begin
    (define (run-tests)
      (test-begin "SRFI 154")

      (test-assert "Dynamic extents"
	(dynamic-extent? (current-dynamic-extent)))

      (test-equal "Parameter bindings"
	'b
	(let*
	    ((x (make-parameter 'a))
	     (de (parameterize
		     ((x 'b))
		   (current-dynamic-extent))))
	  (parameterize
	      ((x 'c))
	    (with-dynamic-extent
	     de
	     (lambda ()
	       (x))))))

      (test-equal "Closed procedures"
	'a
	(let* ((x (make-parameter 'a))
	       (getter (closed-lambda () (x))))
	  (parameterize ((x 'b))
	    (getter))))

      (test-equal "Multiple values"
	'(1 2)
	(call-with-values
	    (lambda ()
	      (with-dynamic-extent (current-dynamic-extent) (lambda ()
							      (values 1 2))))
	  list))

      (test-equal "Nested with-dynamic-extent"
	1
	(let* ((x (make-parameter 1))
	       (e1 (current-dynamic-extent))
	       (e2 (parameterize ((x 2))
		     (current-dynamic-extent))))
	  (with-dynamic-extent e2 (lambda ()
				    (with-dynamic-extent e1 (lambda ()
							      (x)))))))
      
      (test-end "SRFI 154"))))
