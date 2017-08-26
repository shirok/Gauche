;; Copyright (C) Marc Nieper-Wißkirchen (2016).  All Rights Reserved. 

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

(define-library (srfi 147 test)
  (export run-tests)
  (include-library-declarations "../../custom-macro-transformers.scm")
  (import (srfi 64))
  (begin
    (define (run-tests)
      (test-begin "SRFI 147")

      (test-group "R7RS macros"
	(define-syntax foo
	  (syntax-rules ()
	    ((foo)
	     42)))

	(test-equal 42 (foo))

	(test-equal 42 (let-syntax
			   ((foo
			     (syntax-rules ()
			       ((foo)
				42))))
			 (foo)))

	(test-equal 42 (letrec-syntax
			   ((foo
			     (syntax-rules ()
			       ((foo)
				42)))
			    (bar
			     (syntax-rules ()
			       ((bar)
				(foo)))))
			 (bar))))

      (test-group "Custom macro transformers"
	(define-syntax simple-syntax-rules
	  (syntax-rules ()
	    ((simple-syntax-rules . rules)
	     (syntax-rules () . rules))))

	(define-syntax bar-rules
	  (simple-syntax-rules
	   ((bar-rules (pattern template) ...)
	    (simple-syntax-rules (pattern '(bar template)) ...))))
	
	(define-syntax foo
	  (simple-syntax-rules
	   ((foo)
	    42)))

	(define-syntax bar
	  (bar-rules
	   ((bar x) x)))
	
	(test-equal 42 (foo))

	(test-equal 42 (let-syntax
			   ((foo
			     (simple-syntax-rules
			       ((foo)
				42))))
			 (foo)))

	(test-equal '(bar 42) (bar 42)))

      ;; NB: This doesn't work in Gauche (yet)
      '(test-group "Auxiliary definitions in custom macro transformers"
	(define-syntax my-macro-transformer
	  (syntax-rules ()
	    ((my-macro-transformer)
	     (begin (define foo 2)
		    (syntax-rules ()
		      ((_) foo))))))
	
	(test-equal 42 (* 21 (letrec-syntax ((foo (my-macro-transformer)))
			       (foo)))))
      
      (test-group "Scoping of expansion"
	(define-syntax simple-syntax-rules
	  (syntax-rules ()
	    ((simple-syntax-rules . rules)
	     (syntax-rules () . rules))))

	(test-equal 'foo (let-syntax
			     ((simple-syntax-rules
			       (simple-syntax-rules ((_) 'foo))))
			   (simple-syntax-rules))))

      (test-group "Custom ellipsis"
	(define-syntax my-syntax-rules
	  (syntax-rules !!! ()
	    ((my-syntax-rules e l* rule !!!)
	     (syntax-rules e l* rule !!!))))
	
	(define-syntax foo
	  (my-syntax-rules ::: ()
	    ((foo a) 'a)
	    ((foo a b) '(a . b))		   
  	    ((foo a :::) (list 'a :::))))

	(test-equal '(a b c) (foo a b c)))

      (test-group "Aliases for keywords"
	(define-syntax λ lambda)
	(define foo (λ () 'baz))
	(test-equal 'baz (foo)))

      (test-group "Example from specification"
	(define-syntax syntax-rules*
	  (syntax-rules ()
	    ((syntax-rules* (literal ...) (pattern . templates) ...)
	     (syntax-rules (literal ...) (pattern (begin . templates)) ...))
	    ((syntax-rules* ellipsis (literal ...) (pattern . templates) ...)
	     (syntax-rules ellipsis (literal ...) (pattern (begin . templates)) ...))))

	(test-equal '(1 2) (let-syntax
			       ((foo
				 (syntax-rules* ()
						((foo a b)
						 (define a 1)
						 (define b 2)))))
			     (foo x y)
			     (list x y))))
      
      (test-end)))
)
