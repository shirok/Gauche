(define cur-section '())(define errs '())
(define SECTION (lambda args
		  (display "SECTION") (write args) (newline)
		  (set! cur-section args) #t))
(define record-error (lambda (e) (set! errs (cons (list cur-section e) errs))))

(define test
  (lambda (expect fun . args)
    (write (cons fun args))
    (display "  ==> ")
    ((lambda (res)
      (write res)
      (newline)
      (cond ((not (equal? expect res))
	     (record-error (list res expect (cons fun args)))
	     (display " BUT EXPECTED ")
	     (write expect)
	     (newline)
	     #f)
	    (else #t)))
     (if (procedure? fun) (apply fun args) (car args)))))

(define (report-errs)
  (newline)
  (if (null? errs) (display "Passed all tests")
      (begin
	(display "errors were:")
	(newline)
	(display "(SECTION (got expected (call)))")
	(newline)
	(for-each (lambda (l) (write l) (newline))
		  errs)))
  (newline))

(SECTION 2 1);; test that all symbol characters are supported.
'(+ - ... !.. $.+ %.- &.! *.: /:. :+. <-. =. >. ?. ~. _. ^.)

(SECTION 3 4)
(define disjoint-type-functions
  (list boolean? char? null? number? pair? procedure? string? symbol? vector?))
(define type-examples
  (list
   #t #f #\a '() 9739 '(test) record-error "test" "" 'test '#() '#(a b c) ))
(define i 1)
(for-each (lambda (x) (display (make-string i #\ ))
		  (set! i (+ 3 i))
		  (write x)
		  (newline))
	  disjoint-type-functions)
