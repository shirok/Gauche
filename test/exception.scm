;; test exception handling system 
;; this must come after primsyn, error, macro and object tests.
;; $Id: exception.scm,v 1.1 2004-05-20 04:50:34 shirok Exp $

(use gauche.test)
(test-start "exceptions")

;;--------------------------------------------------------------------
(test-section "constructors")

;; cannot directly make <exception>, for it is an abstract class.
(test* "make <exception>" *test-error*
       (make <exception>))

(test* "make <error>" '(#t #t #f)
       (let ((e (make <error>)))
         (list (is-a? e <exception>)
               (is-a? e <error>)
               (ref e 'message))))

(test* "make <error>" "hoge"
       (ref (make <error> :message "hoge") 'message))

(test* "make <system-error>" '("oops" 12)
       (let ((e (make <system-error> :message "oops" :errno 12)))
         (map (cut ref e <>) '(message errno))))

;;--------------------------------------------------------------------
(test-section "guard")

(test* "guard" '(symbol . a)
       (guard (x
               ((symbol? x) (cons 'symbol x))
               ((is-a? x <error>) 'caught-error))
         (raise 'a)))
       
(test* "guard" 'caught-error
       (guard (x
               ((symbol? x) (cons 'symbol x))
               ((is-a? x <error>) 'caught-error))
         (car 'a)))

(test* "guard (uncaught error)" *test-error*
       (guard (x
               ((symbol? x) (cons 'symbol x))
               ((is-a? x <error>) 'caught-error))
         (raise 4)))

(test* "guard (nested)" 'exn
       (with-error-handler
           values
         (lambda ()
           (guard (ball
                   (#f (display "Caught exception.")))
             (guard (ball
                     (#f (raise ball)))
               (raise 'exn))))))

(test-end)





