;; test exception handling system 
;; this must come after primsyn, error, macro and object tests.
;; $Id: exception.scm,v 1.4 2004-10-10 09:52:10 shirok Exp $

(use gauche.test)
(test-start "exceptions")

;;--------------------------------------------------------------------
(test-section "constructors")

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

(test* "guard (uncaught error)" '(else . 4)
       (guard (x
               ((symbol? x) (cons 'symbol x))
               ((is-a? x <error>) 'caught-error)
               (else (cons 'else x)))
         (raise 4)))

(test* "guard (subtype)" 'read-error
       (guard (x
               ((is-a? x <read-error>) 'read-error)
               ((is-a? x <system-error>) 'system-error)
               ((is-a? x <error>) 'error)
               (else (cons 'else x)))
         (read-from-string "(abc")))

(test* "guard (nested)" 'exn
       (with-error-handler
           values
         (lambda ()
           (guard (ball
                   (#f (display "Caught exception.")))
             (guard (ball
                     (#f (raise ball)))
               (raise 'exn))))))

;;--------------------------------------------------------------------
(test-section "subtype")

(define-class <my-error> (<error>)
  ((info :init-keyword :info)))

(define-class <my-exc> (<exception>)
  ((type :init-keyword :type)))

(test* "<my-error>" '(#t "msg" "info")
       (let ((e (make <my-error> :message "msg" :info "info")))
         (list (is-a? e <error>)
               (ref e 'message)
               (ref e 'info))))

(test* "catching <my-error>" '(caught . "ok")
       (guard (x
               ((is-a? x <error>) (cons 'caught (ref x 'message))))
         (raise (make <my-error> :message "ok"))))

(test* "<my-exc>" '(#t #f type)
       (let ((e (make <my-exc> :type 'type)))
         (list (is-a? e <exception>)
               (is-a? e <error>)
               (ref e 'type))))

(test* "catching <my-exc>" 'exception
       (guard (x
               ((is-a? x <error>) 'error)
               ((is-a? x <exception>) 'exception))
         (raise (make <my-exc>))))

(test-end)





