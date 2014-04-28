;; test exception handling system 
;; this must come after primsyn, error, macro and object tests.

(use gauche.test)
(test-start "exceptions")

;;--------------------------------------------------------------------
(test-section "bare constructors")

(test* "make <error>" '(#t #t #t #f)
       (let ((e (make <error>)))
         (list (is-a? e <condition>)
               (is-a? e <serious-condition>)
               (is-a? e <error>)
               (ref e 'message))))

(test* "make <message-condition>" "huge"
       (ref (make <message-condition> :message "huge") 'message))

(test* "make <error>" "hoge"
       (ref (make <error> :message "hoge") 'message))

(test* "make <system-error>" '("oops" 12)
       (let ((e (make <system-error> :message "oops" :errno 12)))
         (map (cut ref e <>) '(message errno))))

;;--------------------------------------------------------------------
(test-section "srfi-35 constructors, predicates and accessors")

(test* "make-condition <error>" '(#t #t #f "moo")
       (let ((e (make-condition <error> 'message "moo")))
         (list
          (condition-has-type? e <error>)
          (condition-has-type? e <serious-condition>)
          (condition-has-type? e <read-error>)
          (condition-ref e 'message))))

(test* "make-condition <port-error>" `(#t #t #t #f "moo" ,(current-input-port))
       (let ((e (make-condition <port-error>
                                'port (current-input-port)
                                'message "moo")))
         (list
          (condition-has-type? e <error>)
          (condition-has-type? e <serious-condition>)
          (condition-has-type? e <io-error>)
          (condition-has-type? e <read-error>)
          (condition-ref e 'message)
          (condition-ref e 'port))))

(test* "make-compound-condition"
       `(#t #t #t "sys" 12 ,(current-input-port))
       (let ((e (make-compound-condition
                 (make-condition <system-error>
                                 'message "sys" 'errno 12)
                 (make-condition <io-read-error>
                                 'message "io" 'port (current-input-port)))))
         (list
          (condition-has-type? e <error>)
          (condition-has-type? e <system-error>)
          (condition-has-type? e <io-read-error>)
          (condition-ref e 'message)
          (condition-ref e 'errno)
          (condition-ref e 'port))))

(test* "make-compound-condition"
       `(#t #t #t "io" 12 ,(current-input-port))
       (let ((e (make-compound-condition
                 (make-condition <io-read-error>
                                 'message "io" 'port (current-input-port))
                 (make-condition <system-error>
                                 'message "sys" 'errno 12))))
         (list
          (condition-has-type? e <error>)
          (condition-has-type? e <system-error>)
          (condition-has-type? e <io-read-error>)
          (condition-ref e 'message)
          (condition-ref e 'errno)
          (condition-ref e 'port))))
          
(test* "make-compound-condition"
       `(#t #t #t "message" 12 ,(current-input-port))
       (let ((e (make-compound-condition
                 (make-compound-condition
                  (make-condition <message-condition> 'message "message")
                  (make-condition <io-read-error>
                                  'message "io" 'port (current-input-port))
                 (make-condition <system-error>
                                 'message "sys" 'errno 12)))))
         (list
          (condition-has-type? e <error>)
          (condition-has-type? e <system-error>)
          (condition-has-type? e <io-read-error>)
          (condition-ref e 'message)
          (condition-ref e 'errno)
          (condition-ref e 'port))))

(test* "extract-condition"
       `(("message")
         ("message" ,(current-input-port))
         ("message" 12))
       (let* ((e (make-compound-condition
                  (make-compound-condition
                   (make-condition <message-condition> 'message "message")
                   (make-condition <io-read-error>
                                   'message "io" 'port (current-input-port))
                   (make-condition <system-error>
                                   'message "sys" 'errno 12))))
              (m (extract-condition e <message-condition>))
              (i (extract-condition e <io-read-error>))
              (s (extract-condition e <system-error>)))
         (list
          (list (condition-ref m 'message))
          (list (condition-ref i 'message) (condition-ref i 'port))
          (list (condition-ref s 'message) (condition-ref s 'errno)))
         ))
       
;;--------------------------------------------------------------------
(test-section "srfi-35 style condition definitions")

(define-condition-type &c &condition
  c?
  (x c-x))

(define-condition-type &c1 &c
  c1?
  (a c1-a))

(define-condition-type &c2 &c
  c2?
  (b c2-b))

(let ((v1 #f) (v2 #f) (v3 #f) (v4 #f) (v5 #f))
  (set! v1 (make-condition &c1 'x "V1" 'a "a1"))

  (test* "v1" '(#t #t #f "V1" "a1")
         (list (c? v1) (c1? v1) (c2? v1) (c-x v1) (c1-a v1)))

  (set! v2 (condition (&c2
                       (x "V2")
                       (b "b2"))))

  (test* "v2" '(#t #f #t "V2" "b2")
         (list (c? v2) (c1? v2) (c2? v2) (c-x v2) (c2-b v2)))

  (set! v3 (condition (&c1
                       (x "V3/1")
                       (a "a3"))
                      (&c2
                       (b "b3"))))
  (test* "v3" '(#t #t #t "V3/1" "a3" "b3")
         (list (c? v3) (c1? v3) (c2? v3) (c-x v3) (c1-a v3) (c2-b v3)))

  (set! v4 (make-compound-condition v1 v2))
  (test* "v4" '(#t #t #t "V1" "a1" "b2")
         (list (c? v4) (c1? v4) (c2? v4) (c-x v4) (c1-a v4) (c2-b v4)))

  (set! v5 (make-compound-condition v2 v3))
  (test* "v5" '(#t #t #t "V2" "a3" "b2")
         (list (c? v5) (c1? v5) (c2? v5) (c-x v5) (c1-a v5) (c2-b v5)))

  (test* "error (&c1)" "ok"
         (with-error-handler
             (lambda (e)
               (and (is-a? e &c1)
                    (slot-ref e 'a)))
           (lambda () (error &c1 :a "ok"))))
  )

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

(test* "guard" 'caught-error
       (guard (x
               ((symbol? x) (cons 'symbol x))
               ((<error> x) 'caught-error))
         (car 'a)))

(test* "guard (uncaught error)" (test-error)
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
               ((<read-error> x) 'read-error)
               ((<system-error> x) 'system-error)
               ((<error> x) 'error)
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

(test* "guard (compound)" 'read-error
       (guard (x
               ((<read-error> x) 'read-error)
               ((<system-error> x) 'system-error)
               ((<error> x) 'error)
               (else (cons 'else x)))
         (raise (condition
                 (<io-error> (message "z"))
                 (<read-error> (message "foo"))
                 (<system-error> (message "bar"))))))

(let ()
  (define aaa '())
  (define (foo)
    (dynamic-wind
        (lambda () (set! aaa '()))
        (lambda ()
          (set! aaa (cons 'a aaa))
          (error "foo"))
        (lambda ()
          (set! aaa (cons 'b aaa)))))
  (test* "guard w/dynamic-wind" '((b a) (b a))
         (let1 x (guard (e (else aaa)) (foo))
           (list x aaa))))


;;--------------------------------------------------------------------
(test-section "unwind-protect")

;; NB: unwind-protect behavior on exit can't be tested here, for we
;; need to invoke a subprocess.  The tests are in process.scm instead.

(let ()
  (define aaa '())
  (define (foo thunk)
    (dynamic-wind
        (lambda () (push! aaa 'a))
        (lambda () (push! aaa 'b) (thunk) (push! aaa 'c))
        (lambda () (push! aaa 'd))))
  (test* "unwind-protect (success)" '(e d c z b a)
         (guard (e (else (push! aaa e) e))
           (set! aaa '())
           (unwind-protect
            (foo (lambda () (push! aaa 'z)))
            (push! aaa 'e))
           aaa))
  (test* "unwind-protect (raise)" '(boo e d b a)
         (guard (e (else (push! aaa e) aaa))
           (set! aaa '())
           (unwind-protect
            (foo (lambda () (raise 'boo)))
            (push! aaa 'e))
           aaa))
  (test* "unwind-protect (error)" '(boo e d b a)
         (guard (e (else (push! aaa 'boo) aaa))
           (set! aaa '())
           (unwind-protect
            (foo (lambda () (error "boo")))
            (push! aaa 'e))
           aaa))

  (test* "unwind-protect (restart)" '(e d c a d z b a)
         (begin
           (set! aaa '())
           (let ((k #f))
             (let/cc k1
               (unwind-protect
                (foo (lambda ()
                       (let/cc k2
                         (set! k k2) (push! aaa 'z) (k1 0))))
                (push! aaa 'e)))
             (when k (let ((k0 k)) (set! k #f) (k0 0))))
           aaa))
  )

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

;;--------------------------------------------------------------------
(test-section "combinations")

(test* "guarding read-error" "Warning: read error:(input string port):1"
       (guard (exc
               ((condition-has-type? exc <read-error>)
                (format "Warning: read error:~a:~a"
                        (port-name (ref exc 'port))
                        (ref exc 'line)))
               ((condition-has-type? exc <io-port-error>)
                (format "Warning: I/O error occurred on port ~a"
                        (port-name (ref exc 'port))))
               (else
                (format "Other error")))
         (read-from-string "(abc")))

(test* "guarding read-error (2)"
       "Warning: read error:(input string port):1"
       (guard (exc
               ((<read-error> exc)
                (format "Warning: read error:~a:~a"
                        (port-name (ref exc 'port))
                        (ref exc 'line)))
               ((<io-port-error> exc)
                (format "Warning: I/O error occurred on port ~a"
                        (port-name (ref exc 'port))))
               (else
                (format "Other error")))
         (read-from-string "(abc")))

(test-end)
