;;> The environment monad with some pre-defined fields for combinator
;;> formatting.

(define-record-type Show-Env
    (make-state port row col width radix precision
                pad-char decimal-sep decimal-align
                string-width ellipsis writer output
                %props)
    state?
  (port env-port env-port-set!)
  (row env-row env-row-set!)
  (col env-col env-col-set!)
  (width env-width env-width-set!)
  (radix env-radix env-radix-set!)
  (precision env-precision env-precision-set!)
  (pad-char env-pad-char env-pad-char-set!)
  (decimal-sep env-decimal-sep env-decimal-sep-set!)
  (decimal-align env-decimal-align env-decimal-align-set!)
  (string-width env-string-width env-string-width-set!)
  (ellipsis env-ellipsis env-ellipsis-set!)
  (writer env-writer env-writer-set!)
  (output env-output env-output-set!)
  (%props get-props set-props!))

(define (ask st x)
  (case x
    ((port) (env-port st))
    ((row) (env-row st))
    ((col) (env-col st))
    ((width) (env-width st))
    ((radix) (env-radix st))
    ((precision) (env-precision st))
    ((pad-char) (env-pad-char st))
    ((decimal-sep) (env-decimal-sep st))
    ((decimal-align) (env-decimal-align st))
    ((string-width) (env-string-width st))
    ((ellipsis) (env-ellipsis st))
    ((writer) (env-writer st))
    ((output) (env-output st))
    (else (cond ((assq x (get-props st)) => cdr) (else #f)))))

(define (tell st x val)
  (case x
    ((port) (env-port-set! st val))
    ((row) (env-row-set! st val))
    ((col) (env-col-set! st val))
    ((width) (env-width-set! st val))
    ((radix) (env-radix-set! st val))
    ((precision) (env-precision-set! st val))
    ((pad-char) (env-pad-char-set! st val))
    ((decimal-sep) (env-decimal-sep-set! st val))
    ((decimal-align) (env-decimal-align-set! st val))
    ((string-width) (env-string-width-set! st val))
    ((ellipsis) (env-ellipsis-set! st val))
    ((writer) (env-writer-set! st val))
    ((output) (env-output-set! st val))
    (else
     (cond
      ((assq x (get-props st))
       => (lambda (cell) (set-cdr! cell val)))
      (else
       (set-props! st (cons (cons x val) (get-props st))))))))

;; External API
;;
;; copy
(define (c st)
  (make-state
   (env-port st)
   (env-row st)
   (env-col st)
   (env-width st)
   (env-radix st)
   (env-precision st)
   (env-pad-char st)
   (env-decimal-sep st)
   (env-decimal-align st)
   (env-string-width st)
   (env-ellipsis st)
   (env-writer st)
   (env-output st)
   (map (lambda (x)
          (cons (car x) (cdr x)))
        (get-props st))))

;; bind - a function
(define-syntax %fn
  (syntax-rules ()
    ((%fn ("step") (params ...) ((p param) . rest) . body)
     (%fn ("step") (params ... (p param)) rest . body))
    ((%fn ("step") (params ...) ((param) . rest) . body)
     (%fn ("step") (params ... (param param)) rest . body))
    ((%fn ("step") (params ...) (param . rest) . body)
     (%fn ("step") (params ... (param param)) rest . body))
    ((%fn ("step") ((p param) ...) () . body)
     (lambda (st)
       (let ((p (ask st 'param)) ...)
         ((let () . body) st))))
    ((%fn params . body)
     (%fn ("step") () params . body))))

(define-syntax fn
  (syntax-rules ()
    ((fn vars expr ... fmt)
     (%fn vars expr ... (displayed fmt)))))

;; fork - run on a copy of the state
(define-syntax forked
  (syntax-rules ()
    ((forked a) a)
    ((forked a b) (lambda (st) (a (c st)) (b st)))
    ((forked a b . c) (forked a (forked b . c)))))

;; sequence
(define-syntax sequence
  (syntax-rules ()
    ((sequence f) f)
    ((sequence f . g) (lambda (st) ((sequence . g) (f st))))))

;; update in place
(define-syntax with!
  (syntax-rules ()
    ((with! (prop value) ...)
     (lambda (st)
       (tell st 'prop value) ...
       st))))

;; local binding - update temporarily
(define-syntax %with
  (syntax-rules ()
    ((%with ("step") ((p tmp v) ...) () . b)
     (lambda (st)
       (let ((tmp (ask st 'p)) ...)
         (dynamic-wind
           (lambda () (tell st 'p v) ...)
           (lambda () ((begin . b) st))
           (lambda () (tell st 'p tmp) ...)))))
    ((%with ("step") (props ...) ((p v) . rest) . b)
     (%with ("step") (props ... (p tmp v)) rest . b))
    ((%with ((prop value) ...) . body)
     (%with ("step") () ((prop value) ...) . body))))

;;> Temporarily bind the parameters in the body \var{x}.
(define-syntax with
  (syntax-rules ()
    ((with params x ... y)
     (%with params (each x ... y)))))

;; run
(define (run proc)
  (proc (make-state #f #f #f #f #f #f #f #f #f #f #f #f #f '())))

;; return
(define (return x)
  (lambda (st) x))
