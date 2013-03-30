;;
;; Test dynamic-wind, call/cc and related stuff
;;

(use gauche.test)

(test-start "dynamic-wind and call/cc")

(define c #f)

;;-----------------------------------------------------------------------
;; Test for continuation

(define (callcc-test1)
  (let ((r '()))
    (let ((w (let ((v 1))
               (set! v (+ (call-with-current-continuation
                           (lambda (c0) (set! c c0) v))
                          v))
               (set! r (cons v r))
               v)))
      (if (<= w 1024) (c w) r))))

(test "call/cc (env)" '(2048 1024 512 256 128 64 32 16 8 4 2)
      callcc-test1)

;; continuation with multiple values

(test* "call/cc (values)" '(1 2 3)
       (receive x (call-with-current-continuation
                   (lambda (c) (c 1 2 3)))
         x))

(test* "call/cc (values2)" '(1 2 3)
       (receive (x y z) (call-with-current-continuation
                         (lambda (c) (c 1 2 3)))
         (list x y z)))

(test* "call/cc (values3)" '(1 2 (3))
       (receive (x y . z)
           (call-with-current-continuation
            (lambda (c) (c 1 2 3)))
         (list x y z)))

(test* "call/cc (values4)" (test-error)
       (receive (x y)
           (call-with-current-continuation
            (lambda (c) (c 1 2 3)))
         (list x y)))

;; continuation with zero values
(test* "call/cc (zero value)" '()
       (call-with-values
           (lambda ()
             (call-with-current-continuation
              (lambda (c) (c))))
         (lambda x x)))

;; continuation invoked while inline procedure is prepared.
;; a test to see call/cc won't mess up the VM stack.

(define (callcc-test2)
  (let ((cc #f)
        (r '()))
    (let ((s (list 1 2 3 4 (call/cc (lambda (c) (set! cc c) 5)) 6 7 8)))
      (if (null? r)
          (begin (set! r s) (cc -1))
          (list r s)))))
    
(test "call/cc (inline)" '((1 2 3 4 5 6 7 8) (1 2 3 4 -1 6 7 8))
      callcc-test2)

;; continuation created during do frame preparation.
;; the TAILBIND instruction failed this test.

(test "call/cc (do)" 6
      (lambda ()
        (do ((x 0 (+ x 1))
             (y 0 (call/cc (lambda (c) c))))
            ((> x 5) x)
          #f)))

;;------------------------------------------------------------------------
;; Test for continuation thrown over C stack boundary
;;

;; We use internal proceduer %apply-rec to cross C stack boundary
;; intentionally.

(define %apply-rec (with-module gauche.internal %apply-rec))

(define (callcc-over-cstack)
  (call-with-current-continuation
   (lambda (c) (%apply-rec (lambda () (c 10))))))

(test "call/cc (cstack)" 10 callcc-over-cstack)

(test "call/cc (cstack2)" '(10 . 11)
      (lambda () (cons (callcc-over-cstack) 11)))

(test "call/cc (cstack, values)" '(10 11)
      (lambda ()
        (receive x
            (call-with-current-continuation
             (lambda (c)
               (%apply-rec (lambda () (c 10 11)))))
          x)))

(test "call/cc (cstack, two level)" '(10 . 11)
      (lambda ()
        (cons (call-with-current-continuation
               (lambda (c)
                 (%apply-rec
                  (lambda ()
                    (%apply-rec (lambda () (c 10)))))))
              11)))

(test "call/cc (cstack, two level, two hop)" '(11 . 11)
      (lambda ()
        (cons (call-with-current-continuation
               (lambda (c)
                 (%apply-rec 
                  (lambda ()
                    (c (+ (call-with-current-continuation
                           (lambda (d)
                             (%apply-rec (lambda () (d 10)))))
                          1))))))
              11)))

(test "call/cc (ghost continuation)" (test-error)
      (lambda ()
        (%apply-rec
         (lambda (k)
           (%apply-rec
            (lambda ()
              (cons 1 (call-with-current-continuation
                       (lambda (c) (set! k c) 1)))))
           (k 2))
         #f)))

(test "call/cc (ghost continuation, escaping out by call/cc)" '(1 . 2)
      (lambda ()
        (call-with-current-continuation
         (lambda (safe-return)
           (let ((s values))
             (%apply-rec
              (lambda (k)
                (%apply-rec
                 (lambda ()
                   (s (cons 1 (call-with-current-continuation
                               (lambda (c) (set! k c) 1))))))
                (set! s safe-return)
                (k 2))
              #f))))))

(test "call/cc (ghost continuation, escaping out by error)" '(1 . 2)
      (lambda ()
        (with-error-handler values
          (lambda ()
            (let ((s values))
              (%apply-rec
               (lambda (k)
                 (%apply-rec
                  (lambda ()
                    (s (cons 1 (call-with-current-continuation
                                (lambda (c) (set! k c) 1))))))
                 (set! s raise)
                 (k 2))
               #f))))))

(test "call/cc (ghost continuation, escaping out by error 2)" '(1 . 2)
      (lambda ()
        (guard (e [else e])
          (let ((s values))
            (%apply-rec
             (lambda (k)
               (%apply-rec
                (lambda ()
                  (s (cons 1 (call-with-current-continuation
                              (lambda (c) (set! k c) 1))))))
               (set! s raise)
               (k 2))
             #f)))))

;; Paranoia

(test "call/cc & dynwind (cstack)" '(a b c)
      (lambda ()
        (let ((x '()))
          (call-with-current-continuation
           (lambda (c)
             (dynamic-wind
              (lambda () (set! x (cons 'c x)))
              (lambda ()
                (%apply-rec (lambda () 
                              (set! x (cons 'b x))
                              (c 0)))
                (set! x (cons 'z x))
                )
              (lambda () (set! x (cons 'a x))))))
          x)))

;;------------------------------------------------------------------------
;; Test for dynamic-wind

;; An example in R5RS
(define (dynwind-test1)
  (let ((path '()))
    (let ((add (lambda (s) (set! path (cons s path)))))
      (dynamic-wind
       (lambda () (add 'connect))
       (lambda ()
         (add (call-with-current-continuation
               (lambda (c0) (set! c c0) 'talk1))))
       (lambda () (add 'disconnect)))
      (if (< (length path) 4)
          (c 'talk2)
          (reverse path)))))

(test "dynamic-wind"
      '(connect talk1 disconnect connect talk2 disconnect)
      dynwind-test1)

;; Test for handler stack.
(define (dynwind-test2)
  (let ((path '()))
    (dynamic-wind
     (lambda () (set! path (cons 1 path)))
     (lambda () (set! path (append (dynwind-test1) path)))
     (lambda () (set! path (cons 3 path))))
    path))

(test "dynamic-wind"
      '(3 connect talk1 disconnect connect talk2 disconnect 1)
      dynwind-test2)

(test "dynamic-wind" '(a b c d e f g b c d e f g h)
      (lambda ()
        (let ((x '())
              (c #f))
          (dynamic-wind
           (lambda () (push! x 'a))
           (lambda ()
             (dynamic-wind
              (lambda () (push! x 'b))
              (lambda ()
                (dynamic-wind
                 (lambda () (push! x 'c))
                 (lambda () (set! c (call/cc identity)))
                 (lambda () (push! x 'd))))
              (lambda () (push! x 'e)))
             (dynamic-wind
              (lambda () (push! x 'f))
              (lambda () (when c (c #f)))
              (lambda () (push! x 'g))))
           (lambda () (push! x 'h)))
          (reverse x))))

;; Test for multiple values
(test "dynamic-wind (multival)" '(a b c)
      (lambda ()
        (receive x
            (dynamic-wind (lambda () #f)
                          (lambda () (values 'a 'b 'c))
                          (lambda () #f))
          x)))

(test "dynamic-wind (multival)" '()
      (lambda ()
        (receive x
            (dynamic-wind (lambda () #f)
                          (lambda () (values))
                          (lambda () #f))
          x)))

;; Test for error handling with dynamic-wind
(test "dynamic-wind - error in before thunk"
      '(a b c d h)
      (lambda ()
        (let ((k '()))
          (with-error-handler (lambda (e) #f)
            (lambda ()
              (push! k 'a)
              (dynamic-wind
                  (lambda () (push! k 'b))
                  (lambda ()
                    (push! k 'c)
                    (dynamic-wind
                        (lambda () (push! k 'd) (error "ho"))
                        (lambda () (push! k 'e))
                        (lambda () (push! k 'f)))
                    (push! k 'g))
                  (lambda () (push! k 'h)))
              (push! k 'i)))
          (reverse k))))
            
(test "dynamic-wind - error in after thunk"
      '(a b c d e f h)
      (lambda ()
        (let ((k '()))
          (with-error-handler (lambda (e) #f)
            (lambda ()
              (push! k 'a)
              (dynamic-wind
                  (lambda () (push! k 'b))
                  (lambda ()
                    (push! k 'c)
                    (dynamic-wind
                        (lambda () (push! k 'd))
                        (lambda () (push! k 'e))
                        (lambda () (push! k 'f) (error "ho")))
                    (push! k 'g))
                  (lambda () (push! k 'h)))
              (push! k 'i)))
          (reverse k))))

;; test for error during dynamic handler reinstallation by call/cc.
;; (problem found and fixed by Kazuki Tsujimoto)

(define (test-thunk body)
  (let ((x '()))
    (with-error-handler
        (lambda (e) (push! x 'x))
      (lambda ()
        (call/cc
         (lambda (c)
           (dynamic-wind
               (lambda () (push! x 'a))
               (lambda ()
                 (dynamic-wind
                     (lambda () (push! x 'b))
                     (lambda () (body c))
                     (lambda () (push! x 'c) (car 3))))
               (lambda () (push! x 'd)))))))
    (reverse x)))

(test* "restart & dynamic-wind with error(1)" '(a b c x d)
       (test-thunk (lambda (cont) (cont #t))))

(test* "restart & dynamic-wind with error(2)" '(a b c x d)
       (test-thunk (lambda (cont)
                     (with-error-handler
                         (lambda (e) (cont #t))
                       (lambda () (car 3))))))

;; test for compiler optimizer propery tracks local procedure usage
;; (problem reported by SaitoAtsushi)

(test* "compiler optimizer for dynamic-wind" 2
       (let ((a 1))
         (let ((lv (lambda () (set! a 2))))
           (dynamic-wind lv (lambda () 'hoge) lv))
         a))

;; make sure the evaluation order isn't changed in presense of
;; dynamic-wind inlining; that is, the arguments should be fully
;; evaluated before before/after thunks are executed.
(test* "optimization and inlined dynamic-wind" 2
       (let ((a 1))
         (define (make-prepost x) (lambda () (set! a x)))
         (define (make-thunk) (lambda () a))
         (dynamic-wind
             (make-prepost 2)
             (make-thunk)
             (make-prepost 3))))

;;-----------------------------------------------------------------------
;; Test for stack overflow handling

;; Single call of fact-rec consumes
;;  5 (continuation) + 1 (n) + 4 (argframe) = 10
;; words.  With the default stack size 10000, n=1000 is enough to generate
;; the stack overflow.  There's no way to obtain compiled-in stack size
;; right now, so you need to adjust the parameters if you change the stack
;; size.

(define (sum-rec n)
  (if (> n 0)
      (+ n (sum-rec (- n 1)))
      0))

(test "stack overflow" (/ (* 1000 1001) 2)
      (lambda () (sum-rec 1000)))

(test "stack overflow" (/ (* 4000 4001) 2)
      (lambda () (sum-rec 4000)))

(define (sum-rec-apply n)
  (if (> n 0)
      (apply + n (apply sum-rec (- n 1) '()) '())
      0))

(test "stack overflow (apply)" (/ (* 2000 2001) 2)
      (lambda () (sum-rec-apply 2000)))
      
(test "stack overflow (apply)" (/ (* 3000 3001) 2)
      (lambda () (sum-rec-apply 3000)))

;;-----------------------------------------------------------------------
;; See if port stuff is cleaned up properly

(test "call-with-output-file -> port-closed?"
      #t
      (lambda ()
        (let ((p #f))
          (call-with-output-file
              "tmp1.o"
              (lambda (port)
                (write '(a b c d e) port)
                (set! p port)))
          (port-closed? p))))

(test "call-with-input-file -> port-closed?"
      '(#t a b c d e)
      (lambda ()
        (let* ((p #f)
               (r (call-with-input-file "tmp1.o"
                    (lambda (port)
                      (set! p port)
                      (read port)))))
          (cons (port-closed? p) r))))

(test "with-output-to-file -> port-closed?"
      '(#t #f)
      (lambda ()
        (let ((p #f))
          (with-output-to-file "tmp1.o"
            (lambda ()
              (set! p (current-output-port))
              (write '(a b c d e))))
          (list (port-closed? p)
                (eq? p (current-output-port))))))

(test "with-input-from-file -> port-closed?"
      '(#t #f)
      (lambda ()
        (let* ((p #f)
               (r (with-input-from-file "tmp1.o"
                    (lambda ()
                      (set! p (current-input-port))
                      (read)))))
          (list (port-closed? p)
                (eq? p (current-input-port))))))

;;-----------------------------------------------------------------------
;; Al Petrofsky's finding
;; http://groups.google.com/groups?dq=&hl=ja&selm=87g00y4b6l.fsf%40radish.petrofsky.org

(test "Al's call/cc test" 1
      (lambda () (call/cc (lambda (c) (0 (c 1))))))

;;-----------------------------------------------------------------------
;; Partial continuations

(test-section "partial continuations")
(use gauche.partcont)
(test-module 'gauche.partcont)

(test "reset" 6
      (lambda () (+ 1 (reset (+ 2 3)))))
(test "reset" '(1 2)
      (lambda () (cons 1 (reset (cons 2 '())))))

(test "shift, ignoring k" 4
      (lambda () (+ 1 (reset (+ 2 (shift k 3))))))
(test "shift, ignoring k" '(1 3)
      (lambda () (cons 1 (reset (cons 2 (shift k (list 3)))))))

(test "calling pc" 10
      (lambda () (+ 1 (reset (+ 2 (shift k (+ 3 (k 4))))))))
(test "calling pc" '(1 3 2 4)
      (lambda ()
        (cons 1 (reset (cons 2 (shift k (cons 3 (k (cons 4 '())))))))))

(test "calling pc multi" 14
      (lambda ()
        (+ 1 (reset (+ 2 (shift k (+ 3 (k 5) (k 1))))))))
(test "calling pc multi" '(1 3 2 2 4)
      (lambda ()
        (cons 1 (reset (cons 2 (shift k (cons 3 (k (k (cons 4 '()))))))))))

;; 'amb' example in Gasbichler&Sperber ICFP2002 paper
(let ()
  (define (eta x) (list (x)))                    ; unit
  (define (extend f l) (apply append (map f l))) ; bind
  (define (reflect meaning) (shift k (extend k meaning)))
  (define (reify thunk) (reset (eta thunk)))
  (define (amb* . t)
    (reflect (apply append (map reify t))))
  (let-syntax ([amb
                (syntax-rules () [(amb x ...) (amb* (lambda () x) ...)])])
    (define (www)
      (let ((f (lambda (x) (+ x (amb 6 4 2 8) (amb 2 4 5 4 1)))))
        (reify (lambda () (f (f (amb 0 2 3 4 5 32)))))))

    (define (wwww)
      (let ((f (lambda (x) (+ x (amb 6 4 2 8) (amb 2 4 5 4 1)))))
        (reify (lambda () (f (f (f (amb 0 2 3 4 5 32))))))))

    (test "www" 2400 (lambda () (length (www))))
    (test "wwww" 48000 (lambda () (length (wwww))))
    ))

;; inversion of iterator
(let ()
  (define (inv lis)
    (define (kk)
      (reset (for-each (lambda (e) (shift k (set! kk k) e)) lis)
             (set! kk (lambda () (eof-object)))
             (eof-object)))
    (lambda () (kk)))
  (define iter (inv '(1 2 3 4 5)))
  (test "inversion" 1 iter)
  (test "inversion" 2 iter)
  (test "inversion" 3 iter)
  (test "inversion" 4 iter)
  (test "inversion" 5 iter)
  (test "inversion" (eof-object) iter)
  (test "inversion" (eof-object) iter))

;; A-normal conversion, from
;; http://pllab.is.ocha.ac.jp/(J~asai/cw2011tutorial/main-j.pdf pp. 11--12(B
(let ()
  (define (gensym) 't) ;; to have reproducible results
  (define (a-normal term)
    (cond [(and (pair? term)
                (eq? (car term) 'lambda))
           (let ((var (car (cadr term)))
                 (body (caddr term)))
             `(lambda (,var) ,(reset (a-normal body))))]
          [(pair? term)
           (let ((abs (car term))
                 (arg (cadr term)))
             (shift k
                    (let* ((v (gensym))
                           (body (k v)))
                      `(let ((,v (,(a-normal abs) ,(a-normal arg))))
                         ,body))))]
          [else term]))

  (test "a-normal"
        '(lambda (x) (lambda (y) (let ((t (x y))) t)))
        (lambda () (a-normal '(lambda (x) (lambda (y) (x y))))))
  )

;; Typed Printf via Delimited Continuations 
;; http://www.cs.ox.ac.uk/ralf.hinze/WG2.8/27/slides/kenichi2.pdf
(let ()
  (define (sprintf format)
    (reset (format)))
  (define (fmt dir)
    (shift k (lambda (v) (k (dir v)))))
  (define s values)
  (test "sprintf" "hello, world"
        (lambda ()
          ((sprintf (lambda () (string-append "hello, " (fmt s)))) "world")))
  (test "sprintf" "world"
        (lambda () ((sprintf (lambda () (fmt s))) "world")))
  )

;; To be written:
;;  - tests for interactions of dynamic handlers and partial continuaions.
;;  - tests for interactions of partial and full continuations.

(test-end)
