;;
;; Test dynamic-wind, call/cc and related stuff
;;

(use gauche.test)

(test-start "dynamic-wind and call/cc")

(define c #f)

;;-----------------------------------------------------------------------
;; Test for continuation

(define (callcc-test1)
  (let1 r '()
    (let1 w (rlet1 v 1
              (set! v (+ (call-with-current-continuation (^[c0] (set! c c0) v))
                         v))
              (set! r (cons v r)))
      (if (<= w 1024) (c w) r))))

(test "call/cc (env)" '(2048 1024 512 256 128 64 32 16 8 4 2)
      callcc-test1)

;; continuation with multiple values

(test* "call/cc (values)" '(1 2 3)
       (receive x (call-with-current-continuation (^c (c 1 2 3)))
         x))

(test* "call/cc (values2)" '(1 2 3)
       (receive (x y z) (call-with-current-continuation (^c (c 1 2 3)))
         (list x y z)))

(test* "call/cc (values3)" '(1 2 (3))
       (receive (x y . z)
           (call-with-current-continuation (^c (c 1 2 3)))
         (list x y z)))

(test* "call/cc (values4)" (test-error)
       (receive (x y)
           (call-with-current-continuation (^c (c 1 2 3)))
         (list x y)))

;; continuation with zero values
(test* "call/cc (zero value)" '()
       (call-with-values (^[] (call-with-current-continuation (^c (c))))
         (^ x x)))

;; continuation invoked while inline procedure is prepared.
;; a test to see call/cc won't mess up the VM stack.

(define (callcc-test2)
  (let ([cc #f]
        [r '()])
    (let1 s (list 1 2 3 4 (call/cc (^c (set! cc c) 5)) 6 7 8)
      (if (null? r)
        (begin (set! r s) (cc -1))
        (list r s)))))
    
(test "call/cc (inline)" '((1 2 3 4 5 6 7 8) (1 2 3 4 -1 6 7 8))
      callcc-test2)

;; continuation created during do frame preparation.
;; the TAILBIND instruction failed this test.

(test "call/cc (do)" 6
      (^[] (do ([x 0 (+ x 1)]
                [y 0 (call/cc (^c c))])
               [(> x 5) x]
             #f)))

;; When the continuation is restarted, the incomplete frame at the
;; time of capture must be restored.  After introducing BOXing of mutable
;; vars and before d84eb08, this was broken since we BOX variables before
;; the frame is completed, and continuation captures the reference to the
;; box instead of the original value---and the content of box wasn't
;; restored by restarting the continuation!
;; The following tests check the regression.
(test "call/cc and set!" '((#t . 2) (#t . 1))
      (lambda ()
        (let ([cont #f]
              [r '()])
          (let ([f (lambda (x y) (set! r (acons x y r)) (set! x #f))])
            (f #t (call/cc (lambda (c) (set! cont c) 1))))
          (if cont
            (let ([k cont])
              (set! cont #f)
              (k 2))
            r))))

(test "call/cc and set! w/LOCAL-ENV-SHIFT"
      '(((0) ()))
      (lambda ()
        (let loop ([j 0]
                   [t '()]
                   [r '()]
                   [k 0])
          (let ([x (list j)]
                [y (list t)])
            (push! t (cons x y))
            (if (= k 1)
              r
              (loop (+ j 1) '() t (+ k 1)))))))

;;------------------------------------------------------------------------
;; Test for continuation thrown over C stack boundary
;;

;; We use internal proceduer %apply-rec to cross C stack boundary
;; intentionally.

(define %apply-rec (with-module gauche.internal %apply-rec))

(define (callcc-over-cstack)
  (call-with-current-continuation (^c (%apply-rec (^[] (c 10))))))

(test "call/cc (cstack)" 10 callcc-over-cstack)

(test "call/cc (cstack2)" '(10 . 11) (^[] (cons (callcc-over-cstack) 11)))

(test "call/cc (cstack, values)" '(10 11)
      (^[] (receive x
               (call-with-current-continuation (^c (%apply-rec (^[] (c 10 11)))))
             x)))

(test "call/cc (cstack, two level)" '(10 . 11)
      (^[] (cons (call-with-current-continuation
                  (^c (%apply-rec (^[] (%apply-rec (^[] (c 10)))))))
                 11)))

(test "call/cc (cstack, two level, two hop)" '(11 . 11)
      (^[] (cons (call-with-current-continuation
                  (^c (%apply-rec
                       (^[] (c (+ (call-with-current-continuation
                                   (^d (%apply-rec (^[] (d 10)))))
                                  1))))))
                 11)))

(test "call/cc (ghost continuation)" (test-error)
      (^[] (%apply-rec
            (^k (%apply-rec
                 (^[] (cons 1 (call-with-current-continuation
                               (^c (set! k c) 1)))))
                (k 2))
            #f)))

(test "call/cc (ghost continuation, escaping out by call/cc)" '(1 . 2)
      (^[] (call-with-current-continuation
            (^[safe-return]
              (let1 s values
                (%apply-rec
                 (^k (%apply-rec
                      (^[] (s (cons 1 (call-with-current-continuation
                                       (^c (set! k c) 1))))))
                     (set! s safe-return)
                     (k 2))
                 #f))))))

(test "call/cc (ghost continuation, escaping out by error)" '(1 . 2)
      (^[] (with-error-handler values
             (^[] (let1 s values
                    (%apply-rec
                     (^k (%apply-rec
                          (^[] (s (cons 1 (call-with-current-continuation
                                           (^c (set! k c) 1))))))
                         (set! s raise)
                         (k 2))
                     #f))))))

(test "call/cc (ghost continuation, escaping out by error 2)" '(1 . 2)
      (^[] (guard (e [else e])
             (let1 s values
               (%apply-rec
                (^k (%apply-rec
                     (^[] (s (cons 1 (call-with-current-continuation
                                      (^c (set! k c) 1))))))
                    (set! s raise)
                    (k 2))
                #f)))))

;; Paranoia

(test "call/cc & dynwind (cstack)" '(a b c)
      (^[] (let1 x '()
             (call-with-current-continuation
              (^c (dynamic-wind
                      (^[] (set! x (cons 'c x)))
                      (^[]
                        (%apply-rec (^[] (set! x (cons 'b x)) (c 0)))
                        (set! x (cons 'z x)))
                      (^[] (set! x (cons 'a x))))))
             x)))

;;------------------------------------------------------------------------
;; Test for dynamic-wind

;; An example in R5RS
(define (dynwind-test1)
  (let1 path '()
    (let1 add (^s (set! path (cons s path)))
      (dynamic-wind
          (^[] (add 'connect))
          (^[] (add (call-with-current-continuation (^[c0] (set! c c0) 'talk1))))
          (^[] (add 'disconnect)))
      (if (< (length path) 4)
        (c 'talk2)
        (reverse path)))))

(test "dynamic-wind"
      '(connect talk1 disconnect connect talk2 disconnect)
      dynwind-test1)

;; Test for handler stack.
(define (dynwind-test2)
  (rlet1 path '()
    (dynamic-wind
        (^[] (set! path (cons 1 path)))
        (^[] (set! path (append (dynwind-test1) path)))
        (^[] (set! path (cons 3 path))))))

(test "dynamic-wind"
      '(3 connect talk1 disconnect connect talk2 disconnect 1)
      dynwind-test2)

(test "dynamic-wind" '(a b c d e f g b c d e f g h)
      (^[] (let ([x '()]
                 [c #f])
             (dynamic-wind
                 (^[] (push! x 'a))
                 (^[]
                   (dynamic-wind
                       (^[] (push! x 'b))
                       (^[] (dynamic-wind
                                (^[] (push! x 'c))
                                (^[] (set! c (call/cc identity)))
                                (^[] (push! x 'd))))
                       (^[] (push! x 'e)))
                   (dynamic-wind
                       (^[] (push! x 'f))
                       (^[] (when c (c #f)))
                       (^[] (push! x 'g))))
                 (^[] (push! x 'h)))
             (reverse x))))

;; Test for multiple values
(test "dynamic-wind (multival)" '(a b c)
      (^[] (receive x
               (dynamic-wind (^[] #f)
                   (^[] (values 'a 'b 'c))
                   (^[] #f))
             x)))

(test "dynamic-wind (multival)" '()
      (^[] (receive x
               (dynamic-wind (^[] #f)
                   (^[] (values))
                   (^[] #f))
             x)))

;; Test for error handling with dynamic-wind
(test "dynamic-wind - error in before thunk"
      '(a b c d h)
      (^[] (let1 k '()
             (with-error-handler (^_ #f)
               (^[]
                 (push! k 'a)
                 (dynamic-wind
                     (^[] (push! k 'b))
                     (^[]
                       (push! k 'c)
                       (dynamic-wind
                           (^[] (push! k 'd) (error "ho"))
                           (^[] (push! k 'e))
                           (^[] (push! k 'f)))
                       (push! k 'g))
                     (^[] (push! k 'h)))
                 (push! k 'i)))
          (reverse k))))
            
(test "dynamic-wind - error in after thunk"
      '(a b c d e f h)
      (^[] (let1 k '()
             (with-error-handler (^_ #f)
               (^[]
                 (push! k 'a)
                 (dynamic-wind
                     (^[] (push! k 'b))
                     (^[]
                       (push! k 'c)
                       (dynamic-wind
                           (^[] (push! k 'd))
                           (^[] (push! k 'e))
                           (^[] (push! k 'f) (error "ho")))
                       (push! k 'g))
                     (^[] (push! k 'h)))
                 (push! k 'i)))
             (reverse k))))

;; test for error during dynamic handler reinstallation by call/cc.
;; (problem found and fixed by Kazuki Tsujimoto)

(define (test-thunk body)
  (let1 x '()
    (with-error-handler
        (^_ (push! x 'x))
      (^[] (call/cc
            (^c (dynamic-wind
                    (^[] (push! x 'a))
                    (^[] (dynamic-wind
                             (^[] (push! x 'b))
                             (^[] (body c))
                             (^[] (push! x 'c) (car 3))))
                    (^[] (push! x 'd)))))))
    (reverse x)))

(test* "restart & dynamic-wind with error(1)" '(a b c x d)
       (test-thunk (^[cont] (cont #t))))

(test* "restart & dynamic-wind with error(2)" '(a b c x d)
       (test-thunk (^[cont] (with-error-handler
                                (^_ (cont #t))
                              (^[] (car 3))))))

;; test for compiler optimizer propery tracks local procedure usage
;; (problem reported by SaitoAtsushi)

(test* "compiler optimizer for dynamic-wind" 2
       (rlet1 a 1
         (let1 lv (^[] (set! a 2))
           (dynamic-wind lv (^[] 'hoge) lv))))

;; make sure the evaluation order isn't changed in presense of
;; dynamic-wind inlining; that is, the arguments should be fully
;; evaluated before before/after thunks are executed.
(test* "optimization and inlined dynamic-wind" 2
       (let1 a 1
         (define (make-prepost x) (^[] (set! a x)))
         (define (make-thunk) (^[] a))
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
      (^[] (sum-rec 1000)))

(test "stack overflow" (/ (* 4000 4001) 2)
      (^[] (sum-rec 4000)))

(define (sum-rec-apply n)
  (if (> n 0)
    (apply + n (apply sum-rec (- n 1) '()) '())
    0))

(test "stack overflow (apply)" (/ (* 2000 2001) 2)
      (^[] (sum-rec-apply 2000)))
      
(test "stack overflow (apply)" (/ (* 3000 3001) 2)
      (^[] (sum-rec-apply 3000)))

;;-----------------------------------------------------------------------
;; See if port stuff is cleaned up properly

(test "call-with-output-file -> port-closed?"
      #t
      (^[] (let ([p #f])
             (call-with-output-file "tmp1.o"
               (^[port]
                 (write '(a b c d e) port)
                 (set! p port)))
             (port-closed? p))))

(test "call-with-input-file -> port-closed?"
      '(#t a b c d e)
      (^[] (let* ([p #f]
                  [r (call-with-input-file "tmp1.o"
                       (^[port]
                         (set! p port)
                         (read port)))])
             (cons (port-closed? p) r))))

(test "with-output-to-file -> port-closed?"
      '(#t #f)
      (^[] (let ([p #f])
             (with-output-to-file "tmp1.o"
               (^[]
                 (set! p (current-output-port))
                 (write '(a b c d e))))
             (list (port-closed? p)
                   (eq? p (current-output-port))))))

(test "with-input-from-file -> port-closed?"
      '(#t #f)
      (^[] (let* ([p #f]
                  [r (with-input-from-file "tmp1.o"
                       (^[]
                         (set! p (current-input-port))
                         (read)))])
             (list (port-closed? p)
                   (eq? p (current-input-port))))))

;;-----------------------------------------------------------------------
;; Al Petrofsky's finding
;; http://groups.google.com/groups?dq=&hl=ja&selm=87g00y4b6l.fsf%40radish.petrofsky.org

(test "Al's call/cc test" 1
      (^[] (call/cc (^c (0 (c 1))))))

;;-----------------------------------------------------------------------
;; Partial continuations

(test-section "partial continuations")
(use gauche.partcont)
(test-module 'gauche.partcont)

(test "reset" 6
      (^[] (+ 1 (reset (+ 2 3)))))
(test "reset" '(1 2)
      (^[] (cons 1 (reset (cons 2 '())))))

(test "shift, ignoring k" 4
      (^[] (+ 1 (reset (+ 2 (shift k 3))))))
(test "shift, ignoring k" '(1 3)
      (^[] (cons 1 (reset (cons 2 (shift k (list 3)))))))

(test "calling pc" 10
      (^[] (+ 1 (reset (+ 2 (shift k (+ 3 (k 4))))))))
(test "calling pc" '(1 3 2 4)
      (^[] (cons 1 (reset (cons 2 (shift k (cons 3 (k (cons 4 '())))))))))

(test "calling pc multi" 14
      (^[] (+ 1 (reset (+ 2 (shift k (+ 3 (k 5) (k 1))))))))
(test "calling pc multi" '(1 3 2 2 4)
      (^[] (cons 1 (reset (cons 2 (shift k (cons 3 (k (k (cons 4 '()))))))))))

;; 'amb' example in Gasbichler&Sperber ICFP2002 paper
(let ()
  (define (eta x) (list (x)))                    ; unit
  (define (extend f l) (apply append (map f l))) ; bind
  (define (reflect meaning) (shift k (extend k meaning)))
  (define (reify thunk) (reset (eta thunk)))
  (define (amb* . t)
    (reflect (apply append (map reify t))))
  (let-syntax ([amb (syntax-rules () [(amb x ...) (amb* (^[] x) ...)])])
    (define (www)
      (let1 f (^x (+ x (amb 6 4 2 8) (amb 2 4 5 4 1)))
        (reify (^[] (f (f (amb 0 2 3 4 5 32)))))))

    (define (wwww)
      (let1 f (^x (+ x (amb 6 4 2 8) (amb 2 4 5 4 1)))
        (reify (^[] (f (f (f (amb 0 2 3 4 5 32))))))))

    (test "www" 2400 (^[] (length (www))))
    (test "wwww" 48000 (^[] (length (wwww))))
    ))

;; inversion of iterator
(let ()
  (define (inv lis)
    (define (kk)
      (reset (for-each (^e (shift k (set! kk k) e)) lis)
             (set! kk (^[] (eof-object)))
             (eof-object)))
    (^[] (kk)))
  (define iter (inv '(1 2 3 4 5)))
  (test "inversion" 1 iter)
  (test "inversion" 2 iter)
  (test "inversion" 3 iter)
  (test "inversion" 4 iter)
  (test "inversion" 5 iter)
  (test "inversion" (eof-object) iter)
  (test "inversion" (eof-object) iter))

;; A-normal conversion, from
;; http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/main-j.pdf pp. 11--12
(let ()
  (define (gensym) 't) ;; to have reproducible results
  (define (a-normal term)
    (cond [(and (pair? term)
                (eq? (car term) 'lambda))
           (let ([var (car (cadr term))]
                 [body (caddr term)])
             `(lambda (,var) ,(reset (a-normal body))))]
          [(pair? term)
           (let ([abs (car term)]
                 [arg (cadr term)])
             (shift k
                    (let* ([v (gensym)]
                           [body (k v)])
                      `(let ((,v (,(a-normal abs) ,(a-normal arg))))
                         ,body))))]
          [else term]))

  (test "a-normal"
        '(lambda (x) (lambda (y) (let ((t (x y))) t)))
        (^[] (a-normal '(lambda (x) (lambda (y) (x y))))))
  )

;; Typed Printf via Delimited Continuations 
;; http://www.cs.ox.ac.uk/ralf.hinze/WG2.8/27/slides/kenichi2.pdf
(let ()
  (define (sprintf format)
    (reset (format)))
  (define (fmt dir)
    (shift k (^v (k (dir v)))))
  (define s values)
  (test "sprintf" "hello, world"
        (^[] ((sprintf (^[] (string-append "hello, " (fmt s)))) "world")))
  (test "sprintf" "world"
        (^[] ((sprintf (^[] (fmt s))) "world")))
  )

;; To be written:
;;  - tests for interactions of dynamic handlers and partial continuaions.
;;  - tests for interactions of partial and full continuations.

(test-end)
