;;
;; Test continuations and related stuff
;;

(use gauche.test)

(test-start "continuations")

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
;; Continuation prompts
;;

(test-section "continuation prompts")

(test* "continuation-prompt-tag" #t
       (continuation-prompt-tag? (make-continuation-prompt-tag)))
(test* "continuation-prompt-tag" #t
       (continuation-prompt-tag? (make-continuation-prompt-tag 'a)))
(test* "continuation-prompt-tag" #t
       (continuation-prompt-tag? (default-continuation-prompt-tag)))
(test* "continuation-prompt-tag" #f
       (equal? (make-continuation-prompt-tag)
               (default-continuation-prompt-tag)))
(test* "continuation-prompt-tag" #f
       (equal? (make-continuation-prompt-tag)
               (make-continuation-prompt-tag)))

(test* "call-with-continuation-prompt basic 1" 1
       (call-with-continuation-prompt (lambda () 1)))
(test* "call-with-continuation-prompt basic 1" 1
       (call-with-continuation-prompt (lambda () 1)
                                      (make-continuation-prompt-tag)))
(test* "call-with-continuation-prompt basic 1" 1
       (call-with-continuation-prompt (lambda () 1)
                                      (make-continuation-prompt-tag)
                                      list))

(test* "abort-current-continuation" '(foo bar)
       (let ([tag (make-continuation-prompt-tag)])
         (call-with-continuation-prompt
          (lambda ()
            (+ 1
               (abort-current-continuation tag 'foo 'bar)
               2))
          tag
          list)))

(test* "abort-current-continuation (default handler)" 'aborted
       (let ([tag (make-continuation-prompt-tag)])
         (define (abt)
           (abort-current-continuation tag (lambda () 'aborted)))
         (call-with-continuation-prompt
          (lambda () (abt) 'normal)
          tag)))

(test* "abort-current-continuation (default handler, nested)" 'aborted
       (let ([tag (make-continuation-prompt-tag)])
         (define (abt)
           ($ abort-current-continuation tag
              (lambda () (abort-current-continuation tag
                           (lambda () 'aborted)))))
         (call-with-continuation-prompt
          (lambda () (abt) 'normal)
          tag)))

(test* "abort-current-continuation (invalid tag)"
       (test-error <continuation-violation>)
       (let ([tag1 (make-continuation-prompt-tag)]
             [tag2 (make-continuation-prompt-tag)])
         (call-with-continuation-prompt
          (lambda ()
            (abort-current-continuation tag2 'foo))
          tag1)))

(let ([tag1 (make-continuation-prompt-tag 'stray)])
  (test* "abort-current-continuation (&continuation violation)"
         tag1
         (guard (e [(continuation-violation? e)
                    (continuation-violation-prompt-tag e)])
           (call-with-continuation-prompt
            (lambda ()
              (abort-current-continuation tag1 'foo))))))

'(let ([tag (make-continuation-prompt-tag 'tag)])
  (test* "abort-current-continuation crosses cstack boundary"
         '(a)
         (call-with-continuation-prompt
          (^[] (with-error-handler (^e (abort-current-continuation tag 'a))
                 (^[] (car 1))))
          tag list)))

;;-----------------------------------------------------------------------
;; Parameterizations
;;

(test-section "parameterization")

;; from SRFI-226
(let ()
  (define p (make-parameter 10 (^x (* x x))))
  (define ps #f)

  (test* "parameter" 100 (p))
  (test* "parameter" 100 (p 12))
  (test* "parameter" 144 (p))
  (test* "parameter?" #t (parameter? p))

  (test* "current-parameterization" 20736
         (parameterize ([p (p)])
           (set! ps (current-parameterization))
           (p)))
  (test* "parameter restore" 144 (p))

  (test* "parameterization?" #t (parameterization? ps))

  (test* "call-with-parameterization" '((20736 0) 144)
         (let1 r (call-with-parameterization ps
                   (^[] (let ([x (p)])
                          (p 0)
                          (list x (p)))))
           (list r (p))))
  )

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

(test* "reset/shift combination 1"
       1000
       (begin
         (define k1 #f)
         (define k2 #f)
         (define k3 #f)
         (reset
          (shift k (set! k1 k)
                 (shift k (set! k2 k)
                        (shift k (set! k3 k))))
          1000)
         (k1)
         ;(k2)
         ;(k3)
         ))

(test* "reset/shift + values 1"
       '(1 2 3)
       (values->list (reset (values 1 2 3))))

(test* "reset/shift + values 2"
       '(1 2 3)
       (begin
         (define k1 #f)
         (reset
          (shift k (set! k1 k))
          (values 1 2 3))
         (values->list (k1))))

(test* "reset/shift + parameterize 1"
       "010"
       (with-output-to-string
         (^[]
           (define p (make-parameter 0))
           (display (p))
           (reset
            (parameterize ([p 1])
              (display (p))
              ;; expr of 'shift' is executed on the outside of 'reset'
              (shift k (display (p))))))))

(test* "reset/shift + call/cc 1"
       "[r01][r02][r02][r03]"
       (with-output-to-string
         (^[]
           (define k1 #f)
           (define done #f)
           (call/cc
            (^[k0]
              (reset
               (display "[r01]")
               (shift k (set! k1 k))
               (display "[r02]")
               (unless done
                 (set! done #t)
                 (k0))
               (display "[r03]"))))
           (k1))))


(test* "reset/shift + call/cc 2"
       "[r01][s01][s02][s02]"
       (with-output-to-string
         (^[]
           (define k1 #f)
           (define k2 #f)
           (reset
            (display "[r01]")
            (shift k (set! k1 k))
            (display "[s01]")
            (call/cc (lambda (k) (set! k2 k)))
            (display "[s02]"))
           (k1)
           (reset (reset (k2))))))

(test* "reset/shift + call/cc 2-B"
       "[r01][s01]"
       (with-output-to-string
         (^[]
           (define k1 #f)
           (define k2 #f)
           (reset
            (display "[r01]")
            (shift k (set! k1 k))
            (display "[s01]")
            (call/cc (lambda (k) (set! k2 k)))
            ;; empty after call/cc
            ;(display "[s02]")
            )
           (k1)
           (reset (reset (k2))))))

(test* "reset/shift + call/cc 2-C"
       "[d01][d02][d03][d01][s01][s02][d03][d01][s02][d03]"
       (with-output-to-string
         (^[]
           (define k1 #f)
           (define k2 #f)
           (reset
            (dynamic-wind
             (^[] (display "[d01]"))
             (^[] (display "[d02]")
                  (shift k (set! k1 k))
                  (display "[s01]")
                  (call/cc (lambda (k) (set! k2 k)))
                  (display "[s02]"))
             (^[] (display "[d03]"))))
           (k1)
           (reset (reset (k2))))))

(test* "reset/shift + call/cc 2-D (from Kahua nqueen broken)"
       "[r01][s01][s02][d01][d02][d03][s02][d01]12345[d03]"
       (with-output-to-string
         (^[]
           (define k1 #f)
           (define k2 #f)
           (reset
            (display "[r01]")
            (shift k (set! k1 k))
            (display "[s01]")
            (call/cc (lambda (k) (set! k2 k)))
            (display "[s02]")
            12345)
           (k1)
           (dynamic-wind
            (^[] (display "[d01]"))
            (^[] (display "[d02]")
                 (display (reset (reset (k2)))))
            (^[] (display "[d03]"))))))

(test* "reset/shift + call/cc 3"
       "[r01][s01][s01]"
       (with-output-to-string
         (^[]
           (define k1 #f)
           (define k2 #f)
           (reset
            (display "[r01]")
            (call/cc (lambda (k)
                       (set! k1 k)
                       (shift k (set! k2 k))))
            (display "[s01]"))
           (k2)
           (reset (k1)))))

(test* "reset/shift + call/cc error 1"
       (test-error)
       (with-output-to-string
         (^[]
           (define k1 #f)
           (define k2 #f)
           (define (f1) (call/cc (^[k] (set! k1 k)))
                        (shift k (set! k2 k))
                        (display "[f01]"))
           (define (f2) (display "[f02]"))
           (reset (f1) (f2))
           (reset (k1)))))

(test* "reset/shift + call/cc error 2"
       (test-error)
       (with-output-to-string
         (^[]
           (define k1 #f)
           (define k2 #f)
           (define k3 #f)
           (define (f1) (call/cc (^[k] (set! k1 k)))
                        (shift k (set! k2 k))
                        (display "[f01]"))
           (define (f2) (display "[f02]"))
           (reset (f1) (f2))
           (reset (shift k (set! k3 k)) (k1))
           (k3))))

(test* "reset/shift + call/cc error 3"
       (test-error)
       (with-output-to-string
         (^[]
           (define k1 #f)
           (define k2 #f)
           (reset
            (call/cc (^[k] (set! k1 k)))
            (shift k (set! k2 k)))
           (k2)
           (k1))))

(let ([p (make-parameter 0)]
      [c #f])
  (define (foo)
    (reset
     (display (p))
     (parameterize ((p 1))
       (let/cc cont
         (display (p))
         (shift k (display (p)) (cont k))
         (display (p))))))
  (test* "reset/shift + call/cc + parameterize" "010"
         (with-output-to-string
           (^[] (set! c (foo)))))
  (test* "reset/shift + call/cc + parameterize" "1"
         (with-output-to-string c)))


(let ([p (make-parameter 1)]
      [c #f])
  (define (foo)
    (parameterize ((p 2))
      (reset
       (display (p))
       (temporarily ((p 3))
         (display (p))
         (shift k (display (p)) (set! c k))
         (display (p)))
       (display (p)))))
  (test* "reset/shift + temporarily + parameterize" "232"
         (with-output-to-string foo))
  (test* "reset/shift + temporarily + parameterize (cont)" "32"
         (with-output-to-string c)))

(test* "reset/shift + with-error-handler 1"
       "[E01][E02]"
       (with-output-to-string
         (^[]
           (with-error-handler
            (^[e] (display (~ e 'message)))
            (^[]
              (display "[E01]")
              (reset (error "[E02]"))
              (display "[E03]"))))))

(test* "reset/shift + guard 1"
       "[W01][D01][D02][W01][D01][D01][E01][D02][D02]"
       (with-output-to-string
         (^[]
           (define queue '())
           (define (yield) (shift k (push! queue k)))
           (push! queue (lambda ()
                          (guard (e (else (display (~ e 'message))))
                            (yield)
                            (error "[E01]"))))
           (while (and (pair? queue) (pop! queue))
             => next
             (display "[W01]")
             (reset
              (dynamic-wind
               (lambda () (display "[D01]"))
               next
               (lambda () (display "[D02]"))))))))

(test* "reset/shift + guard 2"
       "catch error!!"
       (let1 k1 #f
         (reset
          (guard (e [else "catch error!!"])
            (shift k (set! k1 k))
            (error "err")))
         (k1 100)))

(test* "reset/shift + eval 1"
       42
       (reset (eval '(shift k (k 42)) (current-module))))

(test* "reset/shift + eval 2"
       42
       (reset (eval '(+ (shift k (k 42))) (current-module))))

(test* "reset/shift + eval 3"
       42
       (reset (+ (eval '(shift k (k 42)) (current-module)))))

(test* "reset/shift + eval 4"
       '(42 43 44)
       (receive vals (reset (eval '(shift k (k 42 43 44)) (current-module)))
         vals))

(test* "dynamic-wind + reset/shift 1"
       "[d01][d02][d03][d04]"
       ;"[d01][d02][d04][d01][d03][d04]"
       (with-output-to-string
         (^[]
           (reset
            (shift
             k
             (dynamic-wind
              (^[] (display "[d01]"))
              (^[] (display "[d02]")
                   (k)
                   (display "[d03]"))
              (^[] (display "[d04]"))))))))

(test* "dynamic-wind + reset/shift 2"
       "[d01][d02][d04][d01][d03][d04]"
       (with-output-to-string
         (^[]
           (define k1 #f)
           (reset
            (dynamic-wind
             (^[] (display "[d01]"))
             (^[] (display "[d02]")
                  (shift k (set! k1 k))
                  (display "[d03]"))
             (^[] (display "[d04]"))))
           (k1))))

(test* "dynamic-wind + reset/shift 3"
       "[d01][d02][d01][d02][d01][d02][d01][d02]"
       (with-output-to-string
         (^[]
           (define k1 #f)
           (define k2 #f)
           (reset
            (dynamic-wind
             (^[] (display "[d01]"))
             (^[] (shift k (set! k1 k))
                  (shift k (set! k2 k)))
             (^[] (display "[d02]"))))
           (k1)
           (k2)
           (k2))))

(test* "dynamic-wind + reset/shift 3-B"
       "[d01][d02][d01][d11][d12][d02][d01][d11][d12][d02][d01][d11][d12][d02]"
       (with-output-to-string
         (^[]
           (define k1 #f)
           (define k2 #f)
           (reset
            (dynamic-wind
             (^[] (display "[d01]"))
             (^[] (shift k (set! k1 k))
                  (dynamic-wind
                   (^[] (display "[d11]"))
                   (^[] (shift k (set! k2 k)))
                   (^[] (display "[d12]"))))
             (^[] (display "[d02]"))))
           (k1)
           (k2)
           (k2))))

(test* "dynamic-wind + reset/shift 3-C"
       "[d01][d02][d21][d01][d11][d12][d02][d01][d11][d12][d02][d01][d11][d12][d02][d22]"
       ;"[d01][d02][d21][d22][d01][d11][d12][d02][d21][d22][d01][d11][d12][d02][d21][d22][d01][d11][d12][d02][d21][d22]"
       (with-output-to-string
         (^[]
           (define k1 #f)
           (define k2 #f)
           (reset
            (dynamic-wind
             (^[] (display "[d01]"))
             (^[] (shift k (set! k1 k))
                  (dynamic-wind
                   (^[] (display "[d11]"))
                   (^[] (shift k (set! k2 k)))
                   (^[] (display "[d12]"))))
             (^[] (display "[d02]"))))
           (dynamic-wind
            (^[] (display "[d21]"))
            (^[] (k1)
                 (k2)
                 (k2))
            (^[] (display "[d22]"))))))

(test* "dynamic-wind + reset/shift 4"
       "[d01][d11][d12][d02][d11][d12]"
       ;"[d01][d11][d12][d02][d01][d11][d12][d02]"
       (with-output-to-string
         (^[]
           (define k1 #f)
           (reset
            (dynamic-wind
             (^[] (display "[d01]"))
             (^[] (reset
                   (dynamic-wind
                    (^[] (display "[d11]"))
                    (^[] (shift k (set! k1 k)))
                    (^[] (display "[d12]")))))
             (^[] (display "[d02]"))))
           (k1))))

(test* "dynamic-wind + reset/shift 5"
       "[d01][d02][d01][d11][d12][d02][d11][d12][d11][d12]"
       ;"[d01][d02][d01][d11][d12][d02][d01][d11][d12][d02][d01][d11][d12][d02]"
       (with-output-to-string
         (^[]
           (define k1 #f)
           (define k2 #f)
           (define k3 #f)
           (reset
            (dynamic-wind
             (^[] (display "[d01]"))
             (^[] (shift k (set! k1 k))
                  (reset
                   (dynamic-wind
                    (^[] (display "[d11]"))
                    (^[] (shift k (set! k2 k))
                         (shift k (set! k3 k)))
                    (^[] (display "[d12]")))))
             (^[] (display "[d02]"))))
           (k1)
           (k2)
           (k3))))

(test* "dynamic-wind + reset/shift 6"
       "[d01][d02][d11][d12][d13][d14][d03][d04]"
       ;"[d01][d02][d11][d12][d14][d04][d01][d11][d13][d14][d03][d04]"
       (with-output-to-string
         (^[]
           (reset
            (shift
             k
             (dynamic-wind
              (^[] (display "[d01]"))
              (^[] (display "[d02]")
                   (dynamic-wind
                    (^[] (display "[d11]"))
                    (^[] (display "[d12]")
                         (k)
                         (display "[d13]"))
                    (^[] (display "[d14]")))
                   (display "[d03]"))
              (^[] (display "[d04]"))))))))

(test* "dynamic-wind + reset/shift 7"
       "[d01][d02][d11][d12][d14][d04][d01][d11][d13][d14][d03][d04]"
       (with-output-to-string
         (^[]
           (define k1 #f)
           (reset
            (dynamic-wind
             (^[] (display "[d01]"))
             (^[] (display "[d02]")
                  (dynamic-wind
                   (^[] (display "[d11]"))
                   (^[] (display "[d12]")
                        (shift k (set! k1 k))
                        (display "[d13]"))
                   (^[] (display "[d14]")))
                  (display "[d03]"))
             (^[] (display "[d04]"))))
           (k1))))

(test* "dynamic-wind + reset/shift 8"
       "[d01][d02][d04][d11][d12][d01][d03][d04][d13][d14]"
       ;"[d01][d02][d04][d11][d12][d14][d01][d03][d04][d11][d13][d14]"
       (with-output-to-string
         (^[]
           (define k1 #f)
           (reset
            (dynamic-wind
             (^[] (display "[d01]"))
             (^[] (display "[d02]")
                  (shift k (set! k1 k))
                  (display "[d03]"))
             (^[] (display "[d04]"))))
           (dynamic-wind
            (^[] (display "[d11]"))
            (^[] (display "[d12]")
                 (k1)
                 (display "[d13]"))
            (^[] (display "[d14]"))))))

;;
;; Continuation marks
;;

(test-section "continuation marks")

;; Examples from SRFI-157

(test* "continuation marks 1" '(1)
       (let ((key (vector 'key)))
         (with-continuation-mark key 1
           (continuation-mark-set->list (current-continuation-marks) key))))

(test* "continuation marks 2" '(foo 2 1)
       (let ((key (vector 'key)))
         (with-continuation-mark key 1
           (cons 'foo
                 (with-continuation-mark key 2
                   (continuation-mark-set->list (current-continuation-marks) key))))))

(test* "continuation marks 3" '(2)
       (let ((key (vector 'key)))
         (with-continuation-mark key 1
           (with-continuation-mark key 2
             (continuation-mark-set->list (current-continuation-marks) key)))))

(test* "continuation marks w/ captured cont 1" '(1)
       (let ((key '#:key))
         (with-continuation-mark key 1
           (let/cc k
             (with-continuation-mark key 2
               (continuation-mark-set->list (continuation-marks k) key))))))

(test* "continuation marks w/ captured cont 1" '(2 1)
       (let* ([key '#:key]
              [p (with-continuation-mark key 1
                   (cons (with-continuation-mark key 2
                           (let/cc k
                             (with-continuation-mark key 3
                               (continuation-marks k))))
                         'a))])
         (continuation-mark-set->list (car p) key)))

(define-module ccm-fact
  (use gauche.test)

  (define %key (vector 'key))

  (define (ccm)
    (continuation-mark-set->list (current-continuation-marks) %key))

  (define (fact1 n)
    (let loop ((n n))
      (if (zero? n)
        (begin
          (display (ccm))
          (newline)
          1)
        (with-continuation-mark %key n (* n (loop (- n 1)))))))

  (define (fact2 n)
    (let loop ((n n) (a 1))
      (if (zero? n)
        (begin
          (display (ccm))
          (newline)
          a)
        (with-continuation-mark %key n (loop (- n 1) (* n a))))))

  (define (t f)
    (read-from-string (with-output-to-string (cut f 3))))

  (test* "non-tail-recursive fact" '(1 2 3)
         (t fact1))
  (test* "tail-recursive fact" '(1)
         (t fact2)))

(test* "with-continuation-marks"
       '((5 1) (2) (4 3))
       (let1 p (with-continuation-marks (['a 1] ['b 2] ['c 3])
                 (cons (with-continuation-marks (['c 4] ['a 5])
                         (current-continuation-marks))
                       3))
         (list (continuation-mark-set->list (car p) 'a)
               (continuation-mark-set->list (car p) 'b)
               (continuation-mark-set->list (car p) 'c))))

;; delimited
(test* "current-continuation-marks w/tag" '(d c)
       (let* ([tag (make-continuation-prompt-tag)]
              [cms
               (with-continuation-mark 'a 'b
                 (call-with-continuation-prompt
                  (^[]
                    (with-continuation-mark 'a 'c
                      (apply identity
                             (list (with-continuation-mark 'a 'd
                                     (current-continuation-marks tag))))))
                  tag))])
         (continuation-mark-set->list cms 'a)))

(test* "current-continuation-marks w/tag"
       (test-error <continuation-violation>)
       (let1 tag (make-continuation-prompt-tag)
         (current-continuation-marks tag)))

;; call-with-immediate-continuation-mark

(test* "call-with-immediate-continuation-mark" 'mark
       (with-continuation-mark 'key 'mark
         (call-with-immediate-continuation-mark 'key values)))

(test* "call-with-immediate-continuation-mark" '(foo . default)
       (with-continuation-mark 'key 'mark
         (cons 'foo
               (call-with-immediate-continuation-mark 'key values 'default))))

(test* "call-with-immediate-continuation-mark" #f
       (with-continuation-mark 'key 'mark
         (call-with-immediate-continuation-mark '#:nonexistent values)))

;; continuation-mark-set-first

(test* "continuation-mark-set-first" 'mark2
       (with-continuation-mark 'key 'mark1
         (with-continuation-mark 'key 'mark2
           (continuation-mark-set-first #f 'key 'default))))

(test* "continuation-mark-set-first" 'mark1
       (with-continuation-mark 'key 'mark1
         (let1 cm (current-continuation-marks)
           (with-continuation-mark 'key 'mark2
             (continuation-mark-set-first cm 'key 'default)))))

(test* "continuation-mark-set-first" 'default
       (with-continuation-mark 'key 'mark1
         (with-continuation-mark 'key 'mark2
           (continuation-mark-set-first #f 'key2 'default))))

;; See if parameterize body is evaluated in tail context
;; (SRFI-226)
(test* "parameterize body in tail context" #t
       (with-continuation-mark 'in-tail-context #t
         (parameterize ([(make-parameter 0) 1])
           (call-with-immediate-continuation-mark 'in-tail-context identity))))

(test-end)
