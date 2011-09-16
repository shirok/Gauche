;; tests for delayed evaluation constructs

(use gauche.test)

(test-start "lazy evaluation")

;;----------------------------------------------------------------
(test-section "lazy, delay & force")

;; Note: thread-safety of force is tested in ext/threads.

(test* "simple delay" 3
       (force (delay (+ 1 2))))

(test* "delay w/state" 3
       (let1 x 9
         (let1 d (delay (/ x 3))
           (force d)
           (set! x 99)
           (force d))))

(test* "delay recursive" 6  ;; R5RS 6.4
       (letrec ([count 0]
                [x 5]
                [p (delay (begin (set! count (+ count 1))
                                 (if (> count x)
                                   count
                                   (force p))))])
         (force p)
         (set! x 10)
         (force p)))

;; check to see the compiler does the right thing about expanding
;; (delay x) to (lazy (eager x)).
(test* "delay compilation" 3
       (force 
        (let ([lazy list]
              [eager list])
          (delay (force 3)))))

;; srfi-45 test suite
(test* "memoize 1" 1
       (let1 count 0
         (define s (delay (begin (set! count (+ count 1)) 1)))
         (force s)
         (force s)
         count))

(test* "memoize 2" 1
       (let1 count 0
         (define s (delay (begin (set! count (+ count 1)) 1)))
         (+ (force s) (force s))
         count))

(test* "memoize 3" 1  ;; (Alejandro Forero Cuervo)
       (let1 count 0
         (let* ([r (delay (begin (set! count (+ count 1)) 1))]
                [s (lazy r)]
                [t (lazy s)])
           (force t)
           (force r)
           count)))

(test* "memoize 4" 5  ;; stream memoization
       (let1 count 0
         (define (stream-drop s index)
           (lazy
            (if (zero? index)
              s
              (stream-drop (cdr (force s)) (- index 1)))))
         (define (ones)
           (delay (begin
                    (set! count (+ count 1))
                    (cons 1 (ones)))))
         (let ((s (ones)))
           (car (force (stream-drop s 4)))
           (car (force (stream-drop s 4)))
           count)))

(test* "reentrancy 1" 'second       ;; see srfi-40 post-discussion
       (let ()
         (define f
           (let ((first? #t))
             (delay
               (if first?
                 (begin
                   (set! first? #f)
                   (force f))
                 'second))))
         (force f)))

(test* "reentrancy 2" '(5 0 10) ;; (John Shutt)
       (let ()
         (define q
           (let ((count 5))
             (define (get-count) count)
             (define p (delay (if (<= count 0)
                                count
                                (begin (set! count (- count 1))
                                       (force p)
                                       (set! count (+ count 2))
                                       count))))
             (list get-count p)))
         (let* ((get-count (car q))
                (p (cadr q))
                (a (get-count))
                (b (force p))
                (c (get-count)))
           (list a b c))))

;; This leak test takes long time, so we exclude it by default.
;; If you run this on pre-0.8.6 Gauche (with replacing lazy by delay+force),
;; it'll bust the memory.
'(test* "leak 1" 10000000
       (let ()
         (define (stream-filter p? s)
           (lazy
            (let ((lis (force s)))
              (if (null? lis)
                (delay '())
                (let ((h (car lis))
                      (t (cdr lis)))
                  (if (p? h)
                    (delay (cons h (stream-filter p? t)))
                    (stream-filter p? t)))))))
         (define (from n)
           (delay (cons n (from (+ n 1)))))
         (car (force (stream-filter (lambda (n) (= n 10000000))
                                    (from 0))))))

;;----------------------------------------------------------------
(test-section "lazy pairs")

;; It is easier to use gauche.generator to test lazy pairs,
;; but gauche.generator test is done in ext/gauche.  Here
;; we roll our own generators.  We also have some tests for
;; thread safety of lazy pairs in ext/threads.

(define (gnull) (eof-object))

(define (giota n)
  (let1 cnt 0
    (^() (if (< cnt n)
           (begin0 cnt (inc! cnt))
           (eof-object)))))

(define (gerr n) ;; raise an error after generating n items
  (let1 cnt 0
    (^() (if (< cnt n)
           (begin0 cnt (inc! cnt))
           (error "oof")))))

(test* "pair?" #t (pair? (lseq 0 gnull)))
(test* "car" 0    (car (lseq 0 gnull)))
(test* "cdr" '()  (cdr (lseq 0 gnull)))

(test* "lazyness" 0 (car (lseq 0 (gerr 1))))
(test* "lazyness" 1 (caddr (lseq 0 (gerr 3))))
(test* "lazyness" (test-error) (cadddr (lseq 0 (gerr 3))))
(test* "lazyness string?" #f (string? (lseq 0 (gerr 0))))
(test* "lazyness pair?" (test-error) (pair? (lseq 0 (gerr 0))))
(test* "lazyness list?" (test-error) (list? (lseq 0 (gerr 10))))

(test* "apply robustness" '(0) (apply list (lseq 0 gnull)))
(test* "apply robustness" '(0 1) (apply list 0 (lseq 1 gnull)))
(test* "apply robustness" (test-error) (apply list 0 (lseq 1 (gerr 10))))
(test* "object-apply robustness" (test-error) ((lseq 0 gnull)))

(test* "eq? doesn't force lazy pair" #f (eq? '(0) (lseq 0 (gerr 0))))
(test* "eqv? doesn't force lazy pair" #f (eqv? '(0) (lseq 0 (gerr 0))))
(test* "forcing lazy pair doen't change the identity" #t
       (let* ([p0 (cons 0 (lseq 0 gnull))]
              [p1 (cdr p0)])
         (and (length p0) (eq? (cdr p0) p1))))

;; We don't recommend mutating lazy pairs, but we can't enforce it anyway,
;; so we better check it won't break.
(test* "lazy pair and mutation" '(1)
       (rlet1 p (lseq 0 gnull)
         (set! (car p) 1)))
(test* "lazy pair and mutation" '(0 . 1)
       (rlet1 p (lseq 0 gnull)
         (set! (cdr p) 1)))

;; lazy pair returns <pair> for class-of, without forcing.
(test* "class-of" <pair> (class-of (lseq 0 (gerr 0))))
(test* "is-a?" #f (is-a? (lseq 0 gnull) <null>))
(test* "is-a?" #t (is-a? (lseq 0 (gerr 0)) <pair>))

;; Forcing lazy pair during builtin insturction evaluation.
;; For memq & memv test, we coerce the result to boolean so that
;; the result check won't force the rest of the seq.
(let ()
  (define (t nam p)
    (test* nam #t (boolean (p 'c (lseq 'a 'b 'c gnull))))
    (test* #`",nam (lazyness)" #t
           (boolean (p 'b (lseq 'a 'b 'c (gerr 0)))))
    (test* nam #f (boolean (p 'z (lseq 'a 'b 'c gnull)))))
  (t "memq" memq)
  (t "memv" memv))
(let ()
  (define (t nam p)
    (test* nam '(c . 2)
           (p 'c (lseq '(a . 0) '(b . 1) '(c . 2) gnull)))
    (test* #`",nam (lazyness)" '(b . 1)
           (p 'b (lseq '(a . 0) '(b . 1) '(c . 2) (gerr 0))))
    (test* nam #f (p 'z (lseq 'a 'b 'c gnull))))
  (t "assq" assq)
  (t "assv" assv))
(test* "reverse" '(4 3 2 1 0) (reverse (lseq (giota 5))))
(test* "append" '(0 1 2 0 1 2 3 0 1 2 3 4)
       (append (lseq (giota 3))
               (lseq (giota 4))
               (lseq (giota 5))))
(test* "APPEND instruction for quasiquoting" '(0 1 2 0 1 2 3 0 1 2 3 4)
       `(,@(lseq (giota 3))
         ,@(lseq (giota 4))
         ,@(lseq (giota 5))))
(test* "APP-VEC instruction for quasiquoting" '#(0 1 2 0 1 2 3 0 1 2 3 4)
       `#(,@(lseq (giota 3))
          ,@(lseq (giota 4))
          ,@(lseq (giota 5))))
(test* "CONS instruction" '(2 1 . 0)
       (cons 2 (cons 1 (car (lseq 0 gnull)))))

(test* "length" 10 (length (lseq (giota 10))))
(test* "fold" 45 (fold + 0 (lseq (giota 10))))
(test* "equal?" #t (equal? '(0 1 2 3 4) (lseq (giota 5))))

;; Interference with partial continuations
(use gauche.partcont)
(let ()
  (define (inv lis)
    (define (kk)
      (reset (for-each (lambda (e) (shift k (set! kk k) e)) lis)
             (set! kk (lambda () (eof-object)))
             (eof-object)))
    (lambda () (kk)))
  (define (mux g0 g1)
    (define flag #t)
    (lambda ()
      (begin0 (if flag (g0) (g1))
        (set! flag (not flag)))))
  (define (make-seq)
    (lseq 0 (mux (inv '(a b c d e)) (inv '(1 2 3 4 5)))))
  (test* "interference with partcont" '(z 0 a 1 b 2 c 3 d 4 e 5)
         (cons 'z (make-seq)))
  )

;; lcons
(let ()
  (define (take xs n) ; avoid depending srfi-1
    (if (= n 0)
      '()
      (cons (car xs) (take (cdr xs) (- n 1)))))
  (define (fib a b)
    (let1 n (+ a b)
      (lcons n (fib b n))))
  (define (weird-fib a b) ; generates ordinary pairs as well
    (let1 n (+ a b)
      (if (odd? n)
        (lcons n (weird-fib b n))
        (cons n (weird-fib b n)))))
  (define (lim-fib a b k)
    (if (= k 0)
      'yay                 ;returning any non-pair terminates the sequence
      (let1 n (+ a b)
        (lcons n (lim-fib b n (- k 1))))))

  (test* "corecursion with lcons"
         '(1 2 3 5 8 13 21 34 55 89 144 233 377 610 987)
         (take (fib 0 1) 15))

  (test* "corecursion with lcons, mixed"
         '(1 2 3 5 8 13 21 34 55 89 144 233 377 610 987)
         (take (weird-fib 0 1) 15))

  ;; NB: we may make this an error in the future versions.
  (test* "corecursion doesn't allow dotted list"
         '(1 2 3 5 8 13 21 34 55 89)
         (lim-fib 0 1 10))
  )

(test-end)

