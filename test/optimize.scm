;; Test compiler's optimizing routines

(use gauche.test)
(use gauche.vm.insn)
(use srfi-1)
(use util.match)

(define (proc->insn/split proc)
  (let loop ([code ((with-module gauche.internal vm-code->list)
                    (closure-code proc))]
             [acc '()])
    (match code
      [() (reverse acc)]
      [((and (opcode . params) insn) . rest)
       (let1 info (vm-find-insn-info opcode)
         (case (~ info'operand-type)
           [(none) (loop rest `((,insn) ,@acc))]
           [(obj code codes addr)
            (loop (cdr rest) `((,insn ,(car rest)) ,@acc))]
           [(obj+addr)
            (loop (cddr rest) `((,insn ,(car rest) ,(cadr rest)) ,@acc))]))])))

(define (filter-insn proc opcode)
  (filter (^i (match i
                [(([? (cut eq? <> opcode)] . _) . _) #t]
                [_ #f]))
          (proc->insn/split proc)))

(test-start "optimizer")

(test-section "inlining")

;; Simple inlining
(define-inline (const4) 4)
(test* "inlining const4 + constant folding" '(((CONSTI 8)) ((RET)))
       (proc->insn/split (^[] (+ (const4) (const4)))))

;; Combinatorial
(define-inline (make-adder n) (^m (+ n m)))
(define-inline add4 (make-adder 4))
(test* "inlining add4 + constant folding" '(((CONSTI 9)) ((RET)))
       (proc->insn/split (^[] (+ (add4 2) 3))))

(test-section "lambda lifting")

;; bug reported by teppey
(test* "pass4 lambda marking bug" #t
       (begin ((with-module gauche.internal compile)
               '(let loop () (values (^[] #f)) (loop))
               (current-module))
              #t))

;; See if constant lambda won't make closures.
;; The internal (^k (* k k)) should be lifted to the toplevel, so that
;; there shouldn't be CLOSURE instruction.
(test* "lifting constant lambda" '()
       (filter-insn (^(xs) (map (^k (* k k)) xs)) 'CLOSURE))

;; See if constant lambda keeps identity.
;; NB: This isn't a guaranteed behavior, but it holds in the
;; current compiler, and there's no reason to lose it.
(define (make-constant-closure) (^[] #t))

(test* "constant closure identity" #t
       (eq? (make-constant-closure) (make-constant-closure)))

(test-section "transformation")

;; pass2 intermediate lref elimination
(test* "intermediate lref elimination 1" '()
       (filter-insn (^[x] (let1 p (f a) (g x p 0))) 'PUSH-LOCAL-ENV))
(test* "intermediate lref elimination 2" '()
       (filter-insn (^[x] (let ([p (f x a)] [q (g x b)]) (h p x q)))
                    'PUSH-LOCAL-ENV))
(test* "intermediate lref elimination 3" '(((PUSH-LOCAL-ENV 1)))
       (filter-insn (^[x] (let1 p (f a) (g z p 0))) 'PUSH-LOCAL-ENV))
(test* "intermediate lref elimination 4" '(((PUSH-LOCAL-ENV 1)))
       (filter-insn (^[x] (let1 p (f a) (g p (z) 0))) 'PUSH-LOCAL-ENV))
(test* "intermediate lref elimination 5" '(((PUSH-LOCAL-ENV 2)))
       (filter-insn (^[x] (let ([p (f x a)] [q (g x b)]) (h p (r q))))
                    'PUSH-LOCAL-ENV))
(test* "intermediate lref elimination 6" '()
       (filter-insn (^[x] (let* ([p (f x a)] [q (g x p)]) (h x q)))
                    'PUSH-LOCAL-ENV))
(test* "intermediate lref elimination 7" '(((PUSH-LOCAL-ENV 1)))
       (filter-insn (^[x] (let* ([p (f x a)] [q (g x p)]) (h p q)))
                    'PUSH-LOCAL-ENV))
(test* "intermediate lref elimination 8" '()
       (filter-insn (^(x) (let* ([p (f x a)] [q (g x p)] [r (h x q)]) (y r)))
                    'PUSH-LOCAL-ENV))

;; Tests for optimizer bug fixed in 2546f82
(define-inline (foo p xs . xss)
  (if (null? xss)
    (let loop ([xs xs])
      (unless (null? xs) (p (car xs)) (loop (cdr xs))))
    (let loop ([xss (cons xs xss)])
      (receive (cars cdrs) (extract-cars+cdrs xss)
        (when cars
          (apply p cars)
          (loop cdrs))))))

(test* "make sure define-inline'd procs be optimized" '()
       (filter-insn foo 'LOCAL-ENV-CLOSURES))

(test-section "eta reduction")

;; This is actually to check when eta reductino isn't done
;; when there's a hazard (side effects).  0.9.3.3 fails this.

(define (hoop f a) (f a))
(define (hoop2 x y) (list x y))

(test* "eta conversion hazard" '(9 3)
       (let ((a 1))
         (let ((b (hoop (^c (set! a 9) (+ c 1)) 2)))
           (hoop2 a b))))

;; This test exhibits a bug on inlining local function more than once.
;; Fixed by commit dd7a023.
(test* "copy&inline local function" 'bam
       (let ()
         (define (call object message . args)
           (unless (list? args)
             (error "Strange arguments: "
                    `(call (object ,object) (message ,message) (args ,args))))
           (and-let* ((it (object message)))
             (apply it object args)))

         (define (foo)
           (define (bar) (lambda _ (lambda _2 'bam)))

           (define (baz x) (call x 'baz))
           (define (baz2 x) (call x 'baz))

           (define (test x) (baz x))
           (define (test2 x) (baz x))
           (test (bar)))
         (foo)))

(test-end)

