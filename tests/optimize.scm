;; Test compiler's optimizing routines

(use gauche.test)
(use gauche.vm.insn)
(use scheme.list)
(use gauche.uvector)
(use util.match)

(define (proc->insn/split proc)
  (let loop ([code ((with-module gauche.internal vm-code->list)
                    (closure-code proc))]
             [acc '()])
    (match code
      [() (reverse acc)]
      [((and (opcode . params) insn) . rest)
       (let1 info (vm-find-insn-info opcode)
         (ecase (~ info'operand-type)
           [(none) (loop rest `((,insn) ,@acc))]
           [(obj code codes label)
            (loop (cdr rest) `((,insn ,(car rest)) ,@acc))]
           [(obj+label obj+native)
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

;; Closed env (pass1/check-inlinable-lambda $LET branch)
(define-inline lambda-with-closed-env
  (let ((h (make-hash-table 'eq?)))
    (^k (hash-table-ref h k #f))))
(test* "inlining lambda with closed env" '(((GREF-PUSH) |transient id|)
                                           ((CONST-PUSH) a)
                                           ((CONSTF-PUSH))
                                           ((GREF-TAIL-CALL 3) hash-table-ref)
                                           ((RET)))
       (map (^[insn]
              (match insn
                [(op (? wrapped-identifier? id))
                 (let1 name (unwrap-syntax id)
                   (if (symbol-interned? name)
                     `(,op ,name)
                     `(,op |transient id|)))]
                [_ insn]))
            (proc->insn/split (^[] (lambda-with-closed-env 'a)))))

;; Combinatorial
(define-inline (make-adder n) (^m (+ n m)))
(define-inline add4 (make-adder 4))
(test* "inlining add4 + constant folding" '(((CONSTI 9)) ((RET)))
       (proc->insn/split (^[] (+ (add4 2) 3))))

;; pass1 setter inlining
(test* "inlining setter" '(((LREF1-PUSH)) ((LREF0)) ((VEC-SETI 0)) ((RET)))
       (proc->insn/split (^[a b] (set! (vector-ref a 0) b))))
(test* "inlining setter" #t
       (let1 xs (proc->insn/split (^[a b] (set! (car a) b)))
         (or (any (^x (equal? (car x) '(GREF-TAIL-CALL 2))) xs)
             xs))) ;return xs in case of failure for easier diag

;; pass2/inline-tail-apply
(test* "inlining apply" '(((LREF0-PUSH)) ((LREF1)) ((CONS)) ((RET)))
       (proc->insn/split (^[a b] (let1 xs `(,b ,a) (apply cons xs)))))
(test* "inlining apply" '(((LREF0-NUMADDI 1)) ((RET)))
       (proc->insn/split (^[a] (let1 xs `(,a) (apply + 1 xs)))))
(test* "inlining apply" '(((CONSTI-PUSH 1)) ((CONSTI 2)) ((CONS)) ((RET)))
       (proc->insn/split (^[] (let1 xs '(1 2) (apply cons xs)))))
(test* "inlining apply" '(((CONST-PUSH) :a) ((CONSTI 2)) ((CONS)) ((RET)))
       (proc->insn/split (^[] (let1 xs '(2) (apply cons :a xs)))))

;; This should give up apply inlining, for the content of xs may be
;; altered when passed to a global function.
(test* "inlining apply (give up)"
       '(((CONSTI 2)) ((LIST 1)) ((PUSH-LOCAL-ENV 1)) ((PRE-CALL 1) 8)
         ((LREF0-PUSH)) ((GREF-CALL 1) foo)
         ((GREF-PUSH) cons) ((CONSTI-PUSH 1)) ((LREF0)) ((TAIL-APPLY 3))
         ((RET)))
       (unwrap-syntax
        (proc->insn/split
         (^[] (let1 xs (list 2) (foo xs) (apply cons 1 xs))))))
;; Similar to above, but if xs is constant, so we don't worry about mutation.
(test* "inlining apply (ok)"
       '(((PRE-CALL 1) 6) ((CONST-PUSH) (2)) ((GREF-CALL 1) foo)
         ((CONSTI-PUSH 1)) ((CONSTI 2)) ((CONS))
         ((RET)))
       (unwrap-syntax
        (proc->insn/split
         (^[] (let1 xs '(2) (foo xs) (apply cons 1 xs))))))
;; This is the one that exhibits the bug in the issue
;; https://github.com/shirok/Gauche/issues/685
(test* "inlining apply (give up)"
       '(((CONST) a) ((LIST 1)) ((PUSH-LOCAL-ENV 1))
         ((PRE-CALL 2) 10) ((LREF0-PUSH)) ((LREF0-PUSH))
         ((GREF-CALL 2) set-cdr!)
         ((GREF-PUSH) list) ((LREF0)) ((TAIL-APPLY 2)) ((RET)))
       (unwrap-syntax
        (proc->insn/split
         (^[] (let ((x (list 'a)))
                (set-cdr! x x)
                (apply list x))))))

(define-inline (apply-inline-1 . args) (apply + 3 args))

(test* "inlining apply" '(((LREF2-NUMADDI 3))
                          ((LREF-VAL0-NUMADD2 0 1))
                          ((LREF-VAL0-NUMADD2 0 0))
                          ((RET)))
       (proc->insn/split (^[a b c] (apply-inline-1 a b c))))

(test* "inlining apply (later stage)" '(((CONSTI-PUSH 5))
                                        ((LREF2))
                                        ((NUMMUL2))
                                        ((PUSH))
                                        ((LREF1))
                                        ((NUMMUL2))
                                        ((PUSH))
                                        ((LREF0))
                                        ((NUMMUL2))
                                        ((RET)))
        (proc->insn/split (^[a b c]
                            (define (f . args) (apply * 5 args))
                            (define xs (list a b c))
                            (apply f xs))))

;; https://github.com/shirok/Gauche/issues/1167
(let ()
  (define (fm fn lis)
    (let loop ([lis lis] [r '()])
      (cond [(null-list? lis) (reverse r)]
            [(fn (car lis)) => (^x (loop (cdr lis) (cons x r)))]
            [else (loop (cdr lis) r)])))
  (test* "pass2 inlining local recursive proc" '()
         (filter-insn fm 'LOCAL-ENV-CLOSURES))
  )

;; pass3/late-inline
(define-inline (late-inline-test-1 ref) (cut ref <> 0))

(test* "pass3/late-inline"
       '(((LREF0)) ((VEC-REFI 0)) ((RET)))
       (proc->insn/split (late-inline-test-1 vector-ref)))

(test* "pass3/late-inline"
       '(((LREF0-PUSH)) ((CONSTI 0)) ((UVEC-REF 1)) ((RET)))
       (proc->insn/split (late-inline-test-1 u8vector-ref)))


;; pass3/receive
(test* "inlining receive-apply-values pattern" '(((LREF0-CAR))
                                                 ((PUSH-LOCAL-ENV 1))
                                                 ((PRE-CALL 1) 8)
                                                 ((CONST-PUSH) "foo")
                                                 ((GREF-CALL 1) print)
                                                 ((LREF0-RET)))
       (unwrap-syntax (proc->insn/split (^x (begin0 (car x) (print "foo"))))))

;; case-lambda inlining

(define-inline case-lambda-inline-test-1
  (case-lambda
    [(a) `(one ,a)]
    [(a b) `(two ,a ,b)]
    [(a b c d) `(four ,a ,b ,c ,d)]
    [(a . as) 'other]))

(test* "inlining case-lambda 0" `(((GREF-TAIL-CALL 0) case-lambda-inline-test-1)
                                  ((RET)))
       (unwrap-syntax (proc->insn/split (^[] (case-lambda-inline-test-1)))))

(test* "inlining case-lambda 1" `(((CONST-PUSH) one)
                                  ((CONST) x)
                                  ((LIST 2))
                                  ((RET)))
       (proc->insn/split (^[] (case-lambda-inline-test-1 'x))))

(test* "inlining case-lambda 2" `(((CONST-PUSH) two)
                                  ((CONST-PUSH) x)
                                  ((CONST) y)
                                  ((LIST 2))
                                  ((CONS))
                                  ((RET)))
       (proc->insn/split (^[] (case-lambda-inline-test-1 'x 'y))))

(test* "inlining case-lambda 3" `(((CONST-RET) other))
       (proc->insn/split (^[] (case-lambda-inline-test-1 'x 'y 'z))))

(test* "inlining case-lambda 4" `(((CONST-PUSH) four)
                                  ((CONST-PUSH) x)
                                  ((CONST-PUSH) y)
                                  ((CONST-PUSH) z)
                                  ((CONST) w)
                                  ((LIST 2))
                                  ((LIST-STAR 3))
                                  ((CONS))
                                  ((RET)))
       (proc->insn/split (^[] (case-lambda-inline-test-1 'x 'y 'z 'w))))

(test* "inlining case-lambda many" `(((CONST-RET) other))
       (proc->insn/split (^[] (case-lambda-inline-test-1 'x 'y 'z
                                                         'x 'y 'z
                                                         'x 'y 'z))))

(define-inline case-lambda-inline-test-2
  (case-lambda
    [() 'zero]
    [(a) 'one]))

(test* "inlining case-lambda 2-1" `(((CONST-RET) one))
       (proc->insn/split (^[] (case-lambda-inline-test-2 'x))))

(test* "inlining case-lambda 2-2" `(((CONST-PUSH) x)
                                    ((CONST-PUSH) y)
                                    ((GREF-TAIL-CALL 2) case-lambda-inline-test-2)
                                    ((RET)))
       (unwrap-syntax
        (proc->insn/split (^[] (case-lambda-inline-test-2 'x 'y)))))

;; This tests pass2 case-lambda inlining
(define (case-lambda-pass2-inline-1)
  ((case-lambda [(x) (+ x x)] [(x y) (+ x y)]) 1))
(test* "inlining case-lambda in pass2 1-1" `(((CONSTI 2))
                                             ((RET)))
       (proc->insn/split case-lambda-pass2-inline-1))

(define (case-lambda-pass2-inline-2)
  ((case-lambda [(x) (+ x x)] [(x y) (+ x y)]) 1 5))
(test* "inlining case-lambda in pass2 1-2" `(((CONSTI 6))
                                             ((RET)))
       (proc->insn/split case-lambda-pass2-inline-2))

(define (case-lambda-pass2-inline-0)
  ((case-lambda [(x) (+ x x)] [(x y) (+ x y)])))
(test* "inlining case-lambda in pass2 1-0" 'make-case-lambda
       (any (^[insn] (match insn
                       [(_ (? identifier? z)) (unwrap-syntax z)]
                       [_ #f]))
            (proc->insn/split case-lambda-pass2-inline-0)))

(define (case-lambda-pass2-inline-3)
  ((case-lambda [(x) (+ x x)] [(x y) (+ x y)]) 1 5 9))
(test* "inlining case-lambda in pass2 1-3" 'make-case-lambda
       (any (^[insn] (match insn
                       [(_ (? identifier? z)) (unwrap-syntax z)]
                       [_ #f]))
            (proc->insn/split case-lambda-pass2-inline-3)))

;; getter-with-setter inlining
(define-inline (getter-setter-inline-1 x) (car x))
(define-inline (getter-setter-inline-1-set! x v) (set-car! x v))
(define-inline getter-setter-inline-2
  (getter-with-setter getter-setter-inline-1 getter-setter-inline-1-set!))
(test* "inlining getter-with-setter-ed proc, global def, named, get"
       '(((LREF0-CAR)) ((RET)))
       (proc->insn/split (^x (getter-setter-inline-2 x))))
(test* "inlining getter-with-setter-ed proc, global def, named, set"
       '(((LREF0-PUSH)) ((CONSTI-PUSH 2)) ((GREF-TAIL-CALL 2) set-car!) ((RET)))
       (unwrap-syntax
        (proc->insn/split (^x (set! (getter-setter-inline-2 x) 2)))))

(define-inline getter-setter-inline-3
  (getter-with-setter (^x (cdr x))
                      (^[x v] (set-cdr! x v))))
(test* "inlining getter-with-setter-ed proc, global def, anon lambda, get"
       '(((LREF0-CDR)) ((RET)))
       (proc->insn/split (^x (getter-setter-inline-3 x))))
(test* "inlining getter-with-setter-ed proc, global def, anon-lambda, set"
       '(((LREF0-PUSH)) ((CONSTI-PUSH 2)) ((GREF-TAIL-CALL 2) set-cdr!) ((RET)))
       (unwrap-syntax
        (proc->insn/split (^x (set! (getter-setter-inline-3 x) 2)))))



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

(test* "lifting constant lambda 2" '()
       (filter-insn (^[] (let ([a (^[x r] (* x r))])
                           (let ([b (^[xs] (fold a '() xs))])
                             (f b))))
                    'CLOSURE))

;; https://github.com/shirok/Gauche/issues/826
(test* "lifting with clambda" #t
       (procedure?
        (eval '(letrec
                   ((f (lambda (a.7 . rest.8)
                         (lambda rest.9
                           (apply (letrec ((h (case-lambda
                                                (() h)
                                                ((b.12 . rest.13)
                                                 (lambda rest.14
                                                   (apply (+ a.7 b.12)
                                                          rest.14))))))
                                    h)
                                  rest.9)))))
                 f)
              (current-module))))

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

;; Curried inline procedure
(define-inline ((curried x) y) (list x y))
(test* "curried inline procedure" '(((CONSTI-PUSH 3))
                                    ((CONSTI 5))
                                    ((LIST 2))
                                    ((RET)))
       (proc->insn/split (^[] ((curried 3) 5))))
(define-inline curried-1 (curried 7))
(test* "curried inline procedure (indirect)" '(((CONSTI-PUSH 7))
                                               ((CONSTI 9))
                                               ((LIST 2))
                                               ((RET)))
       (proc->insn/split (^[] (curried-1 9))))

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

;; This test exhibits a bug on inlining a procedure within the same
;; compilation unit.  Fixed by commit 43b479fb0.
(test* "inlining procedure in the same compilation unit"
       #f
       (let ((m (make-module #f)))
         ;; If foo is inline-expanded, gref to foo won't remain.
         (define (reference-to-foo? opcode)
           (and (wrapped-identifier? opcode)
                (eq? (identifier->symbol opcode) 'foo)))

         (eval '(begin
                  (define-inline (foo x) (pair? x))
                  (define (bar y) (foo (car y))))
               m)
         (find
          (^[insn]
            (match insn
              [(_ (? reference-to-foo? x)) #t]
              [_ #f]))
          (proc->insn/split (module-binding-ref m 'bar)))))

;; generic function pre-dispatch
;; NB: We're still not sure how to expose this feature in general.
;; For the time being, we test the inlining logic (it's in pass 3)
;; with the hand-wired setting.  Don't copy this way to your production code.
(define-inline static-gf (make <generic> :name 'static-gf))
(define-method static-gf :locked ((x <string>)) 'string)
(define-method static-gf :locked ((x <number>)) 'number)
((with-module gauche.object generic-seal!) static-gf)

(test* "generic function pre-dispatch"
       '(((CONST-RET) string))
       (proc->insn/split (^[] (static-gf "abc"))))
(test* "generic function pre-dispatch"
       '(((CONST-RET) number))
       (proc->insn/split (^[] (static-gf 123))))

(test-end)
