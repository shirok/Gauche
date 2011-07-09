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
       (proc->insn/split (lambda () (+ (const4) (const4)))))

;; Combinatorial
(define-inline (make-adder n) (lambda (m) (+ n m)))
(define-inline add4 (make-adder 4))
(test* "inlining add4 + constant folding" '(((CONSTI 9)) ((RET)))
       (proc->insn/split (lambda () (+ (add4 2) 3))))





(test-end)

