;;;
;;; gauche.vm.register-machine - Register machine emulator
;;;
;;;   Copyright (c) 2021  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; This module interprets the output of basic-block backend (gauche.cgen.bbb)
;; for testing the backend.  Not for practical use---especially, performance
;; is not considered.

(define-module gauche.vm.register-machine
  (extend gauche.internal)  ; for modifying global binding
  (extend gauche.cgen.bbb)
  (use util.match)
  (use gauche.vm.insn)
  (export run-on-register-machine))
(select-module gauche.vm.register-machine)

(define (run-on-register-machine program)
  (let1 benv (compile-to-basic-blocks program)
    (dump-benv benv)
    (execute-basic-blocks benv)))

(define (execute-basic-blocks benv)
  (let1 regs (initialize-regs benv (make-hash-table 'eq?))
    (run-bb benv regs (~ benv'entry))))

(define (initialize-regs benv regs)
  (dolist [reg (~ benv'registers)]
    (hash-table-put! regs reg
                     (cond [(is-a? reg <const>) (unbox (~ reg'value))]
                           [(reg-boxed? reg) (box (undefined))]
                           [else (undefined)])))
  regs)

(define-syntax define-reg-ref
  (syntax-rules ()
    [(_ reg-ref regs val0)
     (define (reg-ref reg)
       (if (eq? reg '%VAL0)
         val0
         (let1 v (or (hash-table-get regs reg #f)
                     (error "[internal] Invalid reg:" reg))
           (if (and (is-a? reg <reg>) (reg-boxed? reg)) (unbox v) v))))]))

(define (run-bb benv regs bb)
  (let loop ([insns (reverse (~ bb'insns))]
             [val0 (undefined)])
    (define-reg-ref reg-ref regs val0)
    (define (reg-set! reg val)
      (if (reg-boxed? reg)
        (set-box! (hash-table-get regs reg) val)
        (hash-table-put! regs reg val)))
    (match insns
      [() val0]
      [(('MOV dreg sreg) . insns)
       (reg-set! dreg (reg-ref sreg))
       (loop insns #f)]
      [(('MOV* . _) . _)(error "MOV* unspported yet")]
      [(('LD reg id) . insns)
       (reg-set! reg (global-variable-ref (~ id'module) (~ id'name)))
       (loop insns #f)]
      [(('ST reg id) . insns)
       (if-let1 gloc (find-binding (~ id'module) (~ id'name) #f)
         (gloc-set! gloc (reg-ref reg))
         (error "[intenral] Attempt to set unbound global variable:" id))
       (loop insns #f)]
      [(('CLOSE reg lbenv) . insns)
       (reg-set! reg (close-benv regs lbenv))
       (loop insns #f)]
      [(('BR reg tbb ebb) . _)
       (loop (reverse (~ (if (reg-ref reg) tbb ebb)'insns)) #f)]
      [(('JP bb) . _)
       (loop (reverse (~ bb'insns)) #f)]
      [(('CONT bb) . insns)
       (let1 val0 (loop insns #f)
         (loop (reverse (~ bb'insns)) val0))]
      [(('CALL reg . args) . _)
       (apply (reg-ref reg) (map reg-ref args))]
      [(('RET reg) . _)
       (reg-ref reg)]
      [(('DEF id flags reg) . insns)
       (%insert-binding (~ id'module) (~ id'name) flags)
       (loop insns #f)]
      [(('ASM op . args) . insns)
       (loop insns (run-asm benv regs op args val0))]
      [(('BUILTIN op . args) . insns)
       (loop insns (run-builtin benv regs op args val0))]
      [(x . _) (error "[intenral] Unknown insn:" x)])))

(define (close-benv parent-regs benv)
  (define newregs (rlet1 regs (hash-table-copy parent-regs)
                    (initialize-regs benv regs)))
  (define (check-input args)
    (if (= (length (~ benv'input-regs)) (~ benv'input-reqargs))
      (unless (= (length args) (~ benv'input-reqargs))
        (errorf "Wrong number of args for closed benv ~s: ~s expected, got ~s"
                (~ benv'name) (~ benv'input-reqargs) (length args)))
      (unless (>= (length args) (~ benv'input-reqargs))
        (errorf "Too few args for closed benv ~s: at least ~s expeced, got ~s"
                (~ benv'name) (~ benv'input-reqargs) (length args)))))
  (define (bind-input args)
    (check-input args)
    (rlet1 regs (hash-table-copy newregs)
      (let loop ([args args]
                 [input (~ benv'input-regs)]
                 [n (~ benv'input-reqargs)])
        (cond [(null? input)]
              [(zero? n) (hash-table-put! regs (car input) args)]
              [else (hash-table-put! regs (car input) (car args))
                    (loop (cdr args) (cdr input) (- n 1))]))))
  (lambda args
    (let1 regs (bind-input args)
      ;(dump-regs regs)
      (run-bb benv regs (~ benv'entry)))))

(define (run-asm benv regs op args val0)
  (define-reg-ref reg-ref regs val0)
  (case (~ (vm-find-insn-info (car op))'name)
    [(NUMADD2) (+ (reg-ref (car args)) (reg-ref (cadr args)))]
    [(NUMSUB2) (- (reg-ref (car args)) (reg-ref (cadr args)))]
    [(NUMMUL2) (* (reg-ref (car args)) (reg-ref (cadr args)))]
    [(NUMDIV2) (/ (reg-ref (car args)) (reg-ref (cadr args)))]
    [(NUMEQ2)  (=  (reg-ref (car args)) (reg-ref (cadr args)))]
    [(NUMLT2)  (<  (reg-ref (car args)) (reg-ref (cadr args)))]
    [(NUMLE2)  (<= (reg-ref (car args)) (reg-ref (cadr args)))]
    [(NUMGT2)  (>  (reg-ref (car args)) (reg-ref (cadr args)))]
    [(NUMGE2)  (>= (reg-ref (car args)) (reg-ref (cadr args)))]
    [else => (^[opc] (error "[internal] Unsupported asm: "
                            (~ (vm-find-insn-info opc)'name)))]))

(define (run-builtin benv regs op args val0)
  (define-reg-ref reg-ref regs val0)
  (match op
    [(opc . _) (error "[internal] Unsupported builtin: " opc)]))

;; for debug
(define (dump-regs regs :optional (port (current-output-port)))
  (print "{{{{{{{{{")
  (hash-table-for-each regs
                       (^[k v] (format port "~30s = ~s\n" k v)))
  (print "}}}}}}}}}"))
