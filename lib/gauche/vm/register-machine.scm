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
;; for testing.  Not for practical use---especially, performance
;; is not considered.

(define-module gauche.vm.register-machine
  (extend gauche.internal)  ; for modifying global binding
  (extend gauche.cgen.bbb)
  (use util.match)
  (use gauche.vm.insn)
  (export run-on-register-machine))
(select-module gauche.vm.register-machine)

;; API
(define (run-on-register-machine program :optional (mod (vm-current-module)))
  (let1 benv (compile-to-basic-blocks program mod)
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

(define *unique* (cons #f #f))

(define-syntax define-reg-ref
  (syntax-rules ()
    [(_ reg-ref regs val0)
     (define (reg-ref reg)
       (if (eq? reg '%VAL0)
         val0
         (let1 v (hash-table-get regs reg *unique*)
           (when (eq? v *unique*)
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
      [(('CALL _ reg . args) . _)
       (apply (reg-ref reg) (map reg-ref args))]
      [(('RET reg) . _)
       (reg-ref reg)]
      [(('DEF id flags reg) . insns)
       (%insert-binding (~ id'module) (~ id'name) (reg-ref reg) flags)
       (loop insns id)]
      [(('ASM op recv . args) . insns)
       (loop insns (run-asm benv regs op recv args val0))]
      [(('BUILTIN op recv . args) . insns)
       (loop insns (run-builtin benv regs op recv args val0))]
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

  ;; Here's the ugly part. We need to reify 'our' closure info into
  ;; Gauche's runtime closure.  However, simply wrapping our closure
  ;; with generic lambda won't do:
  ;;   (lambda args             ;doesn't work
  ;;     (let1 regs (build-input args)
  ;;       (run-bb benv regs (~ benv'entry))))
  ;; That's because if this closure appears within case-lambda, it needs
  ;; actual argument count info in Gauche's runtime layer.  Wrapping with
  ;; generic lambda will lose that info.
  ;; Eval is our only option.
  (define (run-it . args)
    (let1 regs (bind-input args)
      ;(dump-regs regs)
      (run-bb benv regs (~ benv'entry))))
  (define (generate-lambda-formals reg-names)
    (if (length>? reg-names (~ benv'input-reqargs))
      (let1 r (reverse reg-names)
        (fold cons (car r) (cdr r)))
      reg-names))
  (define (generate-apply reg-names)
    (if (length>? reg-names (~ benv'input-reqargs))
      `(apply ,run-it ,@reg-names)
      `(,run-it ,@reg-names)))
  (let1 reg-names (map (cut ~ <>'name) (~ benv'input-regs))
    (eval `(lambda ,(generate-lambda-formals reg-names)
             ,(generate-apply reg-names))
          (current-module))))

(define (run-asm benv regs op recv args val0)
  (define-reg-ref reg-ref regs val0)
  (define val
    (case (~ (vm-find-insn-info (car op))'name)
      [(NULLP)   (null? (reg-ref (car args)))]
      [(PAIRP)   (pair? (reg-ref (car args)))]
      [(CAR)     (car (reg-ref (car args)))]
      [(CDR)     (cdr (reg-ref (car args)))]
      [(CAAR)    (caar (reg-ref (car args)))]
      [(CADR)    (cadr (reg-ref (car args)))]
      [(CDAR)    (cdar (reg-ref (car args)))]
      [(CDDR)    (cddr (reg-ref (car args)))]
      [(CONS)    (cons (reg-ref (car args)) (reg-ref (cadr args)))]
      [(LIST)    (map reg-ref args)]
      [(LIST*)   (apply list* (map reg-ref args))]
      [(LENGTH)  (length (reg-ref (car args)))]
      [(APPEND)  (append (map reg-ref args))]
      [(REVERSE) (apply reverse (map reg-ref args))]
      [(MEMQ)    (memq (reg-ref (car args)) (reg-ref (cadr args)))]
      [(MEMV)    (memv (reg-ref (car args)) (reg-ref (cadr args)))]
      [(ASSQ)    (assq (reg-ref (car args)) (reg-ref (cadr args)))]
      [(ASSV)    (assv (reg-ref (car args)) (reg-ref (cadr args)))]
      [(NUMADD2) (+ (reg-ref (car args)) (reg-ref (cadr args)))]
      [(NUMSUB2) (- (reg-ref (car args)) (reg-ref (cadr args)))]
      [(NUMSUB2) (- (reg-ref (car args)) (reg-ref (cadr args)))]
      [(NUMMUL2) (* (reg-ref (car args)) (reg-ref (cadr args)))]
      [(NUMDIV2) (/ (reg-ref (car args)) (reg-ref (cadr args)))]
      [(NUMMODI) (modulo (reg-ref (car args)) (cadr op))]
      [(NUMREMI) (remainder (reg-ref (car args)) (cadr op))]
      [(NUMASHI) (ash (reg-ref (car args)) (cadr op))]
      [(NUMEQ2)  (=  (reg-ref (car args)) (reg-ref (cadr args)))]
      [(NUMLT2)  (<  (reg-ref (car args)) (reg-ref (cadr args)))]
      [(NUMLE2)  (<= (reg-ref (car args)) (reg-ref (cadr args)))]
      [(NUMGT2)  (>  (reg-ref (car args)) (reg-ref (cadr args)))]
      [(NUMGE2)  (>= (reg-ref (car args)) (reg-ref (cadr args)))]
      [(NEGATE)  (- (reg-ref (car args)))]
      [(LOGAND)  (logand (reg-ref (car args)) (reg-ref (cadr args)))]
      [(LOGIOR)  (logior (reg-ref (car args)) (reg-ref (cadr args)))]
      [(LOGXOR)  (logxor (reg-ref (car args)) (reg-ref (cadr args)))]
      [(NOT)     (not (reg-ref (car args)))]
      [(CURIN)   (current-input-port)]
      [(CUROUT)  (current-output-port)]
      [(CURERR)  (current-error-port)]
      [(TAIL-APPLY) (apply (reg-ref (car args)) (map reg-ref (cdr args)))]
      [else => (^[opc] (error "[internal] Unsupported asm: "
                              (~ (vm-find-insn-info opc)'name)))]))
  (cond [(is-a? recv <reg>)
         (if (reg-boxed? recv)
           (set-box! (hash-table-get regs recv) val)
           (hash-table-put! regs recv val))
         (undefined)]
        [(eq? recv '%VAL0) val]
        [else (undefined)]))

(define (run-builtin benv regs op recv args val0)
  (define-reg-ref reg-ref regs val0)
  (define val
    (match op
      ['APPEND (apply append (map reg-ref args))]
      ['LIST   (apply list (map reg-ref args))]
      ['LIST*  (apply list* (map reg-ref args))]
      ['CONS   (cons (reg-ref (car args)) (reg-ref (cadr args)))]
      [opc     (error "[internal] Unsupported builtin: " opc)]))
  (cond [(is-a? recv <reg>)
         (if (reg-boxed? recv)
           (set-box! (hash-table-get regs recv) val)
           (hash-table-put! regs recv val))
         (undefined)]
        [(eq? recv '%VAL0) val]
        [else (undefined)]))

;; for debug
(define (dump-regs regs :optional (port (current-output-port)))
  (print "{{{{{{{{{")
  (hash-table-for-each regs
                       (^[k v] (format port "~30s = ~s\n" k v)))
  (print "}}}}}}}}}"))
