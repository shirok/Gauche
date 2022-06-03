;;;
;;; gauche.vm.register-machine - Register machine emulator
;;;
;;;   Copyright (c) 2021-2022  Shiro Kawai  <shiro@acm.org>
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
  (use scheme.set)
  (export run-on-register-machine))
(select-module gauche.vm.register-machine)

;; API
(define (run-on-register-machine program :optional (mod (vm-current-module)))
  (let1 benv (compile-b program mod)
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

(define *builtin-op-alist*
  (rlet1 alist `((CONS . ,cons)
                 (CAR . ,car)
                 (CDR . ,cdr)
                 (CAAR . ,caar)
                 (CADR . ,caar)
                 (CDAR . ,cdar)
                 (CDDR . ,cddr)
                 (LIST . ,list)
                 (LIST* . ,list*)
                 (LENGTH . ,length)
                 (MEMQ . ,memq)
                 (MEMV . ,memv)
                 (ASSQ . ,assq)
                 (ASSV . ,assv)
                 (EQ . ,eq?)
                 (EQV . ,eqv?)
                 (APPEND . ,append)
                 (NOT . ,not)
                 (REVERSE . ,reverse)
                 (APPLY . ,apply)
                 (TAIL-APPLY . ,apply)
                 (IS-A . ,is-a?)
                 (NULLP . ,null?)
                 (PAIRP . ,pair?)
                 (CHARP . ,char?)
                 (EOFP . ,eof-object?)
                 (STRINGP . ,string?)
                 (VECTORP . ,vector?)
                 (NUMBERP . ,number?)
                 (REALP . ,real?)
                 (IDENTIFIERP . ,identifier?)
                 (SETTER . ,setter)
                 (VEC . ,vector)
                 (LIST->VEC . ,list->vector)
                 (APP-VEC . ,(lambda xs (list->vector (concatenate xs))))
                 (VEC-LEN . ,vector-length)
                 (VEC-REF . ,vector-ref)
                 (VEC-SET . ,vector-set!)
                 (UVEC-REF . ,uvector-ref)
                 (NUMEQ2 . ,=)
                 (NUMLT2 . ,<)
                 (NUMLE2 . ,<=)
                 (NUMGT2 . ,>)
                 (NUMGE2 . ,>=)
                 (NUMADD2 . ,+)
                 (NUMSUB2 . ,-)
                 (NUMMUL2 . ,*)
                 (NUMDIV2 . ,/)
                 (NUMMOD2 . ,modulo)
                 (NUMREM2 . ,remainder)
                 (NEGATE . ,-)
                 (ASH . ,ash)
                 (LOGAND . ,logand)
                 (LOGIOR . ,logior)
                 (LOGXOR . ,logxor)
                 (CURIN . ,current-input-port)
                 (CUROUT . ,current-output-port)
                 (CURERR . ,current-error-port)
                 (UNBOX . ,unbox)
                 )
    ;; Ensure we weren't missing ops
    (unless (set=? (list->set eq-comparator (map car alist))
                   (list->set eq-comparator (with-module gauche.cgen.bbb
                                              *builtin-ops*)))
      (error "Inconsistencies between builtin ops definitions:"))))

(define (builtin-op opc)
  (assq-ref *builtin-op-alist* opc))

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
      [((op recv . regs) . insns)
       (if-let1 proc (builtin-op op)
         (let1 v (apply proc (map reg-ref regs))
           (reg-set! recv v)
           (loop insns v))
         (error "Unknown builtin op: " op))]
      [(x . _) (error "Invalid insn:" x)])))

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

;; for debug
(define (dump-regs regs :optional (port (current-output-port)))
  (print "{{{{{{{{{")
  (hash-table-for-each regs
                       (^[k v] (format port "~30s = ~s\n" k v)))
  (print "}}}}}}}}}"))
