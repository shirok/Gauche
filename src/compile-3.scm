;;;
;;; compile-3.scm - The compiler: Pass 3
;;;
;;;   Copyright (c) 2004-2018  Shiro Kawai  <shiro@acm.org>
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

;;===============================================================
;; Pass 3.  Optimization stage 2
;;

;; Closure optimization can introduce superfluous $LET, which can
;; be optimized further.  (In fact, pass2 and pass3 can be repeated
;; until no further optimization can be possible.  However, compilation
;; speed is also important for Gauche, so we just run this pass once.)

;; Dispatch pass3 handler.
;; Each handler is called with IForm and a list of label nodes.
;; Returs IForm.
;; *pass3-dispatch-table* is defined below, after all handlers are defined.
(define-inline (pass3/rec iform labels)
  ((vector-ref *pass3-dispatch-table* (iform-tag iform)) iform labels))

;; Pass 3 entry point
;;  This pass may prune the subtree of iform because of constant
;; folding.  It may further allow pruning of other subtrees.  So, when
;; pruning occurs, pass3 records the fact by setting label-dic-info to #t.
;; We repeat the pass then.

(define (pass3 iform show?)
  (if (vm-compiler-flag-no-post-inline?)
    iform
    (let loop ([iform iform] [count 0])
      (when show? (pass3-dump iform count))
      (let* ([label-dic (make-label-dic #f)]
             [iform. (pass3/rec (reset-lvars iform) label-dic)])
        (if (label-dic-info label-dic)
          (loop iform. (+ count 1))
          iform.)))))

(define (pass3-dump iform count)
  (format #t "~78,,,'=a\n" #"pass3 #~count ")
  (pp-iform iform))

;;
;; Pass 3 handlers
;;

(define (pass3/$DEFINE iform labels)
  ($define-expr-set! iform (pass3/rec ($define-expr iform) labels))
  iform)

(define (pass3/$LREF iform labels) (pass2/lref-eliminate iform))

(define (pass3/$LSET iform labels)
  ($lset-expr-set! iform (pass3/rec ($lset-expr iform) labels))
  iform)

(define (pass3/$GREF iform labels) iform)

(define (pass3/$GSET iform labels)
  ($gset-expr-set! iform (pass3/rec ($gset-expr iform) labels))
  iform)

(define (pass3/$CONST iform labels) iform)
(define (pass3/$IT iform labels) iform)

;; If optimization:
;;
;;  If the 'test' clause of $IF node contains another $IF that has $IT in
;;  either then or else clause, the straightforward code generation emits
;;  redundant jump/branch instructions.  We translate the tree into
;;  an acyclic directed graph:
;;
;;    ($if ($if <t0> ($it) <e0>) <then> <else>)
;;     => ($if <t0> #0=($label L0 <then>) ($if <e0> #0# <else>))
;;
;;    ($if ($if <t0> <e0> ($it)) <then> <else>)
;;    ($if ($if <t0> <e0> ($const #f)) <then> <else>)
;;     => ($if <t0> ($if <e0> <then> #0=($label L0 <else>)) #0#)
;;
;;    ($if ($if <t0> ($const #f) <e0>) <then> <else>)
;;     => ($if <t0> #0=($label L0 <else>) ($if <e0> <then> #0#))
;;        iff <else> != ($it)
;;     => ($if <t0> ($const #f) ($if <e0> <then> ($it)))
;;        iff <else> == ($it)
;;
;;  NB: If <then> or <else> clause is simple enough, we just duplicate
;;      it instead of creating $label node.  It is not only for optimization,
;;      but crucial when the clause is ($IT), since it affects the Pass5
;;      code generation stage.
;;
;;  NB: The latter two patterns may seem contrived, but it appears
;;      naturally in the 'cond' clause, e.g. (cond ((some-condition?) #f) ...)
;;      or (cond .... (else #f)).
;;
;;    ($if <t0> #0=($label ...) #0#)
;;     => ($seq <t0> ($label ...))
;;
;;  This form may appear as the result of if optimization.

(define (pass3/$IF iform labels)
  (let ([test-form (pass3/rec ($if-test iform) labels)]
        [then-form (pass3/rec ($if-then iform) labels)]
        [else-form (pass3/rec ($if-else iform) labels)])
    (or (and-let* ([r (pass2/branch-cut iform test-form then-form else-form)])
          (label-dic-info-set! labels #t) ; mark that we cut a branch
          r)
        (and
         (has-tag? test-form $IF)
         (let ([test-then ($if-then test-form)]
               [test-else ($if-else test-form)])
           (cond [($it? test-then)
                  (receive (l0 l1) (pass3/label-or-dup then-form)
                    (pass2/update-if iform ($if-test test-form)
                                     l0
                                     (pass3/rec ($if #f test-else l1 else-form)
                                                 labels)))]
                 [(or ($it? test-else)
                      (and ($const? test-else)
                           (not ($const-value test-else))))
                  (receive (l0 l1) (pass3/label-or-dup else-form)
                    (pass2/update-if iform ($if-test test-form)
                                     (pass3/rec ($if #f test-then then-form l0)
                                                 labels)
                                     l1))]
                 [(and ($const? test-then)
                       (not ($const-value test-then)))
                  (receive (l0 l1) (pass3/label-or-dup else-form)
                    (pass2/update-if iform ($if-test test-form)
                                     (if ($it? l0) ($const-f) l0)
                                     (pass3/rec ($if #f test-else then-form l1)
                                                  labels)))]
                 [else #f])))
        ;; default case
        (pass2/update-if iform test-form then-form else-form))))

(define (pass3/label-or-dup iform)
  (if (memv (iform-tag iform) `(,$LREF ,$CONST ,$IT))
    (values iform (iform-copy iform '()))
    (let1 lab ($label #f #f iform)
      (values lab lab))))

(define (pass3/$LET iform labels)
  (let ([lvars ($let-lvars iform)]
        [inits (imap (cut pass3/rec <> labels) ($let-inits iform))])
    (ifor-each2 (^[lv in] (lvar-initval-set! lv in)) lvars inits)
    (pass2/shrink-let-frame iform lvars (pass3/rec ($let-body iform) labels))))

(define (pass3/$RECEIVE iform labels)
  ($receive-expr-set! iform (pass3/rec ($receive-expr iform) labels))
  ($receive-body-set! iform (pass3/rec ($receive-body iform) labels))
  iform)

(define (pass3/$LAMBDA iform labels)
  ($lambda-body-set! iform (pass3/rec ($lambda-body iform) labels))
  iform)

(define (pass3/$LABEL iform labels)
  (unless (label-seen? labels iform)
    (label-push! labels iform)
    ($label-body-set! iform (pass3/rec ($label-body iform) labels)))
  iform)

(define (pass3/$PROMISE iform labels)
  ($promise-expr-set! iform (pass3/rec ($promise-expr iform) labels))
  iform)

;; We may have a dead code in $SEQ as the result of pass2 main.
;; We eliminate if the value of subtree is not used, and it is
;; referentially transparent.
(define (pass3/$SEQ iform labels)
  (let1 xs ($seq-body iform)
    (if (null? xs)
      iform
      (let loop ([r '()] [xs xs])
        (match xs
          [(x) (cond [(null? r) (pass3/rec x labels)]
                     [else
                      ($seq-body-set! iform
                                      (reverse! (cons (pass3/rec x labels) r)))
                      iform])]
          [(x . xs) (let1 x. (pass3/rec x labels)
                      (loop (if (transparent? x.) r (cons x. r)) xs))])))))

;; Some extra optimization on $CALL.  We need to run this here, since
;; $CALL classifications needs to be done by the surrounding $LET.
;; That is:
;;   pass2 root -> leaf  : gather call sites
;;   pass2 leaf -> root  : classify calls and dissolve some closures
;;   pass3 root -> leaf  : call optimization; *we are here*

(define (pass3/$CALL iform labels)
  ($call-args-set! iform (imap (cut pass3/rec <> labels) ($call-args iform)))
  (case ($call-flag iform)
    [(jump) iform]
    [(embed) ($call-proc-set! iform (pass3/rec ($call-proc iform) labels))
             iform]
    [else (pass3/optimize-call iform labels)]))

(define (pass3/optimize-call iform labels)
  (let ([proc (pass3/rec ($call-proc iform) labels)]
        [args ($call-args iform)])
    (cond [;; If we get ($call ($let (...) body) args ...), we transform it
           ;; to ($let (...) ($call body args...)).  This may allow further
           ;; optimization.
           (has-tag? proc $LET)
           (let loop ([node proc]
                      [body ($let-body proc)])
             (cond [(has-tag? body $LET) (loop body ($let-body body))]
                   [else ($call-proc-set! iform body)
                         ($let-body-set! node iform)
                         (pass3/$LET proc labels)]))]
          [;; As the result of above opration, we may get a direct lambda
           ;; call ($call ($lambda ...) args ...)
           (has-tag? proc $LAMBDA)
           (pass3/inline-call iform proc args labels)]
          [;; If we get ($call ($gref proc) args ...) and proc is inlinable,
           ;; we can inline the call.
           (and-let* ([ (has-tag? proc $GREF) ]
                      [p (gref-inlinable-proc proc)])
             (or (and (%procedure-inliner p)
                      (pass3/late-inline iform proc p labels))
                 (and (slot-ref p 'constant)
                      (every $const? args)
                      (pass3/precompute-constant p args))))]
          [(and-let* ([ (has-tag? proc $GREF) ]
                      [ (pair? args) ]
                      [ (null? (cdr args)) ]
                      [val (if ($lref? (car args))
                             (lvar-const-value ($lref-lvar (car args)))
                             (car args))])
             (pass3/deduce-predicate-result proc val))]
          [;; Like above, but we follow $LREFs.
           ;; NB: We tempted to inline calles if the $lref is bound to $lambda
           ;; node and it has reference count 1---in a non-redundant program,
           ;; it means the $lambda is no recursive call.  Unfortunately it
           ;; breaks when $lambda does have recursive call to itself, and
           ;; the whole $lambda node isn't referenced from anywhere else.
           ;; Such unused $lambda node would be removed later, but it
           ;; introduces loop in the graph which confuses this pass.
           (and-let* ([ ($lref? proc) ]
                      [lvar ($lref-lvar proc)]
                      [val (lvar-const-value lvar)]
                      [ (has-tag? val $GREF) ]
                      [p (gref-inlinable-proc val)]
                      [ (%procedure-inliner p) ])
             (rlet1 iform. (pass3/late-inline iform val p labels)
               (when iform. (lvar-ref--! lvar))))]
          [else ($call-proc-set! iform proc) iform])))

;; Returns GLOC if gref refers to an inlinable binding
(define (gref-inlinable-gloc gref)
  (and-let* ([gloc (id->bound-gloc ($gref-id gref))]
             [ (gloc-inlinable? gloc) ])
    gloc))

;; Get the value of GREF if it is bound and inlinable procedure
(define (gref-inlinable-proc gref)
  (and-let* ([gloc (gref-inlinable-gloc gref)]
             [val  (gloc-ref gloc)]
             [ (procedure? val) ])
    val))

;; An ad-hoc table of builtin predicates that we can deduce its value
;; from what we know about its argument at compile-time.  Even the argument
;; is not a constant, we sometimes know its type and thus we know how
;; the predicate responds.   Ideally, this information should be attached
;; to individual procedures, instead of keeping it in the compiler.  For now,
;; however, we don't know how to show our internal information to such
;; custom handlers.

(define (pass3/pred:null? val)
  (and (initval-never-null? val) ($const-f)))
(define (pass3/pred:not val)
  (and (initval-never-false? val) ($const-f)))
(define (pass3/pred:pair? val)
  (and (initval-always-pair? val) ($const-t)))
(define (pass3/pred:procedure? val)
  (and (initval-always-procedure? val) ($const-t)))
(define (pass3/pred:fallback val) #f)

(define *pass3/pred-table*
  `((,(global-id 'null?)      . ,pass3/pred:null?)
    (,(global-id 'not)        . ,pass3/pred:not)
    (,(global-id 'pair?)      . ,pass3/pred:pair?)
    (,(global-id 'procedure?) . ,pass3/pred:procedure?)))

(define (pass3/find-deducible-predicate id)
  (let loop ([tab *pass3/pred-table*])
    (cond [(null? tab) pass3/pred:fallback]
          [(global-identifier=? id (caar tab)) (cdar tab)]
          [else (loop (cdr tab))])))

(define (pass3/deduce-predicate-result gref arg)
  ((pass3/find-deducible-predicate ($gref-id gref)) arg))

(define (pass3/inline-call call-node proc args labels)
  ;; This inlining may enable further inlining by post pass again.
  (label-dic-info-set! labels #t)
  (expand-inlined-procedure ($call-src call-node) proc args))

;; TODO: This is similar to pass1/expand-inliner.  Call for refactoring.
(define (pass3/late-inline call-node gref-node proc labels)
  (let ([inliner (%procedure-inliner proc)]
        [src ($call-src call-node)])
    (match inliner
     [(? integer?)                      ; VM instruction
      (let ([nargs (length ($call-args call-node))]
            [opt?  (slot-ref proc 'optional)])
        (unless (argcount-ok? ($call-args call-node)
                              (slot-ref proc 'required) opt?)
          (errorf "wrong number of arguments: ~a requires ~a, but got ~a"
                  (variable-name ($gref-id gref-node))
                  (slot-ref proc 'required) nargs))
        ($asm src (if opt? `(,inliner ,nargs) `(,inliner))
              ($call-args call-node)))]
     [(? vector?)
      (pass3/inline-call call-node (unpack-iform inliner)
                          ($call-args call-node) labels)]
     [(? procedure?)
      (let1 r (inliner src ($call-args call-node))
        (and (not (undefined? r)) r))]
     [_ #f])))

;; PROC is inlinable, constant procedure, and args-node is all $const node.
;; So we can precompute the value and replace the $call node to a single
;; $const node.  One caveat: the application may yield an error, but if
;; we let the compiler fail, it will be confusing since even a call in
;; a dead code can be the cause.  So if we get an error, we give up this
;; optimization and let the runtime fail.
(define (pass3/precompute-constant proc arg-nodes)
  (guard (e [else #f])                  ; give up optimization
    (receive r (apply proc (imap (^[a] ($const-value a)) arg-nodes))
      (match r
        [()  ($const-undef)]
        [(r) ($const r)]
        [_   #f]))))             ;for now, don't support multivalue const

(define (pass3/$ASM iform labels)
  (let1 args (imap (cut pass3/rec <> labels) ($asm-args iform))
    (pass2/check-constant-asm iform args)))

(define (pass3/onearg-inliner iform labels)
  ($*-arg0-set! iform (pass3/rec ($*-arg0 iform) labels))
  iform)
(define pass3/$LIST->VECTOR pass3/onearg-inliner)

(define (pass3/twoarg-inliner iform labels)
  ($*-arg0-set! iform (pass3/rec ($*-arg0 iform) labels))
  ($*-arg1-set! iform (pass3/rec ($*-arg1 iform) labels))
  iform)
(define pass3/$CONS   pass3/twoarg-inliner)
(define pass3/$APPEND pass3/twoarg-inliner)
(define pass3/$MEMV   pass3/twoarg-inliner)
(define pass3/$EQ?    pass3/twoarg-inliner)
(define pass3/$EQV?   pass3/twoarg-inliner)

(define (pass3/narg-inliner iform labels)
  ($*-args-set! iform (imap (cut pass3/rec <> labels) ($*-args iform)))
  iform)
(define pass3/$LIST   pass3/narg-inliner)
(define pass3/$LIST*  pass3/narg-inliner)
(define pass3/$VECTOR pass3/narg-inliner)

;; Dispatch table.
(define *pass3-dispatch-table* (generate-dispatch-table pass3))

