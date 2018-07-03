;;;
;;; compile-2.scm - The compiler: Pass 2
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
;; Pass 2.  Optimization stage 1
;;

;; Walk down IForm and perform optimizations.
;; The main focus is to lift or inline closures, and eliminate
;; local frames by beta reduction.
;; This pass may modify the tree by changing IForm nodes destructively.

;; Dispatch pass2 handler.
;; Each handler is called with three arguments: the IForm, Penv, and Tail?
;; Penv is a list of $LAMBDA nodes that we're compiling.   It is used to
;; detect self-recursive local calls.  Tail? is a flag to indicate whether
;; the expression is tail position or not.
;; Each handler returns IForm.
;; *pass2-dispatch-table* is defined below, after all handlers are defined.
(define-inline (pass2/rec iform penv tail?)
  ((vector-ref *pass2-dispatch-table* (iform-tag iform))
   iform penv tail?))

;;
;; Pass 2 entry point.
;;
(define (pass2 iform) (pass2/rec iform '() #t))

;;
;; Pass 2 handlers
;;

(define (pass2/$DEFINE iform penv tail?)
  ($define-expr-set! iform (pass2/rec ($define-expr iform) penv #f))
  iform)

;; LREF optimization.
;; Check if we can replace the $lref to its initial value.
;;  - If the lvar is never set!
;;     - if its init value is $const, just replace it
;;     - if its init value is $lref, replace it iff it is not set!,
;;       then repeat.
;;
;; There's a special LREF optimization when it appears in the operator
;; position.  If it is bound to $LAMBDA, we may be able to inline the
;; lambda.  It is dealt by pass2/head-lref, which is called by pass2/$CALL.

(define (pass2/$LREF iform penv tail?) (pass2/lref-eliminate iform))

(define (pass2/lref-eliminate iform)
  (let1 lvar ($lref-lvar iform)
    (if (lvar-immutable? lvar)
      (let1 initval (lvar-initval lvar)
        (cond [(not (vector? initval)) iform]
              [($const? initval)
               (lvar-ref--! lvar)
               (vector-set! iform 0 $CONST)
               ($const-value-set! iform ($const-value initval))
               iform]
              [(and ($lref? initval)
                    (lvar-immutable? ($lref-lvar initval)))
               (when (eq? iform initval)
                 (error "circular reference appeared in letrec bindings:"
                        (lvar-name lvar)))
               (lvar-ref--! lvar)
               (lvar-ref++! ($lref-lvar initval))
               ($lref-lvar-set! iform ($lref-lvar initval))
               (pass2/lref-eliminate iform)]
              ;; Generally we can't reorder $GREF, since it may change
              ;; the semantics (the value of the variable may be altered,
              ;; or it raises an unbound error).  However, if $GREF refers to
              ;; an inlinable binding, we can assume it is bound and its
              ;; value won't be changed.  NB: Constant bindings are already
              ;; dissolved in pass1, so we don't need to consider it.
              [(and (has-tag? initval $GREF)
                    (gref-inlinable-gloc initval))
               (lvar-ref--! lvar)
               (vector-set! iform 0 $GREF)
               ($gref-id-set! iform ($gref-id initval))
               iform]
              [else iform]))
      iform)))

(define (pass2/$LSET iform penv tail?)
  ($lset-expr-set! iform (pass2/rec ($lset-expr iform) penv #f))
  iform)

(define (pass2/$GREF iform penv tail?) iform)

(define (pass2/$GSET iform penv tail?)
  ($gset-expr-set! iform (pass2/rec ($gset-expr iform) penv #f))
  iform)

(define (pass2/$CONST iform penv tail?) iform)

(define (pass2/$IT iform penv tail?) iform)

(define (pass2/$IF iform penv tail?)
  (let ([test-form (pass2/rec ($if-test iform) penv #f)]
        [then-form (pass2/rec ($if-then iform) penv tail?)]
        [else-form (pass2/rec ($if-else iform) penv tail?)])
    (or (pass2/branch-cut iform test-form then-form else-form)
        (pass2/update-if iform test-form then-form else-form))))

;; NB: pass2/branch-cut and pass2/update-if are also called in pass3/$IF.
(define (pass2/branch-cut iform test-form then-form else-form)
  (and ($const? test-form)
       (let1 val-form (if ($const-value test-form) then-form else-form)
         (if ($it? val-form) test-form val-form))))

(define (pass2/update-if iform new-test new-then new-else)
  (if (eq? new-then new-else)
    ($seq (list new-test new-then))     ;this case happens after pass3.
    (begin ($if-test-set! iform new-test)
           ($if-then-set! iform new-then)
           ($if-else-set! iform new-else)
           iform)))

;; Let optimization:
;;
;; - Unused variable elimination: if the bound lvars becomes unused by
;;   the result of $lref optimization, we eliminate it from the frame,
;;   and move its 'init' expression to the body.  if we're lucky, all
;;   the lvars introduced by this let are eliminated, and we can change
;;   this iform into a simple $seq.
;;
;;  -- Special case: If the lvars are immediately "consumed" as the
;;     arguments to a function calls in the first expression of let,
;;     and the rest of the argument of the first expression is side-effect
;;     free, we can eliminate lvars.  This case often occurs after macro
;;     expansion.
;;       (let ([p (f a)] [q (g b)]) (h p q) (foo))
;;         => (begin (h (f a) (g b)) (foo))
;;
;;     NB: In the following pattern, we can't do the conversion unless p is
;;     constant or unmodified $LREF, for (g b) might modify p.
;;     
;;       (let ([q (g b)]) (h p q) (foo))
;;
;;     NB: We don't need to consider the possibility that (g b) modifies h,
;;     since h is evaluated after all the arguments are evaluated, even
;;     in the form of (h p (g b)).
;;
;; - Closure optimization: when an lvar is bound to a $LAMBDA node, we
;;   may be able to optimize the calls to it.  It is done here since
;;   we need to run pass2 for all the call sites of the lvar to analyze
;;   its usage.
;;
;; CAVEAT: When we go through pass2 on ($let-inits iform), it may inline expand
;; lvars that appear later in the bindings, e.g.:
;;
;;   (letrec ((foo (lambda () (bar x)))
;;            (bar (lambda (a) (baz a))))
;;     ...)
;;
;; In this case, while we're at pass2[(lambda () (bar x))], pass2/$CALL
;; inlines the call of bar to make it (lambda () (let ((a x)) (baz a))).
;; The important thing is that the IForm of original (lambda (a) (baz a))
;; is directly used to inline the call, so we shouldn't rescan it.
;; pass2/$CALL marks the inlined lambda node as 'used, so that we can skip
;; it here.

(define (pass2/$LET iform penv tail?)
  ;; Run pass2 on let-inits, returns new lvars and inits
  (define (process-inits lvars inits)
    (let loop ([lvars lvars] [inits inits]
               [new-lvars '()] [new-inits '()])
      (cond [(null? lvars) (values (reverse! new-lvars) (reverse! new-inits))]
            [(let1 lv (car lvars)
               (and (= (lvar-ref-count lv) 0)
                    (lvar-immutable? lv)
                    (has-tag? (car inits) $LAMBDA)
                    (eq? ($lambda-flag (car inits)) 'used)))
             ;; This lambda node has already been inlinded, so we can skip.
             (loop (cdr lvars) (cdr inits) new-lvars new-inits)]
            [else
             (loop (cdr lvars) (cdr inits)
                   (cons (car lvars) new-lvars)
                   (cons (pass2/rec (car inits) penv #f) new-inits))])))

  (receive (lvars inits) (process-inits ($let-lvars iform) ($let-inits iform))
    (ifor-each2 (^[lv in] (lvar-initval-set! lv in)) lvars inits)
    (let1 obody (pass2/rec ($let-body iform) penv tail?)
      ;; NB: We have to run optimize-closure after pass2 of body.
      (for-each pass2/optimize-closure lvars inits)
      (pass2/shrink-let-frame iform lvars obody))))

(define (pass2/shrink-let-frame iform lvars obody)
  (pass2/intermediate-lref-removal lvars obody)
  (receive (new-lvars new-inits removed-inits)
      (pass2/remove-unused-lvars lvars ($let-type iform))
    (cond [(null? new-lvars)
           (if (null? removed-inits)
             obody
             ($seq (append! removed-inits (list obody))))]
          [else
           ($let-lvars-set! iform new-lvars)
           ($let-inits-set! iform new-inits)
           ($let-body-set! iform obody)
           (unless (null? removed-inits)
             (if (has-tag? obody $SEQ)
               ($seq-body-set! obody
                               (append! removed-inits ($seq-body obody)))
               ($let-body-set! iform
                               ($seq (append removed-inits (list obody))))))
           iform])))

;; handle the special case in the above comment
(define (pass2/intermediate-lref-removal lvars body)
  ;; returns the call node who has replacable intermediate lrefs, or #f if
  ;; we can't do this transformation.
  (define (intermediate-lrefs node lvars)
    (let loop ([args ($call-args node)] [ilrefs '()] [subcall-node #f])
      (match args
        [() (if subcall-node
              (and (null? ilrefs) (intermediate-lrefs subcall-node lvars))
              (and (not (null? ilrefs)) node))]
        [((? $const? n) . args) (loop args ilrefs subcall-node)]
        [((? $lref? n) . args)
         (let1 lv ($lref-lvar n)
           (if (memq lv lvars)
             (and (= (lvar-ref-count lv) 1)
                  (lvar-immutable? lv)
                  (loop args (cons n ilrefs) subcall-node))
             (and (lvar-immutable? lv)
                  (loop args ilrefs subcall-node))))]
        [((? $call? n) . args) (if subcall-node
                                 #f
                                 (loop args ilrefs n))]
        [_ #f])))

  (and-let* ([first-expr (if (has-tag? body $SEQ)
                           (and (not (null? ($seq-body body)))
                                (car ($seq-body body)))
                           body)]
             [ (has-tag? first-expr $CALL) ]
             [node (intermediate-lrefs first-expr lvars)])
    ($call-args-set! node
                     (imap (^[arg] (if (and ($lref? arg)
                                            (memq ($lref-lvar arg) lvars))
                                     (rlet1 v (lvar-initval ($lref-lvar arg))
                                       (lvar-ref--! ($lref-lvar arg))
                                       (lvar-initval-set! ($lref-lvar arg)
                                                          ($const-undef)))
                                     arg))
                           ($call-args node)))))

;; Scan LVARS and returns three values:
;;   - List of needed lvars
;;   - List of init expressions, corresponding to the first return value.
;;   - List of non-transparent init expressions for removed lvars---they
;;     need to be executed at the top of the body of the binding construct.
;;
;; We have to be careful optimizing letrec* - we can't reorder init
;; when it can have side effects.  However, we still have to remove lambda
;; form that is no longer used---that means the lambda form is inlined
;; elsewhere, and its body has been modified to suit the inlined environment,
;; so we can no longer compile the $lambda node safely.
(define (pass2/remove-unused-lvars lvars type)
  (let loop ([lvars lvars] [rl '()] [ri '()] [rr '()])
    (cond [(null? lvars)
           (values (reverse rl) (reverse ri) (reverse rr))]
          [(and (zero? (lvar-ref-count (car lvars)))
                (lvar-immutable? (car lvars)))
           (let1 init (lvar-initval (car lvars))
             (if (and (eq? type 'rec*)
                      (not (transparent? init)))
               (loop (cdr lvars) (cons (car lvars) rl) (cons init ri) rr)
               (loop (cdr lvars) rl ri
                     (cond [($lref? init)
                            (lvar-ref--! ($lref-lvar init))
                            rr]
                           [(transparent? init) rr]
                           [else (cons init rr)]))))]
          [else
           (loop (cdr lvars)
                 (cons (car lvars) rl)
                 (cons (lvar-initval (car lvars)) ri)
                 rr)])))

;; Closure optimization (called from pass2/$LET)
;;
;;   Determine the strategy to optimize each closure, and modify the nodes
;;   accordingly.  We can't afford time to run iterative algorithm to find
;;   optimal strategy, so we take a rather simple-minded path to optimize
;;   common cases.
;;
;;   By now, we have the information of all call sites of the statically
;;   bound closures.   Each call site is marked as either REC, TAIL-REC
;;   or LOCAL.  See explanation of pass2/$CALL below.
;;
;;   Our objective is to categorize each call site to one of the following
;;   four options:
;;
;;     LOCAL: when we can't avoid creating a closure, calls to it are marked
;;     as "local".  The call to the local closure becomes a LOCAL-ENV-CALL
;;     instruction, which is faster than generic CALL/TAIL-CALL instructions.
;;
;;     EMBED: the lambda body is inlined at the call site.  It differs from
;;     the normal inlining in a way that we don't run beta-conversion of
;;     lrefs, since the body can be entered from other 'jump' call sites.
;;
;;     JUMP: the call becomes a LOCAL-ENV-JUMP instruction, i.e. a jump
;;     to the lambda body which is generated by the 'embed' call.
;;
;;   We can inline $LAMBDA if the following conditions are met:
;;
;;     1. The reference count of LVAR is equal to the number of call
;;        sites.  This means all use of this $LAMBDA is first-order,
;;        so we know the closure won't "leak out".
;;
;;     2. It doesn't have any REC call sites, i.e. non-tail self recursive
;;        calls.  (We may be able to convert non-tail self recursive calls
;;        to jump with environment adjustment, but it would complicates
;;        stack handling a lot.)
;;
;;     3. It doesn't have any TAIL-REC calls across closure boundary.
;;
;;         (letrec ((foo (lambda (...)
;;                           ..... (foo ...)) ;; ok
;;           ...
;;
;;         (letrec ((foo (lambda (...) ....
;;                         (lambda () ... (foo ...)) ;; not ok
;;           ...
;;
;;     4. Either:
;;         - It has only one LOCAL call, or
;;         - It doesn't have TAIL-REC calls and the body of $LAMBDA is
;;           small enough to duplicate.
;;
;;   If we determine $LAMBDA to be inlined, all LOCAL calls become EMBED
;;   calls, and TAIL-RECs become JUMP.
;;
;;   Otherwise, if the first condition is met, we can lift $LAMBDA to
;;   the toplevel by...
;;
;;     a. Scan the lambda body to find free lvars.  If there's any,
;;        transform free lvars to bound lvars by adding new arguments
;;        to the $LAMBDA node, and modifies all the call sites
;;        accordingly.
;;
;;   Otherwise, all calls become LOCAL calls.
;;

(define (pass2/optimize-closure lvar lambda-node)
  (when (and (lvar-immutable? lvar)
             (> (lvar-ref-count lvar) 0)
             (has-tag? lambda-node $LAMBDA))
    (or (and (= (lvar-ref-count lvar) (length ($lambda-calls lambda-node)))
             (receive (locals recs tail-recs)
                 (pass2/classify-calls ($lambda-calls lambda-node) lambda-node)
               (and (null? recs)
                    (pair? locals)
                    (or (and (null? (cdr locals))
                             (pass2/local-call-embedder lvar lambda-node
                                                        (car locals)
                                                        tail-recs))
                        (and (null? tail-recs)
                             (< (iform-count-size-upto lambda-node
                                                       SMALL_LAMBDA_SIZE)
                                SMALL_LAMBDA_SIZE)
                             (pass2/local-call-inliner lvar lambda-node
                                                       locals))))))
        (pass2/local-call-optimizer lvar lambda-node))))

;; Classify the calls into categories.  TAIL-REC call is classified as
;; REC if the call is across the closure boundary.
(define (pass2/classify-calls call&envs lambda-node)
  (define (direct-call? env)
    (let loop ([env env])
      (cond [(null? env) #t]
            [(eq? (car env) lambda-node) #t]
            [(eq? ($lambda-flag (car env)) 'dissolved)
             (loop (cdr env))] ;; skip dissolved (inlined) lambdas
            [else #f])))
  (let loop ([call&envs call&envs]
             [local '()]
             [rec '()]
             [trec '()])
    (match call&envs
      [() (values local rec trec)]
      [((call . env) . more)
       (case ($call-flag call)
         [(tail-rec) (if (direct-call? env)
                       (loop more local rec (cons call trec))
                       (loop more local (cons call rec) trec))]
         [(rec)      (loop more local (cons call rec) trec)]
         [else       (loop more (cons call local) rec trec)])])))

;; Set up local calls to LAMBDA-NODE.  Marking $call node as 'local
;; lets pass5 to generate LOCAL-ENV-CALL instruction.
(define (pass2/local-call-optimizer lvar lambda-node)
  (let ([nreqs ($lambda-reqargs lambda-node)]
        [nopts ($lambda-optarg lambda-node)]
        [name  ($lambda-name lambda-node)]
        [calls ($lambda-calls lambda-node)])
    (dolist [call calls]
      ($call-args-set! (car call) (adjust-arglist nreqs nopts
                                                  ($call-args (car call))
                                                  name))
      ($call-flag-set! (car call) 'local))
    ;; We clear the calls list, just in case if the lambda-node is
    ;; traversed more than once.
    ($lambda-calls-set! lambda-node '())))

;; Called when the local function (lambda-node) isn't needed to be a closure
;; and can be embedded.
;; NB: this operation introduces a shared/circular structure in the IForm.
(define (pass2/local-call-embedder lvar lambda-node call rec-calls)
  (let ([nreqs ($lambda-reqargs lambda-node)]
        [nopts ($lambda-optarg lambda-node)]
        [name  ($lambda-name lambda-node)])
    ($call-args-set! call (adjust-arglist nreqs nopts ($call-args call) name))
    (lvar-ref--! lvar)
    ($call-flag-set! call 'embed)
    ($call-proc-set! call lambda-node)
    ($lambda-flag-set! lambda-node 'dissolved)
    ($lambda-body-set! lambda-node ($label ($lambda-src lambda-node) #f
                                           ($lambda-body lambda-node)))
    (unless (null? rec-calls)
      (dolist [jcall rec-calls]
        (lvar-ref--! lvar)
        ($call-args-set! jcall (adjust-arglist nreqs nopts ($call-args jcall)
                                               name))
        ($call-proc-set! jcall call)
        ($call-flag-set! jcall 'jump)))))

;; Called when the local function (lambda-node) doesn't have recursive
;; calls, can be inlined, and called from multiple places.
;; NB: Here we destructively modify $call node to change it to $seq,
;; in order to hold the $LET node.  It breaks the invariance that $seq
;; contains zero or two or more nodes---this may prevent Pass 5 from
;; doing some optimization.
(define (pass2/local-call-inliner lvar lambda-node calls)
  (define (inline-it call-node lambda-node)
    (let1 inlined (expand-inlined-procedure ($*-src lambda-node) lambda-node
                                            ($call-args call-node))
      (vector-set! call-node 0 $SEQ)
      (if (has-tag? inlined $SEQ)
        ($seq-body-set! call-node ($seq-body inlined))
        ($seq-body-set! call-node (list inlined)))))

  (lvar-ref-count-set! lvar 0)
  ($lambda-flag-set! lambda-node 'dissolved)
  (let loop ([calls calls])
    (cond [(null? (cdr calls))
           (inline-it (car calls) lambda-node)]
          [else
           (inline-it (car calls) (iform-copy lambda-node '()))
           (loop (cdr calls))])))

(define (pass2/$RECEIVE iform penv tail?)
  ($receive-expr-set! iform (pass2/rec ($receive-expr iform) penv #f))
  ($receive-body-set! iform (pass2/rec ($receive-body iform) penv tail?))
  iform)

(define (pass2/$LAMBDA iform penv tail?)
  ($lambda-body-set! iform (pass2/rec ($lambda-body iform)
                                      (cons iform penv) #t))
  iform)

(define (pass2/$LABEL iform penv tail?)
  ;; $LABEL's body should already be processed by pass2, so we don't need
  ;; to do it again.
  iform)

(define (pass2/$PROMISE iform penv tail?)
  ($promise-expr-set! iform (pass2/rec ($promise-expr iform) penv #f))
  iform)

(define (pass2/$SEQ iform penv tail?)
  (if (null? ($seq-body iform))
    iform
    (let loop ([body ($seq-body iform)]
               [r '()])
      (cond [(null? (cdr body))
             ($seq-body-set! iform
                             (reverse (cons (pass2/rec (car body) penv tail?)
                                             r)))
             iform]
            [else
             (loop (cdr body)
                   (cons (pass2/rec (car body) penv #f) r))]))))

;; Call optimization
;;   We try to inline the call whenever possible.
;;
;;   1. If proc is $LAMBDA, we turn the whole struct into $LET.
;;
;;        ($call ($lambda .. (LVar ...) Body) Arg ...)
;;         => ($let (LVar ...) (Arg ...) Body)
;;
;;   2. If proc is $LREF which is statically bound to a $LAMBDA,
;;      call pass2/head-lref to see if we can safely inline it.
;;
;;   The second case has several subcases.
;;    2a. Some $LAMBDA nodes can be directly inlined, e.g. the whole
;;        $CALL node can be turned into $LET node.  The original $LAMBDA
;;        node vanishes if all the calls to the $LAMBDA node are first-order.
;;
;;        ($call ($lref lvar0) Arg ...)
;;          where lvar0 => ($lambda .... (LVar ...) Body)
;;
;;         => ($let (LVar ...) (Arg ...) Body)
;;
;;    2b. When $LAMBDA node is recursively called, or is called multiple
;;        times, we need more information to determine how to optimize it.
;;        So at this moment we just mark the $CALL node, and pushes
;;        it and the current penv to the 'calls' slot of the $LAMBDA node.
;;        After we finish Pass 2 of the scope of lvar0, we can know how to
;;        optimize the $LAMBDA node, and those $CALL nodes are revisited
;;        and modified accordingly.
;;
;;        If the $CALL node is a non-recursive local call, the $CALL node
;;        is marked as 'local'.  If it is a recursive call, it is marked
;;        as 'rec'.

(define (pass2/$CALL iform penv tail?)
  (cond
   [($call-flag iform) iform] ;; this node has already been visited.
   [else
    ;; scan OP first to give an opportunity of variable renaming
    ($call-proc-set! iform (pass2/rec ($call-proc iform) penv #f))
    (let ([proc ($call-proc iform)]
          [args ($call-args iform)])
      (cond
       [(vm-compiler-flag-noinline-locals?)
        ($call-args-set! iform (imap (cut pass2/rec <> penv #f) args))
        iform]
       [(has-tag? proc $LAMBDA) ;; ((lambda (...) ...) arg ...)
        (pass2/rec (expand-inlined-procedure ($*-src iform) proc args)
                   penv tail?)]
       [(and ($lref? proc)
             (pass2/head-lref proc penv tail?))
        => (^[result]
             (cond
              [(vector? result)
               ;; Directly inlinable case.  NB: this only happens if the $LREF
               ;; node is the lvar's single reference, so we know the inlined
               ;; procedure is never called recursively.  Thus we can safely
               ;; traverse the inlined body without going into infinite loop.
               ;;
               ;; We directly embed the iform (result), which is a lambda expr
               ;; bound to PROC.  The lambda iform may not be scanned yet by
               ;; pass2/$LET, though.  We mark the node 'used, so that
               ;; pass2/$LET can skip processing it.
               ($lambda-flag-set! result 'used)
               (pass2/rec (expand-inlined-procedure ($*-src iform) result args)
                          penv tail?)]
              [else
               ;; We need more info to decide optimizing this node.  For now,
               ;; we mark the call node by the returned flag and push it
               ;; to the $LAMBDA node.
               (let1 lambda-node (lvar-initval ($lref-lvar proc))
                 ($call-flag-set! iform result)
                 ($lambda-calls-set! lambda-node
                                     (acons iform penv
                                            ($lambda-calls lambda-node)))
                 ($call-args-set! iform (imap (cut pass2/rec <> penv #f) args))
                 iform)]))]
       [else
        ($call-args-set! iform (imap (cut pass2/rec <> penv #f) args))
        iform]))]))

;; Check if IFORM ($LREF node) can be a target of procedure-call optimization.
;;   - If IFORM is not statically bound to $LAMBDA node,
;;     returns #f.
;;   - If the $LAMBDA node that can be directly inlined, returns
;;     the $LAMBDA node.
;;   - If the call is self-recursing, returns 'tail-rec or 'rec, depending
;;     on whether this call is tail call or not.
;;   - Otherwise, returns 'local.

(define (pass2/head-lref iform penv tail?)
  (and-let* ([lvar ($lref-lvar iform)]
             [initval (lvar-const-value lvar)]
             [ (has-tag? initval $LAMBDA) ])
    (cond
     [(pass2/self-recursing? initval penv) (if tail? 'tail-rec 'rec)]
     [(and (= (lvar-ref-count lvar) 1)
           (lvar-immutable? lvar))
      ;; we can inline this lambda directly.
      (lvar-ref--! lvar)
      initval]
     [else 'local])))

(define (pass2/self-recursing? node penv)
  (find (cut eq? node <>) penv))

(define (pass2/$ASM iform penv tail?)
  (let1 args (imap (cut pass2/rec <> penv #f) ($asm-args iform))
    (pass2/check-constant-asm iform args)))

(define (pass2/check-constant-asm iform args)
  (or (and (every $const? args)
           (case/unquote
            (car ($asm-insn iform))
            [(NOT)     (pass2/const-pred not args)]
            [(NULLP)   (pass2/const-pred null? args)]
            [(PAIRP)   (pass2/const-pred pair? args)]
            [(CHARP)   (pass2/const-pred char? args)]
            [(STRINGP) (pass2/const-pred string? args)]
            [(VECTORP) (pass2/const-pred vector? args)]
            [(NUMBERP) (pass2/const-pred number? args)]
            [(REALP)   (pass2/const-pred real? args)]
            [(CAR)     (pass2/const-cxr car args)]
            [(CDR)     (pass2/const-cxr cdr args)]
            [(CAAR)    (pass2/const-cxxr car caar args)]
            [(CADR)    (pass2/const-cxxr cdr cadr args)]
            [(CDAR)    (pass2/const-cxxr car cdar args)]
            [(CDDR)    (pass2/const-cxxr cdr cddr args)]
            [(MEMQ)    (pass2/const-memx memq args)]
            [(MEMV)    (pass2/const-memx memv args)]
            [(ASSQ)    (pass2/const-memx assq args)]
            [(ASSV)    (pass2/const-memx assv args)]
            [(VEC-REF) (pass2/const-vecref args)]
            [(VEC-LEN) (pass2/const-veclen args)]
            [(EQ)      (pass2/const-op2 eq? args)]
            [(EQV)     (pass2/const-op2 eqv? args)]
            [(NUMADD2) (pass2/const-numop2 + args)]
            [(NUMSUB2) (pass2/const-numop2 - args)]
            [(NUMMUL2) (pass2/const-numop2 * args)]
            [(NUMDIV2) (pass2/const-numop2 / args)]
            [(NEGATE)  (pass2/const-numop1 - args)]
            [else #f]))
      (and-let* ([ (pair? args) ]
                 [ (null? (cdr args)) ]
                 [ ($lref? (car args)) ]
                 [lvar ($lref-lvar (car args))]
                 [initval (lvar-const-value lvar)])
        (case/unquote
         (car ($asm-insn iform))
         [(NULLP) (and (initval-never-null? initval)
                       (begin (lvar-ref--! lvar) ($const-f)))]
         [(NOT)   (and (initval-never-false? initval)
                       (begin (lvar-ref--! lvar) ($const-f)))]
         [(PAIRP) (and (initval-always-pair? initval)
                       (begin (lvar-ref--! lvar) ($const-t)))]
         [else #f]))
      (begin ($asm-args-set! iform args) iform)))

(define (pass2/const-pred pred args)
  (if (pred ($const-value (car args))) ($const-t) ($const-f)))

(define (pass2/const-cxr proc args)
  (let1 v ($const-value (car args))
    (and (pair? v) ($const (proc v)))))

(define (pass2/const-cxxr proc0 proc args)
  (let1 v ($const-value (car args))
    (and (pair? v) (pair? (proc0 v)) ($const (proc v)))))

(define (pass2/const-memx proc args)
  (let ([item ($const-value (car args))]
        [lis  ($const-value (cadr args))])
    (and (list? lis) ($const (proc item lis)))))

(define (pass2/const-op2 proc args)
  ($const (proc ($const-value (car args)) ($const-value (cadr args)))))

(define (pass2/const-numop1 proc args)
  (let1 n ($const-value (car args))
    (and (number? n)
         (not (and (eq? proc /) (eqv? y 0))) ; don't fold zero-division case
         ($const (proc n)))))

(define (pass2/const-numop2 proc args)
  (let ([x ($const-value (car args))]
        [y ($const-value (cadr args))])
    (and (number? x) (number? y)
         (not (and (eq? proc /) (exact? x) (eqv? y 0))) ; ditto
         ($const (proc x y)))))

(define (pass2/const-vecref args)       ;args has always 2 elements
  (let ([v ($const-value (car args))]
        [i ($const-value (cadr args))])
    (and (vector? v) (exact? i) (integer? i) (< -1 i (vector-length v))
         ($const (vector-ref v i)))))

(define (pass2/const-veclen args)
  (let1 v ($const-value (car args))
    (and (vector? v) ($const (vector-length v)))))

(define (initval-never-null? val)
  (and (vector? val)
       (let1 tag (iform-tag val)
         (or (and (eqv? tag $LIST) (not (null? ($*-args val))))
             (and (eqv? tag $LIST*) (not (null? ($*-args val))))
             (memv tag `(,$LAMBDA ,$PROMISE ,$CONS ,$EQ? ,$EQV?
                                  ,$VECTOR ,$LIST->VECTOR))))))

(define (initval-never-false? val)
  (and (vector? val)
       (let1 tag (iform-tag val)
         (memv tag `(,$LAMBDA ,$PROMISE ,$CONS ,$VECTOR
                     ,$LIST->VECTOR ,$LIST)))))

(define (initval-always-pair? val)
  (and (vector? val)
       (or (has-tag? val $CONS)
           (and (has-tag? val $LIST)
                (pair? ($*-args val)))
           (and (has-tag? val $LIST*)
                (pair? ($*-args val))
                (pair? (cdr ($*-args val)))))))

(define (initval-always-procedure? val)
  (and (vector? val) (has-tag? val $LAMBDA)))

(define (pass2/onearg-inliner iform penv tail?)
  ($*-arg0-set! iform (pass2/rec ($*-arg0 iform) penv #f))
  iform)

(define pass2/$LIST->VECTOR pass2/onearg-inliner)

(define (pass2/twoarg-inliner iform penv tail?)
  ($*-arg0-set! iform (pass2/rec ($*-arg0 iform) penv #f))
  ($*-arg1-set! iform (pass2/rec ($*-arg1 iform) penv #f))
  iform)

(define pass2/$CONS   pass2/twoarg-inliner)
(define pass2/$APPEND pass2/twoarg-inliner)
(define pass2/$MEMV   pass2/twoarg-inliner)
(define pass2/$EQ?    pass2/twoarg-inliner)
(define pass2/$EQV?   pass2/twoarg-inliner)

(define (pass2/narg-inliner iform penv tail?)
  ($*-args-set! iform (imap (cut pass2/rec <> penv #f) ($*-args iform)))
  iform)

(define pass2/$LIST   pass2/narg-inliner)
(define pass2/$LIST*  pass2/narg-inliner)
(define pass2/$VECTOR pass2/narg-inliner)

;; Dispatch table.
(define *pass2-dispatch-table* (generate-dispatch-table pass2))

