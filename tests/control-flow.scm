;;
;; Fine-grained test for VM control-flow code paths. Each test exercises
;; one specific path.
;;

;; The tests of this file can also be run with srfi-226 reference implementation
;; over ChezScheme, by control-flow-srfi-226.sh script.
;; Because this is also run by Chez, be careful not to include
;; Gauche specific lexical syntax in the tests.

(use gauche.test)
(use gauche.partcont)

(test-start "control-flow")

;; (loop-guarded LIMIT BODY) calls BODY with one argument, TICK!.
;; TICK! is a procedure that, when invoked, increments an internal
;; counter and records its tag in a trail.  Once the counter exceeds
;; LIMIT, TICK! raises an error tagged with the trail seen so far,
;; instead of returning.  TICK! returns its first argument (or #f),
;; so it composes inside expressions.
;;
;; Use TICK! at points where, if the wrong control-flow path is
;; taken, the test would loop or wander.  LIMIT should be one or two
;; above the expected number of ticks for a correct run.

(define (loop-guarded limit body)
  (let ([count 0]
        [trail '()])
    (define (tick! . tag)
      (set! count (+ count 1))
      (set! trail (cons tag trail))
      (when (> count limit)
        (errorf "loop-guard tripped (limit=~s, count=~s, trail=~s)"
                limit count (reverse trail)))
      (if (pair? tag) (car tag) #f))
    (body tick!)))

;; Convenience: make a trail recorder; returns two values, (record!
;; TAG) and (replay) that produces the recorded list in order.
(define (make-trail)
  (let ([log '()])
    (values (lambda items (set! log (cons items log)) (if (pair? items) (car items) #f))
            (lambda () (reverse log)))))

;;=================================================================
;; Section 1.  call/cc -- pure capture, no re-invocation
;;
;; Path: Scm_VMCallCC capture + immediate (or no) invocation.  No
;; dynamic-wind, no prompts, no exceptions.

(test-section "call/cc: no re-invocation")

(test* "call/cc returns thunk value when k is not used" 5
       (call/cc (lambda (k) 5)))

(test* "call/cc invoked inside its own thunk" 42
       (call/cc (lambda (k) (k 42) 'unreached)))

(test* "call/cc with zero values" '()
       (call-with-values (lambda () (call/cc (lambda (k) (k))))
         list))

(test* "call/cc with three values" '(a b c)
       (call-with-values (lambda () (call/cc (lambda (k) (k 'a 'b 'c))))
         list))

(test* "call/cc tail-calls k" 99
       (call/cc (lambda (k) (k 99))))

;;=================================================================
;; Section 2.  call/cc -- single re-invocation after return
;;
;; Path: throw_continuation / throw_cont_body, same-cstack, no
;; dynamic handlers.

(test-section "call/cc: single re-invocation")

(test* "call/cc re-invoke once, branch on counter"
       'second
       (loop-guarded 5
         (lambda (tick!)
           (let* ([k #f]
                  [r (call/cc (lambda (c) (set! k c) 'first))])
             (tick! r)
             (if (eq? r 'first) (k 'second) r)))))

(test* "call/cc re-invoke once, value plumbed through outer expr"
       11
       (loop-guarded 5
         (lambda (tick!)
           (let ([k #f])
             (let1 v (+ 1 (call/cc (lambda (c) (set! k c) 1)))
               (tick! v)
               (if (= v 2) (k 10) v))))))

(test* "call/cc captured outside, invoked later"
       'invoked
       (loop-guarded 5
         (lambda (tick!)
           (let* ([k #f]
                  [r (call/cc (lambda (c) (set! k c) 'first-return))])
             (tick! r)
             (if (eq? r 'first-return)
               (k 'invoked)
               r)))))

;;=================================================================
;; Section 3.  call/cc -- multiple re-invocations
;;
;; Path: re-entrancy through throw_cont_body; each invocation must
;; recompute its own differentials cleanly.  Bounded loop.

(test-section "call/cc: bounded loop via re-invocation")

(test* "call/cc loop, counts down" 0
       (loop-guarded 20
         (lambda (tick!)
           (let* ([k #f]
                  [n (call/cc (lambda (c) (set! k c) 5))])
             (tick! n)
             (if (zero? n) n (k (- n 1)))))))

(test* "call/cc loop accumulates into a list" '(0 1 2 3)
       (loop-guarded 20
         (lambda (tick!)
           (let* ([acc '()]
                  [k #f]
                  [n (call/cc (lambda (c) (set! k c) 3))])
             (tick! n)
             (set! acc (cons n acc))
             (if (zero? n) acc (k (- n 1)))))))

;;=================================================================
;; Section 4.  dynamic-wind -- alone
;;
;; Path: Scm_VMDynamicWind / inline insns; push_dynamic_handlers /
;; pop; before/after on normal return.

(test-section "dynamic-wind: alone")

(test* "dynamic-wind: before, body, after"
       '(b body a)
       (let1 log '()
         (dynamic-wind
           (lambda () (set! log (cons 'b log)))
           (lambda () (set! log (cons 'body log)) 'value)
           (lambda () (set! log (cons 'a log))))
         (reverse log)))

(test* "dynamic-wind: body value is returned"
       'value
       ;; NB: binding the thunks to variables sidesteps an existing
       ;; optimizer issue where literal-lambda dynamic-wind loses the
       ;; body's return value in tail position.  Not the focus here.
       (let ([b (lambda () #f)]
             [m (lambda () 'value)]
             [a (lambda () #f)])
         (dynamic-wind b m a)))

(test* "dynamic-wind: multiple values"
       '(1 2 3)
       (call-with-values
         (lambda ()
           (dynamic-wind (lambda () #f)
                         (lambda () (values 1 2 3))
                         (lambda () #f)))
         list))

(test* "dynamic-wind: nested"
       '(b1 b2 body a2 a1)
       (let1 log '()
         (dynamic-wind
           (lambda () (set! log (cons 'b1 log)))
           (lambda ()
             (dynamic-wind
               (lambda () (set! log (cons 'b2 log)))
               (lambda () (set! log (cons 'body log)))
               (lambda () (set! log (cons 'a2 log)))))
           (lambda () (set! log (cons 'a1 log))))
         (reverse log)))

;;=================================================================
;; Section 5.  call/cc x dynamic-wind
;;
;; Path: throw_cont_calculate_handlers tree-differential.  Each test
;; pins down a single differential shape.

(test-section "call/cc x dynamic-wind")

(test* "re-enter dynamic-wind via call/cc captured inside body"
       '(b body a b body a)
       (loop-guarded 10
         (lambda (tick!)
           (let ([log '()]
                 [k #f]
                 [done #f])
             (dynamic-wind
               (lambda () (set! log (cons 'b log)))
               (lambda ()
                 (call/cc (lambda (c) (set! k c)))
                 (set! log (cons 'body log))
                 (tick! 'body))
               (lambda () (set! log (cons 'a log))))
             (unless done (set! done #t) (k))
             (reverse log)))))

(test* "escape out of dynamic-wind via call/cc captured outside"
       '(b body a)
       (loop-guarded 10
         (lambda (tick!)
           (let ([log '()])
             (call/cc
              (lambda (k)
                (dynamic-wind
                  (lambda () (set! log (cons 'b log)))
                  (lambda ()
                    (set! log (cons 'body log))
                    (tick! 'body)
                    (k #f))
                  (lambda () (set! log (cons 'a log))))))
             (reverse log)))))

(test* "re-enter nested dynamic-wind: both before/after replay (common root algorithm)"
       ;; call/cc captures from inside the INNER body.  On re-invoke
       ;; we are at top level so the common root is "above outer",
       ;; meaning both winds must replay: A,a fire on exit, B,b on
       ;; re-entry.  This pins down throw_cont_calculate_handlers.
       '(B b body a A B b body a A)
       (loop-guarded 10
         (lambda (tick!)
           (let ([log '()]
                 [k #f]
                 [done #f])
             (dynamic-wind
               (lambda () (set! log (cons 'B log)))
               (lambda ()
                 (dynamic-wind
                   (lambda () (set! log (cons 'b log)))
                   (lambda ()
                     (call/cc (lambda (c) (set! k c)))
                     (set! log (cons 'body log))
                     (tick! 'body))
                   (lambda () (set! log (cons 'a log)))))
               (lambda () (set! log (cons 'A log))))
             (unless done (set! done #t) (k))
             (reverse log)))))

;;=================================================================
;; Section 6.  Continuation prompts (modern, same-cstack)
;;
;; Path: push_prompt_cont / find_prompt_frame /
;; Scm_VMAbortCurrentContinuation / vm_abort_body.

(test-section "continuation prompts")

(test* "call-with-continuation-prompt: no abort" 7
       (call-with-continuation-prompt (lambda () 7)))

(test* "call-with-continuation-prompt: no abort, custom tag" 'ok
       (let1 t (make-continuation-prompt-tag)
         (call-with-continuation-prompt (lambda () 'ok) t)))

(test* "abort-current-continuation: default tag, default handler"
       'done
       (call-with-continuation-prompt
        (lambda ()
          (abort-current-continuation
           (default-continuation-prompt-tag)
           (lambda () 'done))
          'unreached)))

(test* "abort-current-continuation: custom tag and handler"
       '(aborted-with 1 2 3)
       (let1 t (make-continuation-prompt-tag)
         (call-with-continuation-prompt
          (lambda ()
            (abort-current-continuation t 1 2 3)
            'unreached)
          t
          (lambda args (cons 'aborted-with args)))))

(test* "abort-current-continuation: passes multiple values to handler"
       '(1 2 3)
       (let1 t (make-continuation-prompt-tag)
         (call-with-continuation-prompt
          (lambda () (abort-current-continuation t 1 2 3))
          t
          list)))

(test* "abort with wrong tag raises &continuation"
       #t
       (let ([t1 (make-continuation-prompt-tag)]
             [t2 (make-continuation-prompt-tag)])
         (guard (e [(continuation-violation? e) #t])
           (call-with-continuation-prompt
            (lambda () (abort-current-continuation t2 'x))
            t1))))

(test* "nested prompts: abort to outer"
       'outer
       (let ([t1 (make-continuation-prompt-tag 'outer)]
             [t2 (make-continuation-prompt-tag 'inner)])
         (call-with-continuation-prompt
          (lambda ()
            (call-with-continuation-prompt
             (lambda ()
               (abort-current-continuation t1 (lambda () 'outer)))
             t2))
          t1)))

(test* "nested prompts: abort to inner"
       'inner
       (let ([t1 (make-continuation-prompt-tag 'outer)]
             [t2 (make-continuation-prompt-tag 'inner)])
         (call-with-continuation-prompt
          (lambda ()
            (call-with-continuation-prompt
             (lambda ()
               (abort-current-continuation t2 (lambda () 'inner)))
             t2))
          t1)))

;;=================================================================
;; Section 6b.  abort-current-continuation across a resume boundary
;;
;; Invoking a composable continuation installs a "resume boundary" on the
;; meta-cont chain; the captured frames keep their capture-time prev.  An
;; abort performed *inside* the invoked continuation must therefore find its
;; target prompt via the meta-cont chain (find_prompt_frame /
;; find_meta_cont_by_tag), not by walking the physical cont chain (which
;; would follow the stale prev across the boundary and miss the prompt).
;; Scm_VMAbortCurrentContinuation also resets currentMetaCont to the target,
;; discarding the boundaries/inner prompts being abandoned.

(test* "abort across a resume boundary to an enclosing prompt"
       '(handler-got aborted-value)
       (let ([t (make-continuation-prompt-tag 'across)]
             [k #f])
         ;; k = the composable continuation that aborts to t.
         (reset
          (shift c (set! k c))
          (abort-current-continuation t 'aborted-value))
         ;; Invoke k under a prompt for t; the abort target is across the
         ;; resume boundary that invoking k installs.
         (call-with-continuation-prompt
          (lambda () (k 'ignored))
          t
          (lambda (v) (list 'handler-got v)))))

(test* "abort across a resume boundary runs dynamic-wind `after`"
       '((b a) (got v))
       (let ([t (make-continuation-prompt-tag)]
             [k #f]
             [log '()])
         (reset
          (shift c (set! k c))
          (dynamic-wind
            (lambda () (set! log (cons 'b log)))
            (lambda () (abort-current-continuation t 'v))
            (lambda () (set! log (cons 'a log)))))
         (let ([r (call-with-continuation-prompt
                   (lambda () (k 'ignored))
                   t
                   (lambda (v) (list 'got v)))])
           (list (reverse log) r))))

(test* "abort across a resume boundary and an inner prompt"
       '(outer deep)
       (let ([t (make-continuation-prompt-tag)]
             [k #f])
         (reset
          (shift c (set! k c))
          (abort-current-continuation t 'deep))
         (call-with-continuation-prompt
          (lambda ()
            (+ 1 (call-with-continuation-prompt
                   (lambda () (k 'ignored))
                   (make-continuation-prompt-tag))))
          t
          (lambda (v) (list 'outer v)))))

;;=================================================================
;; Section 7.  Prompt x dynamic-wind
;;
;; Path: vm_abort_cc loop runs `after` handlers down to the prompt.

(test-section "prompt x dynamic-wind")

(test* "abort runs `after` handlers down to prompt"
       '(b body a result)
       (let ([log '()]
             [t (make-continuation-prompt-tag)])
         (call-with-continuation-prompt
          (lambda ()
            (dynamic-wind
              (lambda () (set! log (cons 'b log)))
              (lambda ()
                (set! log (cons 'body log))
                (abort-current-continuation t 'result))
              (lambda () (set! log (cons 'a log)))))
          t
          (lambda (r) (set! log (cons r log))))
         (reverse log)))

(test* "abort runs `after` handlers of nested winds in order"
       '(b1 b2 body a2 a1 result)
       (let ([log '()]
             [t (make-continuation-prompt-tag)])
         (call-with-continuation-prompt
          (lambda ()
            (dynamic-wind
              (lambda () (set! log (cons 'b1 log)))
              (lambda ()
                (dynamic-wind
                  (lambda () (set! log (cons 'b2 log)))
                  (lambda ()
                    (set! log (cons 'body log))
                    (abort-current-continuation t 'result))
                  (lambda () (set! log (cons 'a2 log)))))
              (lambda () (set! log (cons 'a1 log)))))
          t
          (lambda (r) (set! log (cons r log))))
         (reverse log)))

(test* "abort to outer prompt: afters down to outer fire"
       '(b body a result)
       (let ([log '()]
             [t1 (make-continuation-prompt-tag 'outer)]
             [t2 (make-continuation-prompt-tag 'inner)])
         (call-with-continuation-prompt
          (lambda ()
            (call-with-continuation-prompt
             (lambda ()
               (dynamic-wind
                 (lambda () (set! log (cons 'b log)))
                 (lambda ()
                   (set! log (cons 'body log))
                   (abort-current-continuation t1 'result))
                 (lambda () (set! log (cons 'a log)))))
             t2))
          t1
          (lambda (r) (set! log (cons r log))))
         (reverse log)))

;;=================================================================
;; Section 8.  reset / shift -- basics
;;
;; Path: Scm_VMReset (C boundary today) and Scm_VMCallPC capture.

(test-section "reset/shift: basics")

(test* "reset returns body value" 5
       (reset 5))

(test* "reset with multiple expressions" 3
       (reset 1 2 3))

(test* "reset surrounding expression sees the reset value" 6
       (+ 1 (reset (+ 2 3))))

(test* "shift without using k: body of shift is reset's value" 99
       (reset (+ 2 (shift k 99))))

(test* "shift calling k once: composes"
       10
       (loop-guarded 5
         (lambda (tick!)
           (+ 1 (reset (+ 2 (shift k (tick! 'shift) (+ 3 (k 4)))))))))

(test* "shift calling k twice: composable"
       14
       (loop-guarded 10
         (lambda (tick!)
           (+ 1 (reset (+ 2 (shift k
                                   (tick! 'shift)
                                   (+ 3 (k 5) (k 1)))))))))

(test* "shift k not called -> body of reset is body of shift" '(3)
       (reset (cons 2 (shift k (list 3)))))

;;=================================================================
;; Section 9.  reset/shift x dynamic-wind
;;
;; Path: Scm_VMCallPC's partial-handler differential at capture, plus
;; the splice-at-invocation (or composition) on apply.

(test-section "reset/shift x dynamic-wind")

(test* "shift inside dynamic-wind: capture fires after"
       '(b body a)
       ;; native semantics today: when shift cuts out, the captured
       ;; segment's `after` fires immediately (capture-time unwind).
       (loop-guarded 5
         (lambda (tick!)
           (let ([log '()])
             (reset
              (dynamic-wind
                (lambda () (set! log (cons 'b log)))
                (lambda ()
                  (set! log (cons 'body log))
                  (tick! 'body)
                  (shift k 'cut))
                (lambda () (set! log (cons 'a log)))))
             (reverse log)))))

(test* "applying the partial cont re-runs `before`, captured tail, `after`"
       ;; Captured k = "continuation from just after shift, through
       ;; the rest of the wind body, through `after`, out of reset."
       ;; The body BEFORE shift (and tick!) already happened at
       ;; capture time and is NOT in k.  Native semantics today.
       '(b body a b a)
       (loop-guarded 10
         (lambda (tick!)
           (let ([log '()]
                 [kk #f])
             (reset
              (dynamic-wind
                (lambda () (set! log (cons 'b log)))
                (lambda ()
                  (set! log (cons 'body log))
                  (tick! 'in)
                  (shift k (set! kk k)))
                (lambda () (set! log (cons 'a log)))))
             (kk)
             (reverse log)))))

(test* "shift inside reset that is itself inside dynamic-wind"
       ;; outer wind runs once over the whole expression; inner shift
       ;; captures only up to the reset.
       '(B body A)
       (loop-guarded 5
         (lambda (tick!)
           (let ([log '()])
             (dynamic-wind
               (lambda () (set! log (cons 'B log)))
               (lambda ()
                 (set! log (cons 'body log))
                 (tick! 'body)
                 (reset (shift k 'cut)))
               (lambda () (set! log (cons 'A log))))
             (reverse log)))))

;;=================================================================
;; Section 10.  call/cc inside reset/shift  (DANGER ZONE)
;;
;; These are the cases most prone to infinite loops when partial-cont
;; semantics drift.  Each test pins down a single behavior.  Bounds
;; are tight so a regression bails out fast.

(test-section "call/cc x reset/shift")

(test* "call/cc captured inside reset, invoked inside same reset (bounded)"
       ;; A call/cc continuation captured inside the body of a reset
       ;; is re-invoked from inside the same reset to drive a small
       ;; loop.  Exercises throw_cont_body where the prompt frame is
       ;; part of the captured chain.
       'done
       (loop-guarded 6
         (lambda (tick!)
           (reset
            (let ([n 0] [k #f])
              (call/cc (lambda (c) (set! k c)))
              (tick! n)
              (if (< n 3)
                (begin (set! n (+ n 1)) (k))
                'done))))))

(test* "call/cc captured before shift, invoked from outside reset"
       "[r01][s01][s01]"
       ;; tests/partcont.scm 'reset/shift + call/cc 3' specialization
       (loop-guarded 8
         (lambda (tick!)
           (with-output-to-string
             (lambda ()
               (define k1 #f)
               (define k2 #f)
               (reset
                (display "[r01]")
                (call/cc (lambda (k) (set! k1 k)
                                     (shift k (set! k2 k))))
                (tick! 's01)
                (display "[s01]"))
               (k2)
               (reset (k1)))))))

(test* "call/cc captured after shift in re-invoked partial cont"
       ;; tests/partcont.scm 'reset/shift + call/cc 2' specialization;
       ;; native today prints "[r01][s01][s02][s02]".
       "[r01][s01][s02][s02]"
       (loop-guarded 10
         (lambda (tick!)
           (with-output-to-string
             (lambda ()
               (define k1 #f)
               (define k2 #f)
               (reset
                (display "[r01]")
                (shift k (set! k1 k))
                (display "[s01]")
                (call/cc (lambda (k) (set! k2 k)))
                (tick! 's02)
                (display "[s02]"))
               (k1)
               (reset (reset (k2))))))))

;;=================================================================
;; Section 11.  Multi-shift composition

(test-section "multi-shift")

(test* "nested shifts in same reset: only outermost k is meaningful"
       1000
       (loop-guarded 10
         (lambda (tick!)
           (define k1 #f)
           (reset
            (shift k (set! k1 k)
                   (shift k (shift k 'inner-cut)))
            (tick! 'after-shifts)
            1000)
           (k1))))

;;=================================================================
;; Section 12.  Exception handling -- with-error-handler (Gauche legacy)
;;
;; Path: with_error_handler / new_ep / make_escape_handler;
;; handle_escape (rewindBefore=FALSE: call handler, then rewind).

(test-section "with-error-handler")

(test* "with-error-handler: no error -> body value" 'body
       (with-error-handler (lambda (e) 'handler) (lambda () 'body)))

(test* "with-error-handler: error -> handler value" 'handler
       (with-error-handler (lambda (e) 'handler)
         (lambda () (error "boom"))))

(test* "with-error-handler: handler sees the condition"
       "boom"
       (with-error-handler (lambda (e) (condition-ref e 'message))
         (lambda () (error "boom"))))

(test* "with-error-handler: ordering with dynamic-wind body"
       ;; legacy rewindBefore=FALSE: handler-clause runs BEFORE
       ;; rewinding the dynamic-wind chain.  So `after` fires AFTER
       ;; the handler -- but the handler sees the post-body context.
       '(b body handler a)
       (let1 log '()
         (with-error-handler
             (lambda (e) (set! log (cons 'handler log)))
           (lambda ()
             (dynamic-wind
               (lambda () (set! log (cons 'b log)))
               (lambda () (set! log (cons 'body log))
                          (error "x"))
               (lambda () (set! log (cons 'a log))))))
         (reverse log)))

(test* "with-error-handler: handler can re-raise"
       'outer
       (with-error-handler (lambda (e) 'outer)
         (lambda ()
           (with-error-handler (lambda (e) (raise e))
             (lambda () (error "inner"))))))

;;=================================================================
;; Section 13.  Exception handling -- guard (rewindBefore=TRUE)
;;
;; Path: same handle_escape but rewindBefore=TRUE: rewind FIRST,
;; then run the handler-clause in the guard form's dynamic env.

(test-section "guard")

(test* "guard: no error -> body value" 'body
       (guard (e [else 'handler])
         'body))

(test* "guard: matching clause" 'matched
       (guard (e [(symbol? e) 'matched])
         (raise 'sym)))

(test* "guard: no matching clause -> re-raise"
       'caught
       (with-error-handler (lambda (e) 'caught)
         (lambda ()
           (guard (e [(string? e) 'unreached])
             (raise 'sym)))))

(test* "guard: clause sees the condition"
       "x"
       (guard (e [else (condition-ref e 'message)])
         (error "x")))

(test* "guard: ordering with dynamic-wind body"
       ;; rewindBefore=TRUE: `after` runs BEFORE the handler clause,
       ;; so the clause runs in the guard form's own dynamic env.
       '(b body a handler)
       (let1 log '()
         (guard (e [else (set! log (cons 'handler log))])
           (dynamic-wind
             (lambda () (set! log (cons 'b log)))
             (lambda () (set! log (cons 'body log))
                        (error "x"))
             (lambda () (set! log (cons 'a log)))))
         (reverse log)))

;;=================================================================
;; Section 14.  Exception handling -- with-exception-handler / raise
;;
;; Path: Scm_VMThrowException + denv handler chain.

(test-section "with-exception-handler / raise")

(test* "with-exception-handler: handler runs on raise"
       ;; In Gauche, even for non-continuable `raise`, if the handler
       ;; just returns, its return value becomes the value of the
       ;; (raise ...) expression -- no further error is signaled.
       ;; (R7RS/SRFI-34 would say this is an error; Gauche is lenient.)
       ;; This test pins down the actual behavior so we notice if it
       ;; changes during the SRFI-226 work.
       '(handler-saw)
       (let1 log '()
         (with-exception-handler
             (lambda (e) (set! log (cons 'handler-saw log)) 'ignored)
           (lambda () (raise 'sym)))
         (reverse log)))

(test* "raise-continuable: handler returns to raise site"
       'returned
       (with-exception-handler
           (lambda (e) 'returned)
         (lambda () (raise-continuable 'sym))))

(test* "with-exception-handler: previous handler restored on return"
       'inner-result
       (with-exception-handler
           (lambda (e) 'outer)
         (lambda ()
           (let1 r (with-exception-handler
                       (lambda (e) 'inner-result)
                     (lambda () (raise-continuable 'sym)))
             r))))

;;=================================================================
;; Section 15.  Exception x continuation (re-entry / escape)
;;
;; Path: handle_escape + EP's tracked cont; ghost cont via cstack.

(test-section "exception x continuation")

(test* "with-error-handler: continuation captured inside body, invoked from handler"
       'reached
       (loop-guarded 8
         (lambda (tick!)
           (let1 saved #f
             (with-error-handler
                 (lambda (e) (saved 'reached))
               (lambda ()
                 (call/cc (lambda (k)
                            (set! saved k)
                            (tick! 'before-error)
                            (error "x")
                            'unreached))))))))

(test* "guard: captured cont inside body re-invoked from handler clause"
       'cont-arg
       (loop-guarded 8
         (lambda (tick!)
           (let ([saved #f])
             (guard (e [else (saved 'cont-arg)])
               (call/cc (lambda (k)
                          (set! saved k)
                          (tick! 'before-error)
                          (error "x"))))))))

(test* "guard re-raises into outer with-error-handler"
       '(inner-saw outer-caught)
       (let1 log '()
         (with-error-handler
             (lambda (e) (set! log (cons 'outer-caught log)))
           (lambda ()
             (guard (e [#f 'unreached])
               (set! log (cons 'inner-saw log))
               (error "x"))))
         (reverse log)))

;;=================================================================
;; Section 16.  Exception x reset/shift
;;
;; Path: error raised inside a reset body; handler outside reset.
;; Watch for partial-cont state leaking across the C boundary.

(test-section "exception x reset/shift")

(test* "error inside reset body, handler outside reset"
       'caught
       (with-error-handler (lambda (e) 'caught)
         (lambda () (reset (error "x")))))

(test* "guard inside reset body catches error"
       ;; The guard clause runs first (rewindBefore=TRUE rewinds, then
       ;; runs the clause), pushing 'caught; then reset yields the
       ;; clause value 'reset-value which is consed onto log.
       '(caught reset-value)
       (let1 log '()
         (set! log (cons (reset
                          (guard (e [else (set! log (cons 'caught log))
                                          'reset-value])
                            (error "x")))
                         log))
         (reverse log)))

(test* "shift inside guard body"
       'value
       ;; shift in body of guard: capture, run shift's body (which
       ;; just returns 'value).  Make sure the guard frame doesn't
       ;; cause trouble.
       (reset
        (guard (e [else 'unreached])
          (shift k 'value))))

;;=================================================================
;; Section 17.  Continuation marks
;;
;; Path: denv handler chain; current-continuation-marks walks denv +
;; cont; with-continuation-mark adds an entry.

(test-section "continuation marks")

(test* "with-continuation-mark + current-continuation-marks" '(1)
       (let1 key (vector 'k)
         (with-continuation-mark key 1
           (continuation-mark-set->list (current-continuation-marks) key))))

(test* "innermost mark wins in tail position" '(2)
       (let1 key (vector 'k)
         (with-continuation-mark key 1
           (with-continuation-mark key 2
             (continuation-mark-set->list
              (current-continuation-marks) key)))))

(test* "nested non-tail marks both visible" '(x 2 1)
       ;; Inner with-mark is in non-tail position (cons arg eval), so
       ;; it pushes a new frame; the outer with-mark's frame is still
       ;; on the chain.  Both marks visible.
       (let1 key (vector 'k)
         (with-continuation-mark key 1
           (cons 'x
                 (with-continuation-mark key 2
                   (continuation-mark-set->list
                    (current-continuation-marks) key))))))

(test* "marks of a captured continuation" '(1)
       (loop-guarded 4
         (lambda (tick!)
           (let1 key (vector 'k)
             (with-continuation-mark key 1
               (let/cc k
                 (tick! 'cap)
                 (with-continuation-mark key 2
                   (continuation-mark-set->list
                    (continuation-marks k) key))))))))

;;=================================================================
;; Section 18.  call/cc over C-stack boundary
;;
;; Path: %apply-rec crosses C boundary; throw_cont_body siglongjmps
;; ESCAPE_CONT down the cstack chain.

(test-section "call/cc over C-stack boundary")

(define %apply-rec (with-module gauche.internal %apply-rec))

(test* "call/cc captured below C boundary, invoked below same boundary"
       42
       (loop-guarded 5
         (lambda (tick!)
           (call/cc (lambda (c)
                      (%apply-rec
                       (lambda () (tick! 'in) (c 42))))))))

(test* "call/cc captured below C boundary, invoked from outside"
       '(top 99)
       (loop-guarded 6
         (lambda (tick!)
           (let ([k #f])
             (cons 'top
                   (let1 v (call/cc
                            (lambda (c)
                              (%apply-rec
                               (lambda () (set! k c) 'first))))
                     (tick! v)
                     (if (eq? v 'first) (k 99) (list v))))))))

;;=================================================================
;; Section 19.  Re-entrant composable continuation (splice robustness)
;;
;;   A partial continuation frames may be invoked while those frames
;;   are still active.  Our old model of invoking partial conts
;;   (splicing saved frames onto the current cont frames) caused an
;;   issue, since modification of frames interferes.  After we made
;;   continuation frames immutable (only meta continuation chain is
;;   rebuilt), it is no longer an issue.  This is for regression test.

(test-section "re-entrant composable continuation")

(test* "re-entrant composable cont must not cycle" 42
       (let ()
         (cond-expand
          (gauche
           (define (with-alarm-guard secs thunk)
             (let ([prev (set-signal-handler! SIGALRM
                                              (lambda (sig) (error "alarm-timeout")))])
               (dynamic-wind
                 (lambda () (sys-alarm secs))
                 (lambda () (guard (e [else 'timed-out]) (thunk)))
                 (lambda () (sys-alarm 0) (set-signal-handler! SIGALRM prev))))))
          (else
           (define (with-alarm-guard secs thunk) (thunk))))
         (with-alarm-guard 5
           (lambda ()
             (let ([k #f] [depth 0])
               (reset
                (* 2 (let ([x (shift c (set! k c) (c 1))])
                       (set! depth (+ depth 1))
                       (if (= depth 1) (+ x (k 10)) x)))))))))

;; Sanity: the VM must keep working after an alarm escape, whether or
;; not the test above hung.
(test* "VM still works after alarm escape" 7 (+ 3 4))
(test* "continuations still work after alarm escape" 10
       (reset (+ 1 (shift c (c 9)))))

(test-end)
