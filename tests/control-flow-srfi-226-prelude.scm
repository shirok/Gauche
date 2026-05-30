;;;
;;; Prelude to run tests/control-flow.scm against the SRFI-226 reference
;;; implementation under ChezScheme.
;;;
;;; Kicked from tests/control-flow-srfi-226.sh, which catenates this
;;; prelude with each top-level form of tests/control-flow.scm and runs
;;; the result in a fresh Chez process (the reference's `run` is
;;; single-shot; composable continuations corrupt the next `run` in
;;; the same process).
;;;
;;; All `dynamic-wind` / `call/cc` / control-feature primitives are
;;; taken from `(control-features)` so the test exercises the
;;; reference implementation, not Chez's natives.
;;;

(import (rename (except (rnrs base (6))
                        dynamic-wind call/cc call-with-current-continuation)
                (error r6rs:error))
        (only (rnrs conditions (6)) condition-message)
        (control-features))

;;;-----------------------------------------------------------------
;;; Shims for Gauche syntax / functions used by control-flow.scm.
;;;-----------------------------------------------------------------

;; Gauche's `error` is `(error msg . irritants)`; R6RS wants a `who`
;; symbol first.
(define (error msg . args) (apply r6rs:error 'control-flow msg args))

;; Gauche's `errorf` does ~s/~a formatting; we don't need the exact
;; rendering, only that it raises.
(define (errorf fmt . args) (r6rs:error 'control-flow fmt args))

;; Gauche's condition slot access; only 'message is used in this test.
(define (condition-ref c slot)
  (cond ((eq? slot 'message) (condition-message c))
        (else (r6rs:error 'condition-ref "unknown slot" slot))))

;; Gauche shorthands.
(define-syntax let1
  (syntax-rules ()
    ((_ var val body ...) (let ((var val)) body ...))))

(define-syntax let/cc
  (syntax-rules ()
    ((_ k body ...) (call-with-current-continuation (lambda (k) body ...)))))

;; Gauche `with-error-handler` -- the lazy-EP fast path doesn't exist
;; in the reference; the standard call/cc + with-exception-handler
;; shim gives the same observable ordering (handler runs before the
;; dynamic-wind `after` thunks unwind).
(define-syntax with-error-handler
  (syntax-rules ()
    ((_ handler thunk)
     (call-with-current-continuation
      (lambda (k)
        (with-exception-handler (lambda (e)
                                  (call-with-values (lambda () (handler e)) k))
                                thunk))))))

;; Gauche `with-module M v` -- in the reference we only use this to
;; grab %apply-rec, which we define directly below.
(define-syntax with-module
  (syntax-rules ()
    ((_ mod var) var)))

;; Gauche `%apply-rec` deliberately crosses a C-stack boundary; the
;; reference has no C-stack notion, so plain apply suffices.
(define (%apply-rec f . args) (apply f args))

;; Stub Gauche test-framework macros so stray top-level forms parse.
(define-syntax use         (syntax-rules () ((_ . _) (begin))))
(define-syntax test-start  (syntax-rules () ((_ . _) (begin))))
(define-syntax test-end    (syntax-rules () ((_ . _) (begin))))
(define-syntax test-section (syntax-rules () ((_ . _) (begin))))

(define (with-output-to-string thunk)
  (let ((p (open-output-string)))
    (parameterize ((current-output-port p)) (thunk))
    (get-output-string p)))

;;;-----------------------------------------------------------------
;;; Helpers from control-flow.scm, re-defined here so each per-form
;;; fresh run has them available even when the file's own top-level
;;; `define` forms aren't part of this run.
;;;-----------------------------------------------------------------

(define (loop-guarded limit body)
  (let ((count 0)
        (trail '()))
    (define (tick! . tag)
      (set! count (+ count 1))
      (set! trail (cons tag trail))
      (when (> count limit)
        (errorf "loop-guard tripped (limit=~s, count=~s, trail=~s)"
                limit count (reverse trail)))
      (if (pair? tag) (car tag) #f))
    (body tick!)))

(define (make-trail)
  (let ((log '()))
    (values (lambda items
             (set! log (cons items log))
             (if (pair? items) (car items) #f))
           (lambda () (reverse log)))))

;;;-----------------------------------------------------------------
;;; Gauche reset/shift (also re-defined; control-features doesn't
;;; export shift/reset directly, only the call-with-composable-cont API).
;;;-----------------------------------------------------------------

(define-syntax reset
  (syntax-rules ()
    ((_ e1 e2 ...) (call-with-continuation-prompt (lambda () e1 e2 ...)))))

(define-syntax shift
  (syntax-rules ()
    ((_ k e1 e2 ...)
     (call-with-composable-continuation
      (lambda (c)
        (define k (lambda args (reset (apply c args))))
        (abort-current-continuation (default-continuation-prompt-tag)
          (lambda () e1 e2 ...)))))))

;;;-----------------------------------------------------------------
;;; test* -- emit (RESULTROW name expect pass? actual) from INSIDE
;;; `run`, so the reference's metacontinuation is set up properly.
;;;-----------------------------------------------------------------

(define (condition-message-safely e)
  (guard (_ (#t '?)) (condition-message e)))

;; Render any value as a Scheme-readable string.  We stringify before
;; emitting because Chez may print unreadable forms like `#<void>` for
;; the unspecified value or other host objects; raw `write` of those
;; would crash gosh's reader during aggregation.
(define (write-to-string v)
  (let ((p (open-output-string)))
    (write v p)
    (get-output-string p)))

(define-syntax test*
  (syntax-rules (test-error)
    ((_ name (test-error) expr)
     (run (lambda ()
            (let ((v (guard (e (#t 'GOT-ERROR))
                       (call-with-values (lambda () expr) list)
                       'NO-ERROR)))
              (write (list 'RESULTROW name "(test-error)"
                           (eq? v 'GOT-ERROR)
                           (write-to-string v)))
              (newline)))))
    ((_ name expect expr)
     (run (lambda ()
            (let ((v (guard (e (#t (list '*UNEXPECTED-ERROR*
                                         (condition-message-safely e))))
                       (call-with-values (lambda () expr) list))))
              (write (list 'RESULTROW name
                           (write-to-string (list expect))
                           (equal? (list expect) v)
                           (write-to-string v)))
              (newline)))))))
