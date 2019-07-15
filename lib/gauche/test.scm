;;;
;;; gauche.test - test framework
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
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

#!no-fold-case

;; Note for developers: This module intentionally avoids using
;; Gauche's convenience features and extended libraries; instead,
;; we stick to the minimal primitives here as much as possible.
;; It's because this module is used to test the convenience
;; features and extended libraries.

;; Writing your own test
;;
;;  (use gauche.test)
;;  (test-start "my feature")
;;  (load "my-feature")         ; load your program
;;  (select-module my-feature)  ; if your program defines a module.
;;
;;  (test-module 'my-feature)   ; checks if module binding is sane
;;
;;  (test-section "feature group 1")
;;  (test "feature 1-1" EXPECT (lambda () TEST-BODY))
;;  (test "feature 1-2" EXPECT (lambda () TEST-BODY))
;;  ...
;;
;;  (test-section "feature group 2")
;;  (define test-data ...)
;;  (test "feature 2-1" EXPECT (lambda () TEST-BODY))
;;  ...
;;
;;  (test-end)
;;
;; To run a test interactively, just load the file.
;; It is also recommended to have a "test" target in your Makefile, so that
;; the user of your program can run a test easily.  The rule may look like
;; this:
;;
;;   test :
;;        gosh my-feature-test.scm > test.log
;;
;; If stdout is redirected to other than tty, all the verbose logs will go
;; there, and only a small amount of messages go to stderr.
;;
;; Some environment variables affect the behavior of the tests.
;;
;;  GAUCHE_TEST_REPORT_ERROR  If defined, reports stack trace to stderr
;;         when the test thunk raises an error (even when it is expected).
;;         Useful for diagnosis of unexpected errors.
;;
;;  GAUCHE_TEST_RECORD_FILE   If defined, names a file the test processes
;;         keep the total statistics.  Test-end accumulates the stats
;;         into the named file instead of reporting it out immediately.

(define-module gauche.test
  (export test test* test-start test-end test-running? test-section test-log
          test-module test-script
          test-error test-one-of test-none-of
          test-check test-record-file test-summary-check
          *test-error* *test-report-error* test-error? prim-test
          test-count++ test-pass++ test-fail++))
(select-module gauche.test)

;; Autoload test-script to avoid depending other modules
(autoload "gauche/test/script" test-script)

;; An object to represent error.  This class isn't exported; the user
;; must use `test-error' procedure to create an instance.
;;
;; This object is used in both the expected result and the actual result
;; of test expression.   For the actual result, this object holds the
;; raised condition in `condition' slot, and its class and message in
;; the `class' and `message' slots.
;; For the expected result, class slot must be set as one of <condition>
;; classes, or #f.  If it's a <condition> class, then it is used to test
;; the actual result condition has the condition type.  If it's #f,
;; then any <test-error> object matches.
(define-class <test-error> ()
  ((condition :init-keyword :condition :init-value #f)
   (class     :init-keyword :class     :init-value #f)
   (message   :init-keyword :message   :init-value #f)))

(define-method write-object ((obj <test-error>) out)
  (let1 cname (if (ref obj'class) (class-name (ref obj'class)) 'error)
    (if (ref obj 'message)
      (format out "#<~a ~s>" cname (ref obj 'message))
      (format out "#<~a>" cname))))

(define (test-error? obj) (is-a? obj <test-error>))

(define (test-error :optional (class #f) (message #f))
  (make <test-error> :class class :message message))

;; An object to represent "any one of Xs"
(define-class <test-one-of> ()
  ((choices :init-keyword :choices)))

(define-method write-object ((obj <test-one-of>) out)
  (format out "#<test-one-of: any one of ~s>" (slot-ref obj'choices)))

;; API
(define (test-one-of . choices) (make <test-one-of> :choices choices))

;; An object to represent "none of Xx"
(define-class <test-none-of> ()
  ((choices :init-keyword :choices)))

(define-method write-object ((obj <test-none-of>) out)
  (format out "#<test-none-of: none of ~s>" (slot-ref obj'choices)))

;; API
(define (test-none-of . choices) (make <test-none-of> :choices choices))

;; API
;; We don't use generic function dispatch (at least for the time being),
;; to make it easy to troubleshoot when object system gets messed up.
;; In future we'll make use of generic functions.
(define (test-check expected result :optional (fallback equal?))
  (cond [(test-error? expected)
         (and (test-error? result)
              (let ([c (slot-ref expected'class)]
                    [e (slot-ref result'condition)])
                (or (not c)
                    (condition-has-type? e c)))
              (let ([m (slot-ref expected'message)]
                    [em (slot-ref result'message)])
                (cond [(string? m) (and (string? em) (equal? m em))]
                      [(regexp? m) (and (string? em) (m em))]
                      [else #t])))]
        [(is-a? expected <test-one-of>)
         (any (lambda (choice) (test-check choice result fallback))
              (slot-ref expected 'choices))]
        [(is-a? expected <test-none-of>)
         (every (lambda (choice) (not (test-check choice result fallback)))
                (slot-ref expected 'choices))]
        [else (fallback expected result)]))

(define *test-error* (make <test-error>)) ;DEPRECATED
(define *test-report-error* (sys-getenv "GAUCHE_TEST_REPORT_ERROR"))
(define *test-record-file* (sys-getenv "GAUCHE_TEST_RECORD_FILE"))

;; API
(define (test-record-file file) (set! *test-record-file* file))

;; List of discrepancies
(define *discrepancy-list* '())

(define *test-counts* (vector 0 0 0 0)) ; total/pass/fail/abort

(define (test-count++)
  (vector-set! *test-counts* 0 (+ (vector-ref *test-counts* 0) 1)))
(define (test-pass++)
  (vector-set! *test-counts* 1 (+ (vector-ref *test-counts* 1) 1)))
(define (test-fail++ msg expected result)
  (vector-set! *test-counts* 2 (+ (vector-ref *test-counts* 2) 1))
  (set! *discrepancy-list* (cons (list msg expected result) *discrepancy-list*)))
(define (format-summary)
  (format "Total: ~5d tests, ~5d passed, ~5d failed, ~5d aborted.\n"
          (vector-ref *test-counts* 0)
          (vector-ref *test-counts* 1)
          (vector-ref *test-counts* 2)
          (vector-ref *test-counts* 3)))
(define (read-summary)
  (when (and (string? *test-record-file*)
             (sys-access *test-record-file* F_OK)) ; avoid file-exists? to trigger autoload
    (with-input-from-file *test-record-file*
      (lambda ()
        (let [(m (rxmatch #/Total:\s+(\d+)\s+tests,\s+(\d+)\s+passed,\s+(\d+)\s+failed,\s+(\d+)\s+aborted/ (read-line)))]
          (when m
            (for-each (lambda (i)
                        (vector-set! *test-counts* i
                                     (string->number
                                      (rxmatch-substring m (+ i 1)))))
                      '(0 1 2 3))))))))
(define (prepare-summary)
  ;; We write out aborted+1, in case if the test process fails before test-end
  ;; For normal case, it will be overwritten by test-end.
  (let ([orig-abort (vector-ref *test-counts* 3)])
    (vector-set! *test-counts* 3 (+ orig-abort 1))
    (write-summary)
    (vector-set! *test-counts* 3 orig-abort)))

(define (write-summary)
  (when (string? *test-record-file*)
    (receive [p nam] (sys-mkstemp *test-record-file*)
      (display (format-summary) p)
      (close-output-port p)
      (sys-rename nam *test-record-file*))))

;; Tests ------------------------------------------------------------

;; Primitive test.  This uses neither with-error-handler nor the
;; object system, so it can be used _before_ those constructs are tested.
(define (prim-test msg expect thunk . compare)
  (let ([cmp (if (pair? compare) (car compare) test-check)])
    (format/ss #t "test ~a, expects ~s ==> " msg expect)
    (flush)
    (test-count++)
    (let ([r (thunk)])
      (cond [(cmp expect r)
             (format #t "ok\n")
             (test-pass++)]
            [else
             (format/ss #t "ERROR: GOT ~S\n" r)
             (test-fail++ msg expect r)])
      (flush))))

;; Normal test.
(define (test msg expect thunk . compare)
  (apply prim-test msg expect
         (lambda ()
           (guard (e [else
                      (when *test-report-error*
                        (report-error e))
                      (make <test-error> :condition e
                            :class (class-of e)
                            :message
                            (if (condition-has-type? e <message-condition>)
                              (slot-ref e 'message)
                              e))])
             (thunk)))
         compare))

;; A convenient macro version
;; We use er-macro-transformer, so test* should be used after macro
;; subsystem is tested with more primitive framework.
(define-syntax test*
  (er-macro-transformer
   (lambda (f r c)
     (apply (lambda (_ msg expect form . compare)
              `(,(r 'test) ,msg ,expect (,(r 'lambda) () ,form) ,@compare))
            f))))

;; Toplevel binding sanity check ----------------------------------

;; Try to catch careless typos.  Suggested by Kimura Fuyuki.
;; The toplevel undefined variable screening is suggested by Kazuki Tsujimoto.
;; Keyword argument :allow-undefined takes a list of symbols, which
;; is excluded from undefined variable check.  Keyword argument
;; :bypass-arity-check takes a list of symbols that bypasses arity check.

(define (test-module module :key (allow-undefined '()) (bypass-arity-check '()))
  (test-count++)
  (let1 mod (cond [(module? module) module]
                  [(symbol? module)
                   (or (find-module module)
                       (error "no such module" module))]
                  [else
                   (error "test-module requires module or symbol, but got"
                          module)])
    (format #t "testing bindings in ~a ... " mod) (flush)
    (test-module-common mod allow-undefined bypass-arity-check)))

;; Common op for test-module and test-script.
(define (test-module-common mod allow-undefined bypass-arity-check)
  (define (code-location src-code)
    (let1 src-info (debug-source-info src-code)
      (string-append (if src-info (format "~a:" (cadr src-info)) "")
                     (format "~a" src-code))))
  (let ([bad-autoload '()]
        [bad-export '()]
        [bad-gref '()]
        [bad-arity '()]
        [report '()])
    ;; 1. Check if there's no dangling autoloads.
    (hash-table-for-each (module-table mod)
                         (lambda (sym val)
                           (guard (_ (else (push! bad-autoload sym)))
                             (global-variable-ref mod sym))))
    ;; 2. Check if all exported symbols are properly defined.
    ;; We create an anonymous moudle and import the tested module.  By this
    ;; way, we can test renaming export (in which case, the exported name
    ;; doesn't correspond to the binding in MOD so we can't look up directly
    ;; in MOD.)
    (when (and (module-name mod) (pair? (module-exports mod)))
      (let ([m (make-module #f)])
        (eval `(import ,(module-name mod)) m)
        (eval `(extend) m)
        (for-each (lambda (sym)
                    (guard (_ [else (push! bad-export sym)])
                      (global-variable-ref m sym)))
                  (module-exports mod))))
    ;; 3. Check if all global references are resolvable, and if it is
    ;; called, gets valid number of arguments.
    (for-each
     (lambda (closure)
       (for-each (lambda (arg)
                   (let ([gref (car arg)]
                         [numargs (cadr arg)]
                         [src-code (caddr arg)])
                     (cond [(memq (slot-ref gref 'name) allow-undefined)]
                           [(dangling-gref? gref (or src-code (slot-ref closure 'info)))
                            => (lambda (bad) (push! bad-gref bad))]
                           [(memq (slot-ref gref 'name) bypass-arity-check)]
                           [(arity-invalid? gref numargs (or src-code (slot-ref closure 'info)))
                            => (lambda (bad) (push! bad-arity bad))])))
                 (append-map closure-grefs
                             (cons closure
                                   (filter closure?
                                           ((with-module gauche.internal %closure-env->list) closure))))))
     (toplevel-closures mod))
    ;; report discrepancies
    (unless (null? bad-autoload)
      (push! report (format "found dangling autoloads: ~a" bad-autoload)))
    (unless (null? bad-export)
      (unless (null? report) (push! report " AND "))
      (push! report
             (format "symbols exported but not defined: ~a" bad-export)))
    (unless (null? bad-gref)
      (unless (null? report) (push! report " AND "))
      (push! report
             (format "symbols referenced but not defined: ~a"
                     (string-join (map (lambda (z)
                                         (format "~a(~a)" (car z) (code-location (cdr z))))
                                       bad-gref)
                                  ", "))))
    (unless (null? bad-arity)
      (unless (null? report) (push! report " AND "))
      (push! report
             (format "procedures received wrong number of argument: ~a"
                     (string-join (map (lambda (z)
                                         (format "~a(~a) got ~a"
                                                 (car z) (code-location (cadr z)) (caddr z)))
                                       bad-arity)
                                  ", "))))
    (cond
     [(null? report) (test-pass++) (format #t "ok\n")]
     [else
      (let ([s (apply string-append report)])
        (format #t "ERROR: ~a\n" s)
        (test-fail++ (format #f "bindings in ~a" mod) '() s))])
    ))


;; Auxiliary funcs to catch dangling grefs.  We use the fact that
;; an identifier embedded within the vm code is almost always an
;; operand of GREF or GSET.  (This is because identifiers are
;; introduced by macro expansion, but quoted identifiers are turned
;; back to ortinary symbols when expansion is done.)  However, it
;; may not be impossible to embed identifiers within literals.
;; Eventually we need a builtin procedure that picks identifiers
;; used for GREF/GSET.
;;
;; Note that these identifiers in operands are replaced by GLOCs
;; once the code is executed.  We don't need to consider them; since
;; if the identifier has successufully replaced by a GLOC, it couldn't
;; be an undefined reference.

(define (toplevel-closures module)
  (filter closure?
          (map (lambda (sym)
                 (global-variable-ref module sym #f))
               (hash-table-keys (module-table module)))))

;; Combs closure's instruction list to extract references for the global
;; identifiers.  If it is a call to the global function, we also picks
;; up the number of arguments, so that we can check it against arity.
;; Returns ((<identifier> <num-args> <source-code>|#f) ...)
(define (closure-grefs closure)
  (define code->list (with-module gauche.internal vm-code->list))
  (define (gref-numargs code) (cadr code))
  (define gref-call-insns
    '(GREF-CALL PUSH-GREF-CALL GREF-TAIL-CALL PUSH-GREF-TAIL-CALL))
  (let loop ([r '()]
             [code (code->list (closure-code closure))]
             [i 0]
             [debug-info (~ (closure-code closure)'debug-info)])
    (cond [(null? code) r]
          [(and (pair? (car code))
                (memq (caar code) gref-call-insns))
           (if (pair? (cdr code))
             (if (identifier? (cadr code))
               (let* ([src-code (assq-ref (assv-ref debug-info i '())
                                          'source-info)]
                      ;; If the identifier is in `code' and the source-code
                      ;; field is empty, fill the field with the current
                      ;; `src-code'.
                      [new-r (map (lambda (e)
                                    (let ([ident (car e)]
                                          [numargs (cadr e)]
                                          [orig-src-code (caddr e)])
                                      (if (and (memq (identifier->symbol ident)
                                                     src-code)
                                               (not orig-src-code))
                                        `(,ident ,numargs ,src-code)
                                        e)))
                                  r)])
                 (loop `((,(cadr code) ,(gref-numargs (car code)) ,src-code)
                         ,@new-r)
                       (cddr code)
                       (+ i 2)
                       debug-info))
               (loop r (cddr code) (+ i 2) debug-info))    ;skip #<gloc>
             (loop r '() (+ i 1) debug-info))]
          [(identifier? (car code))
           (loop `((,(car code) #f #f) ,@r) (cdr code) (+ i 1) debug-info)]
          [(is-a? (car code) <compiled-code>)
           (loop (loop r (code->list (car code)) 0 (~ (car code)'debug-info))
                 (cdr code)
                 (+ i 1)
                 debug-info)]
          [(list? (car code)) ; for the operand of LOCAL-ENV-CLOSURES
           (loop (loop r (car code) 0 '()) (cdr code) (+ i 1) debug-info)]
          [else (loop r (cdr code) (+ i 1) debug-info)])))

(define (arity-invalid? gref numargs src-code)
  (and-let* ([ numargs ]
             ;; TODO: What if GREF is nested identifier?
             [proc (global-variable-ref
                    (slot-ref gref'module)
                    (unwrap-syntax gref)
                    #f)]
             ;; We exclude <generic> with no methods.  Such "placeholder"
             ;; generic function may be used in the base module, expecting
             ;; the other module adds methods to it.
             [ (not (and (is-a? proc <generic>) (null? (~ proc'methods)))) ]
             [ (not (apply applicable? proc (make-list numargs <bottom>))) ])
    (list (slot-ref gref 'name)
          src-code
          numargs)))

(define (dangling-gref? ident src-code)
  (let1 name (unwrap-syntax ident)
    (and (not ((with-module gauche.internal id->bound-gloc) ident))
         (cons name src-code))))

;; Logging and bookkeeping -----------------------------------------

;; private global flag, true during we're running tests.
;; (we avoid using parameters intentionally.)
(define *test-running* #f)

(define (test-running?) *test-running*)

(define (test-section msg)
  (let ([msglen (string-length msg)])
    (format #t "<~a>~a\n" msg (make-string (max 5 (- 77 msglen)) #\-))))

(define (not-redirected? port)
  (cond-expand
   [gauche.os.windows
    (or (sys-isatty port)
        ;; for MSYS (mintty)
        ((with-module gauche.internal %sys-mintty?) port)
        ;; for windows console conversion ports
        (port-attribute-ref port 'windows-console-conversion #f))]
   [else
    (sys-isatty port)]))

(define (test-start msg)
  (set! *test-running* #t)
  (let* ([s (format #f "Testing ~a ... " msg)]
         [pad (make-string (max 3 (- 65 (string-length s))) #\space)])
    (display s (current-error-port))
    (display pad (current-error-port))
    (flush (current-error-port))
    (read-summary)
    (prepare-summary)
    (when (and (not-redirected? (current-error-port))
               (not-redirected? (current-output-port)))
      (newline (current-error-port))))
  (set! *discrepancy-list* '())
  (unless (and (not-redirected? (current-error-port))
               (not-redirected? (current-output-port)))
    (let ([msglen (string-length msg)])
      (format #t "Testing ~a ~a\n" msg (make-string (max 5 (- 70 msglen)) #\=)))
    (flush))
  )

;; test-log fmt arg ...
;; The formatted output, prefixed by ";;",  goes to stdout for the logging.
(define (test-log fmt . args)
  (display ";; ")
  (apply format #t fmt args)
  (newline)
  (flush))

;; test-end :key :exit-on-failure
;; avoid using extended formal list since we need to test it.
(define (test-end . args)
  (let ([e (current-error-port)]
        [o (current-output-port)]
        [exit-on-failure (get-keyword :exit-on-failure args #f)])
    (define (fmt . args)
      (if (and (not-redirected? e) (not-redirected? o))
        (apply format/ss o args)
        (begin (apply format/ss e args)
               (apply format/ss o args))))

    (if (null? *discrepancy-list*)
      (fmt "passed.\n")
      (begin
        (fmt "failed.\ndiscrepancies found.  Errors are:\n")
        (for-each (lambda (r)
                    (apply fmt "test ~a: expects ~s => got ~s\n" r))
                  (reverse *discrepancy-list*))))
    (flush)

    (when *test-record-file*
      (write-summary))

    (set! *test-running* #f)

    ;; the number of failed tests.
    (let ([num-failures (length *discrepancy-list*)])
      (when (and (> num-failures 0)
                 exit-on-failure)
        (exit (if (fixnum? exit-on-failure) exit-on-failure 1)))
      num-failures)))

;; Read the test record file (if there's any), and exit with 1
;; if there has been any failure.
(define (test-summary-check)
  (read-summary)
  (unless (and (zero? (vector-ref *test-counts* 2))
               (zero? (vector-ref *test-counts* 3)))
    (exit 1)))
