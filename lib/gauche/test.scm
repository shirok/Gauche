;;;
;;; gauche.test - test framework
;;;  
;;;   Copyright (c) 2000-2006 Shiro Kawai, All rights reserved.
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
;;;  $Id: test.scm,v 1.18 2006-10-16 12:02:44 shirok Exp $

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

(define-module gauche.test
  (export test test* test-start test-end test-section
          test-module
          *test-error* *test-report-error* test-error? prim-test))
(select-module gauche.test)

;; we cannot use srfi-1, since we use gauche.test to test srfi-1 itself.
;; we count on the built-in fold work properly here.
(define (%filter proc lis)
  (reverse (fold (lambda (x r) (if (proc x) (cons x r) r)) '() lis)))

;; An object to represent error.
(define-class <test-error> ()
  ((message :init-keyword :message :initform #f)))

(define-method write-object ((obj <test-error>) out)
  (if (ref obj 'message)
      (format out "#<error ~s>" (ref obj 'message))
      (display "#<error>" out)))

(define-method object-equal? ((x <test-error>) (y <test-error>))
  #t)

(define *test-error* (make <test-error>))

(define *test-report-error*
  (sys-getenv "GAUCHE_TEST_REPORT_ERROR"))

(define (test-error? obj) (is-a? obj <test-error>))

;; List of discrepancies
(define *discrepancy-list* '())

;; Tests ------------------------------------------------------------

;; Primitive test.  This doesn't use neither with-error-handler nor
;; object system, so it can be used _before_ those constructs are tested.
(define (prim-test msg expect thunk . compare)
  (let ((cmp (if (pair? compare) (car compare) equal?)))
    (format/ss #t "test ~a, expects ~s ==> " msg expect)
    (flush)
    (let ((r (thunk)))
      (if (cmp expect r)
          (format #t "ok\n")
          (begin
            (format/ss #t "ERROR: GOT ~S\n" r)
            (set! *discrepancy-list*
                  (cons (list msg expect r) *discrepancy-list*))))
      (flush)
      )))

;; Normal test.
(define (test msg expect thunk . compare)
  (apply prim-test msg expect
         (lambda ()
           (with-error-handler
               (lambda (e)
                 (when *test-report-error*
                   (report-error e))
                 (make <test-error>
                   :message (if (is-a? e <message-condition>)
                              (ref e 'message)
                              e)))
             thunk))
         compare))

;; A convenient macro version
(define-macro (test* msg expect form . compare)
  `(test ,msg ,expect (lambda () ,form) ,@compare))

;; Toplevel binding sanity check ----------------------------------

;; Try to catch careless typos.  Suggested by Kimura Fuyuki.
;; The toplevel undefined variable screening is suggested by Kazuki Tsujimoto.
;; Keyword argument :allow-undefined may take a list of symbols, which
;; is excluded from undefined variable check.

(define (test-module module . opts)
  (let-keywords* opts ((allow-undefined '()))
    (let1 mod (cond ((module? module) module)
                    ((symbol? module)
                     (or (find-module module)
                         (error "no such module" module)))
                    (else
                     (error "test-module requires module or symbol, but got"
                            module)))
      (format #t "testing bindings in ~a ... " mod) (flush)
      (let ((bad-autoload '())
            (bad-export '())
            (bad-gref '())
            (report '()))
        ;; 1. Check if there's no dangling autoloads.
        (hash-table-for-each (module-table mod)
                             (lambda (sym val)
                               (guard (_ (else (push! bad-autoload sym)))
                                 (global-variable-ref mod sym))))
        ;; 2. Check if all exported symbols are properly defined.
        (when (pair? (module-exports mod))
          (for-each (lambda (sym)
                      (guard (_ (else (push! bad-export sym)))
                        (global-variable-ref mod sym)))
                    (module-exports mod)))
        ;; 3. Check if all global references are resolvable.
        (for-each
         (lambda (closure)
           (for-each (lambda (gref)
                       (cond ((and (not (memq (slot-ref gref 'name)
                                              allow-undefined))
                                   (dangling-gref? gref closure))
                              => (lambda (bad) (push! bad-gref bad)))))
                     (closure-grefs closure)))
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
                                             (format "~a(~a)" (car z) (cdr z)))
                                           bad-gref)
                                      ", "))))
        (if (null? report)
          (format #t "ok\n")
          (let ((s (apply string-append report)))
            (format #t "ERROR: ~a\n" s)
            (set! *discrepancy-list*
                  (cons (list (format #f "bindings in module ~a" (module-name mod))
                              '() s)
                        *discrepancy-list*))))
        )
      )))

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
  (%filter closure?
           (map (lambda (sym)
                  (global-variable-ref module sym #f))
                (hash-table-keys (module-table module)))))

(define (closure-grefs closure)
  (%filter identifier?
           ((with-module gauche.internal vm-code->list)
            (closure-code closure))))

(define (dangling-gref? ident closure)
  (and (not ((with-module gauche.internal find-binding)
             (slot-ref ident 'module)
             (slot-ref ident 'name)
             #f))
       (cons (slot-ref ident 'name) (slot-ref closure 'info))))

;; Logging and bookkeeping -----------------------------------------
(define (test-section msg)
  (let ((msglen (string-length msg)))
    (format #t "<~a>~a\n" msg (make-string (max 5 (- 77 msglen)) #\-))))

(define (test-start msg)
  (let* ((s (format #f "Testing ~a ... " msg))
         (pad (make-string (max 3 (- 65 (string-length s))) #\space)))
    (display s (current-error-port))
    (display pad (current-error-port))
    (flush (current-error-port))
    (when (and (sys-isatty (current-error-port))
               (sys-isatty (current-output-port)))
      (newline (current-error-port))))
  (set! *discrepancy-list* '())
  (unless (and (sys-isatty (current-error-port))
               (sys-isatty (current-output-port)))
    (let ((msglen (string-length msg)))
      (format #t "Testing ~a ~a\n" msg (make-string (max 5 (- 70 msglen)) #\=)))
    (flush))
  )

(define (test-end)
  (let ((e (current-error-port))
        (o (current-output-port)))
    (define (fmt . args)
      (if (and (sys-isatty e) (sys-isatty o))
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
    ))

(provide "gauche/test")
