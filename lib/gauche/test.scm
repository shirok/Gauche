;;;
;;; Simple test routine
;;;

;;; $Id: test.scm,v 1.3 2002-05-12 10:59:36 shirok Exp $

;; Writing your own test
;;
;;  (use gauche.test)
;;  (test-start "my feature")
;;  (load "my-feature")         ; load your program
;;  (select-module my-feature)  ; if your program defines a module.
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

(define-module gauche.test)
(select-module gauche.test)
(export test test-start test-end test-section test-undef)

(define *discrepancy-list* '())

(define (test-undef) (when #f #t))

(define (test-section msg)
  (format #t "<~a>-------------------------------------------------\n" msg))

(define (test msg expect thunk . compare)
  (let ((cmp (if (pair? compare) (car compare) equal?)))
    (format #t "test ~a, expects ~s ==> " msg expect)
    (flush)
    (let ((r (thunk)))
      (if (cmp expect r)
          (format #t "ok\n")
          (begin
            (format #t "ERROR: GOT ~S\n" r)
            (set! *discrepancy-list*
                  (cons (list msg expect r) *discrepancy-list*))))
      (flush)
      )))

(define (test-start msg)
  (let* ((s (format #f "Testing ~a ... " msg))
         (pad (make-string (- 65 (string-length s)) #\space)))
    (display s (current-error-port))
    (display pad (current-error-port))
    (flush (current-error-port))
    (when (and (sys-isatty (current-error-port))
               (sys-isatty (current-output-port)))
      (newline (current-error-port))))
  (set! *discrepancy-list* '())
  (unless (and (sys-isatty (current-error-port))
               (sys-isatty (current-output-port)))
    (format #t
            "Testing ~a ==============================================\n"
            msg)
    (flush))
  )

(define (test-end)
  (let ((e (current-error-port))
        (o (current-output-port)))
    (define (fmt . args)
      (if (and (sys-isatty e) (sys-isatty o))
          (apply format o args)
          (begin (apply format e args)
                 (apply format o args))))
    
    (if (null? *discrepancy-list*)
        (fmt "passed.\n")
        (begin
          (fmt "failed.\ndiscrepancies found.  Errors are:\n")
          (for-each (lambda (r)
                      (apply fmt "test ~a: expects ~s => got ~s\n" r))
                    (reverse *discrepancy-list*))))
    ))

(provide "tester/tester")
