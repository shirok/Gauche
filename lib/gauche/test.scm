;;;
;;; gauche.test - test framework
;;;
;;;  Copyright(C) 2000-2003 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: test.scm,v 1.7 2003-01-07 13:28:04 shirok Exp $

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

(define-module gauche.test
  (export test test* test-start test-end test-section
          *test-error* test-error? prim-test))
(select-module gauche.test)

;; An object to represent error.
(define-class <test-error> ()
  ((message :init-keyword :message :initform #f)))

(define-method write-object ((obj <test-error>) out)
  (if (ref obj 'message)
      (format out "#<error ~s>" (ref obj 'message))
      (display "#<error>")))

(define-method object-equal? ((x <test-error>) (y <test-error>))
  #t)

(define *test-error* (make <test-error>))

(define (test-error? obj) (is-a? obj <test-error>))

;; List of discrepancies
(define *discrepancy-list* '())

(define (test-section msg)
  (let ((msglen (string-length msg)))
    (format #t "<~a>~a\n" msg (make-string (max 5 (- 77 msglen)) #\-))))

;; Primitive test.  This doesn't use neither with-error-handler nor
;; object system, so it can be used _before_ those constructs are tested.
(define (prim-test msg expect thunk . compare)
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

;; Normal test.
(define (test msg expect thunk . compare)
  (apply prim-test msg expect
         (lambda ()
           (with-error-handler
               (lambda (e)
                 (make <test-error>
                   :message (if (is-a? e <error>)
                                (ref e 'message)
                                e)))
             thunk))
         compare))

;; A convenient macro version
(define-macro (test* msg expect form . compare)
  `(test ,msg ,expect (lambda () ,form) ,@compare))

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

(provide "gauche/test")
