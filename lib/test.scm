;;;
;;; Simple test routine
;;;

;;; $Id: test.scm,v 1.1 2001-02-28 23:48:49 shiro Exp $

(define *undef* (when #f #t))
(define *discrepancy-list* '())

(define (section msg)
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
      )))


(provide "test")
