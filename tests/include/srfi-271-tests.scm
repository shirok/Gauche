;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT
(import (scheme base)
        (scheme write)
        (srfi 64)
        (prefix (srfi 271 randomized) r:)
        (prefix (srfi 271 determinized) d:)
        )

;;; Test runner

;; The SRFI 64 implementation used by most Schemes has a very basic
;; default test runner. This is slightly more helpful on failures.

'(define (my-test-runner-factory)
  (let*
   ((runner (test-runner-null))
    (test-end
     (lambda (runner)
       (case (test-result-kind runner)
         ((pass)
          (display "Pass: ")
          (display (test-runner-test-name runner))
          (newline))
         ((fail)
          (display "FAIL: ")
          (display (test-runner-test-name runner))
          (display ". Expected ")
          (display (test-result-ref runner 'expected-value))
          (display ", got ")
          (display (test-result-ref runner 'actual-value))
          (display ".\n")))))
    (test-final
     (lambda (runner)
       (display "===============================\n")
       (display "Total passes: ")
       (display (test-runner-pass-count runner))
       (newline)
       (display "Total failures: ")
       (display (test-runner-fail-count runner))
       (newline)
       (display "Total skips: ")
       (display (test-runner-skip-count runner))
       (newline))))

    (test-runner-on-test-end! runner test-end)
    (test-runner-on-final! runner test-final)
    runner))

'(test-runner-factory my-test-runner-factory)


(test-begin "Random ports")

(test-assert "randomized random ports are input ports"
  (input-port? (r:make-random-port)))

(test-assert "determinized random ports are input ports"
  (input-port? (d:make-random-port)))

(test-assert "make-random-port (determinized) accepts an input port"
  (let ((p1 (d:make-random-port)))
    (d:make-random-port p1)))

(test-assert "random-port?"
  (call-with-port (d:make-random-port) d:random-port?))

(test-assert "random-port-initialization-error?"
  (guard (con
           ((d:random-port-initialization-error? con) #t)
           (else #f))
    (call-with-port
     (open-input-bytevector '#u8())
     d:make-random-port)))

(test-assert "random-port-state?"
  (call-with-port (d:make-random-port)
                  (lambda (p)
                    (d:random-port-state? (d:random-port-state p)))))

(test-assert "det. ports with equal states give same initial bytes"
  (let* ((p1 (d:make-random-port))
         (p2 (d:make-random-port (d:random-port-state p1))))
    (equal? (read-bytevector 8 p1) (read-bytevector 8 p2))))

(test-assert "det. ports with equal states (random-port-state=?)"
  (let* ((p1 (d:make-random-port))
         (p2 (d:make-random-port (d:random-port-state p1)))
         (p3 (d:make-random-port (d:random-port-state p2))))
    (read-u8 p1)
    (read-u8 p2)
    (read-u8 p3)
    (d:random-port-state=? (d:random-port-state p1)
                           (d:random-port-state p2)
                           (d:random-port-state p3))))

(test-end)
