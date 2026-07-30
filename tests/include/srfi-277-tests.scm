;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT
'(import (rnrs base)
        (rnrs io ports)
        (rnrs programs)
        (srfi :64)
        (srfi srfi-277)
        )

;;; Test runner

;; The SRFI 64 implementation used by most Schemes has a very basic
;; default test runner. This is slightly more helpful on failures.

(define (my-test-runner-factory)
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
          (write (test-result-ref runner 'expected-value))
          (display ", got ")
          (write (test-result-ref runner 'actual-value))
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
       (newline)
       (exit (test-runner-fail-count runner)))))

    (test-runner-on-test-end! runner test-end)
    (test-runner-on-final! runner test-final)
    runner))

(test-runner-factory my-test-runner-factory)


(test-begin "Cyclic ports")

(test-assert "cyclic bytevector ports are input ports"
  (call-with-port (open-cyclic-input-bytevector '#u8(1)) input-port?))

(test-assert "cyclic bytevector ports are binary ports"
  (call-with-port (open-cyclic-input-bytevector '#u8(1)) binary-port?))

(test-assert "cyclic string ports are input ports"
  (call-with-port (open-cyclic-input-string "a") input-port?))

(test-assert "cyclic string ports are textual ports"
  (call-with-port (open-cyclic-input-string "a") textual-port?))

(test-equal "read from cyclic bytevector port"
  '#u8(1 2 3 1 2 3 1 2)
  (call-with-port (open-cyclic-input-bytevector '#u8(1 2 3))
                  (lambda (p)
                    (get-bytevector-n p 8))))

(test-equal "read from cyclic string port"
  "abcabcab"
  (call-with-port (open-cyclic-input-string "abc")
                  (lambda (p)
                    (get-string-n p 8))))

(test-group "port positioning"
  ;; Skip next test if cyclic bytevector ports aren't positionable.
  (call-with-port
   (open-cyclic-input-bytevector '#u8(1 2 3))
   (lambda (p)
     (unless (and (port-has-port-position? p)
                  (port-has-set-port-position!? p))
       (test-skip 1))))

  (test-assert "read on cyclic bytevector port after positioning"
    (call-with-port
     (open-cyclic-input-bytevector '#u8(1 2 3))
     (lambda (p)
       (let* ((get-bvec (lambda () (get-bytevector-n p 5)))
              (_junk (get-bytevector-n p 13))
              (pos (port-position p))
              (v (get-bvec)))
         (set-port-position! p pos)
         (equal? v (get-bvec))))))

  ;; Skip next test if cyclic string ports aren't positionable.
  (call-with-port
   (open-cyclic-input-string "abc")
   (lambda (p)
     (unless (and (port-has-port-position? p)
                  (port-has-set-port-position!? p))
       (test-skip 1))))

  (test-assert "read on cyclic string port after positioning"
    (call-with-port
     (open-cyclic-input-string "abc")
     (lambda (p)
       (let* ((get-str (lambda () (get-string-n p 5)))
              (_junk (get-string-n p 13))
              (pos (port-position p))
              (s (get-str)))
         (set-port-position! p pos)
         (equal? s (get-str))))))
  )

(test-end)
