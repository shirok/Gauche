;;;
;;; srfi-78 - Lightweight testing
;;;
;;;   Copyright (c) 2020-2022  Shiro Kawai  <shiro@acm.org>
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

;; If srfi-78 is used within gauche.test framework, we integrate
;; it into gauche.test---that is, check works as a wrapper of test*.
;; Otherwise, check bookkeeps its own testing.

(define-module srfi-78
  (use gauche.test)
  (use srfi-42)
  (use util.match)
  (export check check-ec check-report check-set-mode! check-reset!
          check-passed?))
(select-module srfi-78)

(define check-mode (make-parameter 'report))

;; API
;; NB: If srfi-78 runs within gauche.test, it doesn't do any reporting
;; by itself.
(define (check-set-mode! mode)
  (ecase mode
    [(off summary report-failed report) (check-mode mode)]))

;; Track the test results
(define-class <check-results> ()
  ((total-count :init-value 0)
   (pass-count :init-value 0)
   (first-failure :init-value #f)       ; (name expected result aux)
   ))

(define (reset-results! results)
  (set! (~ results'total-count) 0)
  (set! (~ results'pass-count) 0)
  (set! (~ results'first-failure) #f))

(define *global-results* (make <check-results>))

(define (register-failure! results name expected result aux)
  (unless (~ results'first-failure)
    (set! (~ results'first-failure)
          (list name expected result aux))))

;; API
(define (check-reset!) (reset-results! *global-results*))

(define ((%check-hook results) verdict name expected result)
  (inc! (~ results'total-count))
  (case verdict
    [(pass) (inc! (~ results'pass-count))]
    [(fail) (register-failure! results name expected result '())]))

(define (do-check results name expected thunk eqproc aux)
  (if (test-running?)
    ;; delegate to gauche.test
    (test name expected thunk eqproc #f (%check-hook results))
    (guard ([e (else
                (register-failure! results name expected e aux))])
      (inc! (~ results'total-count))
      (let1 result (thunk)
        (if (eqproc expected result)
          (begin
            (when (and (eq? results *global-results*)
                       (eq? (check-mode) 'report))
              (format #t "Checking ~s, expecting ~s => ok\n"
                      name expected))
            (inc! (~ results'pass-count)))
          (begin
            (when (and (eq? result *global-results*)
                       (memq (check-mode) '(report report-failed)))
              (format #t "Checking ~s, expecting ~s => ERROR: got ~s\n"
                      name expected result))
            (register-failure! results name expected result aux)))))))

;; API
(define-syntax check
  (syntax-rules (=>)
    [(_ expr (=> eqproc) expected)
     (unless (eq? (check-mode) 'off)
       (do-check *global-results* 'expr expected (^[] expr) eqproc '()))]
    [(_ expr => expected)
     (check expr (=> equal?) expected)]))

;; API
(define (check-passed? expected-num-passed)
  (and (not (~ *global-results*'first-failure))
       (= expected-num-passed (~ *global-results*'pass-count))))

;; format the argument part of check-ec
(define (%format-aux aux)
  (if (null? aux)
    ""
    (string-append ", with "
                   (string-join (map (^p (format "~s: ~s" (car p) (cdr p)))
                                     aux)
                                ", "))))

;; API
;; This only reports in standalone-mode
(define (check-report)
  (unless (or (test-running?) (eq? (check-mode) 'off))
    (match (~ *global-results*'first-failure)
      [(name expected result aux)
       (format #t "Passed ~d tests out of ~d tests.  First failure on ~s, \
                   expected: ~s, result: ~s~a\n"
               (~ *global-results*'pass-count)
               (~ *global-results*'total-count)
               name expected result
               (%format-aux aux))]
      [_
       (format #t "All ~d tests passed.\n"
               (~ *global-results*'pass-count))])))

;; API
(define-syntax check-ec
  (syntax-rules (=>)
    [(_ q ... expr (=> eqproc) expected (arg ...))
     (let ((results (make <check-results>)))
       (do-ec q ...
              (do-check results 'expr expected (^[] expr) eqproc
                        (list (cons 'arg arg) ...)))
       (inc! (~ *global-results*'total-count))
       (match (~ results'first-failure)
         [(and (name expected result aux) failure)
          (when (and (not (test-running?))
                     (memq (check-mode) '(report report-failed)))
            (format #t "Checking ~s, expecting ~s => ERROR: got ~s~a\n"
                    name expected result
                    (%format-aux aux)))
          (apply register-failure! *global-results* failure)]
         [_
          (when (and (not (test-running?))
                     (eq? (check-mode) 'report))
            (format #t "Checking ~s, expecting ~s => ok\n"
                    'expr 'expected))
          (inc! (~ *global-results*'pass-count))]))]
    [(_ q ... expr => expected (arg ...))
     (check-ec q ... expr (=> equal?) expected (arg ...))]
    [(_ q ... expr (=> eqproc) expected)
     (check-ec q ... expr (=> exproc) expected ())]
    [(_ q ... expr => expected)
     (check-ec q ... expr (=> equal?) expected ())]))
