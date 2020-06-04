;;;
;;; srfi-78 - Lightweight testing
;;;
;;;   Copyright (c) 2020  Shiro Kawai  <shiro@acm.org>
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

;; We realize srfi-78 as a wrapper of gauche.test, so that the tests
;; using srfi-78 can be seamlessly embedded in other Gauche tests.
;; 

(define-module srfi-78
  (use gauche.test)
  (use gauche.parameter)
  (use srfi-42)
  (export check check-ec check-report check-set-mode! check-reset!
          check-passed?))
(select-module srfi-78)

(define check-mode (make-parameter 'report))

;; API
;; NB: The report-as-you-go feature is delegated to gauche.test, so
;; the check mode other than off doesn't make any difference.
(define (check-set-mode! mode)
  (ecase mode
    [(off summary report-failed report) (check-mode mode)]))

;; we track this as well as gauche.test
(define check-pass-count (make-parameter 0))
(define check-total-count (make-parameter 0))
(define check-failure (make-parameter #f))

;; API
(define (check-reset!)
  (check-pass-count 0)
  (check-total-count 0)
  (check-failure #f))

;; API
(define-syntax check
  (syntax-rules (=>)
    [(_ expr (=> eqproc) expected)
     (unless (eq? (check-mode) 'off)
       ;; Allow files that only use srfi-78 to work
       (unless (test-running?) (test-start (current-load-path)))
       (let ([eq eqproc]
             [ex expected])
         (test* 'expr ex expr eq %check-hook)))]
    [(_ expr => expected)
     (check expr (=> equal?) expected)]))

(define (%check-hook verdict name expected result)
  (inc! (check-total-count))
  (case verdict
    [(pass) 
     (inc! (check-pass-count))]
    [(fail)
     ;; we only record the first failure
     (unless (check-failure)
       (check-failure (list name expected result)))]))

;; API
(define (check-passed? expected-num-passed)
  (and (not (check-failure))
       (= expected-num-passed (check-pass-count))))

;; API
(define (check-report)
  (if (check-failure)
    (format #t "Passed ~d tests out of ~d tests.  First failure: ~s\n"
            (check-pass-count) (check-total-count) (check-failure))
    (format #t "All ~d tests passed."
            (check-pass-count))))


;; API
(define-syntax check-ec
  (syntax-rules (=>)
    [(_ q ... expr (=> eqproc) expected (args ...))
     (let ([pass-count 0]
           [total-count 0]
           [first-failure #f])
       (do-ec q ...
              (let ([eq eqproc]
                    [ex expected])
                (test* 'expr expected expr exproc
                       (^[verdict name expected result]
                         (inc! total-count)
                         (if (eq? verdict 'pass)
                           (inc! total-count)
                           (unless first-failure
                             (set! first-failure
                                   (list verdict name expeted result 
                                         (list args ...)))))))))
       (if (= total-count pass-count)
         (inc! (check-pass-count))
         (unless (check-failure)
           (check-failure first-failure))))]
    [(_ q ... expr => expected (args ...))
     (check-ec q ... expr (=> equal?) expected (args ...))]
    [(_ q ... expr (=> eqproc) expected)
     (check-ec q ... expr (=> exproc) expected ())]
    [(_ q ... expr => expected)
     (check-ec q ... expr (=> equal?) expected ())]))
