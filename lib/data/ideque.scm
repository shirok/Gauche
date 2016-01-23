;;;
;;; data.ideque - immutable double-ended queue
;;;
;;;   Copyright (c) 2015  Shiro Kawai  <shiro@acm.org>
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

;; This implements banker's deque
;; as described in Chris Okasaki's Purely Functional Data Structures.
;; It provides amortized O(1) operation

(define-module data.ideque
  (use gauche.record)
  (use gauche.lazy)
  (use util.match)
  (export <ideque>
          make-ideque ideque? ideque-empty?
          ideque-cons ideque-head ideque-tail
          ideque-snoc ideque-last ideque-init
          ideque-reverse))
(select-module data.ideque)

(define-record-type <ideque> %make-dq ideque?
  (lenf dq-lenf)  ; length of front chain
  (f    dq-f)     ; front chain
  (lenr dq-lenr)  ; length of rear chain
  (r    dq-r))    ; rear chain

;; We use a singleton for empty deque
(define-constant *empty* (%make-dq 0 '() 0 '()))

;; API
(define (make-ideque) *empty*)

(define-constant C 3)

(define (check lenf f lenr r)
  (cond [(> lenf (+ (* lenr C) 1))
         (let* ([i (quotient (+ lenf lenr) 2)]
                [j (- (+ lenf lenr) i)]
                [f. (take f i)]
                [r. (lappend r (reverse (drop f i)))])
           (%make-dq i f. j r.))]
        [(> lenr (+ (* lenf C) 1))
         (let* ([j (quotient (+ lenf lenr) 2)]
                [i (- (+ lenf lenr) j)]
                [r. (take r j)]
                [f. (lappend f (reverse (drop r j)))])
           (%make-dq i f. j r.))]
        [else (%make-dq lenf f lenr r)]))

;; API
(define (ideque-empty? dq)
  (and (zero? (dq-lenf dq))
       (zero? (dq-lenr dq))))

;; API
(define (ideque-cons x dq)
  (check (+ (dq-lenf dq) 1) (cons x (dq-f dq)) (dq-lenr dq) (dq-r dq)))

;; API
(define (ideque-head dq)
  (if (zero? (dq-lenf dq))
    (if (zero? (dq-lenr dq))
      (error "Empty deque:" dq)
      (car (dq-r dq)))
    (car (dq-f dq))))

;; API
(define (ideque-tail dq)
  (if (zero? (dq-lenf dq))
    (if (zero? (dq-lenr dq))
      (error "Empty deque:" dq)
      *empty*)
    (check (- (dq-lenf dq) 1) (cdr (dq-f dq)) (dq-lenr dq) (dq-r dq))))

;; API
(define (ideque-snoc dq x)
  (check (dq-lenf dq) (dq-f dq) (+ (dq-lenr dq) 1) (cons x (dq-r dq))))

;; API
(define (ideque-last dq)
  (if (zero? (dq-lenr dq))
    (if (zero? (dq-lenf dq))
      (error "Empty deque:" dq)
      (car (dq-f dq)))
    (car (dq-r dq))))

;; API
(define (ideque-init dq)
  (if (zero? (dq-lenr dq))
    (if (zero? (dq-lenf dq))
      (error "Empty deque:" dq)
      *empty*)
    (check (dq-lenf dq) (dq-f dq) (- (dq-lenr dq) 1) (cdr (dq-r dq)))))

;; API
(define (ideque-reverse dq)
  (if (ideque-empty? dq)
    *empty*
    (%make-dq (dq-lenr dq) (dq-r dq) (dq-lenf dq) (dq-f dq))))

