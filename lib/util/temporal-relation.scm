;;;
;;; Temporal relations
;;;
;;;  Copyright (c) 2022  Shiro Kawai  <shiro@acm.org>
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

;; This module detemines relations between two temporal intervals,
;; one temporal interval and one time point, or two time points.
;; This module doesn't assume a particular representation of temporal
;; intervals or timepoints.

;; This is inspired by Haskell Rampart library.
;; https://hackage.haskell.org/package/rampart-2.0.0.0/docs/Rampart.html

(define-module util.temporal-relation
  (use srfi-114 :only (if3))
  (export make-interval-protocol
          pair-interval-protocol
          relation?
          inverse
          relate
          relate-point
          ))
(select-module util.temporal-relation)

;; Anything that conforms given interval protocol can be used as
;; an interval.

(define-class <interval-protocol> ()
  ((%start-point :init-keyword :start-point)
   (%end-point   :init-keyword :end-point)
   (%compare-points :init-keyword :compare-points)))

(define (make-interval-protocol start end :optional (compare-points compare))
  (make <interval-protocol>
    :start-point start
    :end-point end
    :compare-points compare-points))

(define-inline pair-interval-protocol
  (make-interval-protocol car cdr))


;; Interval relations
(define (relation? sym)
  (boolean (memq sym '(before
                       meets
                       overlaps
                       finished-by
                       contains
                       starts
                       equal
                       started-by
                       during
                       finishes
                       overlapped-by
                       met-by
                       after))))

(define (inverse sym)
  (ecase sym
    [(before)       'after]
    [(meets)        'met-by]
    [(overlaps)     'overlapped-by]
    [(finished-by)  'finishes]
    [(contains)     'during]
    [(starts)       'started-by]
    [(equal)        'equal]
    [(started-by)   'starts]
    [(during)       'contains]
    [(finishes)     'finished-by]
    [(overlapped-by)'overlaps]
    [(met-by)       'meets]
    [(after)        'before]))

(define (relate protocol int-x int-y)
  (let ([xs ((~ protocol'%start-point) int-x)]
        [xe ((~ protocol'%end-point) int-x)]
        [ys ((~ protocol'%start-point) int-y)]
        [ye ((~ protocol'%end-point) int-y)]
        [cmp (~ protocol'%compare-points)])
    (if3 (cmp xs ys)
         (if3 (cmp xe ys)
              'before
              'meets
              (if3 (cmp xe ye)
                   'overlaps
                   'finished-by
                   'contains))
         (if3 (cmp xe ye)
              'starts
              'equal
              'started-by)
         (if3 (cmp xs ye)
              (if3 (cmp xe ye)
                   'during
                   'finishes
                   'overlapped-by)
              'met-by
              'after))))

(define (relate-point protocol int point)
  (let ([s ((~ protocol'%start-point) int)]
        [e ((~ protocol'%end-point) int)]
        [cmp (~ protocol'%compare-points)])
    (if3 (cmp e point)
         'before
         'finished-by
         (if3 (cmp s point)
              'contains
              'started-by
              'after))))
