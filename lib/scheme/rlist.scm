;;;
;;; scheme.rlist - Purely Functional Random-Access Pairs and Lists (R7RS Red)
;;;
;;;   Copyright (c) 2019-2022  Shiro Kawai  <shiro@acm.org>
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

;; Originally srfi-101
(define-module scheme.rlist
  (use gauche.record)
  (use gauche.sequence)
  (use util.match)
  (use data.skew-list)
  (export (rename ra-quote quote)
          (rename ra-pair? pair?)
          (rename ra-cons cons)
          (rename ra-car car)
          (rename ra-cdr cdr)
          (rename ra-caar caar)
          (rename ra-cadr cadr)
          (rename ra-cddr cddr)
          (rename ra-cdar cdar)
          (rename ra-caaar caaar)
          (rename ra-caadr caadr)
          (rename ra-caddr caddr)
          (rename ra-cadar cadar)
          (rename ra-cdaar cdaar)
          (rename ra-cdadr cdadr)
          (rename ra-cdddr cdddr)
          (rename ra-cddar cddar)
          (rename ra-caaaar caaaar)
          (rename ra-caaadr caaadr)
          (rename ra-caaddr caaddr)
          (rename ra-caadar caadar)
          (rename ra-cadaar cadaar)
          (rename ra-cadadr cadadr)
          (rename ra-cadddr cadddr)
          (rename ra-caddar caddar)
          (rename ra-cdaaar cdaaar)
          (rename ra-cdaadr cdaadr)
          (rename ra-cdaddr cdaddr)
          (rename ra-cdadar cdadar)
          (rename ra-cddaar cddaar)
          (rename ra-cddadr cddadr)
          (rename ra-cddddr cddddr)
          (rename ra-cdddar cdddar)
          (rename ra-null? null?)
          (rename ra-list? list?)
          (rename ra-list list)
          (rename ra-make-list make-list)
          (rename ra-length length)
          (rename ra-append append)
          (rename ra-reverse reverse)
          (rename ra-list-tail list-tail)
          (rename ra-list-ref list-ref)
          (rename ra-list-set list-set)
          (rename ra-list-ref/update list-ref/update)
          (rename ra-map map)
          (rename ra-for-each for-each)
          (rename ra-random-access-list->linear-access-list
                  random-access-list->linear-access-list)
          (rename ra-linear-access-list->random-access-list
                  linear-access-list->random-access-list)
          equal?                        ; builtin equal? works
          ))

(select-module scheme.rlist)

;; We base on data.skew-list, but we need to extend it to support
;; improper lists.  We just keep the last cdr separately.

(define-record-type <ra-list>
  %make-ra ra-pair?
  (spine ra-spine)                      ; <skew-list>
  (last-cdr ra-last-cdr))               ; The last cdr

(define (make-ra spine last-cdr)
  (if (skew-list-empty? spine)
    last-cdr
    (%make-ra spine last-cdr)))

;; This is not in srfi-101, but for the convenience
(define-method write-object ((x <ra-list>) port)
  (display "#,(rlist (" port)
  (skew-list-fold (ra-spine x)
                  (^[e c]
                    (unless (zero? c) (display " " port))
                    (write e port)
                    (+ c 1))
                  0)
  (unless (null? (ra-last-cdr x))
    (display " . " port)
    (write (ra-last-cdr x) port))
  (display "))" port))

(define-reader-ctor 'rlist
  (^x (ra-linear-access-list->random-access-list x)))

;; internal
(define (ra-proper? obj)
  (and (ra-pair? obj) (null? (ra-last-cdr obj))))

;; Primitives
(define-syntax ra-quote
  (er-macro-transformer
   (^[f r c]
     (define (%x->ra obj)
       (if (pair? obj)
         (ra-cons (%x->ra (car obj)) (%x->ra (cdr obj)))
         obj))
     (match f
       [(_ x) (quasirename r
                `(quote ,(%x->ra x)))]
       [_ (error "Malformed random-access list quote:" f)]))))

(define (ra-null? x) (null? x))
(define (ra-list? x) (or (null? x) (ra-proper? x)))

(define (ra-cons x y)
  (if (ra-pair? y)
    (make-ra (skew-list-cons x (ra-spine y)) (ra-last-cdr y))
    (make-ra (skew-list-cons x skew-list-null) y)))

(define (ra-car obj)
  (if (ra-pair? obj)
    (skew-list-car (ra-spine obj))
    (error "Attempt to take ra-car of " obj)))

(define (ra-cdr obj)
  (if (ra-pair? obj)
    (make-ra (skew-list-cdr (ra-spine obj)) (ra-last-cdr obj))
    (error "Attempt to take ra-cdr of " obj)))

(define (%ra-ref obj n)
  (assume-type obj <ra-list>)
  (skew-list-ref (ra-spine obj) n))
(define (%ra-drop obj n)
  (assume-type obj <ra-list>)
  (let1 spine (skew-list-drop (ra-spine obj) n)
    (make-ra spine (ra-last-cdr obj))))


(define (ra-caar obj) (ra-car (ra-car obj)))
(define (ra-cadr obj) (%ra-ref obj 1))
(define (ra-cdar obj) (ra-cdr (ra-car obj)))
(define (ra-cddr obj) (%ra-drop obj 2))

(define (ra-caaar obj) (ra-car (ra-caar obj)))
(define (ra-caadr obj) (ra-car (ra-cadr obj)))
(define (ra-cadar obj) (ra-car (ra-cdar obj)))
(define (ra-caddr obj) (%ra-ref obj 2))
(define (ra-cdaar obj) (ra-cdr (ra-caar obj)))
(define (ra-cdadr obj) (ra-cdr (ra-cadr obj)))
(define (ra-cddar obj) (ra-cdr (ra-cdar obj)))
(define (ra-cdddr obj) (%ra-drop obj 3))

(define (ra-caaaar obj) (ra-car (ra-caaar obj)))
(define (ra-caaadr obj) (ra-car (ra-caadr obj)))
(define (ra-caadar obj) (ra-car (ra-cadar obj)))
(define (ra-caaddr obj) (ra-car (ra-caddr obj)))
(define (ra-cadaar obj) (ra-car (ra-cdaar obj)))
(define (ra-cadadr obj) (ra-car (ra-cdadr obj)))
(define (ra-caddar obj) (ra-car (ra-cddar obj)))
(define (ra-cadddr obj) (%ra-ref obj 3))
(define (ra-cdaaar obj) (ra-cdr (ra-caaar obj)))
(define (ra-cdaadr obj) (ra-cdr (ra-caadr obj)))
(define (ra-cdadar obj) (ra-cdr (ra-cadar obj)))
(define (ra-cdaddr obj) (ra-cdr (ra-caddr obj)))
(define (ra-cddaar obj) (ra-cdr (ra-cdaar obj)))
(define (ra-cddadr obj) (ra-cdr (ra-cdadr obj)))
(define (ra-cdddar obj) (ra-cdr (ra-cddar obj)))
(define (ra-cddddr obj) (%ra-drop obj 4))

(define (ra-list . elts) (ra-linear-access-list->random-access-list elts))
(define (ra-make-list n :optional obj)
  (ra-linear-access-list->random-access-list (make-list n obj)))

(define (ra-length ra)
  (cond [(ra-null? ra) 0]
        [(ra-proper? ra) (skew-list-length (ra-spine ra))]
        [else
         (error "Attempt to take length of improper random-access-list:" ra)]))

(define (ra-length<=? ra k)
  (if (ra-pair? ra)
    (skew-list-length<=? (ra-spine ra) k)
    (zero? k)))

(define-method object-equal? ((ra1 <ra-list>) (ra2 <ra-list>))
  (and (equal? (ra-last-cdr ra1) (ra-last-cdr ra2))
       (equal? (ra-spine ra1) (ra-spine ra2))))

(define (ra-append obj . objs)
  (cond [(null? objs) obj]
        [(null? obj) (apply ra-append objs)]
        [(ra-proper? obj)
         (let1 tail (apply ra-append objs)
           (if (ra-pair? tail)
             (make-ra (skew-list-append (ra-spine obj)
                                        (ra-spine tail))
                      (ra-last-cdr tail))
             (make-ra (ra-spine obj) tail)))]
        [else
         (error "Can't append an improper random-access list:" obj)]))

(define (ra-reverse ra)
  (cond [(ra-null? ra) ra]
        [(ra-proper? ra)
         (make-ra (list->skew-list (reverse (skew-list->lseq (ra-spine ra))))
                  '())]
        [else (error "Can't reverse an improper random-access list:" ra)]))

(define (ra-list-tail obj k)
  (cond [(zero? k) obj]
        [(ra-pair? obj)
         (make-ra (skew-list-drop (ra-spine obj) k)
                  (ra-last-cdr obj))]
        [else (error "Index out of range:" k)]))

(define (ra-list-ref ra k)
  (assume (ra-pair? ra))
  (skew-list-ref (ra-spine ra) k))

(define (ra-list-set ra k elt)
  (assume (ra-pair? ra))
  (make-ra (skew-list-set (ra-spine ra) k elt)
           (ra-last-cdr ra)))

(define (ra-list-ref/update ra k proc)
  (assume (ra-pair? ra))
  (let1 v (ra-list-ref ra k)
    (values v (ra-list-set ra k (proc v)))))

(define ra-map
  (case-lambda
    [(proc ra)
     ;; fast path
     (assume (ra-proper? ra))
     (make-ra (skew-list-map (ra-spine ra) proc) '())]
    [(proc) (error "At least one random-access list argument required.")]
    [(proc . ras)
     (assume (every ra-proper? ras))
     (assume (apply = (map ra-length ras)))
     (make-ra (apply map-to <skew-list> proc (map ra-spine ras)) '())]))

(define ra-for-each
  (case-lambda
    [(proc ra)
     (assume (ra-proper? ra))
     (skew-list-fold (ra-spine ra) (^[elt _] (proc elt)) #f)
     (undefined)]
    [(proc) (error "At least one random-access list argument required.")]
    [(proc . ras)
     (assume (every ra-proper? ras))
     (assume (apply = (map ra-length ras)))
     (apply for-each proc (map ra-spine ras))]))

(define (ra-random-access-list->linear-access-list ra)
  (assume (ra-list? ra))
  (if (null? ra)
    ra
    (skew-list->list (ra-spine ra))))

(define (ra-linear-access-list->random-access-list lis)
  (assume (proper-list? lis))
  (make-ra (list->skew-list lis) '()))
