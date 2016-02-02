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
;; It provides amortized O(1) operation.
;; The API is adapted to draft srfi-134

(define-module data.ideque
  (use gauche.record)
  (use gauche.generator)
  (use gauche.lazy)
  (use util.match)
  (use srfi-1)
  (export <ideque>
          make-ideque ideque ideque? ideque-empty? ideque=
          ideque-unfold ideque-unfold-right ideque-tabulate
          list->ideque ideque->list ideque-reverse
          generator->ideque ideque->generator
          ideque-ref
          ideque-front ideque-add-front ideque-remove-front
          ideque-back  ideque-add-back  ideque-remove-back
          ideque-take ideque-take-right ideque-drop ideque-drop-right
          ideque-split-at
          ideque-length ideque-append ideque-count ideque-zip
          ideque-map ideque-for-each ideque-fold ideque-fold-right
          ideque-filter-map ideque-append-map
          ideque-filter ideque-remove ideque-partition
          ideque-find ideque-find-right
          ideque-take-while ideque-take-while-right
          ideque-drop-while ideque-drop-while-right
          ideque-span ideque-break
          ideque-any ideque-every))
(select-module data.ideque)

(define-record-type <ideque> %make-dq ideque?
  (lenf dq-lenf)  ; length of front chain
  (f    dq-f)     ; front chain
  (lenr dq-lenr)  ; length of rear chain
  (r    dq-r))    ; rear chain

;; We use a singleton for empty deque
(define-constant *empty* (%make-dq 0 '() 0 '()))

;;
;; Constructors
;;

;; API
(define (make-ideque) *empty*)

;; API [srfi-134]
(define (ideque . args) (list->ideque args))

;; API [srfi-134]
(define (ideque-unfold p f g seed)
  (list->ideque (unfold p f g seed)))

;; API [srfi-134]
(define (ideque-unfold-right p f g seed)
  (list->ideque (unfold-right p f g seed)))
;; alternatively:
;; (ideque-reverse (list->ideque (unfold p f g seed)))

;; API [srfi-134]
(define (ideque-tabulate size init)
  (let ([lenf (quotient size 2)]
        [lenr (quotient (+ size 1) 2)])
    (%make-dq lenf (list-tabulate lenf init)
              lenr (unfold (cut = <> lenr)
                           (^n (init (- size n 1)))
                           (cut + <> 1)
                           0))))

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

;;
;; Basic operations
;;

;; API [srfi-134]
(define (ideque-empty? dq)
  (and (zero? (dq-lenf dq))
       (zero? (dq-lenr dq))))

;; API [srfi-134]
(define (ideque-add-front dq x)
  (check (+ (dq-lenf dq) 1) (cons x (dq-f dq)) (dq-lenr dq) (dq-r dq)))

;; API [srfi-134]
(define (ideque-front dq)
  (if (zero? (dq-lenf dq))
    (if (zero? (dq-lenr dq))
      (error "Empty deque:" dq)
      (car (dq-r dq)))
    (car (dq-f dq))))

;; API [srfi-134]
(define (ideque-remove-front dq)
  (if (zero? (dq-lenf dq))
    (if (zero? (dq-lenr dq))
      (error "Empty deque:" dq)
      *empty*)
    (check (- (dq-lenf dq) 1) (cdr (dq-f dq)) (dq-lenr dq) (dq-r dq))))

;; API [srfi-134]
(define (ideque-add-back dq x)
  (check (dq-lenf dq) (dq-f dq) (+ (dq-lenr dq) 1) (cons x (dq-r dq))))

;; API [srfi-134]
(define (ideque-back dq)
  (if (zero? (dq-lenr dq))
    (if (zero? (dq-lenf dq))
      (error "Empty deque:" dq)
      (car (dq-f dq)))
    (car (dq-r dq))))

;; API [srfi-134]
(define (ideque-remove-back dq)
  (if (zero? (dq-lenr dq))
    (if (zero? (dq-lenf dq))
      (error "Empty deque:" dq)
      *empty*)
    (check (dq-lenf dq) (dq-f dq) (- (dq-lenr dq) 1) (cdr (dq-r dq)))))

;; API [srfi-134]
(define (ideque-reverse dq)
  (if (ideque-empty? dq)
    *empty*
    (%make-dq (dq-lenr dq) (dq-r dq) (dq-lenf dq) (dq-f dq))))

;;
;; Other operations
;;

;; API [srfi-134]
(define ideque=
  (case-lambda
    [(elt=) #t]
    [(elt= ideque) (check-arg ideque? ideque) #t]
    [(elt= dq1 dq2)
     ;; we optimize two-arg case
     (check-arg ideque? dq1)
     (check-arg ideque? dq2)
     (or (eq? dq1 dq2)
         (let ([len1 (+ (dq-lenf dq1) (dq-lenr dq1))]
               [len2 (+ (dq-lenf dq2) (dq-lenr dq2))])
           (and (= len1 len2)
                (receive (x t1 t2) (list-prefix= elt= (dq-f dq1) (dq-f dq2))
                  (and x
                       (receive (y r1 r2) (list-prefix= elt= (dq-r dq1) (dq-r dq2))
                         (and y
                              (if (null? t1)
                                (list= elt= t2 (reverse r1))
                                (list= elt= t1 (reverse r2))))))))))]
    [(elt= . dqs)
     (apply list= elt= (map ideque->list dqs))]))

;; Compare two lists up to whichever shorter one.
;; Returns the compare result and the tails of uncompared lists.
(define (list-prefix= elt= a b)
  (let loop ([a a] [b b])
    (cond [(or (null? a) (null? b)) (values #t a b)]
          [(elt= (car a) (car b)) (loop (cdr a) (cdr b))]
          [else (values #f a b)])))

;; API [srfi-134]
(define (ideque-ref dq n)
  (let1 len (+ (dq-lenf dq) (dq-lenr dq))
    (cond [(or (< n 0) (>= n len)) (error "Index out of range:" n)]
          [(< n (dq-lenf dq)) (list-ref (dq-f dq) n)]
          [else (list-ref (dq-r dq) (- len n 1))])))

(define (%ideque-take dq n)             ; n is within the range
  (match-let1 ($ <ideque> lenf f lenr r) dq
    (if (<= n lenf)
      (check n (take f n) 0 '())
      (let1 lenr. (- n lenf)
        (check lenf f lenr. (take-right r lenr.))))))

(define (%ideque-drop dq n)             ; n is within the range
  (match-let1 ($ <ideque> lenf f lenr r) dq
    (if (<= n lenf)
      (check n (drop f n) lenr r)
      (let1 lenr. (- lenr (- n lenf))
        (check 0 '() lenr. (take r lenr.))))))

(define (%check-length dq n)
  (unless (<= 0 n (- (ideque-length dq) 1))
    (error "argument is out of range:" n)))

;; API [srfi-134]
(define (ideque-take dq n)
  (%check-length dq n)
  (%ideque-take dq n))

;; API [srfi-134]
(define (ideque-take-right dq n)
  (%check-length dq n)
  (%ideque-drop dq (- (ideque-length dq) n)))

;; API [srfi-134]
(define (ideque-drop dq n)
  (%check-length dq n)
  (%ideque-drop dq n))

;; API [srfi-134]
(define (ideque-drop-right dq n)
  (%check-length dq n)
  (%ideque-take dq (- (ideque-length dq) n)))

;; API [srfi-134]
(define (ideque-split-at dq n)
  (%check-length dq n)
  (values (%ideque-take dq n)
          (%ideque-drop dq n)))

;; API [srfi-134]
(define (ideque-length dq) (+ (dq-lenf dq) (dq-lenr dq)))

;; API [srfi-134]
(define (ideque-append . dqs)
  ;; We could save some list copying by carefully split dqs into front and
  ;; read group and append separately, but for now we don't bother...
  (list->ideque (concatenate (map ideque->list dqs))))

;; API [srfi-134]
(define ideque-count
  (case-lambda
    [(pred dq) (+ (count pred (dq-f dq)) (count pred (dq-r dq)))]
    [(pred . dqs) (apply count pred (map ideque->list dqs))]))

;; API [srfi-134]
(define (ideque-zip . dqs)
  (if (null? dqs) ; allowed?
    (make-ideque)
    (apply ideque-map list dqs)))

;; API [srfi-134]
(define ideque-map
  (case-lambda
    [(proc dq) (%make-dq (dq-lenf dq) (map proc (dq-f dq))
                         (dq-lenr dq) (map proc (dq-r dq)))]
    [(proc . dqs) (list->ideque (apply map proc (map ideque->list dqs)))]))

;; API [srfi-134]
(define ideque-filter-map
  (case-lambda
    [(proc dq) (let ([f (filter-map proc (dq-f dq))]
                     [r (filter-map proc (dq-r dq))])
                 (check (length f) f (length r) r))]
    [(proc . dqs)
     (list->ideque (apply filter-map proc (map ideque->list dqs)))]))

;; API [srfi-134]
(define ideque-for-each
  (case-lambda
    [(proc dq) (for-each proc (dq-f dq)) (for-each proc (reverse (dq-r dq)))]
    [(proc . dqs) (apply for-each proc (map ideque->list dqs))]))

;; API [srfi-134]
(define ideque-fold
  (case-lambda
    [(proc knil dq) (fold proc (fold proc knil (dq-f dq)) (reverse (dq-r dq)))]
    [(proc knil . dqs) (apply fold proc knil (map ideque->list dqs))]))

;; API [srfi-134]
(define ideque-fold-right
  (case-lambda
    [(proc knil dq)
     (fold-right proc (fold-right proc knil (reverse (dq-r dq))) (dq-f dq))]
    [(proc knil . dqs)
     (apply fold-right proc knil (map ideque->list dqs))]))

;; API [srfi-134]
(define (ideque-append-map proc . dqs)
  ;; can be cleverer, but for now...
  (list->ideque (apply append-map proc (map ideque->list dqs))))

(define (%ideque-filter-remove op pred dq)
  (let ([f (op pred (dq-f dq))]
        [r (op pred (dq-r dq))])
    (check (length f) f (length r) r)))

;; API [srfi-134]
(define (ideque-filter pred dq) (%ideque-filter-remove filter pred dq))
(define (ideque-remove pred dq) (%ideque-filter-remove remove pred dq))

;; API [srfi-134]
(define (ideque-partition pred dq)
  (receive (f1 f2) (partition pred (dq-f dq))
    (receive (r1 r2) (partition pred (dq-r dq))
      (values (check (length f1) f1 (length r1) r1)
              (check (length f2) f2 (length r2) r2)))))

(define *not-found* (cons #f #f)) ; unique value

(define (%search pred seq1 seq2 failure)
  ;; we could write seek as CPS, but we employ *not-found* instead to avoid
  ;; closure allocation
  (define (seek pred s)
    (cond [(null? s) *not-found*]
          [(pred (car s)) (car s)]
          [else (seek pred (cdr s))]))
  (let1 r (seek pred seq1)
    (if (not (eq? r *not-found*))
      r
      (let1 r (seek pred (reverse seq2))
        (if (not (eq? r *not-found*))
          r
          (failure))))))

;; API [srfi-134]
(define (ideque-find pred dq :optional (failure (^[] #f)))
  (%search pred (dq-f dq) (dq-r dq) failure))

;; API [srfi-134]
(define (ideque-find-right pred dq :optional (failure (^[] #f)))
  (%search pred (dq-r dq) (dq-f dq) failure))

;; API [srfi-134]
(define (ideque-take-while pred dq)
  (receive (hd tl) (span pred (dq-f dq))
    (if (null? tl)
      (receive (hd. tl.) (span pred (reverse (dq-r dq)))
        (check (dq-lenf dq) (dq-f dq) (length hd.) (reverse hd.)))
      (check (length hd) hd 0 '()))))

;; API [srfi-134]
(define (ideque-take-while-right pred dq)
  (ideque-reverse (ideque-take-while pred (ideque-reverse dq))))

;; API [srfi-134]
(define (ideque-drop-while pred dq)
  (receive (hd tl) (span pred (dq-f dq))
    (if (null? tl)
      (receive (hd. tl.) (span pred (reverse (dq-r dq)))
        (check (length tl.) tl. 0 '()))
      (check (length tl) tl (dq-lenr dq) (dq-r dq)))))

;; API [srfi-134]
(define (ideque-drop-while-right pred dq)
  (ideque-reverse (ideque-drop-while pred (ideque-reverse dq))))

(define (%idq-span-break op pred dq)
  (receive (head tail) (op pred (dq-f dq))
    (if (null? tail)
      (receive (head. tail.) (op pred (reverse (dq-r dq)))
        (values (check (length head) head (length head.) (reverse head.))
                (check (length tail.) tail. 0 '())))
      (values (check (length head) head 0 '())
              (check (length tail) tail (dq-lenr dq) (dq-r dq))))))

;; API [srfi-134]
(define (ideque-span pred dq) (%idq-span-break span pred dq))
(define (ideque-break pred dq) (%idq-span-break break pred dq))

;; API [srfi-134]
(define ideque-any
  (case-lambda
    [(pred dq) (if (null? (dq-r dq))
                 (any pred (dq-f dq))
                 (or (any pred (dq-f dq)) (any pred (reverse (dq-r dq)))))]
    [(pred . dqs) (apply any pred (map ideque->list dqs))]))

;; API [srfi-134]
(define ideque-every
  (case-lambda
    [(pred dq) (if (null? (dq-r dq))
                 (every pred (dq-f dq))
                 (and (every pred (dq-f dq)) (every pred (reverse (dq-r dq)))))]
    [(pred . dqs) (apply every pred (map ideque->list dqs))]))

;; API [srfi-134]
(define (ideque->list dq) (append (dq-f dq) (reverse (dq-r dq))))

;; API [srfi-134]
(define (list->ideque lis) (check (length lis) lis 0 '()))

;; API [srfi-134]
(define (ideque->generator dq)
  (^[] (if (ideque-empty? dq)
         (eof-object)
         (rlet1 v (ideque-front dq)
           (set! dq (ideque-remove-front dq))))))

;; API [srfi-134]
(define (generator->ideque gen)
  (list->ideque (generator->list gen)))
