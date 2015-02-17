;;;
;;; auxiliary treemap utilities.  to be autoloaded.
;;;
;;;   Copyright (c) 2007-2015  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.treeutil
  (export make-tree-map tree-map-empty?
          tree-map-min tree-map-max tree-map-pop-min! tree-map-pop-max!
          tree-map-fold tree-map-fold-right
          tree-map-map tree-map-for-each
          tree-map-keys tree-map-values
          tree-map->alist alist->tree-map)
  )
(select-module gauche.treeutil)

(define make-tree-map
  (case-lambda
    [() (%make-tree-map default-comparator)]
    [(cmp)
     (if (comparator? cmp)
       (begin
         (unless (comparator-comparison-procedure? cmp)
           (error "make-tree-map needs a comparator with comparison \
                  procedure, but got:" cmp))
         (%make-tree-map cmp))
       (%make-tree-map (make-comparator #t #t cmp #f)))]
    [(=? <?)
     (%make-tree-map
      ($ make-comparator #t =?
         (^[x y](cond [(=? x y) 0] [(<? x y) -1] [else 1])) #f))]))

(define (tree-map-empty? tm) (zero? (tree-map-num-entries tm)))

(define (tree-map-min tm) (%tree-map-bound tm #t #f))
(define (tree-map-max tm) (%tree-map-bound tm #f #f))
(define (tree-map-pop-min! tm) (%tree-map-bound tm #t #t))
(define (tree-map-pop-max! tm) (%tree-map-bound tm #f #t))

(define (%tree-map-fold tm kons knil backward)
  (check-arg tree-map? tm)
  (let ((eof (cons #f #f))              ;marker
        (i (%tree-map-iter tm)))
    (let loop ((r knil))
      (receive (k v) (i eof backward)
        (if (eq? k eof)
          r
          (loop (kons k v r)))))))

(define (tree-map-fold tm kons knil)
  (%tree-map-fold tm kons knil #f))

(define (tree-map-fold-right tm kons knil)
  (%tree-map-fold tm kons knil #t))

(define (tree-map-map tm proc)
  (tree-map-fold-right tm (lambda (k v r) (cons (proc k v) r)) '()))

(define (tree-map-for-each tm proc)
  (tree-map-fold tm (lambda (k v r) (proc k v) r) (undefined)))

(define (tree-map-keys tm)
  (tree-map-fold-right tm (lambda (k v r) (cons k r)) '()))

(define (tree-map-values tm)
  (tree-map-fold-right tm (lambda (k v r) (cons v r)) '()))

(define (tree-map->alist tm)
  (tree-map-fold-right tm acons '()))

(define (alist->tree-map alist . args)
  (rlet1 tm (apply make-tree-map args)
    (dolist (kv alist)
      (tree-map-put! tm (car kv) (cdr kv)))))

