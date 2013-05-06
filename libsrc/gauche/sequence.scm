;;;
;;; sequence.scm - sequence operations
;;;
;;;   Copyright (c) 2000-2013  Shiro Kawai  <shiro@acm.org>
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

;; This module defines an unified way to treat sequence-like objects
;; (that is, a collection object that can be accessed by integer index).
;; See also gauche.collection, that defines various mapping functions.

(define-module gauche.sequence
  (use srfi-1)
  (extend gauche.collection)
  (export referencer modifier subseq
          fold-right
          fold-with-index map-with-index map-to-with-index for-each-with-index
          find-index find-with-index group-sequence
          permute-to permute permute!
          shuffle-to shuffle shuffle!)
  )
(select-module gauche.sequence)

;; used by shuffle
(autoload srfi-27 default-random-source random-source-make-integers)

(define-method referencer ((obj <list>))   list-ref)
(define-method referencer ((obj <vector>)) vector-ref)
(define-method referencer ((obj <weak-vector>)) weak-vector-ref)
(define-method referencer ((obj <string>)) string-ref)

(define-method referencer ((obj <tree-map>))
  (define (ref o k from-right)
    (let loop ((i k) (iter (%tree-map-iter o)))
      (cond [(zero? i) (receive (k v) (iter #f from-right) (cons k v))]
            [else (iter #f from-right) (loop (- i 1) iter)])))
  (^[o i . opt]
    (check-arg integer? i)
    (check-arg exact? i)
    (let1 siz (tree-map-num-entries o)
      (cond [(or (< i 0) (<= siz i))
             (get-optional opt (error "index out of range:" i))]
            [(< (* i 2) siz) (ref o i #f)]
            [else (ref o (- siz i 1) #t)]))))

(define-method modifier ((obj <list>)) list-set!)
(define-method modifier ((obj <vector>)) vector-set!)
(define-method modifier ((obj <weak-vector>)) weak-vector-set!)
(define-method modifier ((obj <string>)) string-set!)

(define-method modifier   ((obj <sequence>)) ;fallback
  (errorf "Modifying ~a by index isn't supported." (class-of obj)))

;; ref and (setter ref) --------------------------------

(define-method ref ((obj <sequence>) (index <integer>))
  ((referencer obj) obj index))

(define-method ref ((obj <sequence>) (index <integer>) default)
  ((referencer obj) obj index default))

(define-method (setter ref) ((obj <sequence>) (index <integer>) value)
  ((modifier obj) obj index value))

;; subseq ----------------------------------------------

(define-method subseq ((seq <sequence>))
  (subseq seq 0 (size-of seq)))

(define-method subseq ((seq <sequence>) start)
  (subseq seq start (size-of seq)))

(define-method subseq ((seq <sequence>) start end)
  (when (< end 0) (set! end (modulo end (size-of seq))))
  (when (> start end)
    (errorf "start ~a must be smaller than or equal to end ~a" start end))
  (let1 size (- end start)
    (with-builder ((class-of seq) add! get :size size)
      (with-iterator (seq end? next :start start)
        (dotimes [i size (get)] (add! (next)))))))

(define-method (setter subseq) ((seq <sequence>) start vals)
  (with-iterator (vals end? next)
    (do ([index start (+ index 1)])
        [(end?)]
      (set! (ref seq index) (next)))))

(define-method (setter subseq) ((seq <sequence>) start end vals)
  (with-iterator (vals end? next)
    (do ([index start (+ index 1)])
        [(>= index end)]
      (when (end?) (error "not enough values for (setter subseq)" vals))
      (set! (ref seq index) (next)))))

;; fold-right --------------------------------------------

;;  (proc e1 (proc e2 ... (proc eN seed)))

(define-method fold-right (proc seed (seq <sequence>) . more)
  (if (null? more)
    (with-iterator (seq end? next)
      (let rec ()
        (if (end?)
          seed
          (let1 elt (next)
            (proc elt (rec))))))
    (call-with-iterators
     (cons seq more)
     (^[ends? nexts]
       (let rec ()
         (if (any (cut <>) ends?)
           seed
           (let1 elts (map (cut <>) nexts)
             (apply proc (append! elts (list (rec)))))))))))

;; for list arguments, built-in fold-right is faster.
(define-method fold-right (proc seed (seq <list>))
  ((with-module gauche fold-right) proc seed seq))

(define-method fold-right (proc seed (seq1 <list>) (seq2 <list>))
  ((with-module gauche fold-right) proc seed seq1 seq2))

;; mapping with index ------------------------------------

(define-method fold-with-index (proc knil (seq <sequence>) . more)
  (if (null? more)
    (with-iterator (seq end? next)
      (do ([i 0    (+ i 1)]
           [r knil (proc i (next) r)])
          [(end?) r]))
    (call-with-iterators
     (cons seq more)
     (^[ends? nexts]
       (do ([i 0    (+ i 1)]
            [r knil (apply proc i (fold-right (^[p r] (cons (p) r))
                                              (list r)
                                              nexts))])
           [(any (cut <>) ends?) r])))))

;; shortcut
(define-method fold-with-index (proc knil (seq <list>))
  (do ([i 0     (+ i 1)]
       [seq seq (cdr seq)]
       [r knil  (proc i (car seq) r)])
      [(null? seq) r]))

(define-method fold-with-index (proc knil (seq <vector>))
  (do ([len (vector-length seq)]
       [i 0 (+ i 1)]
       [r knil (proc i (vector-ref seq i) r)])
      [(= i len) r]))

(define-method map-with-index (proc (seq <sequence>) . more)
  (if (null? more)
    (with-iterator (seq end? next)
      (do ([i 0   (+ i 1)]
           [r '() (cons (proc i (next)) r)])
          [(end?) (reverse! r)]))
    (call-with-iterators
     (cons seq more)
     (^[ends? nexts]
       (do ([i 0   (+ i 1)]
            [r '() (cons (apply proc i (map (cut <>) nexts)) r)])
           [(any (cut <>) ends?) (reverse! r)])))))

;; shortcut
(define-method map-with-index (proc (seq <list>))
  (do ([i 0   (+ i 1)]
       [seq seq (cdr seq)]
       [r '() (cons (proc i (car seq)) r)])
      [(null? seq) (reverse! r)]))

(define-method map-with-index (proc (seq <vector>))
  (do ([len (vector-length seq)]
       [i 0   (+ i 1)]
       [r '() (cons (proc i (vector-ref seq i)) r)])
      [(= i len) (reverse! r)]))

(define-method map-to-with-index (class proc (seq <sequence>) . more)
  (if (null? more)
      (with-builder (class add! get :size (size-of seq))
        (with-iterator (seq end? next)
          (do ([i 0   (+ i 1)])
              [(end?) (get)]
            (add! (proc i (next))))))
      (with-builder (class add! get :size (maybe-minimum-size seq more))
        (call-with-iterators
         (cons seq more)
         (^[ends? nexts]
           (do ([i 0   (+ i 1)])
               [(any (cut <>) ends?) (get)]
             (add! (apply proc i (map (cut <>) nexts)))))))))

(define-method map-to-with-index ((class <list-meta>) proc (seq <sequence>) . more)
  (apply map-with-index proc seq more))

(define-method for-each-with-index (proc (seq <sequence>) . more)
  (if (null? more)
    (with-iterator (seq end? next)
      (do ([i 0   (+ i 1)])
          [(end?)]
        (proc i (next))))
    (call-with-iterators
     (cons seq more)
     (^[ends? nexts]
       (do ([i 0   (+ i 1)])
           [(any (cut <>) ends?)]
         (apply proc i (map (cut <>) nexts)))))))

;; shortcut
(define-method for-each-with-index (proc (seq <list>))
  (do ([i 0   (+ i 1)]
       [seq seq (cdr seq)])
      [(null? seq)]
    (proc i (car seq))))

(define-method for-each-with-index (proc (seq <vector>))
  (do ([len (vector-length seq)]
       [i 0 (+ i 1)])
      [(= i len)]
    (proc i (vector-ref seq i))))

;; find with index ------------------------------------

(define-method find-with-index (pred (seq <sequence>))
  (with-iterator (seq end? next)
    (let loop ((i 0))
      (if (end?)
        (values #f #f)
        (let1 elt (next)
          (if (pred elt)
            (values i elt)
            (loop (+ i 1))))))))

;; shortcut
(define-method find-with-index (pred (seq <list>))
  (let loop ([i 0] [seq seq])
    (cond [(null? seq) (values #f #f)]
          [(pred (car seq)) (values i (car seq))]
          [else (loop (+ i 1) (cdr seq))])))
(define-method find-with-index (pred (seq <vector>))
  (let loop ([i 0] [len (vector-length seq)])
    (cond [(= i len) (values #f #f)]
          [(pred (vector-ref seq i)) (values i (vector-ref seq i))]
          [else (loop (+ i 1) len)])))

(define-method find-index (pred (seq <sequence>))
  (receive (i e) (find-with-index pred seq) i))

;; group-sequence ----------------------------------------------

(define-method group-sequence ((seq <sequence>)
                               :key ((:key key-proc) identity)
                                    ((:test test-proc) eqv?))
  (receive (bucket results)
      (fold2 (^[elt bucket results]
               (let1 key (key-proc elt)
                 (cond
                  [(null? bucket) (values (list key elt) results)]
                  [(test-proc key (car bucket))
                   (push! (cdr bucket) elt)
                   (values bucket results)]
                  [else
                   (values (list key elt)
                           (cons (reverse! (cdr bucket)) results))])))
             '() '() seq)
    (if (null? bucket)
      (reverse! results)
      (reverse! (cons (reverse! (cdr bucket)) results)))
    ))

;; permute -------------------------------------------------------

(define-method permute-to ((class <class>) (src <sequence>) (ord <sequence>)
                           . maybe-fallback)
  (let1 *ref (referencer src)
    (with-builder (class add! get :size (lazy-size-of ord))
      (with-iterator (ord end? next)
        (do ()
            [(end?) (get)]
          (add! (apply *ref src (next) maybe-fallback)))))))

(define-method permute ((src <sequence>) (ord <sequence>) . maybe-fallback)
  (apply permute-to (class-of src) src ord maybe-fallback))

(define-method permute! ((seq <sequence>) (ord <sequence>))
  ;; This implementation is really dumb.  If we assume ORD requests proper
  ;; permutation (i.e. no "multicast" of elements) we need just a single
  ;; variable to keep the element being swapped.   I'm not sure, at this
  ;; moment, that we should force the proper permutation, or we should
  ;; allow looser ORD and we choose optimal method on the fly.
  ;; Let's see.  Just don't forget to replace this for better one > me.
  (%permute!-arg-check seq ord)
  (let ([permuted (permute seq ord)]
        [*set     (modifier seq)])
    (with-iterator (permuted end? next)
      (do ([i 0 (+ i 1)])
          [(end?) seq]
        (*set seq i (next))))))

(define-method permute! ((seq <string>) (ord <sequence>))
  ;; for string, this one is faster.
  (%permute!-arg-check seq ord)
  ((with-module gauche.internal %string-replace-body!) seq (permute seq ord)))

(define (%permute!-arg-check seq ord)
  (unless (= (size-of seq) (size-of ord))
    (error "permute! needs the length of sequence and length of permuter the same: sequence is ~,,,,30:s, permuter is ~,,,,30:s" seq ord)))

;; shuffle, shuffle! ---------------------------------------------

(define-method shuffle-to ((class <class>) (seq <sequence>) . args)
  (coerce-to class (apply shuffle! (coerce-to <vector> seq) args)))

(define-method shuffle-to ((class <vector-meta>) (seq <sequence>) . args)
  (apply shuffle! (coerce-to <vector> seq) args))

(define-method shuffle ((seq <sequence>) . args)
  (apply shuffle-to (class-of seq) seq args))

(define-method shuffle ((seq <vector>) . args)
  (apply shuffle! (vector-copy seq) args))

(define-method shuffle! ((seq <sequence>) . args)
  (let* ([size (size-of seq)]
         [shuffler (make-vector size)])
    (dotimes (i size) (vector-set! shuffler i i))
    (shuffle! shuffler)
    (permute! seq shuffler)))

(define-method shuffle! ((seq <vector>) . maybe-random-source)
  (define random-integer
    (random-source-make-integers (get-optional maybe-random-source
                                               default-random-source)))
  (define (pick&swap i)
    (when (> i 1)
      (let1 k (random-integer i)
        (unless (= k (- i 1))
          (let1 t (vector-ref seq k)
            (vector-set! seq k (vector-ref seq (- i 1)))
            (vector-set! seq (- i 1) t)))
        (pick&swap (- i 1)))))
  (pick&swap (vector-length seq))
  seq)

(define-method shuffle! ((seq <string>) . args)
  ;; string-set! is super-slow, so we provide the alternative.
  ((with-module gauche.internal %string-replace-body!)
   seq (apply shuffle seq args)))

