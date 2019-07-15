;;;
;;; sequence.scm - sequence operations
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
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
          find-index find-with-index group-sequence group-contiguous-sequence
          delete-neighbor-dups
          delete-neighbor-dups! delete-neighbor-dups-squeeze!
          sequence->kmp-stepper sequence-contains
          break-list-by-sequence! break-list-by-sequence
          common-prefix-to common-prefix
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
    (let loop ((i k) (iter ((with-module gauche.internal %tree-map-iter) o)))
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

;; (group-contiguous-sequence '(1 2 3 4 7 8 9 11 13 14 16))
;;  => ((1 2 3 4) (7 8 9) (11) (13 14) (16))
;; (group-contiguous-sequence '(1 2 3 4 7 8 9 11 13 14 16) :squeeze #t)
;;  => ((1 4) (7 9) (11) (13 14) (16))
(define-method group-contiguous-sequence ((seq <sequence>)
                                          :key ((:key key-proc) identity)
                                               ((:next next-proc) (cut + 1 <>))
                                               ((:test test-proc) eqv?)
                                               (squeeze #f))
  (receive (last results)
      (fold2 (^[elt last results]
               (let1 key (key-proc elt)
                 (cond
                  [(not last) (values key `((,key)))]  ; initial
                  [(test-proc key (next-proc last))
                   (if squeeze
                     (values key results)
                     (values key `((,key ,@(car results)) ,@(cdr results))))]
                  [else
                   (if squeeze
                     (let1 start (caar results)
                       (if (test-proc last start)
                         (values key `((,key) ,@results))
                         (values key `((,key) (,start ,last)
                                       ,@(cdr results)))))
                     (values key `((,key) ,@results)))])))
             #f '() seq)
    (if (null? results)
      '()
      (if squeeze
        (let1 start (caar results)
          (if (test-proc last start)
            (reverse! results)
            (reverse! `((,start ,last) ,@(cdr results)))))
        (reverse! (map reverse! results))))))
                                          
(define-method delete-neighbor-dups ((seq <sequence>)
                                     :key ((:key key-proc) identity)
                                          ((:test test-proc) eqv?)
                                          (start 0)
                                          (end #f))
  (with-builder ((class-of seq) add! get)
    (with-iterator (seq end? next)
      (dotimes [start] (next))
      (cond [(or (end?) (and end (= start end))) (get)]
            [else (let1 e (next)
                    (add! e)
                    (let loop ([ek (key-proc e)]
                               [k  (+ start 1)])
                      (if (or (end?) (and end (= k end)))
                        (get)
                        (let* ([n (next)]
                               [nk (key-proc n)])
                          (cond [(test-proc ek nk) (loop nk (+ k 1))]
                                [else (add! n) (loop nk (+ k 1))])))))]))))

;; Store result into SEQ, which must be modifiable.
;; Returns the index right after the last modified entry.
(define-method delete-neighbor-dups! ((seq <sequence>)
                                      :key ((:key key-proc) identity)
                                           ((:test test-proc) eqv?)
                                           (start 0)
                                           (end #f))
  (define mod! (modifier seq))
  (with-iterator (seq end? next)
    (dotimes [start] (next))
    (if (or (end?) (and end (= start end)))
      start
      (let1 e (next)
        (mod! seq start e)
        (let loop ([d (+ start 1)]
                   [ek (key-proc e)]
                   [k (+ start 1)]
                   [e e])
          (if (or (end?) (and end (= k end)))
            d
            (let* ([n (next)]
                   [nk (key-proc n)])
              (cond [(test-proc ek nk) (loop d nk (+ k 1) n)]
                    [else (mod! seq d n) (loop (+ d 1) nk (+ k 1) n)]))))))))

;; This can only be defined in sequences whose length can be changed.
;; NB: We can't define generic version, since there's no generic way
;; for length-changing mutation.  Each capable sequence should implement
;; the method.  Here we only provide for <list>.
(define-method delete-neighbor-dups-squeeze! ((seq <list>)            
                                              :key ((:key key-proc) identity)
                                                   ((:test test-proc) eqv?)
                                                   (start 0)
                                                   (end #f))
  (let1 seq (drop* seq start)
    (if (null? seq)
      seq
      (let loop ([p seq]
                 [pk (key-proc (car seq))]
                 [k (+ start 1)]
                 [last seq])
        (if (or (not (pair? (cdr p))) (and end (= k end)))
          (begin (set-cdr! last '()) seq)
          (let* ([q (cdr p)]
                 [qk (key-proc (car q))])
            (if (test-proc pk qk)
              (loop q qk (+ k 1) last)
              (begin
                (set-cdr! last q)
                (loop q qk (+ k 1) q)))))))))

;; searching sequence -----------------------------------------------

;; Returns procedure to do one step of kmp match.  If NEEDLE's length
;; is 0, returns #f.
(define (sequence->kmp-stepper needle :key ((:test test-proc) eqv?))
  (define restarts
    (rlet1 v (make-vector (size-of needle) -1)
      (dotimes [i (- (vector-length v) 1)]
        (let loop ([k (+ (vector-ref v i) 1)])
          (if (and (> k 0)
                   (not (test-proc (ref needle i) (ref needle (- k 1)))))
            (loop (+ (vector-ref v (- k 1)) 1))
            (vector-set! v (+ i 1) k))))))
  (let* ([pat (coerce-to <vector> needle)]
         [plen-1 (- (vector-length pat) 1)])
    (and (>= plen-1 0)
         ;; Match pattern[i] with elt; returns next i and a flag of whether
         ;; match is completed or not.  If the flag is #t, i is always equal
         ;; to the plen.
         ;; or #f if there's no more pattern to check (i.e. match)
         (^[elt i]
           (let loop ([i i])
             (if (test-proc elt (vector-ref pat i))
               (values (+ i 1) (= i plen-1))
               (let1 i (vector-ref restarts i)
                 (if (= i -1) (values 0 #f) (loop i)))))))))

;; Search NEEDLE from SEQ.  Returns index if found, #f if not.
;; The name is aligned to string-contains in srfi-13.
(define-method sequence-contains ((hay <sequence>) (needle <sequence>)
                                  :key ((:test test-proc) eqv?))
  (if-let1 stepper (sequence->kmp-stepper needle :test test-proc)
    (with-iterator [hay end? next]
      (let loop ([s 0] [i 0])
        (if (end?)
          #f
          (receive (i found) (stepper (next) i)
            (if found
              (- s i -1)
              (loop (+ s 1) i))))))
    0))

;; Search NEEDLE from LIS and split LIS right in front of found NEEDLE.
(define-method break-list-by-sequence! (lis (needle <sequence>)
                                            :key (test eqv?))
  (%break-list-1 lis needle test #f))
(define-method break-list-by-sequence (lis (needle <sequence>)
                                           :key (test eqv?))
  (%break-list-1 lis needle test #t))

(define (%break-list-1 lis needle test-proc copy?)
  (if-let1 stepper (sequence->kmp-stepper needle :test test-proc)
    (let loop ([cur lis]
               [prev #f] ; prev cell of cur
               [last #f] ; last cell before the current match
               [i 0])    ; index in needle
      (if (null? cur)
        (values lis '())
        (receive (i found) (stepper (car cur) i)
          (cond [found
                 (if last
                   (let1 head (cdr last)
                     (if copy?
                       (let loop ([p lis] [h '()] [t '()])
                         (cond [(eq? p head) (values h p)]
                               [(null? h) (let1 h (list (car p))
                                            (loop (cdr p) h h))]
                               [else (set-cdr! t (list (car p)))
                                     (loop (cdr p) h (cdr t))]))
                       (begin (set-cdr! last '())
                              (values lis head))))
                   (values '() lis))]
                [(= i 0) ; match failure - we'll start over fresh.
                 (loop (cdr cur) cur cur i)]
                [else
                 (loop (cdr cur) cur last i)]))))
    (values '() lis)))

;; prefix, suffix ------------------------------------------------

(define-method common-prefix-to ((class <class>)
                                 (a <sequence>)
                                 (b <sequence>)
                                 :key (key identity) (test eqv?))
  (with-builder (class add! get)
    (with-iterator (a a-end? a-next)
      (with-iterator (b b-end? b-next)
        (let loop ()
          (if (or (a-end?) (b-end?))
            (get)
            (let ([a1 (a-next)]
                  [b1 (b-next)])
              (if (test (key a1) (key b1))
                (begin (add! a1) (loop))
                (get)))))))))

(define-method common-prefix ((a <sequence>) (b <sequence>) . args)
  (apply common-prefix-to (class-of a) a b args))

;; TODO: suffix

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

