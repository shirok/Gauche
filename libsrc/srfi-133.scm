;;;
;;; srfi-133.scm - vector library
;;;
;;;   Copyright (c) 2016-2019  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi-133
  (use util.match)
  (export
   ;; constructors
   make-vector vector vector-unfold vector-unfold-right 
   vector-copy vector-reverse-copy 
   vector-append vector-concatenate vector-append-subvectors 

   ;; Predicates 
   vector? vector-empty? vector= 

   ;; Selectors 
   vector-ref vector-length 

   ;; Iteration 
   vector-fold vector-fold-right 
   vector-map vector-map! 
   vector-for-each vector-count 
   vector-cumulate 

   ;; Searching 
   vector-index vector-index-right 
   vector-skip vector-skip-right 
   vector-binary-search 
   vector-any vector-every 
   vector-partition 

   ;; Mutators 
   vector-set! vector-swap! 
   vector-fill! vector-reverse! 
   vector-copy! vector-reverse-copy! 
   vector-unfold! vector-unfold-right! 

   ;; Conversion 
   vector->list reverse-vector->list 
   list->vector reverse-list->vector 
   vector->string string->vector
   ))

(select-module srfi-133)

;; Built-ins:
;; make-vector vector vector-copy vector-append
;; vector? vector-ref vector-length
;; vector-map vector-map! vector-for-each
;; vector-set! vector-fill! vector-copy!
;; vector->list list->vector reverse-list->vector
;; vector->string string->vector

;; common checker.  returns start and end index.
;; if open-end? is #t, accept -1 as e, and we make it the length of vec.
(define (%vector-check-start+end v s e open-end?)
  (assume-type v <vector>)
  (let1 len (vector-length v)
    (unless (<= 0 s len)
      (errorf "start index (~s) out of range of a vector: ~s" s v))
    (unless (or (and open-end? (<= e len))
                (<= 0 e len))
      (errorf "end index (~s) out of range of a vector: ~s" e v))
    (unless (or (and open-end? (< e 0)) (<= s e))
      (errorf "start index (~s) is greater than end index (~s)" s e))
    (values s (if (and open-end? (< e 0)) len e))))

;; vector mutability check
;; currently Gauche doesn't track vector mutability (which is a bad thing)
;; eventually we'll check immutable vectors here.
(define-inline (%ensure-mutable v) (values))

(define %vector-unfold!
  (case-lambda
    [(f rvec s e)
     (let loop ([i s])
       (when (< i e)
         (let1 elt (f i)
           (vector-set! rvec i elt)
           (loop (+ i 1)))))]
    [(f rvec s e seed)
     (let loop ([i s] [seed seed])
       (when (< i e)
         (receive (elt seed) (f i seed)
           (vector-set! rvec i elt)
           (loop (+ i 1) seed))))]
    [(f rvec s e . seeds)
     (let loop ([i s] [seeds seeds])
       (when (< i e)
         (receive (elt . seeds) (apply f i seeds)
           (vector-set! rvec i elt)
           (loop (+ i 1) seeds))))]))

(define %vector-unfold-right!
  (case-lambda
    [(f rvec s e)
     (let loop ([i (- e 1)])
       (when (<= s i)
         (let1 elt (f i)
           (vector-set! rvec i elt)
           (loop (- i 1)))))]
    [(f rvec s e seed)
     (let loop ([i (- e 1)] [seed seed])
       (when (<= s i)
         (receive (elt seed) (f i seed)
           (vector-set! rvec i elt)
           (loop (- i 1) seed))))]
    [(f rvec s e . seeds)
     (let loop ([i (- e 1)] [seeds seeds])
       (when (<= s i)
         (receive (elt . seeds) (apply f i seeds)
           (vector-set! rvec i elt)
           (loop (- i 1) seeds))))]))

(define (vector-unfold f len . seeds)
  (rlet1 rvec (make-vector len)
    (apply %vector-unfold! f rvec 0 len seeds)))

(define (vector-unfold-right f len . seeds)
  (rlet1 rvec (make-vector len)
    (apply %vector-unfold-right! f rvec 0 len seeds)))

(define (vector-unfold! f rvec s e . seeds)
  (assume-type rvec <vector>)
  (%ensure-mutable rvec)
  (let1 len (vector-length rvec)
    (unless (<= 0 s len) (error "start index out of range:" s))
    (unless (<= 0 e len) (error "end index out of range:" e))
    (unless (<= s e)
      (errorf "start index (~s) is greater than end index (~s)" s e))
    (apply %vector-unfold! f rvec s e seeds)))

(define (vector-unfold-right! f rvec s e . seeds)
  (assume-type rvec <vector>)
  (%ensure-mutable rvec)
  (let1 len (vector-length rvec)
    (unless (<= 0 s len) (error "start index out of range:" s))
    (unless (<= 0 e len) (error "end index out of range:" e))
    (unless (<= s e)
      (errorf "start index (~s) is greater than end index (~s)" s e))
    (apply %vector-unfold-right! f rvec s e seeds)))

(define (%vector-reverse-copy! target tstart source sstart send)
  ;; assumes args are all valid
  (do ([i tstart (+ i 1)]
       [j (- send 1) (- j 1)])
      [(< j sstart)]
    (vector-set! target i (vector-ref source j))))

(define (vector-reverse-copy vec :optional (start 0) (end -1))
  (receive (s e) (%vector-check-start+end vec start end #t)
    (rlet1 rvec (make-vector (- e s))
      (%vector-reverse-copy! rvec 0 vec s e))))

(define (vector-reverse-copy! target tstart src :optional (sstart 0) (send -1))
  (assume-type target <vector>)
  (%ensure-mutable target)
  (let1 tlen (vector-length target)
    (unless (<= 0 tstart tlen)
      (errorf "target start index (~s) out of range of a target vector" tstart))
    (receive (s e) (%vector-check-start+end src sstart send #t)
      (unless (<= (- e s) (- tlen tstart))
        (errorf "source range (~s,~s) is greater than the target vector" s e))
      (if (and (eq? target src)
               (or (and (<= s tstart) (< tstart e))
                   (and (< tstart s) (< s (+ tstart (- e s))))))
        ;; overlapping self copy.  we need extra step not to overwrite
        ;; before copy.
        (begin
          (vector-copy! target tstart src s e)
          (%vector-reverse! target tstart (+ tstart (- e s))))
        (%vector-reverse-copy! target tstart src s e)))))

(define (vector-concatenate vecs) (apply vector-append vecs))

(define (vector-append-subvectors . args)
  (let* ([subvecs (slices args 3)]
         [len (fold (^[sv c]
                      (match sv
                        [(v s e)
                         (unless (vector? v)
                           (error "vector expected, but got:" v))
                         (let1 l (vector-length v)
                           (unless (and (<= 0 s l) (<= 0 e l) (<= s e))
                             (errorf "invalid subvector range, \
                                     start:~s end:~s subvec:~s" s e v))
                           (+ c (- e s)))]
                        [_ (error "length of argument is not a multiple of 3:"
                                  args)]))
                    0 subvecs)])
    (rlet1 rvec (make-vector len)
      (fold (^[sv c]
              (match-let1 (v s e) sv
                (vector-copy! rvec c v s e)
                (+ c (- e s))))
            0 subvecs))))

(define (vector-empty? vec)
  (assume-type vec <vector>)
  (zero? (vector-length vec)))

(define (vector= elt= . vecs)
  (match vecs
    [() #t]
    [(vec) (assume-type vec <vector>) #t]
    [(v . vecs)
     ;; we could use srfi-114 make-vector-comparator but the overhead of
     ;; creating a comparator might be too much.
     (assume-type v <vector>)
     (let1 len (vector-length v)
       (define (pairwise= va vb)
         (let loop ([i 0])
           (or (= i len)
               (and (elt= (vector-ref va i) (vector-ref vb i))
                    (loop (+ i 1))))))
       (and (every (^w (= (vector-length w) len)) vecs)
            (every pairwise= (cons v vecs) vecs)))]))

(define vector-fold
  (case-lambda
    ([proc seed v] ; fast path
     (assume-type v <vector>)
     (let1 len (vector-length v)
       (let loop ([i 0] [seed seed])
         (if (= i len) seed (loop (+ i 1) (proc seed (vector-ref v i)))))))
    ([proc seed v . vs]
     (let* ([vs (cons v vs)]
            [len (fold (^[v len]
                         (assume-type v <vector>)
                         (if len (min (vector-length v) len) (vector-length v)))
                       #f vs)])
       (let loop ([i 0] [seed seed])
         (if (= i len)
           seed
           (loop (+ i 1) (apply proc seed (map (cut vector-ref <> i) vs)))))))))

(define vector-fold-right
  (case-lambda
    ([proc seed v] ; fast path
     (assume-type v <vector>)
     (let1 len (vector-length v)
       (let loop ([i (- len 1)] [seed seed])
         (if (= i -1) seed (loop (- i 1) (proc seed (vector-ref v i)))))))
    ([proc seed v . vs]
     (let* ([vs (cons v vs)]
            [len (fold (^[v len]
                         (assume-type v <vector>)
                         (if len (min (vector-length v) len) (vector-length v)))
                       #f vs)])
       (let loop ([i (- len 1)] [seed seed])
         (if (= i -1)
           seed
           (loop (- i 1) (apply proc seed (map (cut vector-ref <> i) vs)))))))))

(define vector-count
  (case-lambda
    ([pred v] ; fast path
     (vector-fold (^[c e] (if (pred e) (+ 1 c) c)) 0 v))
    ([pred v . vs]
     (apply vector-fold (^[c . es] (if (apply pred es) (+ 1 c) c)) 0 v vs))))

(define (vector-cumulate f seed vec)
  (assume-type vec <vector>)
  (let1 len (vector-length vec)
    (rlet1 rvec (make-vector len)
      (let loop ([i 0] [seed seed])
        (when (< i len)
          (let1 e (f seed (vector-ref vec i))
            (vector-set! rvec i e)
            (loop (+ i 1) e)))))))

(define vector-index
  (case-lambda
    ([pred v] ; fast path
     (assume-type v <vector>)
     (let1 len (vector-length v)
       (let lp ([i 0])
         (cond [(= i len) #f] [(pred (vector-ref v i)) i] [else (lp (+ i 1))]))))
    ([pred v . vs]
     (let* ([vs (cons v vs)]
            [len (fold (^[v len]
                         (assume-type v <vector>)
                         (if len
                           (min (vector-length v) len)
                           (vector-length v)))
                       #f vs)])
       (let lp ([i 0])
         (cond [(= i len) #f]
               [(apply pred (map (cut vector-ref <> i) vs)) i]
               [else (lp (+ i 1))]))))))

(define vector-index-right
  (case-lambda
    ([pred v] ; fast path
     (assume-type v <vector>)
     (let1 len (vector-length v)
       (let lp ([i (- len 1)])
         (cond [(= i -1) #f] [(pred (vector-ref v i)) i] [else (lp (- i 1))]))))
    ([pred v . vs]
     (let* ([vs (cons v vs)]
            [len (fold (^[v len]
                         (assume-type v <vector>)
                         (if len
                           (min (vector-length v) len)
                           (vector-length v)))
                       #f vs)])
       (let lp ([i (- len 1)])
         (cond [(= i -1) #f]
               [(apply pred (map (cut vector-ref <> i) vs)) i]
               [else (lp (- i 1))]))))))

(define (vector-skip pred v . vs)
  (apply vector-index (complement pred) v vs))

(define (vector-skip-right pred v . vs)
  (apply vector-index-right (complement pred) v vs))

(define (vector-binary-search vec value cmp :optional (start 0) (end -1))
  (assume-type vec <vector>)
  (receive (s e) (%vector-check-start+end vec start end #t)
    (and (not (= s e))
         (let rec ([lo s] [hi e])
           (let* ([mid (ash (+ lo hi) -1)]
                  [r (cmp (vector-ref vec mid) value)])
             (cond [(zero? r) mid]
                   [(= lo mid) #f]
                   [(< r 0) (rec mid hi)]
                   [else (rec lo mid)]))))))
     
(define vector-any
  (case-lambda
    ([pred v]
     (assume-type v <vector>)
     (let1 len (vector-length v)
       (let loop ([i 0])
         (if (= i len)
           #f
           (or (pred (vector-ref v i))
               (loop (+ i 1)))))))
    ([pred v . vs]
     (let* ([vs (cons v vs)]
            [len (fold (^[v len]
                         (assume-type v <vector>) (min (vector-length v) len))
                       +inf.0 vs)])
       (let loop ([i 0])
         (if (= i len)
           #f
           (or (apply pred (map (cut vector-ref <> i) vs))
               (loop (+ i 1)))))))))

(define vector-every
  (case-lambda
    ([pred v]
     (assume-type v <vector>)
     (let1 len (vector-length v)
       (let loop ([i 0] [last #t])
         (if (= i len)
           last
           (and-let1 last (pred (vector-ref v i))
             (loop (+ i 1) last))))))
    ([pred v . vs]
     (let* ([vs (cons v vs)]
            [len (fold (^[v len]
                         (assume-type v <vector>) (min (vector-length v) len))
                       +inf.0 vs)])
       (let loop ([i 0] [last #t])
         (if (= i len)
           last
           (and-let1 last (apply pred (map (cut vector-ref <> i) vs))
             (loop (+ i 1) last))))))))

(define (vector-partition pred vec)
  (assume-type vec <vector>)
  (let* ([len (vector-length vec)]
         [rvec (make-vector len)])
    (let loop ([i 0] [j 0] [fails '()])
      (if (= i len)
        (do ([k (- len 1) (- k 1)]
             [fails fails (cdr fails)])
            [(null? fails) (values rvec j)]
          (vector-set! rvec k (car fails)))
        (let1 elt (vector-ref vec i)
          (if (pred elt)
            (begin (vector-set! rvec j elt)
                   (loop (+ i 1) (+ j 1) fails))
            (loop (+ i 1) j (cons elt fails))))))))

(define (%vector-swap! vec i j)
  (let1 tmp (vector-ref vec i)
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j tmp)))

(define (vector-swap! vec i j)
  (assume-type vec <vector>)
  (%ensure-mutable vec)
  (let1 max (- (vector-length vec) 1)
    (unless (<= 0 i max) (error "index i is out of range:" i))
    (unless (<= 0 j max) (error "index j is out of range:" j))
    (%vector-swap! vec i j)))

(define (%vector-reverse! vec s e)
  (do ([i s (+ i 1)]
       [j (- e 1) (- j 1)])
      [(<= j i)]
    (%vector-swap! vec i j)))

(define (vector-reverse! vec :optional (start 0) (end -1))
  (assume-type vec <vector>)
  (%ensure-mutable vec)
  (receive (s e) (%vector-check-start+end vec start end #t)
    (%vector-reverse! vec s e)))
  
(define (reverse-vector->list vec :optional (start 0) (end -1))
  (assume-type vec <vector>)
  (receive (s e) (%vector-check-start+end vec start end #t)
    (do ([i s (+ i 1)]
         [r '() (cons (vector-ref vec i) r)])
        [(= i e) r]
      )))
