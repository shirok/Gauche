;;;
;;; gauche.generator - generator framework
;;;  
;;;   Copyright (c) 2011  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.generator
  (use srfi-1)
  (use gauche.sequence)
  (export list->generator vector->generator reverse-vector->generator
          string->generator
          bits->generator reverse-bits->generator
          file->generator file->sexp-generator file->char-generator
          file->line-generator file->byte-generator
          
          generator->list null-generator gcons gappend
          circular-generator gunfold giota grange
          gmap gtake gdrop gtake-while gdrop-while gfilter
          )
  )
(select-module gauche.generator)

;; EXPERIMENTAL - API may change.

;; GENERATOR is a thunk, that generates a value one at a time
;; for every invocation.  #<eof> is used to indicate the end
;; of the stream.  Standard input procedures like read or
;; read-char are generators as well.
;;
;; A generator can return more than one values; it is useful
;; to generate auxiliary information without consing.

;; The generic begin0 has the overhead of dealing with multiple
;; values, so we use faster version if possible.
(define-syntax %begin0
  (syntax-rules ()
    [(_ exp body ...) (rlet1 tmp exp body ...)]))

;; Some useful converters
(define (list->generator lis)
  (^() (if (null? lis) (eof-object) (pop! lis))))

(define (vector->generator vec)
  (let ([i 0] [len (vector-length vec)])
    (^() (if (>= i len) (eof-object) (%begin0 (vector-ref vec i) (inc! i))))))

(define (reverse-vector->generator vec)
  (let ([i (- (vector-length vec) 1)])
    (^() (if (< i 0) (eof-object) (%begin0 (vector-ref vec i) (dec! i))))))

(define (string->generator str)
  (let1 p (open-input-string str)
    (^() (read-char p))))

(define (bits->generator n)
  (let1 k (- (integer-length n) 1)
    (^() (if (< k 0) (eof-object) (%begin0 (logbit? k n) (dec! k))))))

(define (reverse-bits->generator n)
  (if (>= n 0)
    (^() (if (= n 0)  (eof-object) (%begin0 (odd? n) (set! n (ash n -1)))))
    (^() (if (= n -1) (eof-object) (%begin0 (odd? n) (set! n (ash n -1)))))))

(define (file->generator filename reader . open-args)
  ;; If the generator is abanboned before reaching EOF, we have to rely
  ;; on the GC to close the file.
  (let1 p (apply open-input-file filename open-args)
    (^() (if (not p)
           (eof-object)
           (rlet1 v (reader p)
             (when (eof-object? v)
               (close-input-port p)
               (set! p #f)))))))

(define (file->sexp-generator filename . open-args)
  (apply file->generator filename read open-args))
(define (file->char-generator filename . open-args)
  (apply file->generator filename read-char open-args))
(define (file->line-generator filename . open-args)
  (apply file->generator filename read-line open-args))
(define (file->byte-generator filename . open-args)
  (apply file->generator filename read-byte open-args))

(define (generator->list gen :optional (n #f))
  (if (integer? n)
    (let loop ([k 0] [r '()])
      (if (>= k n)
        (reverse r)
        (let1 v (gen)
          (if (eof-object? v)
            (reverse r)
            (loop (+ k 1) (cons v r))))))
    (let loop ([r '()])
      (let1 v (gen)
        (if (eof-object? v)
          (reverse r)
          (loop (cons v r)))))))

(define (circular-generator . args)
  (if (null? args)
    (error "circular-generator requires at least one argument")
    (let1 p args
      (^() (when (null? p) (set! p args)) (pop! p)))))

;; trivial, but for completeness
(define (null-generator) (eof-object))

(define (gunfold p f g seed :optional (tail-gen eof-object))
  (let1 end? #f
    (^() (cond [end? (tail-gen)]
               [(p seed) (set! end? #t) (tail-gen)]
               [else (%begin0 (f seed) (set! seed (g seed)))]))))

(define (giota num :optional (start 0) (step 1))
  (let1 val start
    (^() (if (<= num 0)
           (eof-object)
           (%begin0 val (inc! val step) (dec! num))))))

(define (grange :optional (start 0) (end +inf.0) (step 1))
  (let1 val start
    (^() (if (>= val end)
           (eof-object)
           (%begin0 val (inc! val step))))))

;; gcons :: (a, () -> a) -> (() -> a)
(define (gcons item gen)
  (let1 done #f
    (^() (if done (gen) (%begin0 item (set! done #t))))))

;; gappend :: [() -> a] -> (() -> a)
(define (gappend . gens)
  (^() (let rec ()
         (if (null? gens)
           (eof-object)
           (let1 v ((car gens))
             (cond [(eof-object? v) (pop! gens) (rec)]
                   [else v]))))))

;; gmap :: (a -> b, () -> a) -> (() -> b)
(define gmap
  (case-lambda
    [(fn gen) (^() (let1 v (gen) (if (eof-object? v) v (fn v))))]
    [(fn gen . more)
     (let1 gens (cons gen more)
       (^() (let1 vs (map (^f (f)) gens)
              (if (any eof-object? vs) (eof-object) (apply fn vs)))))]))

;; gfilter :: (a -> Bool, () -> a) -> (() -> a)
(define (gfilter pred gen)
  (^() (let loop ([v (gen)])
         (cond [(eof-object? v) v]
               [(pred v) v]
               [else (loop (gen))]))))

;; gtake :: (() -> a, Int) -> (() -> a)
;; gdrop :: (() -> a, Int) -> (() -> a)
;; TODO: what if the input generator is shorter than N?
;;   - work like take*/drop*.  In which case, what about fill? and padding
;;     arguments?
;;   - be consistent with srfi-1 take/drop, and to have gtake*/gdrop*
;;     separately.
(define (gtake gen n)
  (let1 k 0
    (^() (if (< k n) (begin (inc! k) (gen)) (eof-object)))))
(define (gdrop gen n)
  (let1 k 0
    (^() (when (< k n) (dotimes [i n] (inc! k) (gen))) (gen))))

;; gtake-while :: (a -> Bool, () -> a) -> (() -> a)
;; gdrop-while :: (a -> Bool, () -> a) -> (() -> a)
(define (gtake-while pred gen)
  (let1 end? #f
    (^() (if end?
           (eof-object)
           (let1 v (gen)
             (if (or (eof-object? v) (not (pred v)))
               (begin (set! end? #t) (eof-object))
               v))))))
(define (gdrop-while pred gen)
  (let1 found? #f
    (^() (if found?
           (gen)
           (let loop ([v (gen)])
             (cond [(eof-object? v) v]
                   [(pred v) (loop (gen))]
                   [else (set! found? #t) v]))))))

;; TODO:
;;  (->generator OBJ)
;;    a convenence coercer.  call appropriate x->generate function
;;    based on OBJ's type.
;;  (generator .... (yield value) ...)
;;    a macro to use "inversion of iterator" idiom to turn any loop
;;    construct to a generator.  The name and syntax is TBD.
;;  (gen-ec (: i ...) ...)
;;    srfi-42-ish macro to create generator.  it's not "eager", so
;;    the name needs to be reconsidered.
;;  (gflip-flop pred1 pred2 gen)
;;    flip-flop operator to extract a range from the input generator.
;;  multi-valued generators
;;    take a generator and creates a multi-valued generator which
;;    returns auxiliary info in extra values, e.g. item count.
;;  segmentation
;;    takes a predicate, and make {x ...} into {(x ...) ...}.
;;    e.g. character generator -> line generator, etc.
