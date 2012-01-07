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
  (use gauche.partcont)
  (export list->generator vector->generator reverse-vector->generator
          string->generator
          bits->generator reverse-bits->generator
          file->generator file->sexp-generator file->char-generator
          file->line-generator file->byte-generator
          x->generator generate
          
          generator->list null-generator gcons* gappend
          circular-generator gunfold giota grange
          gmap gmap-accum gfilter gfilter-map gstate-filter
          gtake gdrop gtake-while gdrop-while 
          ))
(select-module gauche.generator)

;; GENERATOR is a thunk, that generates a value one at a time
;; for every invocation.  #<eof> is used to indicate the end
;; of the stream.  Standard input procedures like read or
;; read-char are generators as well.
;;
;; Possible future extension: a generator may return more than
;; one values; it is useful to generate auxiliary information
;; without consing.

;; The generic begin0 has the overhead of dealing with multiple
;; values, so we use faster version if possible.
(define-syntax %begin0
  (syntax-rules ()
    [(_ exp body ...) (rlet1 tmp exp body ...)]))

;;;
;;; Converters and constructors
;;;

;; Some useful converters
(define (list->generator lis :optional (start #f) (end #f))
  (let1 start (or start 0)
    (cond [(> start 0)
           (list->generator (drop* lis start) 0 (and end (- end start)))]
          [end  (^[] (if (or (null? lis) (<= end 0))
                       (eof-object)
                       (begin (dec! end) (pop! lis))))]
          [else (^[] (if (null? lis) (eof-object) (pop! lis)))])))

(define (vector->generator vec :optional (start #f) (end #f))
  (let ([i (or start 0)] [len (or end (vector-length vec))])
    (^[] (if (>= i len) (eof-object) (%begin0 (vector-ref vec i) (inc! i))))))

(define (reverse-vector->generator vec :optional (start #f) (end #f))
  (let ([start (or start 0)]
        [i (- (or end (vector-length vec)) 1)])
    (^[] (if (< i start) (eof-object) (%begin0 (vector-ref vec i) (dec! i))))))

(define (string->generator str :optional (start #f) (end #f))
  (let1 p (open-input-string
           ((with-module gauche.internal %maybe-substring) str start end))
    (^[] (read-char p))))

(define (bits->generator n :optional (start #f) (end #f))
  (let* ([len-1 (- (integer-length n) 1)]
         [k     (- len-1 (or start 0))]
         [end   (if end (- len-1 end) -1)])
    (^[] (if (<= k end) (eof-object) (%begin0 (logbit? k n) (dec! k))))))

(define (reverse-bits->generator n :optional (start #f) (end #f))
  (let* ([len-1 (- (integer-length n) 1)]
         [start (- len-1 (or start 0))]
         [k     (if end (- len-1 end -1) 0)])
    (^[] (if (> k start) (eof-object) (%begin0 (logbit? k n) (inc! k))))))

(define (file->generator filename reader . open-args)
  ;; If the generator is abanboned before reaching EOF, we have to rely
  ;; on the GC to close the file.
  (let1 p (apply open-input-file filename open-args)
    (^[] (if (not p)
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

;; generic version
(define-method x->generator ((obj <list>))   (list->generator obj))
(define-method x->generator ((obj <vector>)) (vector->generator obj))
(define-method x->generator ((obj <string>)) (string->generator obj))
(define-method x->generator ((obj <integer>)) (bits->generator obj))

(define-method x->generator ((obj <collection>))
  (generate (^[yield] (call-with-iterator obj (^v (yield v))))))

;; Many generator-filters can take collections as input and
;; implicitly coerce it to a generator.  This is to avoid generic
;; function dispatch for common objects.
(define (%->gen x)
  (cond [(procedure? x) x]
        [(pair? x)   (list->generator x)]
        [(vector? x) (vector->generator x)]
        [(string? x) (string->generator x)]
        [(is-a? x <collection>) (x->generator x)]
        [(not x)     null-generator] ; avoid returning #f not to confuse %ensure-gen!
        [else x]))

(define (%->gens gens)
  (define (rec gens)
    (if (null? (cdr gens))
      (if (procedure? (car gens)) gens (list (%->gen (car gens))))
      (let1 tail (rec (cdr gens))
        (if (and (eq? tail (cdr gens)) (procedure? (car gens)))
          gens
          (cons (%->gen (car gens)) tail)))))
  (if (null? gens) '() (rec gens)))

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

;; Other constructors
(define (circular-generator . args)
  (if (null? args)
    (error "circular-generator requires at least one argument")
    (let1 p args
      (^[] (when (null? p) (set! p args)) (pop! p)))))

;; trivial, but for completeness
(define (null-generator) (eof-object))

;; procedural generation
(define (gunfold p f g seed :optional (tail-gen eof-object))
  (let1 end? #f
    (^[] (cond [end? (tail-gen)]
               [(p seed) (set! end? #t) (tail-gen)]
               [else (%begin0 (f seed) (set! seed (g seed)))]))))

(define (giota num :optional (start 0) (step 1))
  (let1 val start
    (^[] (if (<= num 0)
           (eof-object)
           (%begin0 val (inc! val step) (dec! num))))))

(define (grange :optional (start 0) (end +inf.0) (step 1))
  (let1 val start
    (^[] (if (>= val end)
           (eof-object)
           (%begin0 val (inc! val step))))))

;;;
;;; Generator operations
;;;


;; gcons* :: (a, ..., () -> a) -> (() -> a)
(define gcons*
  (case-lambda
    [() (error "gcons* needs at least one argument")]
    [(gen) (%->gen gen)]
    [(item gen)
     (let ([done #f] [g (%->gen gen)])
       (^[] (if done (g) (begin (set! done #t) item))))]
    [args
     (let* ([lp (last-pair args)] [g (%->gen (car lp))])
       (^[] (if (eq? args lp) (g) (pop! args))))]))

;; gappend :: [() -> a] -> (() -> a)
(define (gappend . gens)
  (let ([gs gens] [g #f])
    (if (null? gs)
      null-generator
      (rec (f)
        (unless g (set! g (%->gen (pop! gs))))
        (let1 v (g)
          (cond [(not (eof-object? v)) v]
                [(null? gs) v]          ;exhausted
                [else (set! g #f) (f)]))))))

;; gmap :: (a -> b, () -> a) -> (() -> b)
(define gmap
  (case-lambda
    [(fn gen)
     (let1 g (%->gen gen)
       (^[] (let1 v (g) (if (eof-object? v) v (fn v)))))]
    [(fn gen . more)
     (let1 gs (%->gens (cons gen more))
       (^[] (let1 vs (map (^f (f)) gs)
              (if (any eof-object? vs) (eof-object) (apply fn vs)))))]))

;; gmap-accum :: ((a,b) -> (c,b), b, () -> a) -> (() -> c)
(define gmap-accum
  (case-lambda
    [(fn seed gen)
     (let1 g (%->gen gen)
       (^[] (let1 v (g)
              (if (eof-object? v)
                v
                (receive (v_ seed_) (fn v seed)
                  (set! seed seed_)
                  v_)))))]
    [(fn seed gen . more)
     (let1 gs (%->gens (cons gen more))
       (^[] (let1 vs (fold-right (^[g s] (if (eof-object? s)
                                           s
                                           (let1 v (g)
                                             (if (eof-object? v)
                                               v
                                               (cons v s)))))
                                 (list seed) gs)
              (if (eof-object? vs)
                (eof-object)
                (receive (v_ seed_) (apply fn vs)
                  (set! seed seed_)
                  v_)))))]))

;; gfilter :: (a -> Bool, () -> a) -> (() -> a)
(define (gfilter pred gen)
  (let1 gen (%->gen gen)
    (^[] (let loop ([v (gen)])
           (cond [(eof-object? v) v]
                 [(pred v) v]
                 [else (loop (gen))])))))

;; gfilter-map :: (a -> b, () -> a) -> (() -> b)
(define gfilter-map
  (case-lambda
    [(fn gen)
     (let1 gen (%->gen gen)
       (^[] (let loop ([v (gen)])
              (cond [(eof-object? v) v]
                    [(fn v)]
                    [else (loop (gen))]))))]
    [(fn gen . more)
     (let1 gens (%->gens (cons gen more))
       (^[] (let loop ()
              (let1 vs (map (^f (f)) gens)
                (cond [(any eof-object? vs) (eof-object)]
                      [(apply fn vs)]
                      [else (loop)])))))]))

;; gstate-filter :: ((a,b) -> (Bool,b), b, () -> a) -> (() -> a)
(define (gstate-filter proc seed gen)
  (let1 gen (%->gen gen)
    (^[] (let loop ([v (gen)])
           (if (eof-object? v)
             v
             (receive (flag seed1) (proc v seed)
               (set! seed seed1)
               (if flag v (loop (gen)))))))))

;; gtake :: (() -> a, Int) -> (() -> a)
;; gdrop :: (() -> a, Int) -> (() -> a)
(define (gtake gen n :optional (fill? #f) (padding #f))
  (let ([k 0] [gen (%->gen gen)])
    (if fill?
      (^[] (if (< k n)
             (let1 v (gen)
               (inc! k)
               (if (eof-object? v) padding v))
             (eof-object)))
      (^[] (if (< k n) (begin (inc! k) (gen)) (eof-object))))))
(define (gdrop gen n)
  (let ([k 0] [gen (%->gen gen)])
    (^[] (when (< k n) (dotimes [i n] (inc! k) (gen))) (gen))))

;; gtake-while :: (a -> Bool, () -> a) -> (() -> a)
;; gdrop-while :: (a -> Bool, () -> a) -> (() -> a)
(define (gtake-while pred gen)
  (let ([end? #f] [gen (%->gen gen)])
    (^[] (if end?
           (eof-object)
           (let1 v (gen)
             (if (or (eof-object? v) (not (pred v)))
               (begin (set! end? #t) (eof-object))
               v))))))
(define (gdrop-while pred gen)
  (let ([found? #f] [gen (%->gen gen)])
    (^[] (if found?
           (gen)
           (let loop ([v (gen)])
             (cond [(eof-object? v) v]
                   [(pred v) (loop (gen))]
                   [else (set! found? #t) v]))))))

;; generate :: ((a -> ()) -> ()) -> (() -> a)
(define (generate proc)
  (define (cont)
    (reset (proc (^[value] (shift k (set! cont k) value)))
           (set! cont null-generator)
           (eof-object)))
  (^[] (cont)))

;; TODO:
;;  (gen-ec (: i ...) ...)
;;    srfi-42-ish macro to create generator.  it's not "eager", so
;;    the name needs to be reconsidered.
;;  (gflip-flop pred1 pred2 gen)
;;    flip-flop operator to extract a range from the input generator.
;;  multi-valued generators
;;    take a generator and creates a multi-valued generator which
;;    returns auxiliary info in extra values, e.g. item count.
;;  gsegment :: ((a,b) -> (a,Bool), a) -> (() -> [b])
