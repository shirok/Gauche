;;;
;;; SRFI-158 Generators and Accumulators
;;;

(define-module srfi-158
  (use gauche.unicode)
  (use gauche.generator)
  (export generator circular-generator make-iota-generator make-range-generator 
          make-coroutine-generator list->generator vector->generator
          reverse-vector->generator string->generator
          bytevector->generator
          make-for-each-generator make-unfold-generator
          gcons* gappend gflatten ggroup gmerge gmap gcombine gfilter gremove
          gstate-filter ggroup generator-map->list
          gtake gdrop gtake-while gdrop-while
          gdelete gdelete-neighbor-dups gindex gselect
          generator->list generator->reverse-list generator-map->list
          generator->vector generator->vector!  generator->string
          generator-fold generator-for-each generator-find
          generator-count generator-any generator-every generator-unfold

          ;; accumulators
          make-accumulator count-accumulator list-accumulator
          reverse-list-accumulator vector-accumulator
          reverse-vector-accumulator vector-accumulator!
          string-accumulator bytevector-accumulator bytevector-accumulator!
          sum-accumulator product-accumulator))
(select-module srfi-158)

(define (make-accumulator kons knil finalizer)
  (^v
   (if (eof-object? v)
     (finalizer knil)
     (begin (set! knil (kons v knil)) (undefined)))))

(define (list-accumulator)
  (make-accumulator cons '() reverse))

(define (reverse-list-accumulator)
  (make-accumulator cons '() identity))

(define (vector-accumulator)
  (make-accumulator cons '() reverse-list->vector))

(define (reverse-vector-accumulator)
  (make-accumulator cons '() list->vector))

(define (vector-accumulator! vec at)
  (make-accumulator (^[v i]
                      (when (>= i (vector-length vec))
                        (error "vector is full"))
                      (vector-set! vec i v)
                      (+ i 1))
                    at
                    (^_ vec)))

(define (string-accumulator)
  (make-accumulator (^[c p] (write-char c p) p)
                    (open-output-string)
                    get-output-string))

(define (bytevector-accumulator)
  (make-accumulator (^[b p] (write-byte b p) p)
                    (open-output-string)
                    (^p (string->utf8 (get-output-string p)))))

(define (bytevector-accumulator! vec at)
  (assume-type vec <u8vector>)
  (make-accumulator (^[v i]
                      (when (>= i (uvector-length vec))
                        (error "bytevector is full"))
                      (u8vector-set! vec i v)
                      (+ i 1))
                    at
                    (^_ vec)))

(define (sum-accumulator)
  (make-accumulator + 0 identity))
(define (product-accumulator)
  (make-accumulator * 1 identity))
(define (count-accumulator)
  (make-accumulator (^[_ c] (+ c 1)) 0 identity))
