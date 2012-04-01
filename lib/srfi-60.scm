;;;
;;;  srfi-60 - integers as bits
;;;
;;;  Since most procedures are already supported as builtins, this
;;;  module only provides a thin wrapper to cover the complete srfi-60 spec.

(define-module srfi-60
  (use gauche.sequence)
  (use srfi-42)
  (export logand bitwise-and
          logior bitwise-ior
          logxor bitwise-xor
          lognot bitwise-not
          bitwise-if bitwise-merge
          logtest any-bits-set?
          logcount bit-count
          integer-length
          log2-binary-factors first-set-bit
          logbit? bit-set?
          copy-bit
          bit-field
          copy-bit-field
          ash arithmetic-shift
          rotate-bit-field
          reverse-bit-field
          integer->list
          list->integer
          booleans->integer))
(select-module srfi-60)

;;;
;;; Bitwise operators
;;;

(define logand (with-module gauche logand))
(define bitwise-and logand)
(define logior (with-module gauche logior))
(define bitwise-ior logior)
(define logxor (with-module gauche logxor))
(define bitwise-xor logxor)
(define lognot (with-module gauche lognot))
(define bitwise-not lognot)

(define (bitwise-if mask n0 n1)
  (logior (logand mask n0) (logand (lognot mask) n1)))
(define bitwise-merge bitwise-if)

(define logtest (with-module gauche logtest))
(define any-bits-set? logtest)

;;;
;;; Integer properties
;;;

(define logcount (with-module gauche logcount))
(define bit-count logcount)
(define integer-length (with-module gauche integer-length))

(define (log2-binary-factors n)
  (- (integer-length (logand n (- n))) 1))
(define first-set-bit log2-binary-factors)

;;;
;;; Bit within word
;;;

(define logbit? (with-module gauche logbit?))
(define bit-set? logbit?)
(define copy-bit (with-module gauche copy-bit))

;;;
;;; Field of bits
;;;

(define bit-field (with-module gauche bit-field))
(define copy-bit-field (with-module gauche copy-bit-field))
(define ash (with-module gauche ash))
(define arithmetic-shift ash)

(define (rotate-bit-field n count start end)
  (if (or (>= start end) (= count 0))
    n                                   ; trivial path
    (let* ([mask (logxor (- (expt 2 end) 1) (- (expt 2 start) 1))]
           [target (logand mask n)]
           [xcount (mod count (- end start))])
      (logior (logand (lognot mask) n)
              (logand mask
                      (logior (ash target xcount)
                              (ash target (- (- (- end start) xcount)))))))))

(define (reverse-bit-field n start end)
  (if (>= start end)
    n                                   ; trivial path
    (let1 mask (logxor (- (expt 2 end) 1) (- (expt 2 start) 1))
      (let loop ([m (logand n (lognot mask))]
                 [i start]
                 [j (- end 1)])
        (if (= i end)
          m
          (loop (copy-bit j m (logbit? i n)) (+ i 1) (- j 1)))))))

;;;
;;; Bits as booleans
;;;

(define (integer->list n :optional (len (integer-length n)))
  (list-ec (: i (- len 1) -1 -1) (logbit? i n)))

(define (list->integer lis)
  (cond [(null? lis) 0]
        [(<= (length lis) (integer-length (greatest-fixnum)))
         ;; fixnum range - it's faster to calculate intermediate results
         (fold (^(bit v) (+ v v (if bit 1 0))) 0 lis)]
        [else
         ;; bignum range - it's faster to create list of integers and merge
         (apply logior
                (fold-with-index (^(i bit ns) (if bit (cons (expt 2 i) ns) ns))
                                 '() (reverse lis)))]))

(define (booleans->integer . lis) (list->integer lis))


