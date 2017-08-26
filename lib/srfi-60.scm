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

;; logand, logior, logxor, lognot, logtest - defined in gauche
(define-inline bitwise-and logand)
(define-inline bitwise-ior logior)
(define-inline bitwise-xor logxor)
(define-inline bitwise-not lognot)

(define (bitwise-if mask n0 n1)
  (logior (logand mask n0) (logand (lognot mask) n1)))
(define bitwise-merge bitwise-if)

(define-inline any-bits-set? logtest)

;;;
;;; Integer properties
;;;

;; logcount - defined in gauche
(define-inline bit-count logcount)

;; these two are the same as built-in twos-exponent-factor
(define-inline log2-binary-factors twos-exponent-factor)
(define-inline first-set-bit twos-exponent-factor)

;;;
;;; Bit within word
;;;

;; logbit?, copy-bit - defined in gauche
(define-inline bit-set? logbit?)

;;;
;;; Field of bits
;;;

;; bit-field, copy-bit-field, ash - defined in gauche
(define-inline arithmetic-shift ash)

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


