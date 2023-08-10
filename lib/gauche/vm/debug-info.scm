;;;
;;; Debug info
;;;
;;;   Copyright (c) 2023  Shiro Kawai  <shiro@acm.org>
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

;; For precompiled code, we save debug-info in 'packed' format instead
;; of Scheme literals.  It reduces the size of generated C code and allows
;; faster loading time.
;; See src/gauche/code.h for the actual runtime format of debug-info.
;;
;; The 'packed' format consists of a constant vector and a nested
;; sequence of indexes.  A constant vector keeps Scheme objects other
;; than pairs and small integers.  A nested sequence of indexes encodes
;; one of the following:
;;  - Index to a constant vector
;;  - Index to a particular pair in the nested sequence
;;  - Small exact integer
;; Each integer is encoded into a variable-length octets, somewhat like
;; BER-encoded integers.
;;
;; The first octet encodes the type of the object and most significant 5
;; bits of index.
;;
;;   C00xxxxx   - A byte offset to the encoded pair
;;   C01xxxxx   - A constant vector index
;;   C10xxxxx   - An exact nonnegative integer
;;
;;   01100000   - Empty list.
;;   01100001   - Start of a list (sol)
;;   01100010   - End of a list (eol)
;;   01100011   - Indicates that the following item is the last cdr of
;;                the list.  Note that we won't have an eol in this case.
;;
;; If the 'C' bit in the first octet is set, the following octet encodes
;; further bits.
;;
;;   1xxxxxxx     Intermediate 7-bits
;;   0xxxxxxx     Last 7-bits
;;
;; Example: Suppose we have the following structure:
;;
;;  #0=(1 . #1=(2 #2=(3) #0# #1# . #2#))
;;
;; It is encoded as follows:
;;
;;   61      ; sol
;;   41      ; integer 1
;;   42      ; integer 2
;;   61      ; sol
;;   43      ; integer 3
;;   62      ; eol
;;   61      ; sol
;;   01      ; backreference to the first pair
;;   61      ; sol
;;   02      ; backreference to the second pair
;;   63      ; dot
;;   04      ; backreference to the third element
;;
;; NB: Backreference to a pair points to the byte offset of the 'car' element
;; of the pair.  If the last element is #x03, it would encode this structure
;; instead.
;;
;;  #0=(1 . #1=(2 . #2=((3) #0# #1# . #2#)))
;;
;; This also means we never have a back reference to offset 0.
;;

;; NB: Try not to depend on other modules, for this module can be
;; loaded if other modules hit an error, and it can create nasty
;; dependency issue.

(define-module gauche.vm.debug-info
  (export make-packed-debug-info
          encode-debug-info
          decode-debug-info)
  )
(select-module gauche.vm.debug-info)

;;
;; Constructor
;; Only to be used by the precompiler.

(define (make-packed-debug-info debug-info-alist)
  (receive (codev constv) (encode-debug-info debug-info-alist)
    ((with-module gauche.internal %make-packed-debug-info codev constv))))

;;
;; Encoder
;;

(define-class <encoder> ()
  ((out :init-form (open-output-string))    ;output string port
   (consts :init-form '())                  ;reverse list of constants
   (tab :init-form (make-hash-table 'eqv?)) ;pair -> index
   ))

(define (encoder-pos encoder) (port-tell (~ encoder'out)))
(define (encoder-num-consts encoder) (length (~ encoder'consts)))

(define (encode-index! encoder type index)
  (define (emit-bytes bytes)
    (if (null? (cdr bytes))
      (write-byte (car bytes) (~ encoder'out))
      (begin (write-byte (logior #x80 (car bytes)) (~ encoder'out))
             (emit-bytes (cdr bytes)))))
  (let loop ([n index] [rs '()])
    (if (< n #x20)
      (emit-bytes (cons (logior (ecase type
                                  [(pair) #x00]
                                  [(const) #x20]
                                  [(int) #x40])
                                n)
                        rs))
      (loop (ash n -7) (cons (logand n #x7f) rs)))))

(define (encode-item! encoder item)
  (define (lookup-pair pair)
    (or (hash-table-get  (~ encoder'tab) pair #f)
        (begin
          (hash-table-put! (~ encoder'tab) pair (encoder-pos encoder))
          #f)))
  (define (encode-marker! type)
    (ecase type
      [(null)  (write-byte #x60 (~ encoder'out))]
      [(start) (write-byte #x61 (~ encoder'out))]
      [(end)   (write-byte #x62 (~ encoder'out))]
      [(dot)   (write-byte #x63 (~ encoder'out))]))
  (define (encode-list! item)
    (encode-marker! 'start)
    (let loop ([lis item])
      (cond [(pair? lis)
             (if-let1 index (lookup-pair lis)
               (begin
                 (encode-marker! 'dot)
                 (encode-index! encoder 'pair index))
               (begin
                 (encode-item! encoder (car lis))
                 (loop (cdr lis))))]
            [(null? lis) (encode-marker! 'end)]
            [else (encode-marker! 'dot)
                  (encode-item! encoder lis)])))
  (define (encode-const! item)
    (let1 num-consts (encoder-num-consts encoder)
      (push! (~ encoder'consts) item)
      (encode-index! encoder 'const num-consts)))

  (cond [(null? item) (encode-marker! 'null)]
        [(pair? item) (encode-list! item)]
        [(and (exact-integer? item) (>= item 0))
         (encode-index! encoder 'int item)]
        [else (encode-const! item)]))

;; Returns bytevector and constant vector
(define (encode-debug-info obj)
  (let1 encoder (make <encoder>)
    (encode-item! encoder obj)
    (values
     (string->u8vector (get-output-string (~ encoder'out)))
     (list->vector (reverse (~ encoder'consts))))))

;;
;; Decoder
;;

#|
;; internal type
(define-class <decoder> ()
  ((buf :init-keyword :buf)        ;u8vector
   (consts :init-keyword :consts)  ;constant vector
   (pos :init-value 0)             ;next position to read
   (len)                           ;(u8vector-length buffer)
   (tab :init-form (make-hash-table 'eqv?)) ;index -> pair
   ))

(define-method initialize ((decoder <decoder>) initargs)
  (next-method)
  (set! (~ decoder'len) (uvector-length (~ decoder'buf))))

(define (next-byte decoder)
  (and (< (~ decoder'pos) (~ decoder'len))
       (begin0 (u8vector-ref (~ decoder'buf) (~ decoder'pos))
         (inc! (~ decoder'pos)))))

(define (peek-byte decoder)
  (and (< (~ decoder'pos) (~ decoder'len))
       (u8vector-ref (~ decoder'buf) (~ decoder'pos))))

(define (save-pair! decoder pair)
  (hash-table-put! (~ decoder'tab)
                   (~ decoder'pos)
                   pair))

(define (decode-index msbs cont? decoder)
  (if cont?
    (let loop ([r msbs] [b (next-byte decoder)])
      (cond [(not b) (error "Premature end of packed debug info")]
            [(< b 128) (logior (ash r 7) b)]
            [else (loop (logior (ash r 7) (logand b #x7f))
                        (next-byte decoder))]))
    msbs))

(define (decode-item decoder)
  (let1 b (next-byte decoder)
    (case (logand b #x60)
      [(#x00) (or ($ hash-table-get (~ decoder'tab)
                     (decode-index (logand b #x1f) (logbit? 7 b) decoder)
                     #f)
                  (error "Stray pair reference"))]
      [(#x20) (or ($ vector-ref (~ decoder'consts)
                     (decode-index (logand b #x1f) (logbit? 7 b) decoder))
                  (error "Constant vector index out-of-range"))]
      [(#x40) (decode-index (logand b #x1f) (logbit? 7 b) decoder)]
      [else
       (case b
         [(#x60) '()]                   ;emptylist
         [(#x61) (decode-list decoder)]
         [(#x62) (error "Stray end-of-list marker")]
         [(#x63) (error "Stray dot-list marker")]
         [else (error "Invalid octent:" b)])])))

(define (decode-list decoder)
  (let1 b (peek-byte decoder)
    (case b
      [(#x62) (inc! (~ decoder'pos)) '()]
      [(#x63) (inc! (~ decoder'pos)) (decode-item decoder)]
      [else (rlet1 pair (cons #f #f)
              (save-pair! decoder pair)
              (set-car! pair (decode-item decoder))
              (set-cdr! pair (decode-list decoder)))])))

(define (decode-debug-info bytevector constvector)
  (let1 decoder (make <decoder> :buf bytevector :consts constvector)
    (decode-item decoder)))
|#

(define (decode-debug-info bytevector constvector)
  ((with-module gauche.internal %decode-packed-debug-info)
   bytevector constvector))
