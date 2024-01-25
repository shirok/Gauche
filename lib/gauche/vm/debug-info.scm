;;;
;;; Debug info
;;;
;;;   Copyright (c) 2023-2024  Shiro Kawai  <shiro@acm.org>
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
;; The very first octet of the code vector is packed format version.
;; It is 0 in the current version.
;;
;; Following is a bitstream.  We use variable bit encoding to have compact
;; representatino of codes.
;;
;;   000       - Start of a list (sol)
;;   001       - End of a list (eol)
;;
;;   0100      - Indicates that the following item is the last cdr of
;;                the list.  Note that we won't have an eol in this case.
;;   0101      - Empty list
;;
;;   011xxxxx  - Predefined values
;;               The top 32 values that appear most frequently
;;               in the debug info of Gauche source itself has a special
;;               pre-assigned tag.
;;
;;   10 + index  - Constant reference
;;
;;   110 + bitindex - A bit offset (excluding version octet) to the encoded pair
;;   111 + index - An exact nonnegative integer
;;
;; Index encoding:
;;   0 + 7bit   - up to 128
;;   1 + 7bit + (1 + 5bit)* + (0 + 5bit)
;;
;; Bit index encoding:
;;   0 + 10bit   - up to 1024
;;   1 + 10bit + (1 + 5bit)* + (0 + 5bit)
;;
;; Example: Suppose we have the following structure:
;;
;;  #0=(1 . #1=(2 #2=(3) #0# #1# . #2#))
;;
;; It is encoded as follows:
;;
;;  Bitpos  Code
;;     0    000               ; sol
;;     3    111 0 0000001     ; integer 1
;;    14    111 0 0000010     ; integer 2
;;    25    000               ; sol
;;    28    111 0 0000011     ; integer 3
;;    39    001               ; eol
;;    42    000               ; sol
;;    45    110 0 0000000011  ; backreference to the first pair
;;    59    000               ; sol
;;    62    110 0 0000001110  ; backreference to the second pair
;;    76    0100              ; dot
;;    80    110 0 0000011100  ; backreference to the third element
;;    94
;;
;; NB: Backreference to a pair points to the bit offset of the 'car' element
;; of the pair.  If the last element bitindex is 25, it would encode this
;; structure instead.
;;
;;  #0=(1 . #1=(2 . #2=((3) #0# #1# . #2#)))
;;
;; This also means we never have a back reference to offset 0.
;;

;; NB: Try not to depend on other modules, for this module can be
;; loaded if other modules hit an error, and it can create nasty
;; dependency issue.

(define-module gauche.vm.debug-info
  (export encode-debug-info
          get-debug-info-const-vector
          has-unit-debug-info?
          decode-debug-info
          keep-debug-info-stat
          record-debug-info-stat!
          show-debug-info-stat)
  )
(select-module gauche.vm.debug-info)

(define-constant *debug-info-version* 0) ;must match the decoder

;; The 32 most frequently appearing constants.  The decoder in
;; code.c has the same list and they must match.
;; In Gauche source, first 7 appear in 93% of the compliation units;
;; even the last one in 63% of the units.
(define-constant *predefined-values*
  '(source-info quote definition lambda let if define error car else cdr null?
    cond let1 #f unless and loop ^ cons + apply or cadr when not begin eq? cddr
    list #t with-module))

;; Per-unit data.  Not for public use.
;;  - Allow to share constant vector among code blocks.
;;  - Stats are gathered per unit, if requested.

(define-class <unit-debug-info> ()
  ((consts :init-form (make-hash-table 'equal?))
   (stat)))

(define-method initialize ((a <unit-debug-info>) initargs)
  (next-method)
  (set! (~ a'stat)
        (and (keep-debug-info-stat)
             (make <debug-info-stat>))))

(define (ensure-unit-debug-info! unit)
  (unless (~ unit'debug-info-bin)
    (set! (~ unit'debug-info-bin) (make <unit-debug-info>))))

(define (has-unit-debug-info? unit)
  (boolean (~ unit'debug-info-bin)))

;; Returns const index
(define (register-const! unit obj)
  (let1 tab (~ unit'debug-info-bin'consts)
    (or (hash-table-get tab obj #f)
        (rlet1 ind (hash-table-num-entries tab)
          (hash-table-put! tab obj ind)))))

;; API
;;  Get the final constant vector
(define (get-debug-info-const-vector unit)
  ($ list->vector
     $ map car
     $ sort-by (hash-table->alist (~ unit'debug-info-bin'consts)) cdr))

;; Gather debug-info statistics
;;   This is purely internal to gather information for optimizing
;;   packed debug info.

(define-class <debug-info-stat> ()
  ((num-instances :init-value 0)
   (total-code-size :init-value 0)
   (total-const-size :init-value 0)
   (code-freq-table :init-form (make-hash-table 'eqv?))
   (const-freq-table :init-form (make-hash-table 'equal?))))

(define keep-debug-info-stat
  (make-parameter (boolean (sys-getenv "GAUCHE_DEBUG_INFO_STAT"))))

(define (record-debug-info-stat! unit codevec constvec)
  (and-let1 stat (~ unit'debug-info-bin'stat)
    (let ([codesize (uvector-length codevec)]
          [constsize (if (vector? constvec) (vector-length constvec) 0)])
      (inc! (~ stat'num-instances))
      (inc! (~ stat'total-code-size) codesize)
      (inc! (~ stat'total-const-size) constsize)
      (do ([i 0 (+ i 1)])
          [(eqv? i codesize)]
        (hash-table-update! (~ stat'code-freq-table)
                            (uvector-ref codevec i)
                            (cut + <> 1)
                            0))
      (when (vector? constvec)
        (do ([i 0 (+ i 1)])
            [(eqv? i constsize)]
          (hash-table-update! (~ stat'const-freq-table)
                              (vector-ref constvec i)
                              (cut + <> 1)
                              0))))
    #t))

;; API to be called from precomp.
(define (show-debug-info-stat unit)
  (define (top-n tab n)
    (take* (sort-by (hash-table->alist tab) cdr >) n))
  (and-let* ([ unit ]
             [ (~ unit'debug-info-bin) ]
             [stat (~ unit'debug-info-bin'stat)])
    (set! (~ stat'total-const-size)
          (vector-length (get-debug-info-const-vector unit)))
    (format #t "Debug info stats (~s):\n" (~ unit'name))
    (format #t "  # of code blocks w/ debug-info: ~5d\n" (~ stat'num-instances))
    (format #t "   code vector size: ~6d total; ~8,2f avg\n"
            (~ stat'total-code-size)
            (/ (~ stat'total-code-size) (~ stat'num-instances)))
    (format #t "  const vector size: ~6d total; ~8,2f avg\n"
            (~ stat'total-const-size)
            (/ (~ stat'total-const-size) (~ stat'num-instances)))
    (format #t "  top 10 frequent codes:\n")
    (dolist [p (top-n (~ stat'code-freq-table) 10)]
      (format #t "       ~4d : #x~2,'0x\n" (cdr p) (car p)))
    (format #t "  const vector:\n")
    (pprint (get-debug-info-const-vector unit))
    ))

;;
;; Encoder
;;

;; Transient structure
(define-class <encoder> ()
  ((out :init-form (open-output-string))    ;output string port
   (bitbuf :init-form (make-vector 8 0))
   (bitpos :init-form 0)
   (unit :init-keyword :unit)               ;<cgen-unit>
   (consts :init-form '())                  ;reverse list of constants
   (tab :init-form (make-hash-table 'eqv?)) ;pair -> index
   ))

(define (make-encoder unit)
  (rlet1 enc (make <encoder> :unit unit)
    (write-byte *debug-info-version* (~ enc'out))))

(define (encoder-pos encoder) (~ encoder'bitpos))

(define (close-encoder encoder)
  (let1 npads (- 8 (modulo (~ encoder'bitpos) 8))
    (when (> npads 0)
      (emit* encoder (make-list npads 0)))))

;; bits :: (0 | 1)*
(define (emit* encoder bits)
  (define (emit-1 bit)
    (let1 bitmod (modulo (~ encoder'bitpos) 8)
      (set! (~ encoder'bitbuf bitmod) bit)
      (inc! (~ encoder'bitpos))
      (when (= bitmod 7)
        (flush-bitbuf))))
  (define (flush-bitbuf)
    (write-byte (+ (ash (~ encoder'bitbuf 0) 7)
                   (ash (~ encoder'bitbuf 1) 6)
                   (ash (~ encoder'bitbuf 2) 5)
                   (ash (~ encoder'bitbuf 3) 4)
                   (ash (~ encoder'bitbuf 4) 3)
                   (ash (~ encoder'bitbuf 5) 2)
                   (ash (~ encoder'bitbuf 6) 1)
                   (ash (~ encoder'bitbuf 7) 0))
                (~ encoder'out)))
  (for-each emit-1 bits))

(define (emit-i encoder prefix width n)
  (do ([c 0 (+ c 1)]
       [bits '() (cons (logand (ash n (- c)) 1) bits)])
      [(= c width) (emit* encoder (if prefix
                                    (cons prefix bits)
                                    bits))]))

(define (encode-index! encoder type index)
  (define leading-bits (ecase type [(pair) 10] [(const int) 7]))
  (define bit-chunks                    ;((int . width) ...)
    (let loop ([n index] [cs '()])
      (if (< n (ash 1 leading-bits))
        (acons n leading-bits cs)
        (loop (ash n -5) (acons (logand n #x1f) 5 cs)))))
  (case type
    [(const) (emit* encoder '(1 0))]
    [(pair)  (emit* encoder '(1 1 0))]
    [(int)   (emit* encoder '(1 1 1))])
  (let loop ([bit-chunks bit-chunks])
    (if (null? (cdr bit-chunks))
      (emit-i encoder 0 (cdar bit-chunks) (caar bit-chunks))
      (begin
        (emit-i encoder 1 (cdar bit-chunks) (caar bit-chunks))
        (loop (cdr bit-chunks))))))

(define (encode-item! encoder item)
  (define (lookup-pair pair)
    (or (hash-table-get  (~ encoder'tab) pair #f)
        (begin
          (hash-table-put! (~ encoder'tab) pair (encoder-pos encoder))
          #f)))
  (define (encode-marker! type)
    (ecase type
      [(start) (emit* encoder '(0 0 0))]
      [(end)   (emit* encoder '(0 0 1))]
      [(dot)   (emit* encoder '(0 1 0 0))]
      [(null)  (emit* encoder '(0 1 0 1))]))
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
    (let1 index (register-const! (~ encoder'unit) item)
      (encode-index! encoder 'const index)))
  (define (predefined-index item)
    ;; avoid using find-with-index for dependency issue
    (let loop ([vs *predefined-values*] [i 0])
      (cond [(null? vs) #f]
            [(eqv? (car vs) item) i]
            [else (loop (cdr vs) (+ i 1))])))
  (cond [(null? item) (encode-marker! 'null)]
        [(pair? item) (encode-list! item)]
        [(and (exact-integer? item) (>= item 0))
         (encode-index! encoder 'int item)]
        [(predefined-index item)
         => (^i (emit* encoder '(0 1 1)) (emit-i encoder #f 5 i))]
        [else (encode-const! item)]))

;; Returns the encoded codevector.
;; Constants are saved in the debug-info-bin attached to the unit, and can
;; be obtained by get-debug-info-const-vector.
(define (encode-debug-info unit obj)
  (ensure-unit-debug-info! unit)
  (let1 encoder (make-encoder unit)
    (encode-item! encoder obj)
    (close-encoder encoder)
    (string->u8vector (get-output-string (~ encoder'out)))))

;;
;; Decoder
;;

;; Decoder is implemeneted in C (code.c), for the decoder may be
;; called during examining VM so it shouldn't change VM state.

(define (decode-debug-info bytevector constvector)
  ((with-module gauche.internal %decode-packed-debug-info)
   bytevector constvector))

;;
;; Utility
;;  Generate switch statement from *predefined-values* for the decoder in
;;  code.c

(define (generate-predefined-switch)
  (define (gen val n)
    (cond [(symbol? val)
           (format #t "case ~2d: return SCM_SYM_~a;\n" n
                   (regexp-replace-all*
                    (symbol->string val)
                    #/\?/ "P"
                    #/-/ "_"
                    #/\^/ "CARET"
                    #/\+/ "PLUS"
                    #/[a-z]/ (^m (string (char-upcase (string-ref (m 0) 0))))))]
          [(not val) (format #t "case ~2d: return SCM_FALSE;\n" n)]
          [(eqv? val #t) (format #t "case ~2d: return SCM_TRUE;\n" n)]
          [else (error "Can't handle:" val)]))
  (print "{")
  (for-each gen *predefined-values* (iota (length *predefined-values*)))
  (print "}")
  (values))

;; You can run it with
;;   gosh -mgauche.vm.debug-info gauche/vm/debug-info
(define (main args)
  (generate-predefined-switch)
  0)
