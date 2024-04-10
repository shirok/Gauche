;;;
;;; gauche.unicode - Various Unicode-specific operations
;;;
;;;   Copyright (c) 2011-2024  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.unicode
  (use gauche.uvector)
  (use gauche.sequence)
  (use gauche.generator)
  (use gauche.dictionary)
  (use util.match)
  (export ucs4->utf8  utf8-length  utf8->ucs4
          ucs4->utf16 utf16-length utf16->ucs4
          utf8->string string->utf8
          utf16->string string->utf16
          utf32->string string->utf32

          make-word-breaker
          make-word-reader
          string->words codepoints->words
          make-grapheme-cluster-breaker
          make-grapheme-cluster-reader
          string->grapheme-clusters codepoints->grapheme-clusters

          string-upcase string-downcase string-titlecase string-foldcase
          codepoints-upcase codepoints-downcase codepoints-titlecase
          codepoints-foldcase

          char-east-asian-width string-east-asian-width
          string-take-width
          string-drop-width

          string-ci=? string-ci<? string-ci<=? string-ci>? string-ci>=?
          )
  )
(select-module gauche.unicode)

(autoload gauche.charconv ces-convert)

;; This module implements some higher-level Unicode operations.
;; Most of them do not depend on the internal encodings---some
;; operates on integer values of codepoints or encoded octets,
;; some of them works on characters, regardless of its encodings.
;; If the operation works on characters and internal encoding is
;; not utf-8, the operation is a partial function of a full
;; unicode version.

(define-constant eof (eof-object))

;;;
;;;
;;; Unicode encoding conversions
;;;
;;;

;; The 'strictness' argument specifies what to do when the procedure
;; encounters a codepoint or an octed stream that does not obey proper
;; Unicode.
;;   'strict     - default, throws an error.
;;   'permissive - convert as if it is a valid value.
;;   'ignore     - drop the data.  behaves as if invalid data weren't there.
;;   'replace    - replace the output char with U+FFFD

(define-macro (utf8-2 c)  `(logior (ash ,c -6) #xc0))
(define-macro (utf8-3 c)  `(logior (ash ,c -12) #xe0))
(define-macro (utf8-4 c)  `(logior (ash ,c -18) #xf0))
(define-macro (utf8-5 c)  `(logior (ash ,c -24) #xf8))
(define-macro (utf8-6 c)  `(logior (ash ,c -30) #xfc))
(define-macro (utf8-z c)  `(logior (logand ,c #x3f) #x80))
(define-macro (utf8-y c)  `(logior (logand (ash ,c -6) #x3f) #x80))
(define-macro (utf8-x c)  `(logior (logand (ash ,c -12) #x3f) #x80))
(define-macro (utf8-w c)  `(logior (logand (ash ,c -18) #x3f) #x80))
(define-macro (utf8-v c)  `(logior (logand (ash ,c -24) #x3f) #x80))

(define-constant *utf8-replacement* '(#xef #xbf #xbd))
(define-constant *utf16-replacement* '(#xfffd))
(define-constant *ucs4-replacement*  #xfffd)

;; Returns list of integers
(define (ucs4->utf8 code :optional (strictness 'strict))
  (cond [(< code 0)
         (ecase strictness
           [(strict permissive) (errorf "Invalid unicode codepoint: ~x" code)]
           [(ignore) '()]
           [(replace) *utf8-replacement*])]
        [(< code #x80)     `(,code)]
        [(< code #x800)    `(,(utf8-2 code) ,(utf8-z code))]
        [(< code #x10000)
         (if (<= #xd800 code #xdfff)
           (ecase strictness
             [(strict)
              (errorf "Codepoint doesn't encode a character: ~4,'0x" code)]
             [(permissive) `(,(utf8-3 code) ,(utf8-y code) ,(utf8-z code))]
             [(ignore) '()]
             [(replace) *utf8-replacement*])
           `(,(utf8-3 code) ,(utf8-y code) ,(utf8-z code)))]
        [(< code #x110000) `(,(utf8-4 code)
                             ,(utf8-x code) ,(utf8-y code) ,(utf8-z code))]
        [else ;; beyond unicode range
         (ecase strictness
           [(strict)   (errorf "Invalid unicode codepoint: ~4,'0x" code)]
           [(ignore)  '()]
           [(permissive)
            (cond [(< code #x200000)
                   `(,(utf8-4 code)
                     ,(utf8-x code) ,(utf8-y code) ,(utf8-z code))]
                  [(< code #x4000000)
                   `(,(utf8-5 code) ,(utf8-w code)
                     ,(utf8-x code) ,(utf8-y code) ,(utf8-z code))]
                  [(< code #x80000000)
                   `(,(utf8-6 code) ,(utf8-v code) ,(utf8-w code)
                     ,(utf8-x code) ,(utf8-y code) ,(utf8-z code))]
                  [else (errorf "Codepoint outside domain: #x~x" code)]
                  )]
           [(replace) *utf8-replacement*])]
        ))

(define (utf8-length octet :optional (strictness 'strict))
  (define (err-oob octet)
    (errorf "Argument out of range as an octet: ~s" octet))
  (define (err-illegal octet)
    (errorf "Illegal utf-8 octet: ~2,'0x" octet))
  (cond [(< octet 0) (err-oob octet)]
        [(< octet #x80) 1]
        [(< octet #xc0) (ecase strictness
                          [(strict) (err-illegal octet)]
                          [(permissive replace) 1]
                          [(ignore) 0])]
        [(< octet #xe0) 2]
        [(< octet #xf0) 3]
        [(< octet #xf8) 4]
        [(> octet #xff) (err-oob octet)]
        [else (ecase strictness
                [(strict) (err-illegal octet)]
                [(ignore) 0]
                [(permissive replace) (cond [(< octet #xfc) 5]
                                            [(< octet #xfe) 6]
                                            [else 1])])]))

;; decoding utf8
;;  To avoid code redundancy and runtime overhead, we generate
;;  state machine by the following macros.
;;  They define state procedures---each procedure represents a
;;  state.  It takes the current input octet (b) and accumulated
;;  value (val), and returns updated value and next state.
;;  #f as the next state indicates the end state, and the value
;;  at that point is the decoded codepoint.
;;  #t as the next state indicates resetting the state without
;;  consuming the input.
;;  The state machine is written so that it doesn't allocate,
;;  and it can be used with various input.

;; generate a state transition procedure of k-th octet of n-octet utf8 sequence
(define-macro (decode-utf8-gen-1 strictness)
  (define (name prefix) (string->symbol #"~|prefix|-~|strictness|"))
  `(define (,(name "u1") b val)
     (if (eof-object? b)
       (values b #f)
       (case (utf8-length b ',strictness)
         [(0) (values 0 ,(name "u1"))]         ;ignore
         [(1) ,(if (eq? strictness 'replace)
                 '(if (>= b #x80)
                    (values *ucs4-replacement* #f)
                    (values b #f))
                 '(values b #f))]
         [(2) (values (logand #x1f b) ,(name "u2-1"))]
         [(3) (values (logand #x0f b) ,(name "u3-1"))]
         [(4) (values (logand #x07 b) ,(name "u4-1"))]
         [(5) (values (logand #x03 b) ,(name "u5-1"))]
         [(6) (values (logand #x01 b) ,(name "u6-1"))]))))

(define-macro (decode-utf8-gen-n n limit strictness)
  (define (rec n k)
    (let1 name (string->symbol #"u~|n|-~|k|-~|strictness|")
      (if (= k (- n 1))
        ;; last byte
        `((define (,name b val)
            (cond
             [(eof-object? b) ,(end-input strictness)]
             [(<= #x80 b #xbf)
              (let1 v (+ (ash val 6) (logand b #x3f))
                ,(if (< limit #xd800)
                   `(cond [(< v ,limit) ,(out-of-range strictness)]
                          [(< v #xd800) (values v #f)]
                          [(< v #xe000) ,(out-of-range strictness)]
                          [(< v #x110000) (values v #f)]
                          [else         ,(out-of-range strictness)])
                   `(cond [(< v ,limit) ,(out-of-range strictness)]
                          [(< v #x110000) (values v #f)]
                          [else         ,(out-of-range strictness)])))]
             [else ,(bad-octet strictness)])))
        ;; intermediate byte
        (let1 next-name (string->symbol #"u~|n|-~(+ k 1)-~|strictness|")
          `((define (,name b val)
              (cond [(eof-object? b) ,(end-input strictness)]
                    [(<= #x80 b #xbf) (values (+ (ash val 6) (logand b #x3f))
                                              ,next-name)]
                    [else ,(bad-octet strictness)]))
            ,@(rec n (+ k 1)))))))
  (define (out-of-range strictness)
    (case strictness
      [(strict)     `(values 'bad #f)]
      [(permissive) `(values v #f)]
      [(replace)    `(values ,*ucs4-replacement* #f)]
      [(ignore)     `(values 0 ,(string->symbol #"u1-~|strictness|"))]))
  (define (bad-octet strictness)
    (case strictness
      [(strict permissive) `(values 'bad #f)]
      [(replace)           `(values ,*ucs4-replacement* #f)]
      [(ignore)            `(values b #t)]))
  (define (end-input strictness)
    (case strictness
      [(strict permissive) `(values 'bad #f)]
      [(replace)           `(values ,*ucs4-replacement* #f)]
      [(ignore)            `(values eof #f)]))
  `(begin ,@(rec n 1)))

(define-macro (decode-utf8-gen strictness)
  `(begin
     (decode-utf8-gen-1 ,strictness)
     (decode-utf8-gen-n 2 #x80 ,strictness)
     (decode-utf8-gen-n 3 #x800 ,strictness)
     (decode-utf8-gen-n 4 #x10000 ,strictness)
     (decode-utf8-gen-n 5 #x200000 ,strictness)
     (decode-utf8-gen-n 6 #x4000000 ,strictness)))

(define utf8->ucs4
  (let ()
    (decode-utf8-gen strict)
    (decode-utf8-gen permissive)
    (decode-utf8-gen replace)
    (decode-utf8-gen ignore)

    (define (utf8->ucs4 octets :optional (strictness 'strict))
      (define initial-state (ecase strictness
                              [(strict)     u1-strict]
                              [(permissive) u1-permissive]
                              [(replace)    u1-replace]
                              [(ignore)     u1-ignore]))
      (if (null? octets)
        (values eof '())
        (let loop ([state initial-state]
                   [val 0]
                   [b (car octets)]
                   [bs (cdr octets)])
          (receive (val next) (state b val)
            (cond [(eq? val 'bad)
                   ;; TODO: better error message
                   (error "invalid utf8 sequence")]
                  [(not next)    (values val bs)]
                  [(eq? next #t) (loop initial-state 0 val bs)]
                  [(null? bs)    (loop next val eof '())]
                  [else          (loop next val (car bs) (cdr bs))])))))

    utf8->ucs4))

(define (ucs4->utf16 code :optional (strictness 'strict))
  (define (error-oob strictness)
    (ecase strictness
      [(strict permissive)
       (errorf "Input outside of Unicode codepoint range: #x~x" code)]
      [(replace) *utf16-replacement*]
      [(ignore) '()]))
  (cond  [(< code 0) (error-oob strictness)]
         [(< code #xd800) `(,code)]
         [(< code #xe000)
          (ecase strictness
            [(strict)     (errorf "Converting codepoint ~x to utf-16 \
                                  will lose information" code)]
            [(ignore)     '()]
            [(replace)    *utf16-replacement*]
            [(permissive) `(,code)])]
         [(< code #x10000) `(,code)]
         [(< code #x110000) (let1 code (- code #x10000)
                              `(,(logior (ash code -10) #xd800)
                                ,(logior (logand code #x3ff) #xdc00)))]
         [else (error-oob strictness)]))

(define (utf16-length word :optional (strictness 'strict))
  (cond [(<= #xd800 word #xdbff) 2]
        [(<= #xdc00 word #xdfff)
         (ecase strictness
           [(strict)    (errorf "unpaired surrogate: #x~x" word)]
           [(permissive replace) 1]
           [(ignore)     0])]
        [(<= 0 word #xffff) 1]
        [else (ecase strictness
                [(strict permissive) (errorf "Illegal utf-16 word: #x~x" word)]
                [(replace) 1]
                [(ignore) 0])]))

(define utf16->ucs4
  (let ()
    ;; The state machine for decoding utf16.  Each state takes the current
    ;; 16bit word, previous word, and stricness parameter.  Return values are
    ;; the value to be passed around, and the action.
    ;; Possible actions:
    ;;   reset  - reset to initial state
    ;;   surrogate - switch to secondary state
    ;;   unget  - don't consume the current word, and use val as a return value.
    ;;   skip   - don't take the next word and move to init
    ;;   lone   - lone surrogate.  val indicates bad word.
    ;;   range  - range error
    ;;   end    - end

    ;; Common routine.  Surrogate? is #t if w is supposed to be the second
    ;; word of the surrogated pair.
    (define (dispatch surrogate? w prev strictness)
      (if (not surrogate?)
        ;; first word
        (cond [(eof-object? w) (values eof 'end)]
              [(< w 0) (values w 'range)]
              [(< w #xd800) (values w 'end)]
              [(< w #xdc00) (values w 'surrogate)]
              [(< w #xe000) (ecase strictness
                              [(strict)     (values w 'lone)]
                              [(permissive) (values w 'end)]
                              [(replace)    (values *ucs4-replacement* 'end)]
                              [(ignore) (values 0 'reset)])]
              [(< w #x10000) (values w 'end)]
              [else (values w 'range)])
        ;; second word
        (cond [(eof-object? w) (ecase strictness
                                 [(strict)     (values prev 'lone)]
                                 [(permissive) (values prev 'end)]
                                 [(replace)    (values *ucs4-replacement* 'end)]
                                 [(ignore)     (values eof 'end)])]
              [(<= #xdc00 w #xdfff)
               (values (+ #x10000 (ash (logand #x3ff prev) 10) (logand #x3ff w))
                       'end)]
              [(<= 0 w #xffff)
               (ecase strictness
                 [(strict)     (values prev 'lone)]
                 [(permissive) (values prev 'unget)]  ; don't consume w
                 [(replace)    (values *ucs4-replacement* 'unget)] ; ditto
                 [(ignore)     (values w 'skip)])]
              [else (values w 'range)])))

    (define (utf16->ucs4 words :optional (strictness 'strict))
      (let loop ([words words]
                 [lookahead #f]
                 [surrogate #f]
                 [val 0])
        (receive (w ws) (cond [lookahead (values lookahead words)]
                              [(null? words) (values eof '())]
                              [else (values (car words) (cdr words))])
          (receive (val action) (dispatch surrogate w val strictness)
            (ecase action
              [(reset)  (loop ws #f #f 0)]
              [(surrogate) (loop ws #f #t val)]
              [(unget)  (values val words)] ; not consuming w
              [(skip)   (loop ws val #f 0)]
              [(lone)   (errorf "lone surrogate in utf-16 stream: #x~4,'0x" val)]
              [(range)  (errorf "out-of-range word in utf-16 stream: ~x" val)]
              [(end)    (values val ws)])))))

    utf16->ucs4))

;; R7RS procedures

;; This module may be precompiled by different platforms, so we dispatch
;; at runtime.
;; At this moment, we don't worry much about performance when the native
;; encoding isn't utf8.
;; If the input contains invalid utf-8 encoding, we replace those parts with
;; U+FFFD.  That is a slow path.
(define (utf8->string bvec :optional (start 0) (end -1))
  (assume-type bvec <u8vector>)
  (if (eq? (gauche-character-encoding) 'utf-8)
    (let1 s (u8vector->string bvec start end)
      (if (string-incomplete? s)
        ;; we have to go through every char
        (with-output-to-string
          (^[]
            (let loop ([in ($ generator->lseq
                              $ uvector->generator bvec start
                              $ if (< end 0) (u8vector-length bvec) end)])
              (receive (ch next) (utf8->ucs4 in 'replace)
                (unless (eof-object? ch)
                  (write-char (ucs->char ch))
                  (loop next))))))
        s))
    ;; If intenral encoding isn't utf-8, we just let ces-convert handle it.
    ;; (invalid char replacement may not be handled well).
    (ces-convert (u8vector->string bvec start end) 'utf-8)))

(define (string->utf8 str :optional (start 0) (end -1))
  (assume-type str <string>)
  (if (eq? (gauche-character-encoding) 'utf-8)
    (string->u8vector str start end)
    (let ([start (or start 0)]
          [end (if (and (number? end) (>= end 0)) end (string-length str))])
      (string->u8vector (ces-convert (substring str start end)
                                     (gauche-character-encoding)
                                     'utf-8)))))

;; ;; R7RS scheme.bytevector

(define (utf16->string bvec :optional (endian #f) (ignore-bom? #f)
                       (start 0) (end -1))
  (assume-type bvec <u8vector>)
  (let1 end (cond [(< end 0) (u8vector-length bvec)]
                  [(or (< start 0) (< end start))
                   (error "invalid start/end positions:" (list start end))]
                  [else end])
    (receive (start endian)
        (if (or ignore-bom? (< (- end start) 2))
          (values start (or endian 'bid-endian))
          (let ([b0 (u8vector-ref bvec start)]
                [b1 (u8vector-ref bvec (+ start 1))])
            (cond [(and (eqv? b0 #xfe) (eqv? b1 #xff))
                   (values (+ start 2) 'big-endian)]
                  [(and (eqv? b0 #xff) (eqv? b1 #xfe))
                   (values (+ start 2) 'little-endian)]
                  [else (values start (or endian 'big-endian))])))
      (unless (even? (- end start))
        (error "number of input octets for utf16 isn't even:" (- end start)))
      (with-output-to-string
        (^[] (let loop ([in ($ generator->lseq
                               $ %u8->u16-generator bvec start end endian)])
               (receive (u32 next) (utf16->ucs4 in 'replace)
                 (or (eof-object? u32)
                     (begin (write-char (ucs->char u32))
                            (loop next))))))))))

(define (%u8->u16-generator bvec start end endian)
  ;; We don't want to depend on binary.io
  (define combine
    (if (memq endian '(big big-endian))
      (^[v k] (logior (ash (u8vector-ref v k) 8)
                      (u8vector-ref v (+ k 1))))
      (^[v k] (logior (u8vector-ref v k)
                      (ash (u8vector-ref v (+ k 1)) 8)))))
  (^[] (if (= start end)
         (eof-object)
         (begin0 (combine bvec start) (inc! start 2)))))

(define (string->utf16 str :optional (endian 'big-endian)
                                     (add-bom? #f)
                                     (start 0) end)
  (with-builder (<u8vector> add! get)
    (define add16!
      (if (memq endian '(big big-endian))
        (^u (add! (ash u -8)) (add! (logand u #xff)))
        (^u (add! (logand u #xff)) (add! (ash u -8)))))
    (when add-bom? (add16! #xfeff))
    (generator-for-each (^[ch] (for-each add16! (ucs4->utf16 (char->ucs ch))))
                        (string->generator str start end))
    (get)))

(define (utf32->string bvec :optional (endian #f) (ignore-bom? #f)
                                      (start 0) (end -1))
  (assume-type bvec <u8vector>)
  (let1 end (cond [(< end 0) (u8vector-length bvec)]
                  [(or (< start 0) (< end start))
                   (error "invalid start/end positions:" (list start end))]
                  [else end])
    (receive (start endian)
        (if (or ignore-bom? (< (- end start) 4))
          (values start (or endian 'bid-endian))
          (let ([b0 (u8vector-ref bvec start)]
                [b1 (u8vector-ref bvec (+ start 1))]
                [b2 (u8vector-ref bvec (+ start 2))]
                [b3 (u8vector-ref bvec (+ start 3))])
            (cond [(and (eqv? b0 0) (eqv? b1 0) (eqv? b2 #xfe) (eqv? b3 #xff))
                   (values (+ start 4) 'big-endian)]
                  [(and (eqv? b0 #xff) (eqv? b1 #xfe) (eqv? b2 0) (eqv? b3 0))
                   (values (+ start 4) 'little-endian)]
                  [else (values start (or endian 'big-endian))])))
      (unless (zero? (modulo (- end start) 4))
        (error "number of input octets for utf32 isn't multiple of 4"
               (- end start)))
      (if (zero? (mod start 4))
        (u32vector->string (uvector-alias <u32vector> bvec start end)
                           0 -1 #f endian)
        (map-to <string> ucs->char
                ($ generator->lseq
                   $ %u8->u32-generator bvec start end endian))))))

(define (%u8->u32-generator bvec start end endian)
  ;; We don't want to depend on binary.io
  (define combine
    (if (memq endian '(big big-endian))
      (^[v k] (logior (ash (u8vector-ref v k) 24)
                      (ash (u8vector-ref v (+ k 1)) 16)
                      (ash (u8vector-ref v (+ k 2)) 8)
                      (u8vector-ref v (+ k 3))))
      (^[v k] (logior (u8vector-ref v k)
                      (ash (u8vector-ref v (+ k 1)) 8)
                      (ash (u8vector-ref v (+ k 2)) 16)
                      (ash (u8vector-ref v (+ k 3)) 24)))))
  (^[] (if (= start end)
         (eof-object)
         (begin0 (combine bvec start) (inc! start 4)))))

(define (string->utf32 str :optional (endian 'big-endian)
                                     (add-bom? #f)
                                     (start 0) end)
  (let* ([s (opt-substring str start end)]
         [len (string-length s)]
         [r (make-u8vector (* 4 (if add-bom? (+ len 1) len)))]
         [rr (uvector-alias <u32vector> r (if add-bom? 4 0))])
    (string->u32vector! rr 0 s 0 -1 endian)
    (when add-bom?
      (case endian
        [(big big-endian)
         (u8vector-set! r 0 #x00)
         (u8vector-set! r 1 #x00)
         (u8vector-set! r 2 #xfe)
         (u8vector-set! r 3 #xff)]
        [else
         (u8vector-set! r 0 #xff)
         (u8vector-set! r 1 #xfe)
         (u8vector-set! r 2 #x00)
         (u8vector-set! r 3 #x00)]))
    r))

;;;
;;;  Character properties
;;;

;; unicode-attr.scm is created with src/gen-unicode.scm during build.
(include "unicode-attr.scm")

;;=========================================================================
;; Low level code to look up properties
;;

(inline-stub
 (declcode
  (.include <gauche/priv/configP.h>
            "gauche/priv/unicode_attr.h"))

 (initcode "init_WIDTH_symbols(Scm_CurrentModule());")

 (define-cise-stmt get-arg
   [(_ var arg)
    (let1 i (gensym)
      `(cond [(SCM_CHARP ,arg) (set! ,var (cast int (SCM_CHAR_VALUE ,arg)))]
             [(SCM_INTP ,arg)
              (let* ([,i :: int (SCM_INT_VALUE ,arg)])
                (when (or (< ,i 0) (> ,i #x10ffff))
                  (Scm_Error "argument outside of valid Unicode codepoint: %d"
                             ,i))
                (set! ,var ,i))]
             [else (SCM_TYPE_ERROR scode "char or fixnum")]))])

 (define-cproc gb-property (scode) ::<int>
   (let* ([ch::int SCM_CHAR_INVALID])
     (get-arg ch scode)
     (cond [(< ch #x20000)
            (let* ([k::u_char (aref GB_break_table (>> ch 8))])
              (if (>= k BOFF)
                (return (- k BOFF)) ; all block is the same
                (return (aref GB_break_subtable k (logand ch #xff)))))]
           [(or (== #xE0001 ch)
                (and (<= #xE0020 ch) (<= ch #xE007F))) (return GB_Control)]
           [(and (<= #xE0100 ch) (<= ch #xE01EF)) (return GB_Extend)]
           [else (return GB_Other)])))

 (define-cproc wb-property (scode) ::<int>
   (let* ([ch::int SCM_CHAR_INVALID])
     (get-arg ch scode)
     (cond [(< ch #x20000)
            (let* ([k::u_char (aref WB_break_table (>> ch 8))])
              (if (>= k BOFF)
                (return (- k BOFF)) ; all block is the same
                (return (aref WB_break_subtable k (logand ch #xff)))))]
           [(or (== #xE0001 ch)
                (and (<= #xE0020 ch) (<= ch #xE007F))) (return WB_Format)]
           [(and (<= #xE0100 ch) (<= ch #xE01EF)) (return WB_Extend)]
           [else (return WB_Other)])))

 (define-cproc width-property (scode) ::<int>
   (let* ([ch::int SCM_CHAR_INVALID])
     (get-arg ch scode)
     (cond [(< ch #x20000)
            (let*([k::u_char (aref width_table (>> ch 8))])
              (if (< k NUM_WIDTH_PROPERTIES)
                (return k)
                (let* ([b::u_char (aref width_subtable
                                        (- k NUM_WIDTH_PROPERTIES)
                                        (>> (logand ch #xfe) 1))])
                  (return (logand #x0f (?: (logand ch 1) (>> b 4) b))))))]
           ;; CJK and reserved area up to 3fffd is 'W'.  There's a gap in
           ;; 2fffe-2ffff and 3fffe-3ffff, but I believe they don't matter.
           [(< ch #x40000) (return WIDTH_W)]
           ;; After 40000, it's sparsely defined---the private area
           ;; (f0000-ffffd, 100000-10fffd) are A.  All the other, we assign
           ;; default N.
           [(< ch #xf0000) (return WIDTH_N)]
           [else (return WIDTH_A)])))
 )

;;;
;;;
;;; Grapheme cluster break and word break
;;;
;;;

;;=========================================================================
;; Utiltiies
;;

;; Given breaker generator, returns a generator that returns a cluster
;; (grapheme cluster or word) at a time.
;; Item = Char | Int
;; generator :: () -> Item
;; return    :: [Item] -> a
(define (make-cluster-reader-maker breaker-maker)
  (^[generator return]
    (define lookahead #f)
    (define breaker (breaker-maker generator))
    (receive (ch break?) (breaker)
      (set! lookahead ch))
    (^[] (if (eof-object? lookahead)
           lookahead
           (let loop ([acc (list lookahead)])
             (receive (ch break?) (breaker)
               (if break?
                 (begin (set! lookahead ch) (return (reverse acc)))
                 (loop (cons ch acc)))))))))

;; NB: These may be refactored once we have the generator framework.
(define (make-string-splitter cluster-reader-maker)
  (case (gauche-character-encoding)
    [(utf-8 none)  ;NB: we treat 'none' as latin-1
     (^[str]
       (with-input-from-string str
         (cut generator->list
              (cluster-reader-maker read-char list->string))))]
    [else
     (^[str]
       (with-input-from-string str
         (cut generator->list
              (cluster-reader-maker (gmap char->ucs read-char)
                                    (^[cs] (map-to <string> ucs->char cs))))))]
    ))

(define (make-sequence-splitter cluster-reader-maker)
  (^[seq]
    (let1 gen (x->generator seq)
      (generator->list
       (cluster-reader-maker gen (cut coerce-to (class-of seq) <>))))))

;;=========================================================================
;; State transition compiler
;;
;; From the state transition description, generates a nested vector.
;; The outer vector is indexed by state.  The inner vector is indexed
;; by input.  The element of inner vector is a pair of
;; (<output> . <next-state-index>)
;;
;; For our purpose, <output> can be either one of these:
;;   #f   - Do not break before the current input.
;;   #t   - Break before the current input.
;;   hold - Do not consume the current input and just go to the next state.
;;   wb6, wb7b, wb12 - This triggers special handling in word breaker
;;
;; Each description is in the form of (state-name arc ...)
;; where arc is (input -> output next-state-name)
;; and input may be a symbol, (or input ...), or :else.
;;
;; For the concise transition description, the compiler takes
;; default-next-state alist, which maps :else clause of the description
;; to the next state.
;;
;; The input-index-map argument is a map to look up input index from
;; the input symbol.

(eval-when (:compile-toplevel)
(define (compile-state-transition-description state-desc default-next-state
                                              input-index-map)
  (define (expand-or alist)
    (append-map (^p (match p
                      [(('or xs ...) . y) (map (^x (cons x y)) xs)]
                      [_ (list p)]))
                alist))
  (let ([default-next-state* (expand-or default-next-state)]
        [input-index-size
         (+ (dict-fold input-index-map (^[k v s] (max v s)) 0) 1)]
        [state-size (length state-desc)]
        [states (map (^s (cons (car s) (expand-or (cdr s)))) state-desc)]
        [state-index (map-with-index (^[i st] (cons (car st) i)) state-desc)])
    (define (next-arc state input-symbol) ; returns Maybe (output . next)
      (match (assq input-symbol (cdr state))
        [(_ '-> output next) (cons output next)]
        [#f (match (assq :else (cdr state))
              [(_ '-> output ':default)
               (if-let1 default (assq-ref default-next-state* input-symbol)
                 (cons output default)
                 (cons output (assq-ref default-next-state* :else)))]
              [(_ '-> output next) (cons output next)]
              [_ #f])]
        [_ #f]))
    (define (build-inner-vec state)
      (rlet1 v (make-vector input-index-size '(#f . #f))
        ($ dict-for-each input-index-map
           (^[input-symbol index]
             (match (next-arc state input-symbol)
               [(output . next-state-name)
                (set! (~ v index)
                      (cons output (assq-ref state-index next-state-name)))]
               [_ (errorf "Can't find transition from state ~s with input ~s"
                          (car state) input-symbol)])))))
    (list->vector (map build-inner-vec states))))
)

;;=========================================================================
;; Word breaker state transition tables
;;
;; The Rule WB6, WB7b and WB12 requires another character lookahead.  However,
;; with WB4, we'll need unlimited lookahead.  To keep the state machine
;; simple, we handle those transition specially.

;; Each state is a vector indexed by Word Break Property.  Each entry
;; is a pair, whose car is a flag, and whose cdr is the next state.
;; The flag may be #t (break), #f (not break), wb6 (special lookahead for
;; WB6) or wb12 (special lookahead for WB12).

(eval-when (:compile-toplevel)
(define-constant *word-break-default-next-states*
  '((CR                 . :cr)
    ((or Newline LF)    . :newline-lf)
    (Regional_Indicator . :regional-indicator)
    (Katakana           . :katakana)
    (Hebrew_Letter      . :hebrew-letter)
    (ALetter            . :a-letter)
    (Numeric            . :numeric)
    (ExtendNumLet       . :extend-num-let)
    (WSegSpace          . :wsegspace)
    (:else              . :other)))

(define-constant *word-break-states*
  '(;; Initial state (sot)
    ;; From WB1, we always break.
    (:sot
     ((or Newline LF)     -> #t :newline-lf)
     (CR                  -> #t :cr)
     ((or Extend Format)  -> #t :sep-extend-format)
     (ZWJ                 -> #t :sep-zwj)
     (:else               -> #t :default))
    ;; CR (WB3, WB3a) - break except followed by LF.
    (:cr
     (LF                  -> #f :newline-lf)
     ((or Extend Format ) -> #t :sep-extend-format)
     (ZWJ                 -> #t :sep-zwj)
     (:else               -> #t :default))
    ;; Newline|LF  (WB3, WB3a)
    (:newline-lf
     ((or Extend Format)  -> #t :sep-extend-format)
     (ZWJ                 -> #t :sep-zwj)
     (:else               -> #t :default))
    ;; Sep + ZWJ x ExtendedPict (WB3c)
    (:sep-zwj
     (ExtPict             -> #f :other)
     (ExtPict_ALetter     -> #f :a-letter)
     ((or Extend Format)  -> #f :sep-extend-format)
     (ZWJ                 -> #f :sep-zwj)
     (:else               -> #t :default))
    ;; Sep + (Extend|Format)*  (WB4)
    (:sep-extend-format
     ((or Extend Format)  -> #f :sep-extend-format)
     (ZWJ                 -> #f :sep-zwj)
     (:else               -> #t :default))
    ;; ALetter
    (:a-letter
     ((or Extend Format)  -> #f :a-letter)       ; WB4
     (ZWJ                 -> #f :a-letter-zwj)   ; WB4
     (ALetter             -> #f :a-letter)       ; WB5
     (Hebrew_Letter       -> #f :hebrew-letter)  ; WB5
     ((or MidLetter MidNumLet Single_Quote) -> wb6 :a-letter+mid) ; WB6
     (Numeric             -> #f :numeric)        ; WB9
     (ExtendNumLet        -> #f :extend-num-let) ; WB13a
     (:else               -> #t :default))
    ;; ALetter x ZWJ x ExtendedPict (WB3c)
    (:a-letter-zwj
     (ExtPict             -> #f :other)
     (ExtPict_ALetter     -> #f :a-letter)
     ((or Extend Format)  -> #f :a-letter)
     (ZWJ                 -> #f :a-letter-zwj)
     (:else               -> hold :a-letter))
    ;; ALetter x (MidLetter|MidNumLetQ)
    (:a-letter+mid
     ((or Extend Format)  -> #f :a-letter+mid)   ; WB4
     (ZWJ                 -> #f :a-letter+mid-zwj) ; WB4
     (ALetter             -> #f :a-letter)       ; WB7
     (Hebrew_Letter       -> #f :hebrew-letter)  ; WB7
     (:else               -> #t :default))
    ;; ALetter + (MidLetter|MidNumLetQ) + ZWJ x ExtendedPict (WB3c)
    (:a-letter+mid-zwj
     (ExtPict             -> #f :other)
     (ExtPict_ALetter     -> #f :a-letter)
     ((or Extend Format)  -> #f :a-letter+mid)
     (ZWJ                 -> #f :a-letter+mid-zwj)
     (:else               -> hold :a-letter+mid))
    ;; Hebrew_Letter
    ;; NB: WB7a is actually covered by WB6.
    (:hebrew-letter
     ((or Extend Format)  -> #f :hebrew-letter)  ; WB4
     (ZWJ                 -> #f :hebrew-letter-zwj) ; WB4
     (ALetter             -> #f :a-letter)       ; WB5
     (Hebrew_Letter       -> #f :hebrew-letter)  ; WB5
     (Single_Quote        -> #f :a-letter+mid)   ; WB7a
     ((or MidLetter MidNumLet Single_Quote) -> wb6 :a-letter+mid) ; WB6
     (Double_Quote        -> wb7b :hebrew-letter+dq) ; WB7b
     (Numeric             -> #f :numeric)        ; WB9
     (ExtendNumLet        -> #f :extend-num-let) ; WB13a
     (:else               -> #t :default))
    ;; Hebrew_Letter x ZWJ x ExtendedPict (WB3c)
    (:hebrew-letter-zwj
     (ExtPict             -> #f :other)
     (ExtPict_ALetter     -> #f :a-letter)
     ((or Extend Format)  -> #f :hebrew-letter)
     (ZWJ                 -> #f :hebrew-letter-zwj)
     (:else               -> hold :hebrew-letter))
    ;; Hebrew_Letter + Double_Quote
    (:hebrew-letter+dq
     ((or Extend Format)  -> #f :hebrew-letter+dq) ; WB4
     (ZWJ                 -> #f :hebrew-letter+dq-zwj) ; WB4
     (Hebrew_Letter       -> #f :hebrew-letter)    ; WB7c
     (:else               -> #t :default))
    ;; Hebrew_Letter + Double_Quote + ZWJ x ExtendedPict (WB3c)
    (:hebrew-letter+dq-zwj
     (ExtPict             -> #f :other)
     (ExtPict_ALetter     -> #f :a-letter)
     ((or Extend Format)  -> #f :hebrew-letter+dq) ; WB4
     (ZWJ                 -> #f :hebrew-letter+dq-zwj)
     (:else               -> hold :hebrew-letter+dq))
    (:numeric
     ((or Extend Format)  -> #f :numeric)        ; WB4
     (ZWJ                 -> #f :numeric-zwj)    ; WB4
     (Numeric             -> #f :numeric)        ; WB8
     (ALetter             -> #f :a-letter)       ; WB10
     (Hebrew_Letter       -> #f :hebrew-letter)  ; WB10
     ((or MidNum MidNumLet Single_Quote) -> wb12 :numeric+mid) ; WB12
     (ExtendNumLet        -> #f :extend-num-let) ; WB13a
     (:else               -> #t :default))
    (:numeric-zwj
     (ExtPict             -> #f :other)
     (ExtPict_ALetter     -> #f :a-letter)
     ((or Extend Format)  -> #f :numeric)        ; WB4
     (ZWJ                 -> #f :numeric-zwj)
     (:else               -> hold :numeric))
    (:numeric+mid
     ((or Extend Format)  -> #f :numeric+mid)    ; WB4
     (ZWJ                 -> #f :numeric+mid-zwj) ; WB4
     (Numeric             -> #f :numeric)        ; WB11
     (:else               -> #t :default))
    (:numeric+mid-zwj
     (ExtPict             -> #f :other)
     (ExtPict_ALetter     -> #f :a-letter)
     ((or Extend Format)  -> #f :numeric+mid)    ; WB4
     (ZWJ                 -> #f :numeric+mid-zwj)
     (:else               -> hold :numeric+mid))
    (:katakana
     ((or Extend Format)  -> #f :katakana)       ; WB4
     (ZWJ                 -> #f :katakana-zwj)   ; WB4
     (Katakana            -> #f :katakana)       ; WB13
     (ExtendNumLet        -> #f :extend-num-let) ; WB13a
     (:else               -> #t :default))
    (:katakana-zwj
     (ExtPict             -> #f :other)
     (ExtPict_ALetter     -> #f :a-letter)
     ((or Extend Format)  -> #f :katakana)       ; WB4
     (ZWJ                 -> #f :katakana-zwj)
     (:else               -> hold :katakana))
    (:extend-num-let
     ((or Extend Format)  -> #f :extend-num-let) ; WB4
     (ZWJ                 -> #f :extend-num-let-zwj) ; WB4
     (ExtendNumLet        -> #f :extend-num-let) ; WB13a
     (ALetter             -> #f :a-letter)       ; WB13b
     (Hebrew_Letter       -> #f :hebrew-letter)  ; WB13b
     (Numeric             -> #f :numeric)        ; WB13b
     (Katakana            -> #f :katakana)       ; WB13b
     (:else               -> #t :default))
    (:extend-num-let-zwj
     (ExtPict             -> #f :other)
     (ExtPict_ALetter     -> #f :a-letter)
     ((or Extend Format)  -> #f :extend-num-let)
     (ZWJ                 -> #f :extend-num-let-zwj)
     (:else               -> hold :extend-num-let))
    (:regional-indicator
     ((or Extend Format)  -> #f :regional-indicator) ; WB4
     (ZWJ                 -> #f :regional-indicator-zwj) ; WB4
     (Regional_Indicator  -> #f :regional-indicator-1) ; WB15, WB16
     (:else               -> #t :default))
    (:regional-indicator-zwj
     (ExtPict             -> #f :other)
     (ExtPict_ALetter     -> #f :a-letter)
     ((or Extend Format)  -> #f :regional-indicator)
     (ZWJ                 -> #f :regional-indicator-zwj)
     (:else               -> hold :regional-indicator))
    (:regional-indicator-1
     (Regional_Indicator  -> #t :regional-indicator) ; WB15, WB16
     ((or Extend Format)  -> #f :regional-indicator-1)
     (ZWJ                 -> #f :regional-indicator-1-zwj)
     (:else               -> #t :default))
    (:regional-indicator-1-zwj
     (ExtPict             -> #f :other)
     (ExtPict_ALetter     -> #f :a-letter)
     ((or Extend Format)  -> #f :regional-indicator-1)
     (ZWJ                 -> #f :regional-indicator-1-zwj)
     (:else               -> hold :regional-indicator-1))
    ;; WSegSpace (WB3d)
    ;; NB: WordBreakTest.txt indicates WSegSpace x Extend ÷ WSegSpace,
    ;; so we need :wsegspace-ex
    (:wsegspace
     (WSegSpace           -> #f :wsegspace)      ; WB3d
     ((or Extend Format)  -> #f :wsegspace-ex)
     (ZWJ                 -> #f :wsegspace-zwj)  ; WB4
     (:else               -> #t :default))
    (:wsegspace-ex
     ((or Extend Format)  -> #f :wsegspace-ex)
     (ZWJ                 -> #f :wsegspace-zwj)
     (:else               -> #t :default))
    (:wsegspace-zwj
     (ExtPict             -> #f :other)
     (ExtPict_ALetter     -> #f :a-letter)
     ((or Extend Format)  -> #f :wsegspace-ex)   ; WB4
     (ZWJ                 -> #f :wsegspace-zwj)
     (:else               -> hold :wsegspace))
    (:other
     ((or Extend Format)  -> #f :other)          ; WB4
     (ZWJ                 -> #f :other-zwj)      ; WB4
     (:else               -> #t :default))       ; WB14
    (:other-zwj
     (ExtPict             -> #f :other)
     (ExtPict_ALetter     -> #f :a-letter)
     ((or Extend Format)  -> #f :other)
     (ZWJ                 -> #f :other-zwj)
     (:else               -> hold :other))
    ))

(define-constant *word-break-fa*
  (compile-state-transition-description *word-break-states*
                                        *word-break-default-next-states*
                                        (alist->hash-table
                                         *WB-properties* ; in unicode-attr.scm
                                         'eq?)))
);eval-when

;; Simple queue to avoid depending data.queue
(define (makeq) (cons '() '()))
(define (q-empty? q) (null? (car q)))
(define (enq! q item) (let1 p (list item)
                        (if (q-empty? q)
                          (begin (set-car! q p) (set-cdr! q p))
                          (begin (set-cdr! (cdr q) p) (set-cdr! q p)))))
(define (deq! q) (if (q-empty? q)
                   (error "[internal] queue is empty")
                   (rlet1 p (caar q) (set-car! q (cdar q)))))
(define (pushq! q item) (if (q-empty? q)
                          (let1 p (list item)
                            (set-car! q p) (set-cdr! q p))
                          (set-car! q (cons item (car q)))))

;; API
;; Returns a generator procedure.  Every time it is called, it returns
;; two values - a character/codepoint, taken from the generator,
;; and a boolean value indicates if a word breaks before that
;; character/codepoint.  Note that unlimited characters/codepoints
;; can be taken from the generator to determine word boundaries.
;; The end of the input is indicated by #<eof>.
(define (make-word-breaker generator)
  (define current-state (vector-ref *word-break-fa* 0))
  (define q (makeq))  ;characters looked ahead.
  (rec (gen)
    (let1 ch (if (q-empty? q) (generator) (deq! q))
      (if (eof-object? ch)
        (values ch #t) ; WB2
        (let1 p (vector-ref current-state (wb-property ch))
          (set! current-state (vector-ref *word-break-fa* (cdr p)))
          (cond [(boolean? (car p)) (values ch (car p))]
                [(eq? (car p) 'hold) (pushq! q ch) (gen)]
                [(eq? (car p) 'wb6)
                 (wb-lookahead generator q ch `(,WB_ALetter ,WB_Hebrew_Letter))]
                [(eq? (car p) 'wb12)
                 (wb-lookahead generator q ch `(,WB_Numeric))]
                [(eq? (car p) 'wb7b)
                 (wb-lookahead generator q ch `(,WB_Hebrew_Letter))]
                [else (error "[internal] bad state:" p)]))))))

(define (wb-lookahead generator q ch next-props)
  ;; q must be empty here, but we check just in case.
  (unless (q-empty? q) (error "[internal] wb6 or wb12 incorrect state"))
  (let loop ([ch2 (generator)])
    (if (eof-object? ch2)
      (values ch #t)
      (let1 prop (wb-property ch2)
        (enq! q ch2)
        (cond [(memv prop next-props) (values ch #f)]
              [(eqv? prop WB_Extend) (loop (generator))]
              [(eqv? prop WB_Format) (loop (generator))]
              [(eqv? prop WB_ZWJ) (loop (generator))]
              [else (values ch #t)])))))

;; API
(define make-word-reader
  (make-cluster-reader-maker make-word-breaker))

;; API
(define string->words (make-string-splitter make-word-reader))
(define codepoints->words (make-sequence-splitter make-word-reader))

;; Grapheme Cluster Break Finite Automaton
;; Each state is a vector, input is grapheme-cluster-break property,
;; and entry is a cons of boolean value (#t - break, #f - not break),
;; and the next state vector index.

(eval-when (:compile-toplevel)
(define-constant *grapheme-break-default-next-states*
  '((CR                 . :cr)
    (LF                 . :control-lf)
    (Control            . :control-lf)
    (L                  . :l)
    ((or LV V)          . :lv-v)
    ((or LVT T)         . :lvt-t)
    (Regional_Indicator . :regional-indicator)
    (Prepend            . :prepend)
    (ExtPict            . :ext-pict)
    (:else              . :other)))

(define-constant *grapheme-break-states*
  '(;; Initial state - sot
    (:sot
     (:else              -> #t :default))   ; GB1
    (:cr
     (LF                 -> #f :control-lf) ; GB3
     (Control            -> #t :control-lf) ; GB4
     (:else              -> #t :default))   ; GB4
    (:control-lf
     (:else              -> #t :default))   ; GB4
    (:l
     (L                  -> #f :l)          ; GB6
     ((or V LV)          -> #f :lv-v)       ; GB6
     (LVT                -> #f :lvt-t)      ; GB6
     ((or Extend ZWJ)    -> #f :other)      ; GB9
     (SpacingMark        -> #f :other)      ; GB9a
     (:else              -> #t :default))
    (:lv-v
     (V                  -> #f :lv-v)       ; GB7
     (T                  -> #f :lvt-t)      ; GB7
     ((or Extend ZWJ)    -> #f :other)      ; GB9
     (SpacingMark        -> #f :other)      ; GB9a
     (:else              -> #t :default))
    (:lvt-t
     (T                  -> #f :lvt-t)      ; GB8
     ((or Extend ZWJ)    -> #f :other)      ; GB9
     (SpacingMark        -> #f :other)      ; GB9a
     (:else              -> #t :default))
    (:regional-indicator
     (Regional_Indicator -> #f :regional-indicator-1) ; GB12, GB13
     ((or Extend ZWJ)    -> #f :other)                ; GB9
     (SpacingMark        -> #f :other)                ; GB9a
     (:else              -> #t :default))
    (:regional-indicator-1
     (Regional_Indicator -> #t :regional-indicator) ; GB12, GB13
     ((or Extend ZWJ)    -> #f :other)              ; GB9
     (SpacingMark        -> #f :other)              ; GB9a
     (:else              -> #t :default))
    (:prepend
     (CR                 -> #t :cr)         ; GB5
     ((or Control LF)    -> #t :control-lf) ; GB5
     (:else              -> #f :default))   ; GB9b
    (:ext-pict
     (Extend             -> #f :ext-pict)   ; GB11
     (ZWJ                -> #f :ext-pict-1) ; GB9
     (SpacingMark        -> #f :other)      ; GB9a
     (:else              -> #t :default))
    (:ext-pict-1
     (ExtPict            -> #f :ext-pict)   ; GB11
     (SpacingMark        -> #f :other)      ; GB9a
     (:else              -> #t :default))
    (:other
     ((or Extend ZWJ)    -> #f :other)      ; GB9
     (SpacingMark        -> #f :other)      ; GB9a
     (:else              -> #t :default))   ; GB10
    ))

(define-constant *grapheme-break-fa*
  (compile-state-transition-description *grapheme-break-states*
                                        *grapheme-break-default-next-states*
                                        (alist->hash-table
                                         *GB-properties* ; in unicode_attr.scm
                                         'eq?)))
);eval-when

;; API
;; Returns a generator, that returns two values, a character and
;; indicating grapheme cluster breaks before the character.
(define (make-grapheme-cluster-breaker generator)
  (define current-state (vector-ref *grapheme-break-fa* 0))
  (^[]
    (let1 ch (generator)
      (if (eof-object? ch)
        (values ch #t) ; GB2
        (let1 p (vector-ref current-state (gb-property ch))
          (set! current-state (vector-ref *grapheme-break-fa* (cdr p)))
          (values ch (car p)))))))

;; API
(define make-grapheme-cluster-reader
  (make-cluster-reader-maker make-grapheme-cluster-breaker))

;; API
(define string->grapheme-clusters
  (make-string-splitter make-grapheme-cluster-reader))
(define codepoints->grapheme-clusters
  (make-sequence-splitter make-grapheme-cluster-reader))

;;;
;;; East asian width
;;;

;; returns a symbol
(define (char-east-asian-width char-or-code)
  (vector-ref *east-asian-widths* (width-property char-or-code)))

(define (%char->width ch kargs)
  (let-keywords* kargs ((F 2)  ;full-width
                        (H 1)  ;half-width
                        (W 2)  ;wide
                        (Na 1) ;narrow
                        (N 1)  ;neutral
                        (A 2)) ;ambiguous
    (case (char-east-asian-width ch)
      [(F) F]
      [(H) H]
      [(W) W]
      [(Na) Na]
      [(N) N]
      [(A) A])))

;; compute the on-screen 'width' according to the East Asian Width property.
;; See UAX #11 https://www.unicode.org/reports/tr11/tr11-40.html
(define (string-east-asian-width str :key F H W Na N A :rest keys)
  (generator-fold (^[ch sum]
                    (+ sum (%char->width ch keys)))
                  0 (string->generator str)))

(define (string-take-width str width :key F H W Na N A :rest keys)
  (with-string-io str
    (^[] (let loop ([w 0] [ch (read-char)])
           (unless (eof-object? ch)
             (let1 w1 (%char->width ch keys)
               (unless (> (+ w w1) width)
                 (write-char ch)
                 (loop (+ w w1) (read-char)))))))))

(define (string-drop-width str width :key F H W Na N A :rest keys)
  (call-with-string-io str
    (^[in out]
      (let loop ([w 0] [ch (read-char in)])
        (unless (eof-object? ch)
          (let1 w1 (%char->width ch keys)
            (if (> (+ w w1) width)
              (begin
                (write-char ch out)
                (copy-port in out))
              (loop (+ w w1) (read-char in)))))))))

;;;
;;; String casing
;;;

(inline-stub
 (declcode
  (.include <gauche/char_attr.h>)
  (.define CHAR_UPCASE 0)
  (.define CHAR_DOWNCASE 1)
  (.define CHAR_TITLECASE 2))

 (define-enum CHAR_UPCASE)
 (define-enum CHAR_DOWNCASE)
 (define-enum CHAR_TITLECASE)

 (define-cise-stmt fill-result
   [(_ to_x_full to_x_simple)
    `(cond
      [(== (aref (-> pcm ,to_x_full) 0) -1)
       (set! (SCM_VECTOR_ELEMENT buf 0)
             (?: charp
                 (SCM_MAKE_CHAR (+ ch (-> pcm ,to_x_simple)))
                 (SCM_MAKE_INT  (+ ch (-> pcm ,to_x_simple)))))
       (return 1)]
      [else
       (for ((set! i 0) (< i SCM_CHAR_FULL_CASE_MAPPING_SIZE) (post++ i))
            (when (== (aref (-> pcm ,to_x_full) i) -1)
              (return i))
            (set! (SCM_VECTOR_ELEMENT buf i)
                  (?: charp
                      (SCM_MAKE_CHAR (aref (-> pcm ,to_x_full) i))
                      (SCM_MAKE_INT  (aref (-> pcm ,to_x_full) i)))))
       (return i)])])

 (define-cproc %char-xcase-extended (scode buf::<vector>
                                     kind::<int> charp::<boolean>)
   ::<int>
   (let* ([ch::int SCM_CHAR_INVALID]
          [cm::ScmCharCaseMap]
          [pcm::(const ScmCharCaseMap*)]
          [i::int 0])
     (get-arg ch scode)
     (when (< (SCM_VECTOR_SIZE buf) SCM_CHAR_FULL_CASE_MAPPING_SIZE)
       (Scm_Error "[internal] buffer too small for %char-xcase-extended."))
     (set! pcm (Scm__CharCaseMap ch (& cm) TRUE))
     (case kind
       [(CHAR_UPCASE) (fill-result to_upper_full to_upper_simple)] ;toupper
       [(CHAR_DOWNCASE) (fill-result to_lower_full to_lower_simple)] ;tolower
       [(CHAR_TITLECASE) (fill-result to_title_full to_title_simple)] ;totitle
       [else (return 0)])))

 (define-enum SCM_CHAR_FULL_CASE_MAPPING_SIZE)
 )

;; Common args in the following routines
;;
;; Item = Char | Integer
;; generator :: IO () -> Item
;;
;; sink :: ([Item], Item) -> IO ()
;;  Receives a list of items (first arg) and put them (after suitable
;;  conversion) to the current output.  If any of the items cannot be
;;  supportable in the current encoding, the items in the first arg should
;;  be all discarded and the second item should be output (after conversion).

(define (%tr ch buf kind sink char?)
  (let1 cnt (%char-xcase-extended ch buf kind char?)
    (sink (map (cut vector-ref buf <>) (iota cnt)) ch)))

(define (%upcase generator sink char?)
  (let1 buf (make-vector SCM_CHAR_FULL_CASE_MAPPING_SIZE)
    (generator-for-each (^[ch] (%tr ch buf CHAR_UPCASE sink char?)) generator)))

;; Greek capital sigma U+03a3 and final sigma U+03c2.
(define-constant .capital-sigma. #\u03a3)
(define-constant .final-sigma. #\u03c2)

;; Greek capital sigma U+03a3 needs context-sensitive conversion.
(define (%downcase generator sink char?)
  (let ([breaker (make-word-breaker generator)]
        [buf (make-vector SCM_CHAR_FULL_CASE_MAPPING_SIZE)])
    (receive (ch break?) (breaker)
      (let loop ([ch ch] [prev-break? break?])
        (unless (eof-object? ch)
          (receive (next break?) (breaker)
            (if (and (memv ch `(,.capital-sigma. #x03a3))
                     (not prev-break?)
                     break?)
              (sink `(,(if char? .final-sigma. #x03c2)) ch)
              (%tr ch buf CHAR_DOWNCASE sink char?))
            (loop next break?)))))))

(define (%titlecase generator sink char?)
  (let ([breaker (make-word-breaker generator)]
        [buf (make-vector SCM_CHAR_FULL_CASE_MAPPING_SIZE)])
    (receive (ch break?) (breaker)
      (let loop ([ch ch] [prev-break? break?])
        (unless (eof-object? ch)
          (receive (next break?) (breaker)
            (cond [prev-break? (%tr ch buf CHAR_TITLECASE sink char?)]
                  [(and (memv ch `(,.capital-sigma. #x03a3))
                        (not prev-break?)
                        break?)
                   (sink `(,(if char? .final-sigma. #x03c2)) ch)]
                  [else (%tr ch buf CHAR_DOWNCASE sink char?)])
            (loop next break?)))))))

(define (%foldcase generator sink char?)
  (let ([breaker (make-word-breaker generator)]
        [buf1 (make-vector SCM_CHAR_FULL_CASE_MAPPING_SIZE)]
        [buf2 (make-vector SCM_CHAR_FULL_CASE_MAPPING_SIZE)])
    (receive (ch break?) (breaker)
      (let loop ([ch ch] [prev-break? break?])
        (unless (eof-object? ch)
          (receive (next break?) (breaker)
            (dotimes [i (%char-xcase-extended ch buf1 CHAR_UPCASE char?)]
              (%tr (vector-ref buf1 i) buf2 CHAR_DOWNCASE sink char?))
            (loop next break?)))))))

(define string-xcase
  (case (gauche-character-encoding)
    [(utf-8)
     (^[str doer] (with-string-io str
                    (^[] (doer read-char (^[cs alt] (map display cs)) #t))))]
    [else
     (^[str doer] (with-string-io str
                    (^[] (doer (gmap char->ucs read-char)
                               (^[cs alt]
                                 (let1 cs_ (map ucs->char cs)
                                   (if (every char? cs_)
                                     (for-each display cs_)
                                     (display alt))))
                               #f))))]))

(define (codepoints-xcase seq doer)
  (with-builder ((class-of seq) add! get)
    (doer (x->generator seq) (^(cs alt) (for-each add! cs)) #f)
    (get)))

;; APIs
(define (string-upcase str)    (string-xcase str %upcase))
(define (string-downcase str)  (string-xcase str %downcase))
(define (string-titlecase str) (string-xcase str %titlecase))
(define (string-foldcase str)  (string-xcase str %foldcase))

(define (codepoints-upcase seq)    (codepoints-xcase seq %upcase))
(define (codepoints-downcase seq)  (codepoints-xcase seq %downcase))
(define (codepoints-titlecase seq) (codepoints-xcase seq %titlecase))
(define (codepoints-foldcase seq)  (codepoints-xcase seq %foldcase))

;;
;; R7RS-compatible case-insensitive string comparison
;;

;; NB: Builtin string=? etc. can take more than two args, so the simplest
;; definition would be:
;;  (define (string-ci=? . args) (apply string=? (map string-foldcase args)))
;; We compare pairwise instead, so that we don't need to foldcase
;; more than necessary when we find the condition doesn't meet.

(define (string-ci-cmp =? s0 s1 more)
  (let loop ([s0 (string-foldcase s0)] [s1 (string-foldcase s1)] [more more])
    (if (null? more)
      (=? s0 s1)
      (and (=? s0 s1) (loop s1 (string-foldcase (car more)) (cdr more))))))

(define (string-ci=? s0 s1 . more)  (string-ci-cmp string=? s0 s1 more))
(define (string-ci<? s0 s1 . more)  (string-ci-cmp string<? s0 s1 more))
(define (string-ci<=? s0 s1 . more) (string-ci-cmp string<=? s0 s1 more))
(define (string-ci>? s0 s1 . more)  (string-ci-cmp string>? s0 s1 more))
(define (string-ci>=? s0 s1 . more) (string-ci-cmp string>=? s0 s1 more))
