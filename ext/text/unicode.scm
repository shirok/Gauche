;;;
;;; text.unicode - Various Unicode-specific operations
;;;
;;;   Copyright (c) 2011-2015  Shiro Kawai  <shiro@acm.org>
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

(define-module text.unicode
  (use gauche.uvector)
  (use gauche.sequence)
  (use gauche.generator)
  (use util.match)
  (use data.queue)
  (use srfi-42)
  (export ucs4->utf8  utf8-length  utf8->ucs4
          ucs4->utf16 utf16-length utf16->ucs4
          utf8->string string->utf8

          make-word-breaker
          make-word-reader
          string->words codepoints->words
          make-grapheme-cluster-breaker
          make-grapheme-cluster-reader
          string->grapheme-clusters codepoints->grapheme-clusters

          string-upcase string-downcase string-titlecase string-foldcase
          codepoints-upcase codepoints-downcase codepoints-titlecase
          codepoints-foldcase

          string-ci=? string-ci<? string-ci<=? string-ci>? string-ci>=?
          )
  )
(select-module text.unicode)

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


;; Returns list of integers
(define (ucs4->utf8 code :optional (strictness 'strict))
  (cond [(< code 0)
         (ecase strictness
           [(strict permissive) (errorf "Invalid unicode codepoint: ~x" code)]
           [(ignore) '()])]
        [(< code #x80)     `(,code)]
        [(< code #x800)    `(,(utf8-2 code) ,(utf8-z code))]
        [(< code #x10000)
         (if (<= #xd800 code #xdfff)
           (ecase strictness
             [(strict)
              (errorf "Codepoint doesn't encode a character: ~4,'0x" code)]
             [(permissive) `(,(utf8-3 code) ,(utf8-y code) ,(utf8-z code))]
             [(ignore) '()])
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
                  )])]
        ))

(define (utf8-length octet :optional (strictness 'strict))
  (define (err-illegal octet)
    (errorf "Illegal utf-8 octet: ~2,'0x" octet))
  (cond [(< octet 0) (err-illegal octet)]
        [(< octet #x80) 1]
        [(< octet #xc0) (ecase strictness
                          [(strict permissive) (err-illegal octet)]
                          [(ignore) 0])]
        [(< octet #xe0) 2]
        [(< octet #xf0) 3]
        [(< octet #xf8) 4]
        [(> octet #xff) (err-illegal octet)]
        [else (ecase strictness
                [(strict) (err-illegal octet)]
                [(ignore) 0]
                [(permissive) (cond [(< octet #xfc) 5]
                                    [(< octet #xfe) 6]
                                    [else (err-illegal octet)])])]))

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
         [(1) (values b #f)]
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
      [(ignore)     `(values 0 ,(string->symbol #"u1-~|strictness|"))]))
  (define (bad-octet strictness)
    (case strictness
      [(strict permissive) `(values 'bad #f)]
      [(ignore)            `(values b #t)]))
  (define (end-input strictness)
    (case strictness
      [(strict permissive) `(values 'bad #f)]
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

(define utf8->ucs4 #f)

(let ()
  (decode-utf8-gen strict)
  (decode-utf8-gen permissive)
  (decode-utf8-gen ignore)

  (define (%utf8->ucs4 octets :optional (strictness 'strict))
    (define initial-state (ecase strictness
                            [(strict)     u1-strict]
                            [(permissive) u1-permissive]
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

  (set! utf8->ucs4 %utf8->ucs4)
  )

(define (ucs4->utf16 code :optional (strictness 'strict))
  (cond [(< code #xd800) `(,code)]
        [(< code #xe000)
         (ecase strictness
           [(strict)     (errorf "Converting codepoint ~x to utf-16 \
                                  will lose information" code)]
           [(ignore)     '()]
           [(permissive) `(,code)])]
        [(< code #x10000) `(,code)]
        [(< code #x110000) (let1 code (- code #x10000)
                             `(,(logior (ash code -10) #xd800)
                               ,(logior (logand code #x3ff) #xdc00)))]
        [else
         (ecase strictness
           [(strict permissive)
            (errorf "Input outside of Unicode codepoint range: #x~x" code)]
           [(ignore) '()])]))

(define (utf16-length word :optional (strictness 'strict))
  (cond [(<= #xd800 word #xdbff) 2]
        [(<= #xdc00 word #xdfff)
         (ecase strictness
           [(strict)    (errorf "unpaired surrogate: #x~x" word)]
           [(permissive) 1]
           [(ignore)     0])]
        [(<= 0 word #xffff) 1]
        [else (ecase strictness
                [(strict permissive) (errorf "Illegal utf-16 word: #x~x" word)]
                [(ignore) 0])]))

(define utf16->ucs4 #f)

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
                            [(strict) (values w 'lone)]
                            [(permissive) (values w 'end)]
                            [(ignore) (values 0 'reset)])]
            [(< w #x10000) (values w 'end)]
            [else (values w 'range)])
      ;; second word
      (cond [(eof-object? w) (ecase strictness
                               [(strict)     (values prev 'lone)]
                               [(permissive) (values prev 'end)]
                               [(ignore)     (values eof 'end)])]
            [(<= #xdc00 w #xdfff)
             (values (+ #x10000 (ash (logand #x3ff prev) 10) (logand #x3ff w))
                     'end)]
            [(<= 0 w #xffff)
             (ecase strictness
               [(strict)     (values prev 'lone)]
               [(permissive) (values prev 'unget)]  ; don't consume w
               [(ignore)     (values w 'skip)])]
            [else (values w 'range)])))

  (define (%utf16->ucs4 words :optional (strictness 'strict))
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

  (set! utf16->ucs4 %utf16->ucs4)
  )

;; R7RS procedures

;; This module may be precompiled by different platforms, so we dispatch
;; at runtime.
;; At this moment, we don't worry much about performance when the native
;; encoding isn't utf8.
(define (utf8->string bvec :optional (start 0) (end -1))
  (check-arg u8vector? bvec)
  (if (eq? (gauche-character-encoding) 'utf-8)
    (rlet1 s (u8vector->string bvec start end)
      (when (string-incomplete? s)
        (error "invalid utf-8 sequence in u8vector:" bvec)))
    (ces-convert (u8vector->string bvec start end) 'utf-8)))

(define (string->utf8 str :optional (start 0) (end -1))
  (check-arg string? str)
  (if (eq? (gauche-character-encoding) 'utf-8)
    (string->u8vector str start end)
    (let ([start (or start 0)]
          [end (if (and (number? end) (>= end 0)) end (string-length str))])
      (string->u8vector (ces-convert (substring str start end)
                                     (gauche-character-encoding)
                                     'utf-8)))))

;;;
;;;
;;; Grapheme cluster break and word break
;;;
;;;

;;=========================================================================
;; Low level code to look up properties
;;

(inline-stub
 "#include \"unicode_attr.h\""

 (initcode "init_GB_symbols(Scm_CurrentModule());")
 (initcode "init_WB_symbols(Scm_CurrentModule());")

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
   (let* ([ch::int])
     (get-arg ch scode)
     (cond [(< ch #x20000)
            (let* ([k::u_char (aref break_table (>> ch 8))])
              (if (== k 255)
                (return GB_Other)
                (let* ([b::u_char (aref break_subtable k (logand ch #xff))])
                  (return (>> b 4)))))]
           [(or (== #xE0001 ch)
                (and (<= #xE0020 ch) (<= ch #xE007F))) (return GB_Control)]
           [(and (<= #xE0100 ch) (<= ch #xE01EF)) (return GB_Extend)]
           [else (return GB_Other)])))

 (define-cproc wb-property (scode) ::<int>
   (let* ([ch::int])
     (get-arg ch scode)
     (cond [(< ch #x20000)
            (let* ([k::u_char (aref break_table (>> ch 8))])
              (if (== k 255)
                (return WB_Other)
                (let* ([b::u_char (aref break_subtable k (logand ch #xff))])
                  (return (logand b #x0f)))))]
           [(or (== #xE0001 ch)
                (and (<= #xE0020 ch) (<= ch #xE007F))) (return WB_Format)]
           [(and (<= #xE0100 ch) (<= ch #xE01EF)) (return WB_Extend)]
           [else (return WB_Other)])))
 )

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
              (cluster-reader-maker (generator-map char->ucs read-char)
                                    (^[cs] (map-to <string> ucs->char cs))))))]
    ))

(define (get-sequence-generator seq)
  (let-syntax ([make-dispatcher
                (syntax-rules ()
                  [(_ (pred ref) ...)
                   (cond [(pred seq)
                          (let ([len (size-of seq)]
                                [i 0])
                            (^[] (if (= i len)
                                   eof
                                   (begin0 (ref seq i) (inc! i)))))]
                         ...)])])
    (if (list? seq)
      (let1 q (list->queue seq)
        (cut dequeue! q eof))
      (make-dispatcher
       [vector?    vector-ref]
       [u8vector?  u8vector-ref]
       [s8vector?  s8vector-ref]
       [u16vector? u16vector-ref]
       [s16vector? s16vector-ref]
       [u32vector? u32vector-ref]
       [s32vector? s32vector-ref]
       [identity   ref]))))

(define (make-sequence-splitter cluster-reader-maker)
  (^[seq]
    (let1 gen (get-sequence-generator seq)
      (generator->list (cluster-reader-maker gen identity)))))

;;=========================================================================
;; Word breaker state transition tables
;;
;; The Rule WB6 and WB12 requires another character lookahead.  However,
;; with WB4, we'll need unlimited lookahead.  To keep the state machine
;; simple, we handle WB6 and WB12 specially.
;; Each state is a vector indexed by Word Break Property.  Each entry
;; is a pair, whose car is a flag, and whose cdr is the next state.
;; The flag may be #t (break), #f (not break), wb6 (special lookahead for
;; WB6) or wb12 (special lookahead for WB12).

(define *word-break-fa*
  '#(;; 0 - Initial state (sot)
     #((#t   .  1)                      ; CR (WB1)
       (#t   .  2)                      ; LF (WB1)
       (#t   .  2)                      ; Newline (WB1)
       (#t   .  3)                      ; Extend (WB1)
       (#t   .  3)                      ; Format (WB1)
       (#t   .  8)                      ; Katakana (WB1)
       (#t   .  4)                      ; ALetter (WB1)
       (#t   . 10)                      ; MidLetter (WB1)
       (#t   . 10)                      ; MidNum (WB1)
       (#t   . 10)                      ; MidNumLet (WB1)
       (#t   .  6)                      ; Numeric (WB1)
       (#t   .  9)                      ; ExtendNumLet (WB1)
       (#t   . 10))                     ; Other (WB1)
     ;; 1 - CR
     #((#t   .  1)                      ; CR (WB3a)
       (#f   .  2)                      ; LF (WB3)
       (#t   .  2)                      ; Newline (WB3a)
       (#t   .  3)                      ; Extend (WB3a)
       (#t   .  3)                      ; Format (WB3a)
       (#t   .  8)                      ; Katakana (WB3a)
       (#t   .  4)                      ; ALetter (WB3a)
       (#t   . 10)                      ; MidLetter (WB3a)
       (#t   . 10)                      ; MidNum (WB3a)
       (#t   . 10)                      ; MidNumLet (WB3a)
       (#t   .  6)                      ; Numeric (WB3a)
       (#t   .  9)                      ; ExtendNumLet (WB3a)
       (#t   . 10))                     ; Other (WB3a)
     ;; 2 - LF and Newline
     #((#t   .  1)                      ; CR (WB3a)
       (#t   .  2)                      ; LF (WB3a)
       (#t   .  2)                      ; Newline (WB3a)
       (#t   .  3)                      ; Extend (WB3a)
       (#t   .  3)                      ; Format (WB3a)
       (#t   .  8)                      ; Katakana (WB3a)
       (#t   .  4)                      ; ALetter (WB3a)
       (#t   . 10)                      ; MidLetter (WB3a)
       (#t   . 10)                      ; MidNum (WB3a)
       (#t   . 10)                      ; MidNumLet (WB3a)
       (#t   .  6)                      ; Numeric (WB3a)
       (#t   .  9)                      ; ExtendNumLet (WB3a)
       (#t   . 10))                     ; Other (WB3a)
     ;; 3 - Sep + (Extend|Format)
     ;;  This rule is not explicit in Word Boundary Rules, but we need it to
     ;;  handle the case that sequence of (Extend|Format) right after sot, lf,
     ;;  and newline elements.
     #((#t   .  1)                      ; CR (WB3b)
       (#t   .  2)                      ; LF (WB3b)
       (#t   .  2)                      ; Newline (WB3b)
       (#f   .  3)                      ; Extend (WB4)
       (#f   .  3)                      ; Format (WB4)
       (#t   .  8)                      ; Katakana
       (#t   .  4)                      ; ALetter
       (#t   . 10)                      ; MidLetter
       (#t   . 10)                      ; MidNum
       (#t   . 10)                      ; MidNumLet
       (#t   .  6)                      ; Numeric
       (#t   .  9)                      ; ExtendNumLet
       (#t   . 10))                     ; Other
     ;; 4 - ALetter
     #((#t   .  1)                      ; CR (WB3b)
       (#t   .  2)                      ; LF (WB3b)
       (#t   .  2)                      ; Newline (WB3b)
       (#f   .  4)                      ; Extend (WB4)
       (#f   .  4)                      ; Format (WB4)
       (#t   .  8)                      ; Katakana
       (#f   .  4)                      ; ALetter (WB5)
       (wb6  .  5)                      ; MidLetter (WB6)
       (#t   . 10)                      ; MidNum
       (wb6  .  5)                      ; MidNumLet (WB6)
       (#f   .  6)                      ; Numeric (WB9)
       (#f   .  9)                      ; ExtendNumLet (WB13a)
       (#t   . 10))                     ; Other
     ;; 5 - ALetter + (MidLetter|MidNumLet)
     #((#t   .  1)                      ; CR (WB3b)
       (#t   .  2)                      ; LF (WB3b)
       (#t   .  2)                      ; Newline (WB3b)
       (#f   .  5)                      ; Extend (WB4)
       (#f   .  5)                      ; Format (WB4)
       (#t   .  8)                      ; Katakana
       (#f   .  4)                      ; ALetter (WB7)
       (#t   . 10)                      ; MidLetter
       (#t   . 10)                      ; MidNum
       (#t   . 10)                      ; MidNumLet
       (#t   .  6)                      ; Numeric
       (#t   .  9)                      ; ExtendNumLet
       (#t   . 10))                     ; Other
     ;; 6 - Numeric
     #((#t   .  1)                      ; CR (WB3b)
       (#t   .  2)                      ; LF (WB3b)
       (#t   .  2)                      ; Newline (WB3b)
       (#f   .  6)                      ; Extend (WB4)
       (#f   .  6)                      ; Format (WB4)
       (#t   .  8)                      ; Katakana
       (#f   .  4)                      ; ALetter (WB10)
       (#t   . 10)                      ; MidLetter
       (wb12 .  7)                      ; MidNum (WB12)
       (wb12 .  7)                      ; MidNumLet (WB12)
       (#f   .  6)                      ; Numeric (WB8)
       (#f   .  9)                      ; ExtendNumLet (WB13a)
       (#t   . 10))                     ; Other
     ;; 7 - Numeric + (MidNum|MidNumLet)
     #((#t   .  1)                      ; CR (WB3b)
       (#t   .  2)                      ; LF (WB3b)
       (#t   .  2)                      ; Newline (WB3b)
       (#f   .  7)                      ; Extend (WB4)
       (#f   .  7)                      ; Format (WB4)
       (#t   .  8)                      ; Katakana
       (#t   .  4)                      ; ALetter
       (#t   . 10)                      ; MidLetter
       (#t   . 10)                      ; MidNum
       (#t   . 10)                      ; MidNumLet
       (#f   .  6)                      ; Numeric (WB11)
       (#t   .  9)                      ; ExtendNumLet
       (#t   . 10))                     ; Other
     ;; 8 - Katakana
     #((#t   .  1)                      ; CR (WB3b)
       (#t   .  2)                      ; LF (WB3b)
       (#t   .  2)                      ; Newline (WB3b)
       (#f   .  8)                      ; Extend (WB4)
       (#f   .  8)                      ; Format (WB4)
       (#f   .  8)                      ; Katakana (WB13)
       (#t   .  4)                      ; ALetter
       (#t   . 10)                      ; MidLetter
       (#t   . 10)                      ; MidNum
       (#t   . 10)                      ; MidNumLet
       (#t   .  6)                      ; Numeric
       (#f   .  9)                      ; ExtendNumLet (WB13a)
       (#t   . 10))                     ; Other
     ;; 9 - ExtendNumLet
     #((#t   .  1)                      ; CR (WB3b)
       (#t   .  2)                      ; LF (WB3b)
       (#t   .  2)                      ; Newline (WB3b)
       (#f   .  9)                      ; Extend (WB4)
       (#f   .  9)                      ; Format (WB4)
       (#f   .  8)                      ; Katakana (WB13b)
       (#f   .  4)                      ; ALetter (WB13b)
       (#t   . 10)                      ; MidLetter
       (#t   . 10)                      ; MidNum
       (#t   . 10)                      ; MidNumLet
       (#f   .  6)                      ; Numeric (WB13b)
       (#f   .  9)                      ; ExtendNumLet (WB13a)
       (#t   . 10))                     ; Other
     ;; 10 - Other
     #((#t   .  1)                      ; CR (WB3b)
       (#t   .  2)                      ; LF (WB3b)
       (#t   .  2)                      ; Newline (WB3b)
       (#f   . 10)                      ; Extend (WB4)
       (#f   . 10)                      ; Format (WB4)
       (#t   .  8)                      ; Katakana
       (#t   .  4)                      ; ALetter
       (#t   . 10)                      ; MidLetter
       (#t   . 10)                      ; MidNum
       (#t   . 10)                      ; MidNumLet
       (#t   .  6)                      ; Numeric
       (#t   .  9)                      ; ExtendNumLet
       (#t   . 10))                     ; Other
     ))

;; API
;; Returns a generator procedure.  Every time it is called, it returns
;; two values - a character/codepoint, taken from the generator,
;; and a boolean value indicates if a word breaks before that
;; character/codepoint.  Note that unlimited characters/codepoints
;; can be taken from the generator to determine word boundaries.
;; The end of the input is indicated by #<eof>.
(define (make-word-breaker generator)
  (define current-state (vector-ref *word-break-fa* 0))
  (define q (make-queue))  ;characters looked ahead.
  (^[]
    (let1 ch (if (queue-empty? q) (generator) (dequeue! q))
      (if (eof-object? ch)
        (values ch #t) ; WB2
        (let1 p (vector-ref current-state (wb-property ch))
          (set! current-state (vector-ref *word-break-fa* (cdr p)))
          (cond [(boolean? (car p)) (values ch (car p))]
                [(eq? (car p) 'wb6) (wb-lookahead generator q ch WB_ALetter)]
                [(eq? (car p) 'wb12)(wb-lookahead generator q ch WB_Numeric)]
                [else (error "[internal] bad state:" p)]))))))

(define (wb-lookahead generator q ch next-prop)
  ;; q must be empty here, but we check just in case.
  (unless (queue-empty? q) (error "[internal] wb6 or wb12 incorrect state"))
  (let loop ([ch2 (generator)])
    (if (eof-object? ch2)
      (values ch #t)
      (let1 prop (wb-property ch2)
        (enqueue! q ch2)
        (cond [(eqv? prop next-prop) (values ch #f)]
              [(eqv? prop WB_Extend) (loop (generator))]
              [(eqv? prop WB_Format) (loop (generator))]
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
(define *grapheme-break-fa*
  '#(;; 0 initial state - sot
     #((#t .  1)                        ;CR (GB1)
       (#t .  2)                        ;LF (GB1)
       (#t .  2)                        ;Control (GB1)
       (#t .  3)                        ;Extend (GB1)
       (#t .  4)                        ;Prepend (GB1)
       (#t .  5)                        ;SpacingMark (GB1)
       (#t .  6)                        ;L (GB1)
       (#t .  7)                        ;V (GB1)
       (#t .  8)                        ;T (GB1)
       (#t .  9)                        ;LV (GB1)
       (#t . 10)                        ;LVT (GB1)
       (#t . 11))                       ;Other (GB1)
     ;; 1 CR
     #((#t .  1)                        ;CR (GB4)
       (#f .  2)                        ;LF (GB3)
       (#t .  2)                        ;Control (GB4, GB5)
       (#t .  3)                        ;Extend (GB4)
       (#t .  4)                        ;Prepend (GB4)
       (#t .  5)                        ;SpacingMark (GB4)
       (#t .  6)                        ;L (GB4)
       (#t .  7)                        ;V (GB4)
       (#t .  8)                        ;T (GB4)
       (#t .  9)                        ;LV (GB4)
       (#t . 10)                        ;LVT (GB4)
       (#t . 11))                       ;Other (GB4)
     ;; 2 LF, Control
     #((#t .  1)                        ;CR (GB4)
       (#t .  2)                        ;LF (GB4, GB5)
       (#t .  2)                        ;Control (GB4, GB5)
       (#t .  3)                        ;Extend (GB4)
       (#t .  4)                        ;Prepend (GB4)
       (#t .  5)                        ;SpacingMark (GB4)
       (#t .  6)                        ;L (GB4)
       (#t .  7)                        ;V (GB4)
       (#t .  8)                        ;T (GB4)
       (#t .  9)                        ;LV (GB4)
       (#t . 10)                        ;LVT (GB4)
       (#t . 11))                       ;Other (GB4)
     ;; 3 Extend
     #((#t .  1)                        ;CR (GB5)
       (#t .  2)                        ;LF (GB5)
       (#t .  2)                        ;Control (GB5)
       (#f .  3)                        ;Extend (GB9)
       (#t .  4)                        ;Prepend (GB10)
       (#f .  5)                        ;SpacingMark (GB9a)
       (#t .  6)                        ;L (GB10)
       (#t .  7)                        ;V (GB10)
       (#t .  8)                        ;T (GB10)
       (#t .  9)                        ;LV (GB10)
       (#t . 10)                        ;LVT (GB10)
       (#t . 11))                       ;Other (GB10)
     ;; 4 Prepend
     #((#t .  1)                        ;CR (GB5)
       (#t .  2)                        ;LF (GB5)
       (#t .  2)                        ;Control (GB5)
       (#f .  3)                        ;Extend (GB9, GB9b)
       (#f .  4)                        ;Prepend (GB9b)
       (#f .  5)                        ;SpacingMark (GB9a, GB9b)
       (#f .  6)                        ;L (GB9b)
       (#f .  7)                        ;V (GB9b)
       (#f .  8)                        ;T (GB9b)
       (#f .  9)                        ;LV (GB9b)
       (#f . 10)                        ;LVT (GB9b)
       (#f . 11))                       ;Other (GB9b)
     ;; 5 SpacingMark
     #((#t .  1)                        ;CR (GB5)
       (#t .  2)                        ;LF (GB5)
       (#t .  2)                        ;Control (GB5)
       (#f .  3)                        ;Extend (GB9)
       (#t .  4)                        ;Prepend (GB10)
       (#f .  5)                        ;SpacingMark (GB9a)
       (#t .  6)                        ;L (GB10)
       (#t .  7)                        ;V (GB10)
       (#t .  8)                        ;T (GB10)
       (#t .  9)                        ;LV (GB10)
       (#t . 10)                        ;LVT (GB10)
       (#t . 11))                       ;Other (GB10)
     ;; 6 Hangul L
     #((#t .  1)                        ;CR (GB5)
       (#t .  2)                        ;LF (GB5)
       (#t .  2)                        ;Control (GB5)
       (#f .  3)                        ;Extend (GB9)
       (#t .  4)                        ;Prepend (GB10)
       (#f .  5)                        ;SpacingMark (GB9a)
       (#f .  6)                        ;L (GB6)
       (#f .  7)                        ;V (GB6)
       (#t .  8)                        ;T (GB10)
       (#f .  9)                        ;LV (GB6)
       (#f . 10)                        ;LVT (GB6)
       (#t . 11))                       ;Other (GB10)
     ;; 7 Hangul V
     #((#t .  1)                        ;CR (GB5)
       (#t .  2)                        ;LF (GB5)
       (#t .  2)                        ;Control (GB5)
       (#f .  3)                        ;Extend (GB9)
       (#t .  4)                        ;Prepend (GB10)
       (#f .  5)                        ;SpacingMark (GB9a)
       (#t .  6)                        ;L (GB10)
       (#f .  7)                        ;V (GB7)
       (#f .  8)                        ;T (GB7)
       (#t .  9)                        ;LV (GB10)
       (#t . 10)                        ;LVT (GB10)
       (#t . 11))                       ;Other (GB10)
     ;; 8 Hangul T
     #((#t .  1)                        ;CR (GB5)
       (#t .  2)                        ;LF (GB5)
       (#t .  2)                        ;Control (GB5)
       (#f .  3)                        ;Extend (GB9)
       (#t .  4)                        ;Prepend (GB10)
       (#f .  5)                        ;SpacingMark (GB9a)
       (#t .  6)                        ;L (GB10)
       (#t .  7)                        ;V (GB10)
       (#f .  8)                        ;T (GB8)
       (#t .  9)                        ;LV (GB10)
       (#t . 10)                        ;LVT (GB10)
       (#t . 11))                       ;Other (GB10)
     ;; 9 Hangul LV
     #((#t .  1)                        ;CR (GB5)
       (#t .  2)                        ;LF (GB5)
       (#t .  2)                        ;Control (GB5)
       (#f .  3)                        ;Extend (GB9)
       (#t .  4)                        ;Prepend (GB10)
       (#f .  5)                        ;SpacingMark (GB9a)
       (#t .  6)                        ;L (GB10)
       (#f .  7)                        ;V (GB7)
       (#f .  8)                        ;T (GB7)
       (#t .  9)                        ;LV (GB10)
       (#t . 10)                        ;LVT (GB10)
       (#t . 11))                       ;Other (GB10)
     ;; 10 Hangul LVT
     #((#t .  1)                        ;CR (GB5)
       (#t .  2)                        ;LF (GB5)
       (#t .  2)                        ;Control (GB5)
       (#f .  3)                        ;Extend (GB9)
       (#t .  4)                        ;Prepend (GB10)
       (#f .  5)                        ;SpacingMark (GB9a)
       (#t .  6)                        ;L (GB10)
       (#t .  7)                        ;V (GB10)
       (#f .  8)                        ;T (GB8)
       (#t .  9)                        ;LV (GB10)
       (#t . 10)                        ;LVT (GB10)
       (#t . 11))                       ;Other (GB10)
     ;; 11 Other
     #((#t .  1)                        ;CR (GB5)
       (#t .  2)                        ;LF (GB5)
       (#t .  2)                        ;Control (GB5)
       (#f .  3)                        ;Extend (GB9)
       (#t .  4)                        ;Prepend (GB10)
       (#f .  5)                        ;SpacingMark (GB9a)
       (#t .  6)                        ;L (GB10)
       (#t .  7)                        ;V (GB10)
       (#t .  8)                        ;T (GB10)
       (#t .  9)                        ;LV (GB10)
       (#t . 10)                        ;LVT (GB10)
       (#t . 11))                       ;Other (GB10)
     ))

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
;;; String casing
;;;

(inline-stub
 "#include <gauche/char_attr.h>"
 "#define CHAR_UPCASE 0"
 "#define CHAR_DOWNCASE 1"
 "#define CHAR_TITLECASE 2"
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
   (let* ([ch::int]
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
       )))

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
    (sink (list-ec (: i cnt) (vector-ref buf i)) ch)))

(define (%upcase generator sink char?)
  (let1 buf (make-vector SCM_CHAR_FULL_CASE_MAPPING_SIZE)
    (generator-for-each (^[ch] (%tr ch buf CHAR_UPCASE sink char?)) generator)))

;; Greek capital sigma U+03a3 and final sigma U+03c2.
;; We can't use character literal #\u03a3 etc., for they're not valid
;; when internal encoding is 'none'.  We intend to make utf-8 support
;; mandatory in future, and this limitation will be lifted.
;; For now, they'll be #f when the internal encoding is 'none'.
(define-constant .capital-sigma.
  (cond-expand
   [gauche.ces.none #f]
   [else (ucs->char #x03a3)]))
(define-constant .final-sigma.
  (cond-expand
   [gauche.ces.none #f]
   [else (ucs->char #x03c2)]))

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
            (do-ec
             (: i (%char-xcase-extended ch buf1 CHAR_UPCASE char?))
             (%tr (vector-ref buf1 i) buf2 CHAR_DOWNCASE sink char?))
            (loop next break?)))))))

(define string-xcase
  (case (gauche-character-encoding)
    [(utf-8)
     (^[str doer] (with-string-io str
                    (^[] (doer read-char (^[cs alt] (map display cs)) #t))))]
    [else
     (^[str doer] (with-string-io str
                    (^[] (doer (generator-map char->ucs read-char)
                               (^[cs alt]
                                 (let1 cs_ (map ucs->char cs)
                                   (if (every char? cs_)
                                     (for-each display cs_)
                                     (display alt))))
                               #f))))]))

(define (codepoints-xcase seq doer)
  (with-builder ((class-of seq) add! get)
    (doer (get-sequence-generator seq) (^(cs alt) (for-each add! cs)) #f)
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
