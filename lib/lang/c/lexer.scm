;;;
;;; C lexical analyzer
;;;
;;;   Copyright (c) 2021-2022  Shiro Kawai  <shiro@acm.org>
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

;; This is a submodule of lang.c.parser.  Not for general public use.
(define-module lang.c.lexer
  (use util.match)
  (use parser.peg)
  (use lang.c.type)
  (export c-tokenize)
  )
(select-module lang.c.lexer)

;;;
;;; Lexical elements (6.4)
;;;

;; Tokenizer output: sequence of <token>s.
;;
;; <token> := <punctuator>
;;         |  <keyword>
;;         |  <identifier>
;;         |  <constant>
;;         |  <string-literal>
;;
;; <punctuator>     : <symbol> such as '|(| etc.
;; <keyword>        : <symbol>
;; <identifier>     : (ident <symbol>)
;; <constant>       : (const <const-type> <value>)
;; <string-literal> : (string <value>) | (wstring <value>)
;;
;;  <const-type> is either one of the basic type symbols defined in
;;  lang.c.type, or wchar_t.
;;
;;  <value> is a Scheme string as it appears in the source, except unicode
;;  character sequences and excape sequences are converted to corresponding
;;  characters.

;; Intertoken stuff

(define %whitespace ($one-of #[ \n\r\t]))
(define %long-comment ($seq ($. "/*")
                            ($many_ ($seq ($not ($. "*/"))
                                          ($any)))
                            ($. "*/")))
(define %line-comment ($seq ($. "//")($many-till_ ($any) ($. "\n"))))
(define %pragma ($seq ($. #\#) ($many_ ($none-of #[\n]))))

(define %spacing
  ($many_ ($or %whitespace %long-comment %line-comment %pragma
               ($lazy %ignorable-keyword))))

;; Punctuators (6.4.6)
;; We return a char for paren-like stuff, and
;; symbols for other punctuators.

(define ($->sym x) ($lift string->symbol ($. x)))

(define %punctuator
  ($or ($->sym"...") ($->sym"<<=") ($->sym">>=")
       ($->sym"->")
       ($->sym"++") ($->sym"--")
       ($->sym"&&") ($->sym"||")
       ($->sym"<<") ($->sym">>")
       ($->sym"<=") ($->sym">=") ($->sym"==") ($->sym"!=")
       ($->sym"*=") ($->sym"/=") ($->sym"%=") ($->sym"+=")
       ($->sym"-=") ($->sym"&=") ($->sym"^=") ($->sym"|=")
       ($seq ($."<:") ($return #\[))
       ($seq ($.":>") ($return #\]))
       ($seq ($."<%") ($return #\{))
       ($seq ($."%>") ($return #\}))
       ($seq ($."%:%:") ($return '|##|))
       ($seq ($."%:") ($return '|#|))
       ($->sym"*") ($->sym"+") ($->sym"-") ($->sym "/") ($->sym "%")
       ($->sym"&") ($->sym"|") ($->sym"^") ($->sym"~") ($->sym "!")
       ($->sym"<") ($->sym">")
       ($->sym"?") ($->sym":") ($->sym";") ($->sym"=")
       ($->sym",") ($->sym"##") ($->sym"#") ($->sym".")
       ($. #[\[\]\(\)\{\}])))

;; Keywords (6.4.1)
;; We return a symbol.

(define %hexquad ($lift (^[xs] (string->number (list->string xs) 16))
                        ($repeat ($. #[[:xdigit:]]) 4)))

(define %unichar-escaped
  ($or ($lift (^[_ n] (integer->char n))
              ($. #\u) %hexquad)
       ($lift (^[_ n0 n1] (integer->char (+ (* n0 #x10000) n1)))
              ($. #\U) %hexquad %hexquad)))

(define %unichar-name ($seq ($. #\\) %unichar-escaped))

(define %idnondigit ($or ($. #[a-zA-Z_]) %unichar-name))

(define %idchar ($or ($. #[a-zA-Z0-9_]) %unichar-name))

;; <symbol> - standard
;; (<symbol> <alt-symbol>) - match <symbol>, return <alt-symbol>
(define *keywords*
  '(auto break case char const continue default double do else enum
    extern float for goto if int inline long register restrict
    return short signed sizeof static struct switch typedef
    union unsigned void volatile while
    _Bool _Complex _Imaginary
    ;; nonstandard keywords
    _stdcall __declspec __builtin_va_list
    ;; gcc additional floating types
    _Float128 (__float128 _Float128)
    _Float64x (__float80 _Float64x)
    __ibm128
    ;; gcc additional keywords
    __attribute__
    (__restrict  restrict)
    (__restrict__ restrict)
    (__inline inline)
    (__inline__ inline)
    (__volatile volatile)
    (__volatile__ volatile)
    (asm asm)
    (__asm asm)
    (__asm__ asm)
    (__alignof__ __alignof__)
    ;; clang additional keywords
    _Nullable _Nonnull _Null_unspecified _Nullable_result
    ))

(define %keyword
  (apply $or (map (^e (if (pair? e)
                        ($try ($seq ($. (symbol->string (car e)))
                                    ($not %idchar)
                                    ($return (cadr e))))
                        ($try ($seq ($. (symbol->string e))
                                    ($not %idchar)
                                    ($return e)))))
                  *keywords*)))

;; gcc specific
(define %ignorable-keyword
  ($try ($seq ($. "__extension__") ($not %idchar))))

;; Identifiers (6.4.2)
;; (ident <symbol>)

(define %identifier
  ($seq ($not %keyword)
        ($lift (^[c cs]
                 `(ident ,(string->symbol (list->string (cons c cs)))))
               %idnondigit ($many %idchar))))

;; Constants (6.4.4)

;; We handle integers and floating point numbers at once.
;; The initial assert is to rule out suffix-only match without consuming
;; input.
(define %dec-oct-numeric-constant
  ($let ([ ($assert ($. #[0-9.])) ]
         [integral   ($many ($. #[0-9]))]
         [point      ($optional ($. "."))]
         [fractional ($many ($. #[0-9]))]
         [exponent   ($optional ($lift list
                                       ($. #[eE])
                                       ($optional ($. #[+-]))
                                       ($many1 ($. #[0-9]))))]
         [suffix     ($many %idchar)])
    (cond
     [(or point exponent) ;; must be flonum
      (if (or (and (pair? integral) point)
              (and point (pair? fractional))
              (and (pair? integral) exponent))
        (if-let1 suf (check-float-suffix suffix)
          ($return `(const ,suf
                           ,(rope->string (list integral
                                                point
                                                fractional
                                                exponent
                                                suffix))))
          ($fail (format "invalid floating point number constant suffix: ~a"
                         (list->string suffix))))
        ($fail "malformed floating point number constant"))]
     [(and (pair? integral) (not point) (null? fractional) (not exponent))
      ;; TODO: check octal constant vailidity
      (if-let1 suf (check-int-suffix suffix)
        ($return `(const ,suf ,(rope->string (list integral suffix))))
        ($fail (format "malformed integer constant suffix: ~a"
                       (list->string suffix))))]
     [else ($fail "malformed numeric constant")])))

(define %hex-numeric-constant
  ($let ([ ($. "0x") ]
         [integral   ($many ($. #[[:xdigit:]]))]
         [point      ($optional ($. "."))]
         [fractional ($many ($. #[[:xdigit:]]))]
         [bin-exp    ($optional ($lift list
                                       ($. #[pP])
                                       ($optional ($. #[+-]))
                                       ($many1 ($. #[[:xdigit:]]))))]
         [suffix     ($many %idchar)])
    (cond
     [(or point bin-exp) ;; must be flonum
      (if (or (and (pair? integral) point)
              (and point (pair? fractional))
              (and (pair? integral) bin-exp))
        (if-let1 suf (check-float-suffix suffix)
          ($return `(const ,suf
                           ,(rope->string (list "0x"
                                                integral
                                                point
                                                fractional
                                                bin-exp
                                                suffix))))
          ($fail (format "invalid floating point number constant suffix: ~a"
                         (list->string suffix))))
        ($fail "malformed hex floating point number constant"))]
     [(and (pair? integral) (not point) (null? fractional) (not bin-exp))
      (if-let1 suf (check-int-suffix suffix)
        ($return `(const ,suf ,(rope->string (list "0x" integral suffix))))
        ($fail (format "malformed integer constant suffix: ~a"
                       (list->string suffix))))]
     [else ($fail "malformed numeric constant")])))

;; we need to reject invalid suffix, so we first gather suffix characters
;; then look at them, instead of using BNF rules.
(define (check-float-suffix chars)
  (match chars
    [() 'double]
    [((or #\f #\F)) 'float]
    [((or #\l #\L)) 'long-double]
    [_ #f]))

(define (check-int-suffix chars)
  (match chars
    [() 'int]
    [((or #\u #\U)) 'u-int]
    [((or #\l #\L)) 'long]
    [((or #\u #\U) (or #\l #\L)) 'u-long]
    [((or #\l #\L) (or #\u #\U)) 'u-long]
    [(or (#\l #\l) (#\L #\L)) 'long-long]
    [(or ((or #\u #\U) #\l #\l) ((or #\u #\U) #\L #\L)) 'u-long-long]
    [(or (#\l #\l (or #\u #\U)) (#\L #\L (or #\u #\U))) 'u-long-long]
    [_ #f]))

(define %escaped-sequence
  ($seq ($. #\\)
        ($or ($. #[\'\"\\\?])
             ($seq ($. #\a) ($return #\alarm))
             ($seq ($. #\b) ($return #\backspace))
             ($seq ($. #\f) ($return #\x0c))
             ($seq ($. #\n) ($return #\newline))
             ($seq ($. #\r) ($return #\return))
             ($seq ($. #\t) ($return #\tab))
             ($seq ($. #\v) ($return #\x0b))
             ($lift (^s (integer->char
                         (string->number (list->string s) 8)))
                    ($many ($. #[0-7]) 1 3))
             ($lift (^[_ s] (integer->char
                             (string->number (list->string s) 16)))
                    ($. #\x) ($many ($. #[[:xdigit:]])))
             %unichar-escaped)))


(define %c-char-sequence ($many ($or ($. #[^'\\\n]) %escaped-sequence)))

(define %character-constant
  ($or ($lift (^c `(const char ,(list->string c)))
              ($between ($. #\')  %c-char-sequence ($. #\')))
       ($lift (^c `(const wchar ,(list->string c)))
              ($between ($. "L'") %c-char-sequence ($. #\')))))

;; enum-constant is syntactically identical to %identifier
(define %constant
  ($or %hex-numeric-constant
       %dec-oct-numeric-constant
       %character-constant))

(define %s-char-sequence ($many ($or ($. #[^\"\\\n]) %escaped-sequence)))

(define %string-literal
  ($or ($lift (^s `(string ,(list->string s)))
              ($between ($. #\")  %s-char-sequence ($. #\")))
       ($lift (^s `(wstring ,(list->string s)))
              ($between ($. "L\"")  %s-char-sequence ($. #\")))))

;; NB: need to match %constant first, to recognize .0 as a number instead
;; of '.' followed by '0'.  Also need to match %string-literal before
;; %identifier, for L"..." should be a wstring instead of an identifier 'L'
;; followed by a string.
(define %token
  ($seq0 ($or ($try %constant)
              %string-literal
              %punctuator
              %keyword
              %identifier)
         %spacing))

(define (make-c-tokenize-generator char-seq)
  (receive (_ char-seq)
      (peg-run-parser %spacing char-seq) ;skip leading whitespaces
    (rec (gen)
      (if (null? char-seq)
        (eof-object)
        (receive (tok next) (peg-run-parser %token char-seq)
          (let1 attrs (pair-attributes char-seq)
            (set! char-seq next)
            (values tok attrs)))))))

;; API - Lexer entry
(define (c-tokenize char-seq)
  (generator->lseq (make-c-tokenize-generator char-seq)))
