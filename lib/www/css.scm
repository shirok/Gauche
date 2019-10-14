;;;
;;; css.scm - Generate and parse CSS
;;;
;;;   Copyright (c) 2014-2019  Shiro Kawai  <shiro@acm.org>
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

;; EXPERIMENTAL : API may change

(define-module www.css
  (use gauche.generator)
  (use gauche.lazy)
  (use parser.peg)
  (use util.match)
  (use text.tree)
  (use srfi-1)
  (use srfi-13)
  (export construct-css simple-selector?
          parse-css parse-css-file
          parse-css-selector-string)
  )
(select-module www.css)

;;;
;;; S-expression CSS
;;;
;;;  <s-css>      : {<style-rule> | <at-rule>} ...
;;;
;;;  <style-rule> : (style-rule <pattern> <declaration> ...)
;;;               | (style-decls <declaration> ...)
;;;
;;;  <pattern>   : <selector> | (:or <selector> ...)
;;;  <selector>  : <simple-selector>
;;;              | <chained-selector>
;;;  <chained-selector> : (<simple-selector> . (<op>? . <chained-selector>))
;;;  <op>        : > | + | ~
;;;  <simple-selector> : <element-name>
;;;              | (<element-name> <option> ...)
;;;  <option>    : (id <name>)                           ; E#id
;;;              | (class <ident>)                       ; E.class
;;;              | (has <ident>)                         ; E[attrib]
;;;              | (= <ident> <attrib-value>)            ; E[attrib=val]
;;;              | (~= <ident> <attrib-value>)           ; E[attrib~=val]
;;;              | (:= <ident> <attrib-value>)           ; E[attrib|=val]
;;;              | (*= <ident> <attrib-value>)           ; E[attrib*=val]
;;;              | (^= <ident> <attrib-value>)           ; E[attrib^=val]
;;;              | ($= <ident> <attrib-value>)           ; E[attrib$=val]
;;;              | (:not <negation-arg>)                 ; E:not(s)
;;;              | (: <ident>)                           ; E:pseudo-class
;;;              | (: (<fn> <ident> ...))                ; E:pseudo-class(arg)
;;;              | (:: <ident>)                          ; E::pseudo-element
;;;  <element-name> : <ident> | *
;;;  <attrib-value> : <ident> | <string>
;;;  <negation-arg> | <element-name> | * | <option>  ; except <negation-arg>
;;;
;;;  <declaration>  : (<ident> <expr> <expr2> ... <important>?)
;;;  <important> : !important
;;;  <expr>      : <term>
;;;              | (/ <term> <term> ...)
;;;              | (:or <term> <term> ...)
;;;              | #(<term> <term> ...)             ; juxtaposition
;;;  <term>      : <quantity> | (- <quantity>) | (+ <quantity>)
;;;              | <string> | <ident> | <url> | <hexcolor> | <function>
;;;  <quantity>  : <number>
;;;              | (<number> %)
;;;              | (<number> <ident>)
;;;  <url>       | (url <string>)
;;;  <hexcolor>  | (color <string>)  ; <string> must be hexdigits
;;;  <function>  | (<fn> <arg> ...)
;;;  <arg>       | <term> | #(<term> ...) | (/ <term> <term> ...)
;;;
;;;  <at-rule>    : <at-media-rule> | <at-import-rule>
;;;                     ; NB: Other at-rules are not supported yet
;;;  <at-media-rule>  : (@media (<symbol> ...) <style-rule> ...)
;;;  <at-import-rule> : (@import <string> (<symbol> ...))
;;;
;;; NB: style-decls shouldn't appear in a complete stylesheet, but can
;;;     appear when the 'style' attribute of the document is parsed.
;;;     It is also useful to render the value for the style attribute.
;;;
;;; NB: Negaton op is :not instead of not, since (not <negation-arg>)
;;;     would be ambiguous from the simple node named "not" with one option.

;;
;; Some utilities on S-expr CSS
;;

;; API
;; Returns canonicalized simple selector (element . options) or #f
(define (simple-selector? sel)
  (define (option? x)
    (match x
      [(a b . _) (memq a '(id class has = ~= := *= ^= $= :not : ::))]
      [_ #f]))
  (match sel
    [(? symbol?) (list sel)]
    [((? symbol?) (? option?) ...) sel]
    [_ #f]))

;;
;; S-expr CSS -> CSS
;;

;; API
(define (construct-css sexpr :optional (oport (current-output-port)))
  (define (render top)
    (match top
      [('style-rule pattern . decls)
       (write-tree (render-style-rule pattern decls) oport)
       (newline oport)]
      [('style-decls . decls)
       (write-tree (intersperse ";" (map render-decl decls)))]
      [_ (error "invalid or unsupported sexpr css:" top)]))
  (define (render-style-rule pattern decls)
    `(,(render-pattern pattern) "{"
      ,(intersperse ";" (map render-decl decls))
      "}\n"))
  (define (render-pattern pattern)
    (match pattern
      [(':or selector ...) (intersperse "," (map render-selector selector))]
      [_ (render-selector pattern)]))
  (define (render-selector selector)
    (if-let1 s (simple-selector? selector)
      (render-simple-selector s)
      (let loop ([sels selector])
        (cond [(null? sels) '()]
              [(simple-selector? (car sels))
               => (^s `(,(render-simple-selector s)" " ,@(loop (cdr sels))))]
              [(memq (car sels) '(> + ~)) `(,(car sels) ,@(loop (cdr sels)))]
              [else (error "invalid selector in sexpr css:" selector)]))))
  (define (render-simple-selector s)
    `(,(car s) ,@(map render-options (cdr s))))
  (define (render-options op)
    (match op
      [('id name) `("#" ,name)]
      [('class name) `("." ,name)]
      [('has name) `("[" ,name "]")]
      [((and (or '= '~= '^= '$= '*=) op) ident value)
       `("[" ,ident ,op ,(render-attrval value) "]")]
      [(':= ident value) `("[" ,ident "|=" ,(render-attrval value) "]")]
      [(':not not-arg) `(":(" ,(if (symbol? not-arg)
                                 not-arg
                                 (render-options not-arg)) ")")]
      [(': (fn arg ...)) `(":" ,(render-fn fn arg))]
      [(': ident) `(":" ,ident)]
      [(':: ident) `("::" ,ident)]
      [_ (error "invalid selector option:" op)]))
  (define (render-attrval val) (if (string? val) (escape-string val) val))
  (define (escape-string val)
    (if (string-index val #\')
      `(#\",(regexp-replace-all* val #/\"/ "\\22") #\")
      `(#\',val #\')))
  (define (render-fn fn args)
    `(,fn "(" ,@(intersperse "," (map render-arg args)) ")"))
  (define (render-arg arg)
    (match arg
      [#(v ...)  (intersperse " " (map render-attrval v))]
      [(/ v ...) (intersperse "/" (map render-attrval v))]
      [v (render-attrval v)]))

  (define (render-decl decl)
    (match decl
      [(ident expr . exprs)
       (if (and (pair? exprs)
                (eq? (last exprs) '!important))
         `(,ident ":" ,@($ intersperse " " $ map render-expr
                           $ cons expr (drop-right exprs 1))
                  " !important")
         `(,ident ":" ,@($ intersperse " " $ map render-expr
                           $ cons expr exprs)))]
      [_ (error "invalid declaration:" decl)]))
  (define (render-expr expr)
    (match expr
      [('/ term ...)  (intersperse "/" (map render-term term))]
      [(':or term ...) (intersperse "," (map render-expr term))]
      [#(term ...) (intersperse " " (map render-expr term))]
      [term (render-term term)]))
  (define (render-term term)
    (match term
      [('- quantity) `(- ,(render-quantity quantity))]
      [('+ quantity) `(+ ,(render-quantity quantity))]
      [('color digs)  `("#" ,digs)]
      [#(expr ...) (intersperse " " (map render-expr expr))]
      [((? number?) x) (render-quantity term)]
      [((? symbol? fn) expr ...)
       `(,fn "(" ,@(intersperse "," (map render-expr expr)) ")")] ;also url
      [(? string?) (escape-string term)]
      [(? symbol?) term]
      [(? number?) term]
      [_ (error "invalid term:" term)]))
  (define (render-quantity quant)
    (match quant
      [(num '%) `(,num "%")]
      [(num unit) `(,num ,unit)]
      [num num]))

  (for-each render sexpr))

;; experimental
(define ($/ parser . parsers)
  (if (null? parsers)
    parser
    ($or ($try parser) (apply $/ parsers))))

;;
;; CSS -> S-expr CSS
;;

;; Core syntax
;; http://www.w3.org/TR/css3-syntax/

;; CSS grammar definition is layered in order to allow different support
;; levels of the client.  It's conceptually parallel to Lisp syntax---
;; the S-expression defines the core syntax, and more complex syntax is
;; defined on top of S-expr.

;; The API provides access for the lower layer, with the option to hook
;; a translator for certain syntax units.

;; NB: @charset rule is not handled here; we need to switch port encodings

;; 4.3 Tokenizer
;; css-token-generator :: [Char] -> ( () -> Token )
;; Token = (IDENT . name) | (FUNCTION . name) | (AT-KEYWORD . name)
;;       | (HASH name flag) | (STRING . value) | (BAD-STRING)
;;       | (URL . value) | (BAD-URL)
;;       | (DELIM . char) | (NUMBER . value) | (PERCENTAGE . value)
;;       | (DIMENSION value unit)
;;       | (UNICODE-RANGE start end)
;;       | (INCLUDE-MATCH) | (DASH-MATCH) | (PREFIX-MATCH) | (SUFFIX-MATCH)
;;       | (SUBSTRING-MATCH) | (COLUMN) | (WHITESPACE) | (CDO) | (CDC)
;;       | (COLON) | (SEMICOLON) | (COMMA) | (OPEN-BRACKET) | (CLOSE-BRACKET)
;;       | (OPEN-PAREN) | (CLOSE-PAREN) | (OPEN-BRACE) | (CLOSE-BRACE)

(define %w ($skip-many ($one-of #[ \t\r\n\f])))
(define %s ($skip-many1 ($one-of #[ \t\r\n\f])))
(define %nl ($/ ($. "\n") ($. "\r\n") ($. "\n") ($. "\f")))
;; comment => vlid
(define %comment
  ($seq ($. "/*")
        ($skip-many ($none-of #[*]))
        ($skip-many1 ($. #\*))
        ($skip-many ($seq ($none-of #[/*])
                          ($skip-many ($none-of #[*]))
                          ($skip-many1 ($. #\*))))
        ($. #\/)))

;; nonascii [^\0-\237] => char
(define %nonascii ($none-of #[\x00-\x9f]))
;; unicode \\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?  => char
(define %unicode ($let ([ ($. #\\) ]
                        [code ($many ($one-of #[0-9a-fA-F]) 1 6)]
                        [ ($optional ($/ ($. "\r\n") ($one-of #[ \n\r\t\f]))) ])
                   ($return ($ integer->char
                               $ (cut string->number <> 16)
                               $ list->string code))))
;; escape {unicode}|\\[^\n\r\f0-9a-f] => char
(define %escape ($/ %unicode ($seq ($. #\\) ($none-of #[\n\r\f0-9a-f]))))
;; nmstart [_a-z]|{nonascii}|{escape} => char
(define %nmstart ($/ ($one-of #[_a-zA-Z]) %nonascii %escape))
  
;; nonascii [^\0-\237] => char
(define %nonascii ($none-of #[\x00-\x9f]))
;; unicode \\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?  => char
(define %unicode ($let ([ ($. #\\) ]
                        [code ($many ($one-of #[0-9a-fA-F]) 1 6)]
                        [ ($optional ($/ ($. "\r\n") ($one-of #[ \n\r\t\f]))) ])
                   ($return ($ integer->char
                               $ (cut string->number <> 16)
                               $ list->string code))))
;; escape {unicode}|\\[^\n\r\f0-9a-f] => char
(define %escape ($/ %unicode
                    ($seq ($. #\\) ($none-of #[\n\r\f0-9a-f]))))

;; nmstart [_a-z]|{nonascii}|{escape} => char
(define %nmstart ($/ ($one-of #[_a-zA-Z]) %nonascii %escape))
;; nmchar [_a-z0-9-]|{nonascii}|{escape} => char
(define %nmchar ($/ ($one-of #[_a-zA-Z0-9-]) %nonascii %escape))

;; name {nmchar}+ => string
(define %name ($->string ($many1 %nmchar)))

;; ident [-]?{nmstart}{nmchar}* => string
(define %ident ($->symbol ($optional ($. #\-)) %nmstart ($many %nmchar)))

(define %hexdigit ($one-of #[0-9a-fA-F]))

;; num [0-9]+|[0-9]*\.[0-9]+
(define %num
  (let* ([digs ($many1 ($one-of #[0-9]))]
         [frac ($->rope ($. #\.) digs)])
    ($lift ($ string->number $ rope->string $)
           ($/ ($->rope digs ($optional frac))
               ($->rope frac)))))

;; string {string1}|{string2}
;; string1 \"([^\n\r\f\\"]|\\{nl}|{escape})*\"
;; string2 \'([^\n\r\f\\']|\\{nl}|{escape})*\'
(define %string
  ($->rope
   ($/ ($between ($. #\")
                 ($many ($/ ($none-of #[\n\r\f\"\\])
                            ($try ($seq ($. #\\) %nl))
                            %escape))
                 ($or ($. #\") ($eos)))
       ($between ($. #\')
                 ($many ($/ ($none-of #[\n\r\f\'\\])
                            ($try ($seq ($. #\\) %nl))
                            %escape))
                 ($or ($. #\') ($eos))))))

(define %bad-string
  ($/ ($seq ($. #\")
            ($skip-many ($/ ($none-of #[\n\r\f\"])
                            ($try ($seq ($. #\\) %nl))
                            %escape))
            ($. #\newline))
      ($seq ($. #\')
            ($skip-many ($/ ($none-of #[\n\r\f\'])
                            ($try ($seq ($. #\\) %nl))
                            %escape))
            ($. #\newline))))

(define %url
  ($lift (^[_ _ r _ _] (rope->string r))
         ($. "url(") %w
         ($->rope ($/ %string
                      ($many ($/ ($one-of #[!#$%&*-~])
                                 %nonascii
                                 %escape))))
         %w ($. #\))))

(define %unicode-range
  (letrec [(code (^[cs] (string->number (list->string cs) 16)))]
    ($seq ($one-of #[Uu]) ($. #\+)
          ($/ ($lift (^[cs1 _ cs2] `(UNICODE-RANGE ,(code cs1) ,(code cs2)))
                     ($many %hexdigit 1 6) ($. #\-) ($many %hexdigit 1 6))
              ($let* ([ds ($many %hexdigit 1 5)]
                      [ws ($many ($. #\?) 1 (- 6 (length ds)))])
                ($return
                 `(UNICODE-RANGE ,(code (append ds (map (^_ #\0) ws)))
                                 ,(max (code (append ds (map (^_ #\f) ws)))
                                       #x10ffff))))
              ($lift (^[cs] `(UNICODE-RANGE ,(code cs) ,(code cs)))
                     ($many %hexdigit 1 6))))))

;; Tokenizer
;; comment returns :comment, which is ignored by the token generator
;; note that this parser never fails except at the end of input.
(define %tokenize
  ($/ ($seq %comment ($return :comment))
      ($seq %s ($return '(WHITESPACE)))
      %unicode-range
      ($seq ($. "~=")   ($return '(INCLUDE-MATCH)))
      ($seq ($. "|=")   ($return '(DASH-MATCH)))
      ($seq ($. "^=")   ($return '(PREFIX-MATCH)))
      ($seq ($. "$=")   ($return '(SUFFIX-MATCH)))
      ($seq ($. "*=")   ($return '(SUBSTRING-MATCH)))
      ($seq ($. "||")   ($return '(COLUMN)))
      ($seq ($. "<!--") ($return '(CDO)))
      ($seq ($. "-->")  ($return '(CDC)))
      ($seq ($. #\:)    ($return '(COLON)))
      ($seq ($. #\;)    ($return '(SEMICOLON)))
      ($seq ($. #\,)    ($return '(COMMA)))
      ($seq ($. #\[)    ($return '(OPEN-BRACKET)))
      ($seq ($. #\])    ($return '(CLOSE-BRACKET)))
      ($seq ($. #\()    ($return '(OPEN-PAREN)))
      ($seq ($. #\))    ($return '(CLOSE-PAREN)))
      ($seq ($. #\{)    ($return '(OPEN-BRACE)))
      ($seq ($. #\})    ($return '(CLOSE-BRACE)))
      ($lift (^[val] `(URL . ,val)) %url)
      ($lift (^[ident _] `(FUNCTION . ,ident)) %ident ($. #\())
      ($lift (^[ident] `(IDENT . ,ident)) %ident)
      ($lift (^[_ ident] `(AT-KEYWORD . ,ident)) ($. #\@) %ident)
      ($lift (^[_ val] `(HASH ,val #t)) ($. #\#) %ident)
      ($lift (^[_ val] `(HASH ,val #f)) ($. #\#) %name)
      ($lift (^[val] `(STRING . ,val)) %string)
      ($lift (^[_] '(BAD-STRING)) %bad-string)
      ($lift (^[val _] `(PERCENTAGE . ,val)) %num ($. #\%))
      ($lift (^[val unit] `(DIMENSION ,val ,unit)) %num %ident)
      ($lift (^[val] `(NUMBER . ,val)) %num)
      ($let ([c ($any)]) ($return `(DELIM . ,c)))))

(define (css-token-generator chars)
  (define cs chars)
  (rec (gen)
    (if (null? cs)
      (eof-object)
      (receive (val next) (peg-run-parser %tokenize cs)
        (set! cs next)
        (if (eq? val :comment) (gen) val)))))

(define (css-tokenize chars)
  (generator->lseq (css-token-generator chars)))

;;
;; 5. Core parser
;;

;; Accept token of specific type
(define ($tok type)
  (^s
   (cond [(null? s) (return-failure/expect type s)]
         [(eq? (caar s) type) (return-result (cdar s) (cdr s))]
         [else (return-failure/expect type s)])))

(define ($delim char)
  (^s
   (cond [(null? s) (return-failure/expect char s)]
         [(and (eqv? (caar s) 'DELIM) (eqv? (cdar s) char))
          (return-result (cdar s) (cdr s))]
         [else (return-failure/expect char s)])))

(define (%preserved-token s)
  (if (null? s)
    (return-failure/unexpect "EOF" s)
    (return-result (car s) (cdr s))))

(define %WS* ($skip-many ($tok 'WHITESPACE)))

(define %component-value
  ($lazy
   ($between %WS*
             ($/ %brace-block %paren-block %bracket-block %function-call
                 %preserved-token)
             %WS*)))

(define %component-value+  ; whitespace preserving
  ($lazy ($/ ($seq ($tok 'WHITESPACE) ($return '(WHITESPACE)))
             %brace-block %paren-block %bracket-block %function-call
             %preserved-token)))

(define (block-parser opener closer tag :optional (preserve-ws? #f))
  ($lift (^[_ _ v _] `(,tag ,@v))
         ($tok opener) %WS*
         ($many ($seq ($not ($tok closer))
                      (if preserve-ws? %component-value+ %component-value)))
         ($tok closer)))

(define %brace-block (block-parser 'OPEN-BRACE 'CLOSE-BRACE 'brace-block))
(define %paren-block (block-parser 'OPEN-PAREN 'CLOSE-PAREN 'paren-block))
(define %bracket-block (block-parser 'OPEN-BRACKET 'CLOSE-BRACKET 'bracket-block))
;; like %brace-block but we preserve whitespaces at the toplevel inside
;; the block.  necessary to handle @media directive.
(define %brace-block+ (block-parser 'OPEN-BRACE 'CLOSE-BRACE 'brace-block #t))

(define %function-call
  ($lift (^[fn args _] `(funcall ,fn ,@args))
         ($tok 'FUNCTION)
         ($many ($seq ($not ($tok 'CLOSE-PAREN)) %component-value))
         ($tok 'CLOSE-PAREN)))

(define %at-rule
  ($lift (^[keyword _ args block] `(,keyword ,args ,block))
         ($tok 'AT-KEYWORD) %WS*
         ($many ($seq ($not ($tok 'OPEN-BRACE))
                      ($not ($tok 'SEMICOLON))
                      %component-value))
         ($/ %brace-block+ ($tok 'SEMICOLON))))

(define %qualified-rule
  ;; NB: We preserve whitespaces between qualifiers, for it is
  ;; required to parse selectors; see below.
  ($lift (^[_ words block _] (list words block))
         %WS*
         ($many ($seq ($not ($tok 'OPEN-BRACE))
                      ($/ %paren-block %bracket-block %function-call
                          %preserved-token ($tok 'WHITESPACE))))
         %brace-block %WS*))

(define %important
  ($let ([ ($delim #\!) ]
         [ %WS* ]
         [v ($tok 'IDENT)])
    (if (string-ci=? (symbol->string v) "important")
      ($return '!important)
      ($fail "!important expected"))))
        
(define %declaration
  ($lift (^[name _ _ block important _] (list name block important))
         ($tok 'IDENT) %WS* ($tok 'COLON)
         ($many ($seq ($not ($tok 'SEMICOLON))
                      ($not ($delim #\!))
                      %component-value))
         ($optional %important)
         %WS*))

(define %declaration-list
  ($lazy
   ($seq %WS* ($/ ($sep-by ($optional %declaration) ($tok 'SEMICOLON))
                  ($lift list %at-rule %declaration-list)))))

(define %inter-rule-spaces
  ($skip-many ($/ ($tok 'CDO) ($tok 'CDC) ($tok 'WHITESPACE))))

(define %rule-list
  ($many ($/ ($tok 'WHITESPACE) %qualified-rule %at-rule)))
               
;;
;; Higher-level parser
;;
;; The core syntax defined in section 5 of css3 spec is very tolerable---
;; it accepts variety of syntaxes that doesn't make sense at all.  This
;; layer uses higher-level semantics to get a meaningful representation.

;; Selector syntax
;; http://www.w3.org/TR/css3-selectors/

(define %attr-selector-internal
  ($lift (^[_ name _ opval _]
           (if opval `(,(car opval) ,name ,(cdr opval)) `(has ,name)))
         %WS* ($tok 'IDENT) %WS*
         ($optional
          ($lift (^[op _ val] (cons op val))
                 ($/ ($seq ($delim #\=) ($return '=))
                     ($seq ($tok 'INCLUDE-MATCH) ($return '~=))
                     ($seq ($tok 'DASH-MATCH)    ($return ':=))
                     ($seq ($tok 'PREFIX-MATCH)  ($return '^=))
                     ($seq ($tok 'SUFFIX-MATCH)  ($return '$=))
                     ($seq ($tok 'SUBSTRING-MATCH) ($return '*=)))
                 %WS*
                 ($/ ($tok 'IDENT) ($tok 'STRING))))
         %WS*))

(define (%attr-selector s)
  (match s
    [(('bracket-block . content) . rest)
     (receive (r v s) (%attr-selector-internal content)
       (if (and (parse-success? r)
                (null? s))
         (return-result v rest)
         (return-failure/message "attr-selector" s)))]
    [_ (return-failure/message "attr-selector" s)]))

(define %class-selector
  ($lift (^[_ class] `(class ,class)) ($delim #\.) ($tok 'IDENT)))

(define %id-selector
  ($let ([hashval ($tok 'HASH)])
    (if (cadr hashval) ; hashval is ident
      ($return `(id ,(car hashval)))
      ($fail "identifier required for id selector"))))

(define %pseudo-fn-arg
  ;; NB: To support an+b syntax
  ($/ ($tok 'DIMENSION)
      ($tok 'NUMBER)
      ($tok 'STRING)
      ($tok 'IDENT)))

(define %negation-selector-arg
  ($lazy
   ($/ %type-selector %attr-selector %class-selector %id-selector
       %pseudo-selector)))

;; NB: negation-selector is also treated in pseudo-fn
(define (%pseudo-fn s)
  (match s
    [(('funcall name . arg-tokens) . rest)
     (receive (r v s) (if (eq? name 'not)
                        (%negation-selector-arg arg-tokens)
                        (%pseudo-fn-arg arg-tokens))
       (if (and (parse-success? r)
                (null? s))
         (return-result (list name v) rest)
         (return-failure/message "pseudo-fn" s)))]
    [_ (return-failure/message "pseudo-fn" s)]))

(define %pseudo-selector
  ($lift (^[_ elem? sel]
           (match sel
             [('not not-arg) `(:not ,not-arg)]
             [(? symbol?) `(,(if elem? ':: ':) ,sel)]
             [_ `(,(if elem? ':: ':) ,@sel)]))
         ($tok 'COLON) ($optional ($tok 'COLON))
         ($/ ($tok 'IDENT) %pseudo-fn)))

(define %selector-option
  ($/ %attr-selector
      %class-selector
      %id-selector
      %pseudo-selector))

(define %type-selector
  ;; TODO: namespace support & case conversion
  ($/ ($tok 'IDENT) ($seq ($delim #\*) ($return '*))))

(define %simple-selector
  ($/ ($lift (^[type opts] (if (null? opts) type (cons type opts)))
             %type-selector ($many %selector-option))
      ($lift (^[ops] `(* ,@ops)) ($many1 %selector-option))))

(define %selector-seq
  ($lazy
   ($lift (^[sel rest] (if rest (cons sel rest) (list sel)))
          %simple-selector
          ($optional
           ($try
            ($lift (^[op rest] (if op (cons op rest) rest))
                   ($/ ($between %WS*
                                 ($/ ($seq ($delim #\>) ($return '>))
                                     ($seq ($delim #\+) ($return '+))
                                     ($seq ($delim #\~) ($return '~)))
                                 %WS*)
                       ($followed-by ($seq ($tok 'WHITESPACE) ($return #f))
                                     %WS*))
                   %selector-seq))))))

(define %selector-group  ;; section 5
  ($lift (^[sels] (cond [(null? sels) #f]
                        [(null? (cdr sels)) (car sels)]
                        [else (cons ':or sels)]))
         ($sep-by ($lift (^[_ seq _] (if (null? (cdr seq)) (car seq) seq))
                         %WS* %selector-seq %WS*)
                  ($tok 'COMMA))))

;; TOKENS is the first item of the value of %qualified-rule.  Interpret
;; it as a selector and returns <pattern> structure of S-expr CSS.
;; If it can't be parsed, returns #f.
(define (parse-selectors tokens)
  (receive (val next) (peg-run-parser %selector-group tokens)
    (and (null? next) val)))

;; default declaration value parser
;; This part roughly follows CSS2.1 spec appendix G.
;; The difficulty is in expression;
;; CSS spec dosn't define general precedence among juxtaposition, comma and
;; slash operators, leaving its interpretation to each property.  However,
;; the current defined property seems to use this rule:
;; - A rule of thumb is that '/' > juxtaposition > ','
;; - For some shorthand properties, '/' > ',' > juxtaposition
;; TODO: We need another rule for the argument of 'calc'.

(define (%function-expr s)
  (match s
    [(('funcall name . arg-tokens) . rest)
     (receive (r v s) (%comma-separated-exprs arg-tokens)
       (if (and (parse-success? r)
                (null? s))
         (return-result (cons name v) rest)
         (return-failure/expect "list of function arguments" s)))]
    [_ (return-failure/expect "list of function arguments" s)]))

(define %term
  ($/ ($lift (^[u qual] (if u `(,(string->symbol (string u)) ,qual) qual))
             ($optional ($/ ($delim #\+) ($delim #\-)))
             ($/ ($tok 'NUMBER)
                 ($lift (^[val] `(,val %)) ($tok 'PERCENTAGE))
                 ($tok 'DIMENSION)))
      ($tok 'STRING)
      ($tok 'IDENT)
      ($lift (^[v] `(url ,v)) ($tok 'URL))
      ($lift (^[v] `(color ,(x->string (car v)))) ($tok 'HASH))
      %function-expr))

(define %term-/
  ($lift (^[a b] (if b `(/ ,a ,b) a))
         %term ($optional ($seq ($delim #\/) %term))))

(define %comma-separated-exprs
  ($sep-by ($lift (^[v vs] (if (null? vs) v (list->vector (cons v vs))))
                  %term-/ ($many %term-/))
           ($tok 'COMMA)))

(define %juxtaposed-exprs
  ($lift (^[vs] (match vs [(x) x] [xs (list->vector xs)]))
         ($many ($/ ($lift (^[a _ b] `(/ ,a ,b))
                           %term ($delim #\/) %term)
                    ($lift (^[v vs] (if (null? vs) v (cons* ':or v vs)))
                           %term ($many ($seq ($tok 'COMMA) %term)))))))

(define *juxta-properties*
  '(margin padding
    border border-top border-right border-bottom border-left
    font list-style pause cue))

(define (%decl-value property)
  (if (memq property *juxta-properties*)
    %juxtaposed-exprs
    ($let ([exprs %comma-separated-exprs])
      ($return (match exprs [(x) x] [(xs ...) `(:or ,@xs)])))))

;; val :: (property-name . tokens)
(define (parse-declaration val)
  (match val
    [(name tokens important?)
     (receive (r v s) ((%decl-value (car val)) tokens)
       (if (and (parse-success? r)
                (null? s))
         `(,name ,v ,@(cond-list [important? '!important]))
         (begin
           (css-parser-warn "Ignored unparsable value for property ~s: ~s\n"
                            (car val) (cdr val))
           #f)))]
    [_ #f]))

(define (parse-declarations block)
  (match block
    [('brace-block . component-values)
     (receive (vals next) (peg-run-parser %declaration-list component-values)
       (and (null? next)
            (filter-map parse-declaration vals)))]
    [_ #f]))

;; Interpret %qualified-rule as a style-rule and returns <ruleset> structure of
;; S-expr CSS.  (section 8.1 of CSS3 sytnax)
;; If it can't be parsed, returns #f.
(define (parse-style-rule selector-tokens block-token)
  (let ([selector (parse-selectors selector-tokens)]
        [decls (parse-declarations block-token)])
    (if (and selector decls)
      `(style-rule ,selector ,@decls)
      (begin
        (unless selector
          (css-parser-warn "Ignored unparsable selector: ~s\n"
                           selector-tokens))
        (unless decls
          (css-parser-warn "Ignored unparsable declarations block: ~s\n"
                           block-token))
        #f))))

;; Parse %at-rule.  This must be customizable.  For now, we only handle
;; @media.
(define (parse-at-rule at-keyword args block-token
                       qualified-rule-handler)
  (or (and-let* ([ (eq? at-keyword 'media) ]
                 [syms (map (^a (match a [('IDENT . x) x] [_ #f])) args)]
                 [tokens (match block-token [('brace-block . xs) xs] [_ #f])])
        (let loop ([tokens tokens]
                   [decls '()])
          (if (null? tokens)
            `(@media ,syms
                     ,@(map (^p (qualified-rule-handler (car p) (cdr p)))
                            (reverse decls)))
            (receive (sels rest)
                (break (^t (match t [('brace-block . _) #t] [_ #f])) tokens)
              (if (null? rest)
                (begin
                  (unless (every (^s (match s [('WHITESPACE) #t][_ #f])) sels)
                    (css-parser-warn "A selector in media block missing rules: ~s\n"
                                     sels))
                  (loop '() decls))
                (loop (cdr rest) (acons sels (car rest) decls)))))))
      (and (eq? at-keyword 'import)
           (match args
             [(url . media)
              (and-let* ([url-string (match url
                                       [('STRING . s) s]
                                       [('URL . s) s]
                                       [_ #f])]
                         [media-kinds (filter-map (^x (match x
                                                        [('IDENT . s) s]
                                                        [_ #f]))
                                                  media)])
                `(@import ,url-string ,media-kinds))]
             [_ #f]))
      (begin
        (css-parser-warn "Ignored unsupported at-rule: ~s\n"
                         (list at-keyword args block-token))
        #f)))

;; Integrated stylesheet parser
(define (make-stylesheet-parser qualified-rule-handler at-rule-handler)
  ($seq %inter-rule-spaces
        ($many ($followed-by
                ($/ ($lift (^[a] (at-rule-handler (car a) (cadr a) (caddr a)
                                                  qualified-rule-handler))
                           %at-rule)
                    ($lift (^[q] (qualified-rule-handler (car q) (cadr q)))
                           %qualified-rule))
                %inter-rule-spaces))))

(define (parse-stylesheet tokens
                          :key (qualified-rule-handler parse-style-rule)
                               (at-rule-handler parse-at-rule))
  (receive (r rest)
      (peg-run-parser (make-stylesheet-parser qualified-rule-handler
                                              at-rule-handler)
                      tokens)
    (filter identity r)))

;; API
;; TODO: Handle @charset
(define (parse-css-file file :key (encoding #f))
  (call-with-input-file file parse-css
                        :encoding (or encoding (gauche-character-encoding))))

;; API
(define (parse-css :optional (port (current-input-port)))
  ($ parse-stylesheet
     $ css-tokenize $ generator->lseq $ port->char-generator port))

;; API
;; A utility function to parse only selector part.
;; We can't simply use parse-selectors, for it assumes the tokens is
;; processed as %qualified-rule.
;; Returns #f if input is unparsable.  It is ok that input has extra
;; blocks; they're ignored.
(define (parse-css-selector-string s)
  (receive (r v s) ($ %selector-only $ css-tokenize $ x->lseq s)
    (and (parse-success? r)
         (parse-selectors v))))

(define %selector-only
  ($seq %WS*
        ($many ($seq ($not ($tok 'OPEN-BRACE))
                     ($/ %paren-block %bracket-block %function-call
                         %preserved-token ($tok 'WHITESPACE))))))

;; TODO: make this customizable
(define css-parser-warn warn)
