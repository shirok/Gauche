;;-------------------------------------------------------------------
(use gauche.test)
(test-start "line-edit")
(test-section "line-edit")
(use text.line-edit)
(test-module 'text.line-edit)

;; Test internal functions
(let ()
  (define (t s pos expect)
    (test* #"buffer-find-matching-paren: ~s ~pos" expect
           ((with-module text.line-edit buffer-find-matching-paren)
            ((with-module text.gap-buffer string->gap-buffer) s)
            0 pos)))

  (t "(abc)" 4 0)
  (t "aa(abc)zz" 6 2)
  (t "(a(abc)z)" 6 2)
  (t "(a)(abc)z" 7 3)
  (t "(a\\(b)z" 5 0)
  (t "(a\\(\\))z" 6 0)
  (t "(a\\)\\()z" 6 0)
  (t "(a(abc)(def)z)" 11 7)
  (t "(a(abc)(def)z)" 13 0)
  (t "aaaabc)zz" 6 #f)
  (t "(a{b}c)" 4 2)
  (t "(a{b}c)" 6 0)
  (t "(a{b(}c)" 5 #f)
  (t "(a{b(}c)" 7 4)
  (t "(a{b)}c)" 4 #f)
  (t "(a{b)}c)" 5 2)
  (t "(a\"(\"b)" 6 0)
  (t "(a\")\"b)" 6 0)
  (t "(a\"\\\"(\"b)" 8 0)
  (t "(a#/bc([d\\//)]" 12 0)
  (t "(a#/bc([d\\//)]" 13 #f)
  (t "(a#(b)c)" 5 3)
  (t "(a#(b)c)" 7 0)
  (t "(a#[b[:space:]]c)" 14 3)
  (t "(a#[b[:space:]]c)" 13 5)
  (t "(a#[b[:space:]]c)" 16 0)
  (t "(a#{b}c)" 5 3)
  (t "(a#{b)c}" 5 #f)
  (t "(a#{b)c}" 7 3)
  (t "(a\\#(b)c)" 6 4)
  (t "(a#\\(b)c)" 6 0)
  )

;; Test buffer-forward-sexp and buffer-backward-sexp
(let ()
  (define %buffer-forward-sexp
    (with-module text.line-edit buffer-forward-sexp))
  (define %buffer-backward-sexp
    (with-module text.line-edit buffer-backward-sexp))
  (define (make-buf s pos)
    (let1 buf ((with-module text.gap-buffer string->gap-buffer) s)
      (gap-buffer-move! buf pos)
      buf))

  ;; (t <input-string> <cursor-pos> <expected-result>)
  (define (tf s pos expect)
    (test* (format "buffer-forward-sexp: ~s ~d" s pos) expect
           (%buffer-forward-sexp (make-buf s pos))))
  (define (tb s pos expect)
    (test* (format "buffer-backward-sexp: ~s ~d" s pos) expect
           (%buffer-backward-sexp (make-buf s pos))))

  ;;----------------------------------------------
  ;; forward-sexp tests
  ;;

  ;; Simple symbol
  (tf "abc" 0 3)
  (tf "abc def" 0 3)
  (tf "abc def" 4 7)

  ;; Cursor in the middle of a symbol
  (tf "abcdef" 3 6)

  ;; Numbers
  (tf "123" 0 3)
  (tf "123 456" 0 3)

  ;; Parenthesized list
  (tf "(abc)" 0 5)
  (tf "(abc def)" 0 9)
  (tf "(a (b c) d)" 0 11)
  (tf "[a b]" 0 5)
  (tf "{a b}" 0 5)

  ;; Nested parens
  (tf "((a))" 0 5)
  (tf "(a (b) c)" 0 9)

  ;; Inside a list, move over sub-expressions
  (tf "(a b c)" 1 2)    ; cursor after '(' -> over 'a'
  (tf "(a b c)" 2 4)    ; cursor after 'a' -> skip space, over 'b'
  (tf "(abc def)" 1 4)  ; over 'abc'
  (tf "(abc def)" 4 8)  ; skip space, over 'def'

  ;; String
  (tf "\"hello\"" 0 7)
  (tf "\"hello\" world" 0 7)
  (tf "\"he\\\"llo\"" 0 9)  ; string with escaped quote

  ;; |symbol|
  (tf "|foo bar|" 0 9)
  (tf "|foo\\|bar|" 0 10)

  ;; Character literal
  (tf "#\\a" 0 3)
  (tf "#\\space" 0 7)
  (tf "#\\a bc" 0 3)

  ;; Vector
  (tf "#(a b)" 0 6)

  ;; Regexp
  (tf "#/abc/" 0 6)

  ;; Gauche string interpolation
  (tf "#\"hello\"" 0 8)

  ;; Charset
  (tf "#[a-z]" 0 6)
  (tf "#[ab[:digit:][:punct:]]" 0 23)

  ;; Quote prefix
  (tf "'a" 0 2)
  (tf "'(a b)" 0 6)
  (tf "`(a b)" 0 6)
  (tf ",a" 0 2)
  (tf ",@a" 0 3)

  ;; Whitespace skipping
  (tf "  abc" 0 5)
  (tf "  (a b)" 0 7)

  ;; Comment skipping
  (tf ";comment\nabc" 0 12)

  ;; At end of buffer
  (tf "" 0 #f)
  (tf "abc" 3 #f)
  (tf "   " 0 #f)

  ;; Unmatched paren
  (tf "(abc" 0 #f)

  ;; Closing paren - no forward sexp
  (tf ")" 0 #f)

  ;; Boolean literals
  (tf "#t" 0 2)
  (tf "#f" 0 2)
  (tf "#t #f" 0 2)

  ;;----------------------------------------------
  ;; backward-sexp tests
  ;;

  ;; Simple symbol
  (tb "abc" 3 0)
  (tb "abc def" 7 4)
  (tb "abc def" 3 0)

  ;; Numbers
  (tb "123" 3 0)
  (tb "123 456" 7 4)

  ;; Parenthesized list
  (tb "(abc)" 5 0)
  (tb "(abc def)" 9 0)
  (tb "(a (b c) d)" 11 0)
  (tb "[a b]" 5 0)
  (tb "{a b}" 5 0)

  ;; Nested parens
  (tb "((a))" 5 0)

  ;; Inside a list, move backward over sub-expressions
  (tb "(a b c)" 6 5)    ; cursor before ')' -> over 'c'
  (tb "(a b c)" 4 3)    ; cursor after 'b' -> over 'b'
  (tb "(abc def)" 8 5)  ; over 'def'
  (tb "(abc def)" 4 1)  ; over 'abc'

  ;; String
  (tb "\"hello\"" 7 0)
  (tb "world \"hello\"" 13 6)
  (tb "\"he\\\"llo\"" 9 0)

  ;; |symbol|
  (tb "|foo bar|" 9 0)

  ;; Character literal
  (tb "#\\a" 3 0)
  (tb "#\\space" 7 0)
  (tb "bc #\\a" 6 3)

  ;; Vector
  (tb "#(a b)" 6 0)

  ;; Regexp
  (tb "#/abc/" 6 0)

  ;; Gauche string interpolation
  (tb "#\"hello\"" 8 0)

  ;; Charset
  (tb "#[a-z]" 6 0)
  (tb " #[ab[:digit:][:punct:]]" 24 1)

  ;; Quote prefix
  (tb "'a" 2 0)
  (tb "'(a b)" 6 0)
  (tb "`(a b)" 6 0)
  (tb ",a" 2 0)
  (tb ",@a" 3 0)

  ;; Whitespace skipping
  (tb "abc   " 6 0)
  (tb "(a b)   " 8 0)

  ;; At beginning of buffer
  (tb "" 0 #f)
  (tb "abc" 0 #f)
  (tb "   " 3 #f)

  ;; Unmatched paren
  (tb "abc)" 4 #f)

  ;; Opening paren - can't go backward past it
  (tb "(" 0 #f)

  ;; Boolean literals
  (tb "#t" 2 0)
  (tb "#f" 2 0)
  (tb "#t #f" 5 3)

  ;; Multiple sexps in sequence
  (tf "(a) (b) (c)" 0 3)
  (tf "(a) (b) (c)" 4 7)
  (tf "(a) (b) (c)" 8 11)
  (tb "(a) (b) (c)" 11 8)
  (tb "(a) (b) (c)" 7 4)
  (tb "(a) (b) (c)" 3 0)

  ;; Round-trip: forward then backward returns to the same position
  ;; (we test this conceptually by checking that forward from 0 gives N
  ;; and backward from N gives 0)
  (tf "(define (foo x) (+ x 1))" 0 24)
  (tb "(define (foo x) (+ x 1))" 24 0)
  )

(test-end)
