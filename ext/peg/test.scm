;;;
;;; test/peg.scm - Unit Tests for PEG module
;;;
;;;   Copyright (c) 2006 Rui Ueyama (rui314@gmail.com)
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

(use gauche.test)
(use srfi-1)

(test-start "parser.peg")
(debug-print-width 1024)
;(set! *test-report-error* #t)

(test-section "peg")
(use parser.peg)
(test-module 'parser.peg)

;;;============================================================
;;; helper macros
;;;
(define (%compare test-type accessors)
  (lambda (e o)
    (and (test-type o)
         (every equal? e (map (cut <> o) accessors)))))

(define-syntax test-succ
  (syntax-rules ()
    ((_ label expect parse input)
     (test* #`",label (success)" expect
            (parse-string parse input)))))

(define-syntax test-fail
  (syntax-rules ()
    ((_ label expect parse input)
     (test* #`",label (failure)" expect
            (guard (e ((<parse-error> e)
                       (list (ref e 'position) (ref e 'objects)))
                      (else e))
              (parse-string parse input)
              (error "test-fail failed"))))))

;;;============================================================
;;; Ropes
;;;
(receive (make-rope rope->string)
    (with-module parser.peg (values make-rope rope->string))
  (let* ((rope1 (make-rope "abc"))
         (rope2 (make-rope `("012" ,rope1 #\X)))
         (rope3 (make-rope 'foo)))
    (test* "rope->string" "abc"        (rope->string rope1))
    (test* "rope->string" "012abcX"    (rope->string rope2))
    (test* "rope->string" *test-error* (rope->string rope3))))


;;;============================================================
;;; Primitives
;;;
(test-section "primitives")

(test-succ "$return" "xyz" ($return "xyz") "abc")
(test-fail "$fail" '(0 "error") ($fail "error") "foo")
(test-fail "$expect" '(0 "character")
           ($expect ($fail "zzz") "character") "foo")

;;;============================================================
;;; Error handlers
;;;
; (test-section "error handling")

; (let1 merge-failure (with-module peg merge-failure)
;   (test* "merge-failure"
;          '#(fail expect ("a") 1)
;          (merge-failure '(#(fail expect ("a") 1)
;                           #(fail expect ("b") 0))))
;   (test* "merge-failure (same position)"
;          '#(fail expect ("a" "b") 1)
;          (merge-failure '(#(fail expect ("a") 1)
;                           #(fail expect ("b") 1)))))

;;;============================================================
;;; Parsers for string
;;;
(test-section "string")

(test-succ "$string" "abc" ($string "abc") "abcdef")
(test-fail "$string)" '(0 "ABC") ($string "ABC") "012")
(test-succ "$char" #\a ($char #\a) "abc")
(test-fail "$char" '(0 #\a) ($char #\a) "012")
(test-succ "$one-of" #\j ($one-of #[a-z]) "j")
(test-fail "$one-of" '(0 #[A-Z]) ($one-of #[A-Z]) "j")
(test-succ "$none-of" #\j ($none-of #[A-Z]) "j")
(test-fail "$none-of" '(0 #[^a-z]) ($none-of #[a-z]) "j")
(test-succ "$string-ci" "aBC" ($string-ci "abc") "aBCdef")
(test-fail "$string-ci" '(0 "abc") ($string-ci "abc") "012")

(define-syntax test-char
  (syntax-rules ()
    ((_ parse expin unexpin message)
     (begin
       (test-succ (symbol->string 'parse)
                  (string-ref expin 0)
                  parse expin)
       (test-fail (symbol->string 'parse)
                  '(0 message)
                  parse unexpin)))))

(test-char anychar  "a" "" "character")
(test-char upper    "A" "a" "upper case letter")
(test-char lower    "a" "A" "lower case letter")
(test-char letter   "a" "." "letter")
(test-char alphanum "0" "." "letter or digit")
(test-char digit    "0" "." "digit")
(test-char hexdigit "f" "." "hexadecimal digit")
(test-char newline  "\n" "." "newline")
(test-char tab      "\t" "." "tab")
(test-char space    "\t" "." "space")

(test-succ "spaces" " " spaces " ")
(test-succ "spaces" " \n\t" spaces " \n\tj")

(test-succ "$satisfy" #\0
           ($satisfy char-numeric? "numeric")
           "0")
(test-fail "$satisfy" '(0 "numeric")
           ($satisfy char-numeric? "numeric")
           "a")

(test-succ "eof" #t eof "")
(test-fail "eof" '(0 "end of input") eof "a")

;;;============================================================
;;; Combinators
;;;
(test-section "combinators")

(define (map-tree proc tree)
  (if (pair? tree)
    (cons (map-tree proc (car tree))
          (map-tree proc (cdr tree)))
    (proc tree)))

;; $or
(test-succ "$or" "foo"
           ($or ($string "foo") ($string "bar"))
           "foo...")

(test-succ "$or" "bar"
           ($or ($string "foo") ($string "bar"))
           "bar...")

(test-fail "$or" '(0 ((fail-expect . "foo") (fail-expect . "bar")))
           ($or ($string "foo") ($string "bar"))
           "012345")

;; $try
(test-succ "$try" "foo"
           ($try ($string "foo"))
           "foo...")
(test-succ "$try" "bar"
           ($try ($string "foo") ($string "bar"))
           "foobar...")
(test-fail "$try" '(0 "foo")
           ($try ($string "foo"))
           "bar...")
(test-fail "$try" '(0 "bar")
           ($try ($string "foo") ($string "bar"))
           "foobaz...")

;; $do
(test-succ "$do" "j"
           ($do (j ($string "j"))
                ($return j))
           "j")

(test-succ "$do" "j"
           (let1 parse ($string "j")
             ($do (j parse) ($return j)))
           "j")

(test-succ "$do" '("foo" "bar")
           ($do (foo ($string "foo"))
                (bar ($string "bar"))
                ($return (list foo bar)))
           "foobar...")

(test-succ "$do" '("foo" "baz")
           ($do (foo ($string "foo"))
                (    ($string "bar") )
                (baz ($string "baz"))
                ($return (list foo baz)))
           "foobarbaz$$$")

(test-succ "$do" "foo"
           (let1 dot ($string ".")
             ($do (foo ($string "foo"))
                  dot
                  ($return foo)))
           "foo.")

(test-fail "$do" '(3 "bar")
           ($do (foo ($string "foo"))
                (bar ($string "bar"))
                ($return (list foo bar)))
           "foo12")

(test-succ "$do" '("AB" "cd" "EF")
           ($do (x ($or ($string "AB") ($string "ab")))
                (y ($or ($string "CD") ($string "cd")))
                (z ($or ($string "EF") ($string "ef")))
                ($return (list x y z)))
           "ABcdEF")

(test-fail "$do and $or" '(3 "foo")
           ($do (v ($or ($string "foo") ($string "bar")))
                ($string (semantic-value-finalize! v)))
           "foobar")

;; $seq
(test-succ "$seq" "b"
           ($seq ($string "a") ($string "b"))
           "abcd")
(test-succ "$seq" "b"
           ($seq ($or ($string "A") ($string "a"))
                 ($string "b"))
           "abcd")
(test-fail "$seq" '(1 "b")
           ($seq ($string "a") ($string "b"))
           "a123")

;; $many
(test-succ "$many" '("a" "b" "a" "a" "b")
           ($many ($or ($string "a") ($string "b")))
           "abaabcd")
(test-succ "$many" '()
           ($many ($string "a")) "012")
(test-fail "$many" '(0 "a")
           ($many ($string "a") 1) "012")
(test-succ "$many" '("a" "a")
           ($many ($string "a") 1 2) "aaaaa")

;; $skip-many
(test-succ "$skip-many" '("a" "a")
           ($skip-many ($string "a") 1 2) "aaaaa")

;; $repeat
(test-succ "$repeat" '(#\a #\b #\a)
           ($repeat anychar 3) "abab")
(test-fail "$repeat" '(2 "character")
           ($repeat anychar 3) "ab")

;; $optional
(test-succ "$optional" #\a
           ($optional ($char #\a)) "abab")
(test-succ "$optional" #f
           ($optional ($char #\a)) "ABAB")

;; $sep-by
(test-succ "$sep-by" '(#\a #\b)
           ($sep-by ($one-of #[ab]) ($string ","))
           "a,b,c,d")

(test-succ "$sep-by" '(#\a #\b #\c)
           ($sep-by ($one-of #[abc]) ($string ","))
           "a,b,c,d")

(test-succ "$sep-by" '(#\a #\b)
           ($sep-by ($one-of #[abc]) ($string ",") 0 2)
           "a,b,c,d")

(test-fail "$sep-by" '(2 #[abc])
           ($sep-by ($one-of #[abc]) ($string ",") 2 3)
           "a,2,3")

;; $end-by
(test-succ "$end-by" '(#\a #\b)
           ($end-by ($one-of #[a-z]) ($string ","))
           "a,b,")

;; $sep-end-by
(test-succ "$sep-end-by" '(#\a #\b)
           ($sep-end-by ($one-of #[ab]) ($string ","))
           "a,b,c,d")

(test-succ "$sep-end-by" '(#\a #\b #\c #\d)
           ($sep-end-by ($one-of #[a-z]) ($string ","))
           "a,b,c,d")

(test-succ "$sep-end-by" '(#\a #\b)
           ($sep-end-by ($one-of #[abc]) ($string ",") 0 2)
           "a,b,c,d")

(test-fail "$sep-end-by" '(2 #[abc])
           ($sep-end-by ($one-of #[abc]) ($string ",") 2 3)
           "a,2,3")

;; $count
(test-succ "$count" '(#\a #\b)
           ($count ($one-of #[ab]) 2) "abab")

(test-fail "$count" '(1 #[ab])
           ($count ($one-of #[ab]) 2) "a012")

;; $between
(test-succ "$between" "foo"
           ($between ($string "(")
                     ($string "foo")
                     ($string ")"))
           "(foo)")

(test-fail "$between" '(4 ")")
           ($between ($string "(")
                     ($string "foo")
                     ($string ")"))
           "(foo]")

;; $not
(test-succ "$not" #f
           ($not ($one-of #[a-z]))
           "ABC")
(test-fail "$not" '(0 #\a)
           ($not ($one-of #[a-z]))
           "abc")

;; $many-till
(test-succ "$many-till" '(#\a #\b)
           ($many-till alphanum digit)
           "ab78")

;; $chain-left
(let ((integer
       ($do (v ($many digit 1))
            ($return (string->number (apply string v)))))
      (op
       ($or ($do (($char #\*)) ($return *))
            ($do (($char #\+)) ($return +)))))
  (test-succ "$chain-left" 9
             ($chain-left integer op)
             "1+2*3")
  (test-succ "$chain-left" 3
             ($chain-left integer op)
             "1+2*")
  (test-fail "$chain-left" '(0 "digit")
             ($chain-left integer op)
             "abc"))

;; $chain-right
(let ((integer
       ($do (v ($many digit 1))
            ($return (string->number (apply string v)))))
      (op
       ($or ($do (($char #\*)) ($return *))
            ($do (($char #\+)) ($return +)))))
  (test-succ "$chain-right" 7
             ($chain-right integer op)
             "1+2*3")
  (test-succ "$chain-right" 3
             ($chain-right integer op)
             "1+2*")
  (test-fail "$chain-right" '(0 "digit")
             ($chain-right integer op)
             "abc"))

;; $lazy
(test-succ "$lazy" #\a
           ($lazy ($char #\a)) "a")
(test-fail "$lazy" '(0 #\a)
           ($lazy ($char #\a)) "b")

;;;============================================================
;;; Backtrack control
;;;
(test-section "backtrack control")

(test-succ "$or and $try" "abc"
           ($or ($try ($do [foo ($string "abc")]
                           [bar ($string "foo")]
                           ($return (list foo bar))))
                ($do (v ($string "abc"))
                     ($return v)))
           "abcdefg")

(let1 parser ($or ($do [foo ($try ($do [v ($string "abc")]
                                       [($one-of #[+-])]
                                       ($return v)))]
                       [bar ($string "foo")]
                       ($return (list foo bar)))
                  ($do (v ($string "abc"))
                       ($return v)))
  (test-succ "$or and $try" "abc" parser
             "abcdefg")
  (test-fail "$or and $try" '(4 "foo") parser
             "abc+efg")
  )

;;;============================================================
;;; Token Parsers
;;;

;;;============================================================
;;; Permulate Parsers
;;;

;;;============================================================
;;; Examples
;;;
(test-section "examples")

;; Count the maximal nesting level
(letrec ((nesting
          ($lazy ($or ($do [n ($try ($do [($char #\()] nesting))]
                           [($char #\))]
                           [m nesting]
                           ($return (max (+ n 1) m)))
                      ($return 0)))))
  (define (%test depth str)
    (test* "nesting parenthesis" depth
           (parse-string nesting str)))
  (test-succ "nesting parenthesis" 3 nesting "((()))")
  (test-succ "nesting parenthesis" 3 nesting "((()))()")
  (test-succ "nesting parenthesis" 3 nesting "(()(()))")
  (test-fail "nesting parenthesis" '(1 #\) ) nesting "((("))

;; CSV parser
(let* ((spaces ($many ($one-of #[ \t])))
       (comma ($seq spaces ($char #\,) spaces))
       (dquote ($char #\"))
       (double-dquote ($do (($string "\"\"")) ($return #\")))
       (quoted-body ($many ($or double-dquote ($one-of #[^\"]))))
       (quoted ($between dquote quoted-body dquote))
       (unquoted ($many-till anychar ($or comma newline)))
       (field ($or quoted unquoted))
       (record ($sep-by ($->rope field) comma)))
  (test-succ "CSV" '("a" "b" "c")
             record "a,b,c")
  (test-succ "CSV" '("a" "b" "c")
             record "\"a\" , b  , c")
  (test-succ "CSV" '("a  \" \n" "b" "c")
             record "\"a  \"\" \n\" , b  , c"))

;; hand-tuned version
(let* ((spaces_ ($many_ ($one-of #[ \t])))
       (spaces ($many ($one-of #[ \t])))
       (comma  ($seq spaces_ ($char #\,) spaces_))
       (dquote ($char #\"))
       (double-dquote ($do (($string "\"\"")) ($return #\")))
       (quoted-body ($many ($or ($one-of #[^\"]) double-dquote)))
       (quoted ($between dquote quoted-body dquote))
       (unquoted ($alternate ($one-of #[^ \t\r\n,]) spaces))
       (field ($or quoted unquoted))
       (record ($sep-by ($->rope field) comma 1))
       (records ($sep-by record ($string "\r\n"))))
  (test-succ "CSV 2" '(("a" "b" "c") ("x" "y" "z"))
             records "a,b,c\r\nx,y,z\r\n")
  (test-succ "CSV 2" '(("a " "b" "c") ("zzz\nyyy " " w \" "))
             records "\"a \" , b  , c\r\n\"zzz\nyyy \", \" w \"\" \"\r\n")
  (test-succ "CSV 2" '(("a  \" \n" "b" "c"))
             records "\"a  \"\" \n\" , b  , c"))

;; Poor-man's XML parser
(letrec ((open-tag ($->rope ($between ($char #\<) alphanum ($char #\>))))
         (close-tag (lambda (tagname)
                      ($seq ($string "</") ($string tagname) ($char #\>))))
         (text ($->rope ($many ($none-of #[<]))))
         (body ($do (t text)
                    (r ($many ($do (e element)
                                   (t text)
                                   ($return (list e t)))))
                    ($return `(,t ,@(apply append r)))))
         (element ($do* [tagname open-tag]
                        [body body]
                        [(close-tag (semantic-value-finalize! tagname))]
                        ($return (cons tagname body)))))
  (test-succ "tag element" '("a" "")
             element "<a></a>")
  (test-succ "tag element" '("a" "foo" ("b" "bar") "baz")
             element "<a>foo<b>bar</b>baz</a>"))

;; Calculator
(letrec ((integer ($do (v ($many digit 1))
                       ($return (string->number (apply string v)))))
         (mulop ($or ($seq ($char #\*) ($return *))
                     ($seq ($char #\/) ($return /))))
         (addop ($or ($seq ($char #\+) ($return +))
                     ($seq ($char #\-) ($return -))))
         (factor ($lazy ($or ($between ($char #\() expr ($char #\)))
                             integer)))
         (term ($chain-left factor mulop))
         (expr ($chain-left term addop)))
  (test-succ "calculator" 11 expr "1+2*3+4")
  (test-succ "calculator" 36 expr "2/2+5*(3+4)")
  (test-succ "calculator" -1 expr "1-2"))

(test-end)
