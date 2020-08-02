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
(use srfi-13)
(use gauche.generator)
(use gauche.parameter)

(test-start "parser.peg")

(test-section "peg")
(use parser.peg)
(test-module 'parser.peg)

;;;============================================================
;;; helper macros
;;;
(define (%compare test-type accessors)
  (^[e o] (and (test-type o)
               (every equal? e (map (cut <> o) accessors)))))

(define-syntax test-succ
  (syntax-rules ()
    [(_ label expect parse input)
     (test* #"~label (success)" expect
            (peg-parse-string parse input))]))

(define-syntax test-fail
  (syntax-rules ()
    [(_ label expect parse input)
     (test* #"~label (failure)" expect
            (guard (e [(<parse-error> e)
                       (list (ref e 'position) (ref e 'objects))]
                      [else e])
              (peg-parse-string parse input)
              (error "test-fail failed")))]))

;;;============================================================
;;; Ropes
;;;
(receive (make-rope rope->string)
    (with-module parser.peg (values make-rope rope->string))
  (let* ([rope1 (make-rope "abc")]
         [rope2 (make-rope `("012" ,rope1 #\X))]
         [rope3 (make-rope 'foo)])
    (test* "rope->string" "abc"        (rope->string rope1))
    (test* "rope->string" "012abcX"    (rope->string rope2))
    (test* "rope->string" (test-error) (rope->string rope3))))


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
           ($try ($seq ($string "foo") ($string "bar")))
           "foobar...")
(test-fail "$try" '(0 "foo")
           ($try ($string "foo"))
           "bar...")
(test-fail "$try" '(0 "bar")
           ($try ($seq ($string "foo") ($string "bar")))
           "foobaz...")

;; $let, $let*
(test-succ "$let" "j"
           ($let ([j ($string "j")])
             ($return j))
           "j")

(test-succ "$let" "j"
           (let1 parse ($string "j")
             ($let ([j parse]) ($return j)))
           "j")

(test-succ "$let" '("foo" "bar")
           ($let ([foo ($string "foo")]
                  [bar ($string "bar")])
             ($return (list foo bar)))
           "foobar...")

(test-succ "$let" '("foo" "baz")
           ($let ([foo ($string "foo")]
                  [    ($string "bar")]
                  [baz ($string "baz")])
             ($return (list foo baz)))
           "foobarbaz$$$")

(test-succ "$let" "foo"
           (let1 dot ($string ".")
             ($let ([foo ($string "foo")]
                    dot)
               ($return foo)))
           "foo.")

(test-fail "$let" '(3 "bar")
           ($let ([foo ($string "foo")]
                  [bar ($string "bar")])
             ($return (list foo bar)))
           "foo12")

(test-succ "$let" '("AB" "cd" "EF")
           ($let ([x ($or ($string "AB") ($string "ab"))]
                  [y ($or ($string "CD") ($string "cd"))]
                  [z ($or ($string "EF") ($string "ef"))])
             ($return (list x y z)))
           "ABcdEF")

(test-fail "$let and $or" '(3 "foo")
           ($let ([v ($or ($string "foo") ($string "bar"))])
             ($string (rope-finalize v)))
           "foobar")

(test-succ "$let" '("foo" "baz")
           ($let ([foo ($string "foo")]
                  [    ($string "bar")]
                  [baz ($string "baz")])
                 ($return (list foo baz)))
           "foobarbaz$$$")

(test-succ "$let" '(#\0 "zyx" #\0)
           ($let* ([opener ($one-of #[\d])]
                   [meat   ($->string ($many ($one-of #[a-z])))]
                   [closer ($char opener)])
                  ($return (list opener meat closer)))
           "0zyx0")

;; $parameterize
(define param1 (make-parameter "zzz"))
(define match-param1 ($let ([word ($->string ($many ($one-of #[a-z])))])
                       (if (string-contains word (param1))
                         ($fail #"word can't have '~(param1)'")
                         ($return word))))
(test-succ "$parameterize" "lineament"
           ($parameterize ((param1 "cicle"))
              match-param1)
           "lineament")
(test-fail "$parameterize" '(9 "word can't have 'line'")
           ($parameterize ((param1 "line"))
              match-param1)
           "lineament")

;; $lift and $lift*
(test-succ "$lift" '(#\a . #\b)
           ($lift cons ($any) ($any))
           "abc")
(test-fail "$lift" '(1 #\z)
           ($lift cons ($any) ($char #\z))
           "abc")

(test-succ "$lift*" "abc"
           ($lift* list->string ($any) ($any) ($any))
           "abc")
(test-fail "$lift" '(2 #\z)
           ($lift* list->string ($any) ($any) ($char #\z))
           "abc")

;; $fold-parsers and $fold-parsers-right
(test-succ "$fold-parsers" '()                  ; base case
           ($fold-parsers cons '() '())
           "abc")
(test-succ "$fold-parsers" '(#\c #\b #\a)
           ($fold-parsers cons '() (list ($char #\a) ($char #\b) ($char #\c)))
           "abc")
(test-fail "$fold-parsers" '(2 #\c)
           ($fold-parsers cons '() (list ($char #\a) ($char #\b) ($char #\c)))
           "abd")

(test-succ "$fold-parsers-right" '()            ; base case
           ($fold-parsers-right cons '() '())
           "abc")
(test-succ "$fold-parsers-right" '(#\a #\b #\c)
           ($fold-parsers-right cons '() (list ($char #\a) ($char #\b) ($char #\c)))
           "abc")
(test-fail "$fold-parsers-right" '(2 #\c)
           ($fold-parsers-right cons '() (list ($char #\a) ($char #\b) ($char #\c)))
           "abd")

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

;; $many_
(test-succ "$many_" #\a
           ($seq ($many_ ($string "a") 1 2) ($one-of #[a-z])) "aaaaa")
(test-succ "$many_" #\b
           ($seq ($many_ ($string "a")) ($one-of #[a-z])) "baaaaa")

;; $repeat
(test-succ "$repeat" '(#\a #\b #\a)
           ($repeat ($any) 3) "abab")
(test-fail "$repeat" '(2 "end of input")
           ($repeat ($any) 3) "ab")

;; $optional
(test-succ "$optional" #\a
           ($optional ($char #\a)) "abab")
(test-succ "$optional" #f
           ($optional ($char #\a)) "ABAB")
(test-fail "$optional" '(1 #\b)
           ($optional ($seq ($char #\a) ($char #\b)))
           "ac")

;; $sep-by
(test-succ "$sep-by" '(#\a #\b)
           ($sep-by ($one-of #[ab]) ($string ","))
           "a,b")

(test-fail "$sep-by" '(2 #[ab])
           ($sep-by ($one-of #[ab]) ($string ","))
           "a,c")

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
(let1 p ($sep-end-by ($seq ($one-of #[a-z]) ($one-of #[a-z])) ($string ","))
  (define (succ in exp) (test-succ "$sep-end-by" exp p in))
  (define (fail in exp) (test-fail "$sep-end-by" exp p in))

  (succ "ZZ" '())
  (succ ""   '())
  (fail "aZ" '(1 #[a-z]))
  (succ "aa"  '(#\a))
  (succ "bb," '(#\b))
  (succ "cc,Z" '(#\c))
  (fail "cc,dZ" '(4 #[a-z]))
  (succ "ee,ff," '(#\e #\f))
  (succ "ggZ"  '(#\g))
  (succ "hh,," '(#\h))
  (succ "ii,jjZ"  '(#\i #\j))
  (fail "kk,ll,mZ" '(7 #[a-z]))

  (test-succ "$sep-end-by (min)" '(#\a #\b)
             ($sep-end-by ($seq ($one-of #[a-z]) ($one-of #[a-z]))
                          ($string ",") 2)
             "aa,bb")
  (test-succ "$sep-end-by (min)" '(#\a #\b)
             ($sep-end-by ($seq ($one-of #[a-z]) ($one-of #[a-z]))
                          ($string ",") 2)
             "aa,bb,")
  (test-fail "$sep-end-by (min)" '(5 ",")
             ($sep-end-by ($seq ($one-of #[a-z]) ($one-of #[a-z]))
                          ($string ",") 3)
             "aa,bb")
  (test-fail "$sep-end-by (min)" '(6 #[a-z])
             ($sep-end-by ($seq ($one-of #[a-z]) ($one-of #[a-z]))
                          ($string ",") 3)
             "aa,bb,")
  )

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
           ($many-till ($one-of #[[:alnum:]]) ($one-of #[\d]))
           "ab78")

;; $chain-left
(let ([integer
       ($let ([v ($many ($one-of #[\d])  1)])
         ($return (string->number (apply string v))))]
      [op
       ($or ($seq ($char #\*) ($return *))
            ($seq ($char #\+) ($return +)))])
  (test-succ "$chain-left" 9
             ($chain-left integer op)
             "1+2*3")
  (test-succ "$chain-left" 3
             ($chain-left integer op)
             "1+2*")
  (test-fail "$chain-left" '(0 #[0-9])
             ($chain-left integer op)
             "abc"))

;; $chain-right
(let ([integer
       ($let ([v ($many ($one-of #[\d])  1)])
         ($return (string->number (apply string v))))]
      [op
       ($or ($seq ($char #\*) ($return *))
            ($seq ($char #\+) ($return +)))])
  (test-succ "$chain-right" 7
             ($chain-right integer op)
             "1+2*3")
  (test-succ "$chain-right" 3
             ($chain-right integer op)
             "1+2*")
  (test-fail "$chain-right" '(0 #[0-9])
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
           ($or ($try ($lift list ($string "abc") ($string "foo")))
                ($string "abc"))
           "abcdefg")

(let1 parser ($or ($let ([foo ($try ($let ([v ($string "abc")]
                                           [($one-of #[+-])])
                                      ($return v)))]
                         [bar ($string "foo")])
                    ($return (list foo bar)))
                  ($string "abc"))
  (test-succ "$or and $try" "abc" parser
             "abcdefg")
  (test-fail "$or and $try" '(4 "foo") parser
             "abc+efg")
  )

;;;============================================================
;;; Lookahead assertion
;;;
(test-section "lookahead assertion")

(let ([parser ($seq0 
               ($->rope ($many 
                         ($try ($seq0 ($any)
                                             ($assert ($. #[a-z]))))))
               ($any))])
  (test-succ "positive lookahead" " abc" parser
             " abcd ")
  )

(let ([parser1 ($between ($. "/*")
                         ($->rope ($many ($seq ($not ($. "*/"))
                                               ($any))))
                         ($. "*/"))])
  (test-succ "negative lookahead 1" " abc * def " parser1
             "/* abc * def */ def")
  (test-fail "negative lookahead 1" '(12 "*/")  parser1
             "/* abc * def")
  )


;;;============================================================
;;; Error handling
;;;

(test-section "error handling")

;; compound error

(let* ([p1 ($or ($char #\a) ($char #\b))]
       [p2 ($seq ($any) ($or ($char #\c) p1))])
  (test* "nested compound error"
         (test-error <parse-error> "expecting one of (#\\c #\\a #\\b) at 1")
         (peg-parse-string p2 "dd")))

;;;============================================================
;;; Optional arg for peg-parse-string and peg-parse-port
;;;

(test-section "optional cont arg")

(test* "peg-parse-string cont" '("aaaa" #\b #\b #\c)
       (peg-parse-string ($->rope ($many ($. #\a))) "aaaabbc" cons))
(test* "peg-parse-port cont" '("aaaa" #\b #\b #\c)
       (call-with-input-string "aaaabbc"
         (cut peg-parse-port ($->rope ($many ($. #\a))) <> cons)))

;;;============================================================
;;; Examples
;;;
(test-section "examples")

;; PEG walkthrough
(test* "walkthrough char" #\a
       (peg-parse-string ($. #\a) "abc"))
(test* "walkthrough error" (test-error <parse-error>
                                       "expecting #\\a at 0, but got #\\x")
       (peg-parse-string ($char #\a) "xyz"))
(test* "walkthrough $many" '(#\a #\a #\a #\a #\a)
       (peg-parse-string ($many ($. #\a)) "aaaaabc"))
(test* "walkthrough $many" '()
       (peg-parse-string ($many ($. #\a)) "xxxxxyz"))

(let ()
  (define digits    ($many1 ($. #[\d])))
  (define ws        ($many_ ($. #[\s])))
  (define separator ($seq ws ($. #\,) ws))
  (define integer
    ($let ([ds digits])
      ($return (x->integer (list->string ds)))))
  (define integers1 
    ($seq integer ($many ($seq separator integer))))
  (define integers2 
    ($let ([n  integer]
           [ns ($many ($seq separator integer))])
      ($return (cons n ns))))
  (define integers3
    ($lift cons
           integer
           ($many ($seq separator integer))))
  (define integers4
    ($or ($let ([n  integer]
                [ns ($many ($seq separator integer))])
           ($return (cons n ns)))
         ($return '())))
  (define (sep-by stuff separator)
    ($or ($let ([n  stuff]
                [ns ($many ($seq separator stuff))])
           ($return (cons n ns)))
         ($return '())))
  (define integers5 (sep-by integer separator))
  (define begin-list 
    ($seq0 ($. #[\(\[\{]) ws))
  (define (end-list opener) 
    ($seq ws (case opener
               [(#\() ($. #\))]
               [(#\[) ($. #\])]
               [(#\{) ($. #\})])))
  
  (define int-list
    ($let* ([opener begin-list]
            [ints ($sep-by integer separator)]
            [ (end-list opener) ])
      ($return ints)))

  (define nested-list
    ($lazy
     ($let* ([opener begin-list]
             [ints ($sep-by elem separator)]
             [ (end-list opener) ])
       ($return ints))))
  (define elem  ($or integer nested-list))

  (test* "integers1" '(456 789)
         (peg-parse-string integers1 "123, 456, 789"))
  (test* "integers2" '(123 456 789)
         (peg-parse-string integers2 "123, 456, 789"))
  (test* "integers3" '(123 456 789)
         (peg-parse-string integers3 "123, 456, 789"))
  (test* "integers4" '(123 456 789)
         (peg-parse-string integers4 "123, 456, 789"))
  (test* "integers4" '()
         (peg-parse-string integers4 ""))
  (test* "integers5" '(123 456 789)
         (peg-parse-string integers5 "123, 456, 789"))
  (test* "integers5" '()
         (peg-parse-string integers5 ""))

  (test* "int-list" '(123 456 789)
         (peg-parse-string int-list "[123, 456, 789]"))
  (test* "int-list" '(123 456 789)
         (peg-parse-string int-list "{123, 456, 789}"))
  (test* "int-list" (test-error <parse-error>
                                "expecting #\\) at 14, but got #\\}")
         (peg-parse-string int-list "(123, 456, 789}"))

  (test* "nested-list" '(123 (456 () 789) 987)
         (peg-parse-string nested-list "(123, [456, {}, 789], 987)"))
  )

(let ()
  (define paren  ($. #\())
  (define thesis ($. #\)))

  (test* "$or example" 
         (test-error <parse-error> "expecting ab at 1, but got #\\c")

         (peg-parse-string ($or ($seq paren ($."ab") thesis)
                                ($seq paren ($."cd") thesis))
                           "(cd)"))
  )
  
(let ()
  (define integer   ($many1 ($. #[\d])))
  (define ws        ($many_ ($. #[\s])))
  (define int-list0 ($seq ($. #\[ )
                          ($many ($seq ws integer))
                          ws
                          ($char #\] )))
  (define int-list
    ($let ([ ($char #\[ )]
           [digs ($many ($seq ws integer))]
           [ ws ]
           [ ($char #\] ) ])
      ($return (map (compose x->integer list->string) digs))))

  (test-succ "integer" '(#\1 #\2 #\3) integer "123")
  (test-succ "int-list0" #\] int-list0 "[123 456 789]")
  (test-succ "int-list" '(123 456 789) int-list "[123 456 789]")
  )

;; Count the maximal nesting level
(letrec ((nesting
          ($lazy ($or ($let ([ ($char #\() ]
                             [n nesting]
                             [ ($char #\)) ]
                             [m nesting])
                        ($return (max (+ n 1) m)))
                      ($return 0)))))
  (test-succ "nesting parenthesis" 3 nesting "((()))")
  (test-succ "nesting parenthesis" 3 nesting "((()))()")
  (test-succ "nesting parenthesis" 3 nesting "(()(()))")
  (test-fail "nesting parenthesis" '(3 #\) ) nesting "((("))

;; number parser (1) - simple
(let* ([sign?    ($optional ($one-of #[-+]))]
       [digits   ($seq sign? ($many ($. #[\d]) 1))]
       [point    ($seq ($char #\.) ($many ($. #[\d]) 1))]
       [exponent ($seq ($. #[eE]) digits)]
       [number?  ($seq digits ($optional point) ($optional exponent))]
       [p        ($seq number? ($eos) ($return #t))])
  (define (%test str . fails?)
    (if (null? fails?)
      (test-succ #"number(1) \"~str\"" #t p str)
      (test-fail #"number(1) \"~str\"" (car fails?) p str)))
  (%test "27652") (%test "-27652") (%test "+27652")
  (%test "" '(0 #[0-9])) (%test "-" '(1 #[0-9])) (%test "+" '(1 #[0-9]))
  (%test "+27.46") (%test "0.46")
  (%test ".46" '(0 #[0-9]))
  (%test "0..3" '(2 #[0-9]))
  (%test "0.4.2" '(3 "end of input"))
  (%test "3e5")
  (%test "3.0e5")
  (%test "3.e5" '(2 #[0-9]))
  (%test "3e-53")
  (%test "3e+53")
  (%test "-3.0e+53")
  (%test "-3.0e+" '(6 #[0-9]))
  )

;; CSV parser
(let ()
  (define ws     ($many ($. #[ \t])))
  (define comma  ($seq ws ($. #\,) ws))
  (define dquote ($. #\"))
  (define double-dquote ($seq ($. "\"\"") ($return #\")))
  (define quoted-body ($many ($or double-dquote ($. #[^\"]))))
  (define quoted ($between dquote quoted-body dquote))
  (define unquoted ($many-till ($any) ($or comma ($. #\newline))))
  (define field ($or quoted unquoted))
  (define record ($sep-by ($->rope field) comma))
  (test-succ "CSV" '("a" "b" "c")
             record "a,b,c")
  (test-succ "CSV" '("a" "b" "c")
             record "\"a\" , b  , c")
  (test-succ "CSV" '("a  \" \n" "b" "c")
             record "\"a  \"\" \n\" , b  , c"))

;; hand-tuned version
(let ()
  (define ws     ($many_ ($. #[ \t])))
  (define comma  ($seq ws ($. #\,) ws))
  (define dquote ($. #\"))
  (define double-dquote ($seq ($. "\"\"") ($return #\")))
  (define quoted-body ($many ($or ($. #[^\"]) double-dquote)))
  (define quoted ($between dquote quoted-body dquote))
  (define unquoted ($sep-end-by ($many1 ($. #[^ \t\r\n,]))
                                ($many_ ($. #[ \t]))))
  (define field   ($or quoted unquoted))
  (define record  ($sep-by ($->rope field) comma 1))
  (define records ($sep-end-by record ($. "\r\n")))
  ;; NB: In CSV spec there's an ambiguity of treatment of the ending CRLF.
  ;; The CRLF after the last record can be omitted; so if the file ends with
  ;; CRLF, it may be the end of whole records, or there may be one record
  ;; with an empty field.  Customary it is resolved to take the first
  ;; interpretation.  Since PEG is eager the straightforward implementation
  ;; takes the second interpretation.   For the sake of testing here, it
  ;; doesn't matter either way.
  (test-succ "CSV 2" '(("a" "b" "c") ("x" "y" "z") (""))
             records "a,b,c\r\nx,y,z\r\n")
  (test-succ "CSV 2" '(("a " "b" "c") ("zzz\nyyy " " w \" ") (""))
             records "\"a \" , b  , c\r\n\"zzz\nyyy \", \" w \"\" \"\r\n")
  (test-succ "CSV 2" '(("a  \" \n" "b" "c"))
             records "\"a  \"\" \n\" , b  , c"))

;; Poor-man's XML parser
(letrec ([open-tag ($->rope ($between ($char #\<)
                                      ($one-of #[[:alnum:]])
                                      ($char #\>)))]
         [close-tag (^[tagname]
                      ($seq ($string "</") ($string tagname) ($char #\>)))]
         [text ($->rope ($many ($none-of #[<])))]
         [body ($let ([t text]
                      [r ($many ($let ([e element]
                                       [t text])
                                  ($return (list e t))))])
                 ($return `(,t ,@(apply append r))))]
         [element ($let* ([tagname ($try open-tag)]
                          [body body]
                          [ (close-tag (rope-finalize tagname)) ])
                    ($return (cons tagname body)))])
  (test-succ "tag element" '("a" "")
             element "<a></a>")
  (test-succ "tag element" '("a" "foo" ("b" "bar") "baz")
             element "<a>foo<b>bar</b>baz</a>"))

;; Calculator
(letrec ([integer ($let ([v ($many ($one-of #[\d]) 1)])
                    ($return (string->number (apply string v))))]
         [mulop ($or ($seq ($char #\*) ($return *))
                     ($seq ($char #\/) ($return /)))]
         [addop ($or ($seq ($char #\+) ($return +))
                     ($seq ($char #\-) ($return -)))]
         [factor ($lazy ($or ($between ($char #\() expr ($char #\)))
                             integer))]
         [term ($chain-left factor mulop)]
         [expr ($chain-left term addop)])
  (test-succ "calculator" 11 expr "1+2*3+4")
  (test-succ "calculator" 36 expr "2/2+5*(3+4)")
  (test-succ "calculator" -1 expr "1-2"))

;;;
;;; deprecated
;;;

(test-section "deprecated")
;; kludge - we can't say (use parser.peg.deprecated) until peg/deprecated.scm
;; is installed in place.  
(or (load "peg/deprecated" :error-if-not-found #f)
    (load "parser/peg/deprecated" :error-if-not-found #f))
(import parser.peg.deprecated)
(test-module 'parser.peg.deprecated)

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

(test-succ "$match1*" #\a
           ($match1* (#\c x #\r) x)
           "car foo")
(test-succ "$match1*" #\d
           ($match1* (#\c x #\r) x)
           "cdr foo")
(test-succ "$match1*" '(#\a (#\space #\f #\o #\o))
           ($match1* (#\c x #\r . t) (list x t))
           "car foo")
(test-succ "$match1* (no result arg)" '(#\c #\u #\r)
           ($match1* (#\c x #\r))
           "cur foo")
(test-fail "$match1*" '(0 "(#\\c x #\\r)")
           ($match1* (#\c x #\r) x)
           "zxy foo")
(let ((p ($match1* (#\c x #\r) (=> F)
                   (if (#[ad] x) x (F "car or cdr expected")))))
  (test-succ "$match1* fail proc" #\a p "car")
  (test-succ "$match1* fail proc" #\d p "cdr")
  (test-fail "$match1* fail proc" '(0 "car or cdr expected") p "cxr"))
(let ((p ($match1* (#\c x #\r . t) (=> F)
                   (if (#[ad] x) (list x t) (F "car or cdr expected")))))
  (test-succ "$match1* fail proc" '(#\a (#\z)) p "carz")
  (test-succ "$match1* fail proc" '(#\d (#\z)) p "cdrz")
  (test-fail "$match1* fail proc" '(0 "car or cdr expected") p "cxrz"))

(test-succ "$match1" #\a ($match1 #\a) "abc")
(test-succ "$match1" #\0 ($match1 (and (? #[0-9] d)) d) "0ab")
(test-fail "$match1" '(0 "#\\a") ($match1 #\a 'yo) "0ab")
(let ((p ($match1 (? char-numeric? x) (=> F)
                  (if (eqv? x #\0)
                    'ok
                    (F "0 expected")))))
  (test-succ "$match1 fail proc" 'ok p "012") 
  (test-fail "$match1 fail proc" '(0 "0 expected") p "123")
  (test-fail "$match1 not enough input" '(0 "(? char-numeric? x)") p ""))

(test-succ "eof" (eof-object) eof "")
(test-fail "eof" '(0 "end of input") eof "a")

;;;============================================================
;;; rfc.json
;;;   Test is here since the module uses parser.peg.

(test-section "rfc.json")
(use rfc.json)
(test-module 'rfc.json)

(let ()
  (define (t str val)
    (test* "primitive" `(("x" . ,val)) (parse-json-string str)))
  (t "{\"x\": 100 }" 100)
  (t "{\"x\" : -100}" -100)
  (t "{\"x\":  +100 }" 100)
  (t "{\"x\": 12.5} " 12.5)
  (t "{\"x\":-12.5}" -12.5)
  (t "{\"x\":+12.5}"  12.5)
  (t "{\"x\": 1.25e1 }" 12.5)
  (t "{\"x\":125e-1}" 12.5)
  (t "{\"x\":1250.0e-2}" 12.5)
  (t "{\"x\":  false  }" 'false)
  (t "{\"x\":true}" 'true)
  (t "{\"x\":null}" 'null)
  (t "{\"x\": \"abc\\\"\\\\\\/\\b\\f\\n\\r\\t\\u0040abc\"}"
     "abc\"\\/\u0008\u000c\u000a\u000d\u0009@abc")
  )

(let ()
  (define (t str)
    (test* #"parse error ~str" (test-error <json-parse-error>)
           (parse-json-string str)))
  (t "{\"x\": 100")
  (t "{x : 100}}")
  )

(test* "parsing an object"
       '(("Image"
          ("Width"  . 800)
          ("Height" . 600)
          ("Title"  . "View from 15th Floor")
          ("Thumbnail"
           ("Url"    . "http://www.example.com/image/481989943")
           ("Height" . 125)
           ("Width"  . "100"))
          ("IDs" . #(116 943 234 38793))))
       (parse-json-string "{
   \"Image\": {
       \"Width\":  800,
       \"Height\": 600,
       \"Title\":  \"View from 15th Floor\",
       \"Thumbnail\": {
           \"Url\":    \"http://www.example.com/image/481989943\",
           \"Height\": 125,
           \"Width\":  \"100\"
       },
       \"IDs\": [116, 943, 234, 38793]
     }
}"))

(test* "parsing an array containing two objects"
       '#((("precision" . "zip")
           ("Latitude"  . 37.7668)
           ("Longitude" . -122.3959)
           ("Address"   . "")
           ("City"      . "SAN FRANCISCO")
           ("State"     . "CA")
           ("Zip"       . "94107")
           ("Country"   . "US"))
          (("precision" . "zip")
           ("Latitude"  . 37.371991)
           ("Longitude" . -122.026020)
           ("Address"   . "")
           ("City"      . "SUNNYVALE")
           ("State"     . "CA")
           ("Zip"       . "94085")
           ("Country"   . "US")))
       (parse-json-string "[
   {
      \"precision\": \"zip\",
      \"Latitude\":  37.7668,
      \"Longitude\": -122.3959,
      \"Address\":   \"\",
      \"City\":      \"SAN FRANCISCO\",
      \"State\":     \"CA\",
      \"Zip\":       \"94107\",
      \"Country\":   \"US\"
   },
   {
      \"precision\": \"zip\",
      \"Latitude\":  37.371991,
      \"Longitude\": -122.026020,
      \"Address\":   \"\",
      \"City\":      \"SUNNYVALE\",
      \"State\":     \"CA\",
      \"Zip\":       \"94085\",
      \"Country\":   \"US\"
   }
]"))

(test* "Parsing sequence of json objects"
       '((("a" . 1)("b" . 2)) (("c" . 3) ("d" . 4)))
       (with-input-from-string "{\"a\":1, \"b\":2}{\"c\":3, \"d\":4}"
         parse-json*))

(let ()
  (define (parse-with-generator str)
    (generator->list (call-with-input-string str
                       (cut make-json-generator <>))))

  (test* "generator basic" '(null) (parse-with-generator "  null "))
  (test* "generator basic" '(true) (parse-with-generator "true"))
  (test* "generator basic" '(false) (parse-with-generator " false foo foo"))
  (test* "generator basic" '(123) (parse-with-generator " 123;567"))
  (test* "generator basic" '("json") (parse-with-generator " \"json\"515"))

  (test* "generator array"
         '(array-start 1 2 3 "abc" null true false array-end)
         (parse-with-generator " [1,2 , 3  ,\"abc\",null , true, false] 1234"))
  (test* "generator array"
         '(array-start array-end)
         (parse-with-generator " [] "))
  (test* "generator object"
         '(object-start "a" 1 "b" 2 object-end)
         (parse-with-generator "{\"a\":1, \"b\" : 2} zzz"))
  (test* "generator object"
         '(object-start object-end)
         (parse-with-generator " {} "))

  (test* "generator nesting"
         '(array-start object-start "a" array-start 1 2 3 array-end 
                       "b" 456 object-end null
                       array-start 3 1 2 array-end array-end)
         (parse-with-generator "[ {\"a\": [1,2 ,3],\"b\":456}, null, [ 3, 1, 2]]"))
  )


(test* "Customizing constructors"
       '(object ("x" array 1 2 3) ("y" array #f #t null))
       (parameterize ([json-array-handler (^[elts] (cons 'array elts))]
                      [json-object-handler (^[pairs] (cons 'object pairs))]
                      [json-special-handler (^y (case y
                                                  [(false) #f]
                                                  [(true) #t]
                                                  [(null) 'null]))])
         (parse-json-string "{\"x\":[1,2,3],\"y\":[false,true,null]}")))

(let ()
  (define (test-writer name obj)
    (test* name obj
           (parse-json-string (construct-json-string obj))))

  (test-writer "writing an object"
               '(("Image"
                  ("Width"  . 800)
                  ("Height" . 600)
                  ("Title"  . "View from 15th Floor \"magnificent\"")
                  ("Thumbnail"
                   ("Url"    . "http://www.example.com/image/481989943")
                   ("Height" . 125)
                   ("Width"  . "100"))
                  ("Description" . "Foo\nbackslash \\and tab\t and \u00a1")
                  ("IDs" . #(116 943 234 38793))
                  ("Misc" . ()))))

  (test-writer "writing an array containing two objects"
               '#((("precision" . "zip")
                   ("Latitude"  . 37.7668)
                   ("Longitude" . -122.3959)
                   ("Address"   . "")
                   ("City"      . "SAN FRANCISCO")
                   ("State"     . "CA")
                   ("Zip"       . "94107")
                   ("Country"   . "US"))
                  (("precision" . "zip")
                   ("Latitude"  . 37.371991)
                   ("Longitude" . -122.026020)
                   ("Address"   . "")
                   ("City"      . "SUNNYVALE")
                   ("State"     . "CA")
                   ("Zip"       . "94085")
                   ("Country"   . "US"))))
  )

(cond-expand
 [gauche.ces.utf8
  (let1 data `(("[\"\\u03bb\"]" #("\x3bb;"))
               ("[\"\\ud800\"]" ,(test-error <json-parse-error>))
               ("[\"\\ud867\\ude3d\\u03bb\"]" #("\x29e3d;\x3bb;"))
               ("[\"\\ude3d\\ud867\"]" ,(test-error <json-parse-error>))
               ("[\"\\uf020\\u03bb\"]"  #("\xf020;\x3bb;")))
    (dolist [d data]
      (test* (format "unicode escape reading (~s)" (car d))
             (cadr d)
             (parse-json-string (car d)))
      (when (vector? (cadr data))
        (test* (format "unicode escape writing (~s)" (cadr d))
               (car d)
               (construct-json-string (cadr d))))))]
 [else])

(let ()
  (define (t obj)
    (test* #"writer error ~obj" (test-error <json-construct-error>)
           (construct-json-string obj)))
  (t "a")
  (t '#(1 2 x))
  (t '(("a" . 2) 9)))

(test* "generalized array" "[1,2,3]"
       (construct-json-string '#u8(1 2 3)))
(test* "generalized object" (test-one-of "{\"a\":1,\"b\":2}"
                                         "{\"b\":2,\"a\":1}")
       (construct-json-string (hash-table 'eq? '(a . 1) '(b . 2))))

;; https://sourceforge.net/p/gauche/mailman/message/36786284/
(test* "read/write invariance"
       '#(48.529166)
       (parse-json-string (construct-json-string '#(48.529166))))

;; json-nesting-depth-limit
(test* "depth-limit" '#(#(#(#(null))))
       (parameterize ((json-nesting-depth-limit 4))
         (parse-json-string "[[[[null]]]]")))
(test* "depth-limit" (test-error <json-parse-error> #/nesting is too deep/)
       (parameterize ((json-nesting-depth-limit 4))
         (parse-json-string "[[[[[null]]]]]")))
(test* "depth-limit" 123
       (parameterize ((json-nesting-depth-limit 1))
         (parse-json-string "123")))
(test* "depth-limit" (test-error <json-parse-error> #/nesting is too deep/)
       (parameterize ((json-nesting-depth-limit 1))
         (parse-json-string "{\"x\":123}")))
       
(test-end)
