;;
;; testing regexp
;;

(use gauche.test)
(use srfi-1)
(use srfi-14)

(test-start "regexp")

;; Some test examples are taken from the test suite of Henry Spencer's
;; regexp package and PCRE.

(define (test-re re str expect)
  (test* (write-to-string `(,re ,str)) expect
         (rxmatch-substrings (rxmatch re str))))

(define (test-before re str expect . opts)
  (test* (write-to-string `(rxmatch-before (,re ,str))) expect
         (apply rxmatch-before (re str) opts)))
(define (test-after re str expect . opts)
  (test* (write-to-string `(rxmatch-after (,re ,str))) expect
         (apply rxmatch-after (re str) opts)))
(define (test-substring re str expect . opts)
  (test* (write-to-string `(rxmatch-substring (,re ,str))) expect
         (apply rxmatch-substring (re str) opts)))

(define-syntax test-parse
  (syntax-rules ()
    ((_ re ast)
     (test* #"regexp-parse \"~re\"" ast (regexp-parse re)))))

;;-------------------------------------------------------------------------
(test-section "regexp-parse")

(test-parse "a" '(0 #f #\a))
(test-parse "ab" '(0 #f #\a #\b))
(test-parse "(?:ab)" '(0 #f (seq #\a #\b)))
(test-parse "(a)" '(0 #f (1 #f #\a)))
(test-parse "a?" '(0 #f (rep 0 1 #\a)))
(test-parse "a*" '(0 #f (rep 0 #f #\a)))
(test-parse "a+" '(0 #f (rep 1 #f #\a)))
(test-parse "a{3,5}" '(0 #f (rep 3 5 #\a)))
(test-parse "a{3}" '(0 #f (rep 3 3 #\a)))
(test-parse "a|b" '(0 #f (alt #\a #\b)))
(test-parse "[ab]" '(0 #f #[ab]))
(test-parse "[^ab]" '(0 #f (comp . #[ab])))
(test-parse "\\x0d;\\x0a;" '(0 #f #\return #\newline))
(test-parse "\\x0dbc\\x0acd" ;legacy fallback
            '(0 #f (seq #\return #\b #\c) (seq #\newline #\c #\d)))
(test-parse "\\u000dbc;" '(0 #f (seq #\return #\b #\c #\;)))
(test-parse "\x0d;[\x0a;-\x0d]" '(0 #f #\return #[\u000a-\u000d]))
(test-parse "." '(0 #f any))
(test-parse "^" '(0 #f bol))
(test-parse "$" '(0 #f eol))
(test-parse "\\b" '(0 #f wb))
(test-parse "\\B" '(0 #f nwb))
(test-parse "(?>a)" '(0 #f (once #\a)))
(test-parse "a*+" '(0 #f (once (rep 0 #f #\a))))
(test-parse "a++" '(0 #f (once (rep 1 #f #\a))))
(test-parse "a?+" '(0 #f (once (rep 0 1 #\a))))
(test-parse "(?i:a)" '(0 #f (seq-uncase #\a)))
(test-parse "(?-i:a)" '(0 #f (seq-case #\a)))
(test-parse "(?=a)" '(0 #f (assert #\a)))
(test-parse "(?!a)" '(0 #f (nassert #\a)))
(test-parse "(?<=ab)" '(0 #f (assert (lookbehind #\a #\b))))
(test-parse "(?<!ab)" '(0 #f (nassert (lookbehind #\a #\b))))
(test-parse "(?<name>a)" '(0 #f (1 name #\a)))
(test-parse "(?(?=)y)" '(0 #f (cpat (assert) (#\y) ())))
(test-parse "(?(?=)y|n)" '(0 #f (cpat (assert) (#\y) (#\n))))
(test-parse "(?(?<=)y)" '(0 #f (cpat (assert (lookbehind)) (#\y) ())))
(test-parse "(?(?<=)y|n)" '(0 #f (cpat (assert (lookbehind)) (#\y) (#\n))))
(test-parse "()(?(1)y)" '(0 #f (1 #f) (cpat 1 (#\y) ())))
(test-parse "()(?(1)y|n)"'(0 #f (1 #f) (cpat 1 (#\y) (#\n))))
(test-parse "()\\1" '(0 #f (1 #f) (backref . 1)))
(test-parse "(?<name>)\\k<name>" '(0 #f (1 name) (backref . 1)))
(test-parse "(?<name>)(?<name>)\\k<name>"
            '(0 #f (1 name) (2 name) (alt (backref . 2) (backref . 1))))

;;-------------------------------------------------------------------------
(test-section "regexp-unparse")

(define (test-regexp-unparse src)
  (let1 ast (if (string? src) (regexp-optimize (regexp-parse src)) src)
    (test* (format "regexp-unparse ~s" src) ast
           (regexp-optimize (regexp-parse (regexp-unparse ast)))
           equal?)))

(for-each test-regexp-unparse
          '(;; simple ones
            "" "a" "ab" "a*" "a+" "a?" "ab*" "ab+" "ab?" "a*b" "a+b" "a?b"
            "a{3}" "a{2,4}" "a{3,}" "a|b" "a|b|c" "^ab" "^a|b" "^a|b$"
            "." "\\." "\\**" "\\(\\)\\{\\}\\[\\]" "\\bfoo\\B"
            ;; charset
            "[a-z]" "[^a-z]" "[a-z]+" "[a^ef-]" "[a]" "[.]" "[\\[]" "[\\^]"
            ;; grouping
            "(a)(b)" "(a|b)c(d)" "(a(b(c|d)|e))f"
            "(?:abc)*" "(ab(?:cd)?e{3,4})" "(?i:ab(?-i:cd)ef)"
            ;; backref
            "(a)bc\\1" "ab(?<foo>c)(d)\\k<foo>" "((.)\\2)"
            ;; once, lookahead, lookbehind
            "(?>abc)" "(?=a*b*c)" "(?!a*b*c)" "(?<=a[bc])" "(?<!a[bc])"
            "(a)(?(1)b)" "(a)(?(1)b|c)" "(a)(?(1)|c)"
            "(?(?=ab)cd|ef)" "(?(?!ab)cd|ef)"
            "(?(?<=ab)cd|ef)" "(?(?<!ab)cd|ef)" 
            ))

;;-------------------------------------------------------------------------
(test-section "compile")

(define-syntax test-regexp-compile
  (syntax-rules ()
    [(_ pat)
     (test* #"regexp-compile \"~|pat|\""
            (let1 orig (string->regexp pat)
              (list (regexp->string orig)
                    (regexp-num-groups orig)
                    (regexp-named-groups orig)))
            (let1 compiled (regexp-compile (regexp-parse pat))
              (list (regexp->string compiled)
                    (regexp-num-groups compiled)
                    (regexp-named-groups compiled))))]))

(test-regexp-compile "a")
(test-regexp-compile "ab")
(test-regexp-compile "(?:ab)")
(test-regexp-compile "(a)")
(test-regexp-compile "a?")
(test-regexp-compile "a*")
(test-regexp-compile "a+")
(test-regexp-compile "a{3,5}")
(test-regexp-compile "a{3}")
(test-regexp-compile "a|b")
(test-regexp-compile "[ab]")
(test-regexp-compile "[^ab]")
(test-regexp-compile ".")
(test-regexp-compile "^")
(test-regexp-compile "$")
(test-regexp-compile "\\b")
(test-regexp-compile "\\B")
(test-regexp-compile "(?>a)")
(test-regexp-compile "a*+")
(test-regexp-compile "a++")
(test-regexp-compile "a?+")
(test-regexp-compile "(?i:a)")
(test-regexp-compile "(?-i:a)")
(test-regexp-compile "(?=a)")
(test-regexp-compile "(?!a)")
(test-regexp-compile "(?<=ab)")
(test-regexp-compile "(?<!ab)")
(test-regexp-compile "(?<name>a)")
(test-regexp-compile "(?(?=)y)")
(test-regexp-compile "(?(?=)y|n)")
(test-regexp-compile "(?(?<=)y)")
(test-regexp-compile "(?(?<=)y|n)")
(test-regexp-compile "()(?(1)y)")
(test-regexp-compile "()(?(1)y|n)")
(test-regexp-compile "()\\1")
(test-regexp-compile "(?<name>)\\k<name>")
(test-regexp-compile "(?<name>)(?<name>)\\k<name>")

;; Renumbering groups
(test* "regexp-compile group renumbering"
       '(5 (0 #f (1 #f (2 #f #\a) (3 #f #\b)) (4 #f #\c)))
       (let1 c (regexp-compile
                '(0 #f (3 #f (100 #f #\a) (0 #f #\b)) (3 #f #\c)))
         (list (regexp-num-groups c)
               (regexp-ast c))))

(define %regexp-laset (with-module gauche.internal %regexp-laset))
(define-syntax test-regexp-laset
  (syntax-rules ()
    [(_ pat exp)
     (test* #"regexp-laset \"~|pat|\"" exp
            (%regexp-laset (regexp-compile (regexp-parse pat))))]))

(test-regexp-laset "abc" #[a])
(test-regexp-laset "(abc)" #[a])
(test-regexp-laset "a|b|c" #[a-c])
(test-regexp-laset "(a|b)|c" #[a-c])
(test-regexp-laset "a*b" #[ab])
(test-regexp-laset "a+b" #[a])
(test-regexp-laset "(abc)*(bcd)*ef" #[abe])
(test-regexp-laset "([^\"]|\"\")+" (char-set-complement #[]))

;;-------------------------------------------------------------------------
(test-section "boundary")

(test* "submatch #f" #f (rxmatch-start #f))
(test* "submatch #f" #f (rxmatch-end #f))
(test* "submatch #f" #f (rxmatch-substring #f))
(test* "num-match #f" 0 (rxmatch-num-matches #f))

;;-------------------------------------------------------------------------
(test-section "basics")

(test-re #/a/ "a"   '("a"))
(test-re #/a/ "A"   '())
(test-re #/a/ "ba"  '("a"))
(test-re #/a/ "bac" '("a"))
(test-re #/a/ ""    '()) ;; input null str
(test-re #/a/ (string (integer->char 0) #\a #\b) '("a")) ;; NUL character
(test-re #/abc/ "abc" '("abc"))
(test-re #/abc/ "abbc" '())
(test-re #/abc/ "babcd" '("abc"))
(test-re #/abc|de/ "dabce" '("abc"))
(test-re #/abc|de/ "abdec" '("de"))
(test-re #/abc|de/ "abe" '())
(test-re #/a|b|c/ "abc" '("a"))
(test-re #/a|b|c/ "bac" '("b"))
(test-re #/a|b|c/ "def" '())
(test-re #/|abc/ "abc" '(""))
(test-re #/abc|/ "abc" '("abc"))
(test-re #/abc|/ "abd" '(""))

;;-------------------------------------------------------------------------
(test-section "parens")

(test-re #/a(b)c/        "abc"     '("abc" "b"))
(test-re #/a((b)(c))/    "abc"     '("abc" "bc" "b" "c"))
(test-re #/a((((b))))c/  "abc"     '("abc" "b" "b" "b" "b"))
(test-re #/a((((b))))c/  "a(b)c"   '())
(test-re #/a\(/          "a("      '("a("))
(test-re #/a()b/         "ab"      '("ab" ""))
(test-re #/a()()b/       "ab"      '("ab" "" ""))
(test-re #/(we|wee|week|frob)(knights|night|day)/ "weeknights"
         '("weeknights" "wee" "knights"))
(test-re #/aa|(bb)|cc/   "aabb"    '("aa" #f))
(test-re #/aa|(bb)|cc/   "abbaa"   '("bb" "bb"))
(test-re #/aa|(bb)|cc/   "bccaa"   '("cc" #f))
(test-re #/aa|a(b)|cc/   "abaab"   '("ab" "b"))
(test-re #/aa|a(b)/      "abaab"   '("ab" "b"))
(test-re #/aa|(a(b))|cc/ "abaabcc" '("ab" "ab" "b"))
(test-re #/(ab)|ac/      "aaaabcc" '("ab" "ab"))
(test-re #/(a(b))|ac/    "abaabcc" '("ab" "ab" "b"))
(test-re #/ab|(ac)/      "aaaabcc" '("ab" #f))
(test-re #/ab|(ac)/      "aaaacbc" '("ac" "ac"))
(test-re #/aa|(ab|(ac))|ad/ "cac"  '("ac" "ac" "ac"))
(test-re #/(aa|(a(b)|a(c))|ad)/ "cac" '("ac" "ac" "ac" #f "c"))
(test-re #/(.)*/         "abc"     '("abc" "c"))
(test-re #/(a([^a])*)*/  "abcaBC"  '("abcaBC" "aBC" "C"))
(test-re #/b|()|a/       "cac"     '("" ""))
(test-re #/(a)*a/        "a"       '("a" #f))
(test-re #/(a)*a/        "aa"      '("aa" "a"))
(test-re #/(a)*a/        "aaa"     '("aaa" "a"))
(test-re #/(a)*?a/       "a"       '("a" #f))
(test-re #/(a)*?a/       "aa"      '("a" #f))

;;-------------------------------------------------------------------------
(test-section "simple meta")

(test-re #/a.c/ "abc" '("abc"))
(test-re #/a../ "abc" '("abc"))
(test-re #/a../ "ab"  '())
(test-re #/.../ "ab"  '())
(test-re #/./   "abc" '("a"))
(test-re #/./   ""    '())

;;-------------------------------------------------------------------------
(test-section "anchors")

(test-re #/^abc/       "abcd" '("abc"))
(test-re #/^abc/       "aabcd" '())
(test-re #/^^/         "^^abc" '("^"))
(test-re #/^^/         "a^^c" '())
(test-re #/^abc|def/   "abc" '("abc"))
(test-re #/^abc|def/   "zabc" '())
(test-re #/^abc|def/   "zabcdef" '("def"))
(test-re #/abc|^def/   "defabc" '("def"))
(test-re #/abc|^def/   "abcdef" '("abc"))
(test-re #/abc|^def/   "defabbc" '("def"))
(test-re #/abc|^def/   "adefbc" '())
(test-re #/^(abc|def)/ "abc" '("abc" "abc"))
(test-re #/^(abc|def)/ "aabc" '())
(test-re #/(^abc|def)/ "abcdef" '("abc" "abc"))
(test-re #/(^abc|def)/ "^abcdef" '("def" "def"))
(test-re #/a(^bc|def)/ "a^bcdef" '("a^bc" "^bc"))
(test-re #/a(^bc|def)/ "abcdef" '())
(test-re #/^/          "hoge" '(""))
(test-re (string->regexp "^a" :multi-line #t)
         "def\nabc\nghi"
         '("a"))
(test-re #/$/          "hoge" '(""))
(test-re (string->regexp "c$" :multi-line #t)
         "def\nabc\nghi"
         '("c"))
(test-re #/abc$/       "bcabc" '("abc"))
(test-re #/abc$/       "abcab" '())
(test-re #/^abc$/      "abc" '("abc"))
(test-re #/abc$$/      "abc" '())
(test-re #/abc$$/      "abc$" '("abc$"))
(test-re #/$$/         "abc$" '("$"))
(test-re #/^$/         "" '(""))
(test-re #/^$/         "a" '())
(test-re #/^^$$/       "^$" '("^$"))
(test-re #/abc$|def/   "abc" '("abc"))
(test-re #/abc$|def/   "defabc" '("def"))
(test-re #/^abc|def$/  "abcdef" '("abc"))
(test-re #/^abc|def$/  "defabc" '())
(test-re #/^abc|def$/  "defabc" '())
(test-re #/(^abc|def$)/ "aaadef" '("def" "def"))
(test-re #/(^abc|def$)$/ "aaadef" '())
(test-re #/(^abc|def$)$/ "aaadef$" '("def$" "def$"))
(test-re #/(abc$|def)$/ "aaabc" '())
(test-re #/(abc$|def)$/ "aaabc$" '("abc$" "abc$"))
(test-re #/a$b/         "aa$bb" '("a$b"))
(test-re #/ab\$/        "ab$cd" '("ab$"))

;;-------------------------------------------------------------------------
(test-section "backslash escape")

(test-re #/a\*c/        "a*c" '("a*c"))
(test-re #/a\.c/        "a.c" '("a.c"))
(test-re #/a\.c/        "abc" '())
(test-re #/a\\b/        "a\\b" '("a\\b"))
(test-re #/a\\\*b/      "a\\*b" '("a\\*b"))
(test-re #/a\jc/        "ajc" '("ajc"))
(test-re #/a\\bc/       "a\\bc" '("a\\bc"))
(test-re #/a\[b/        "a[b" '("a[b"))

;;-------------------------------------------------------------------------
(test-section "word boundary")

(test-re #/.z\b/ "bzbazoz ize" '("oz"))
(test-re #/\b.z/ "brzbazoz ize" '("iz"))
(test-re #/.z\B/ "bz baz oz ize" '("iz"))
(test-re #/\B.z/ "bz baz oz ize" '("az"))

;;-------------------------------------------------------------------------
(test-section "repetitions")

(test-re #/ab*c/     "abc"                  '("abc"))
(test-re #/ab*c/     "ac"                   '("ac"))
(test-re #/ab*c/     "abbbc"                '("abbbc"))
(test-re #/ab*c/     "abbabaabbc"           '("abbc"))
(test-re #/ab+c/     "abc"                  '("abc"))
(test-re #/ab+c/     "abbc"                 '("abbc"))
(test-re #/ab+c/     "abbabaabbc"           '("abbc"))
(test-re #/ab?c/     "abc"                  '("abc"))
(test-re #/ab?c/     "abbaac"               '("ac"))
(test-re #/a.*c/     "abc"                  '("abc"))
(test-re #/a.*c/     "zaabcabcabcabcczab"   '("aabcabcabcabcc"))
(test-re #/a(b*|c)d/ "abbd"                 '("abbd" "bb"))
(test-re #/a(b*|c)d/ "ad"                   '("ad" ""))
(test-re #/a(b*|c)d/ "acd"                  '("acd" "c"))
(test-re #/a(b*|c)d/ "abcd"                 '())
(test-re #/a.*c/     "bacbababbbbadbaba"    '("ac"))
(test-re #/a.*c/     "abaaaabababbadbabdba" '())

;; Tests for input skipping using laset
(test-re #/a+b+/     "aaaacccccccbaaabbccc" '("aaabb"))
(test-re #/a*b+/     "aaaacccccccbaaabbccc" '("b"))

(test-re #/a+@a+\.a+/ "aaaaaaaaaaa@aaaaaaaa@@a.aaaaa@aaaaa" '())
(test-re #/a+@a+\.a+/ "aaaaaaaaaaa@aaaaaaaa@@a.aaaaa@aaaaa.a" '("aaaaa@aaaaa.a"))
 
;; Repetition of empty string
(test-re #/a(?:)*b/ "zabr" '("ab"))
(test-re #/a(?:)?b/ "zabr" '("ab"))
(test-re #/a(?:){2,5}b/ "zabr" '("ab"))

;;-------------------------------------------------------------------------
(test-section "repetitions (non-greedy)")

(test-re #/ab*?./         "abc"        '("ab"))
(test-re #/ab*?./         "ac"         '("ac"))
(test-re #/a.*?c/         "abbbc"      '("abbbc"))
(test-re #/a.*?a/         "abbabaabbc" '("abba"))
(test-re #/<.*?>/         "<tag1><tag2><tag3>" '("<tag1>"))

(test-re #/ab+?./         "abc"        '("abc"))
(test-re #/ab+?./         "abbc"       '("abb"))
(test-re #/a.+?a/         "abbabaabbc" '("abba"))
(test-re #/<.+?>/         " <><tag1><tag2>" '("<><tag1>"))

(test-re #/ab??c/         "abc"        '("abc"))
(test-re #/ab??c/         "abbaac"     '("ac"))
(test-re #/ab??./         "abbaac"     '("ab"))
(test-re #/a(hoge)??hoge/ "ahogehoge"  '("ahoge" #f))
(test-re #/(foo)??bar/    "foobar"     '("foobar" "foo"))
(test-re #/(foo)??bar/    "foofoobar"  '("foobar" "foo"))
(test-re #/(foo)*?bar/    "foofoobar"  '("foofoobar" "foo"))

;;-------------------------------------------------------------------------
(test-section "character class")

(test-re #/a[bc]d/   "abd" '("abd"))
(test-re #/a[bc]d/   "acd" '("acd"))
(test-re #/a[bc]d/   "aed" '())
(test-re #/a[a-z]d/  "aed" '("aed"))
(test-re #/a[a-z]d/  "aEd" '())
(test-re #/a[]]d/    "a]d" '("a]d"))
(test-re #/a[]-]d/   "a-d" '("a-d"))
(test-re #/a[]-^]d/  "a-d" '())
(test-re #/a[]-^]d/  "a]d" '("a]d"))
(test-re #/a[a-z-]d/ "a-d" '("a-d"))
(test-re #/a[a-z-]d/ "afd" '("afd"))
(test-re #/a[az-]d/  "a-d" '("a-d"))
(test-re #/a[a-]d/   "a-d" '("a-d"))
(test-re #/a[az-]d/  "afd" '())
(test-re #/a[az-]d/  "azd" '("azd"))
(test-re #/a[^ab]c/  "abacc" '("acc"))
(test-re #/a[^]]c/   "abc" '("abc"))
(test-re #/a[^]]c/   "a]c" '())
(test-re #/a[^^]c/   "abc" '("abc"))
(test-re #/a[^^]c/   "a^c" '())
(test-re #/a[Bc]*d/  "aBccBd" '("aBccBd"))
(test-re #/[a]b[c]/  "abc" '("abc"))
(test-re #/[abc]b[abc]/ "abc" '("abc"))
(test-re #/a[bc]d/   "xyzaaabcaababdacd" '("abd"))
(test-re #/a[ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab](wee|week)(knights|night)/
         "aaaaabaaaabaaaabaaaabweeknights"
         '("aaaaabaaaabaaaabaaaabweeknights" "wee" "knights"))
(test-re #/[ab][cd][ef][gh][ij][kl][mn]/ "xacegikmoq" '("acegikm"))
(test-re #/[ab][cd][ef][gh][ij][kl][mn][op]/ "xacegikmoq" '("acegikmo"))
(test-re #/[ab][cd][ef][gh][ij][kl][mn][op][qr]/ "xacegikmoqy" '("acegikmoq"))
(test-re #/[ab][cd][ef][gh][ij][kl][mn][op][q]/ "xacegikmoqy" '("acegikmoq"))

;; this tests optimizer
(test-re #/[^a]*./    "b"    '("b"))
(test-re #/\s/        "  "   '(" "))
(test-re #/\s\s/      "  "   '("  "))
(test-re #/\s/        "\t "  '("\t"))
(test-re #/\s\s/      "\\s"  '())
(test-re #/\\s\s/     "\\s " '("\\s "))
(test-re #/(\s*)(\S+)(\s*)/ "\txyz   abc" '("\txyz   " "\t" "xyz" "   "))
(test-re #/(\d+)(\D+)(\d+)(\D+)(\d+)/ " (1234) 5678-9012 "
         '("1234) 5678-9012" "1234" ") " "5678" "-" "9012"))
(test-re #/(\w+)(\W+)(\w+)/ "three o'clock" '("three o" "three" " " "o"))

(test-re #/[ab\sc]+/  "d ba e"     '(" ba "))
(test-re #/[\sabc]+/  "d ba e"     '(" ba "))
(test-re #/[\S\t]+/   "\tab cd"    '("\tab"))
(test-re #/[\s\d]+/   "a 1 2 3 b " '(" 1 2 3 "))
(test-re #/[\s\D]+/   "a 1 2 3 b " '("a "))

(cond-expand
 [gauche.ces.none #f]
 [else
  (test-re #/\u3000*x/ "\u3000\u3000\u3000x" '("\u3000\u3000\u3000x"))
  (test-re #/\u3000+/  "\u3001\u3000\u3000" '("\u3000\u3000"))
  (test-re #/\u3000+x/ "\u3000\u3000\u3001x" '())
  (test-re #/\u3000+[xy]/ "\u3000\u3000\u3001x\u3001\u3000y" '("\u3000y"))])

;; this tests optimizer
(test-re #/^\[?([^\]]*)\]?:(\d+)$/ "127.0.0.1:80"
         '("127.0.0.1:80" "127.0.0.1" "80"))
(test-re #/^\[?([^\]]*)\]?:(\d+)$/ "[127.0.0.1:80"
         '("[127.0.0.1:80" "127.0.0.1" "80"))
(test-re #/^\[?([^\]]*)\]?:(\d+)$/ "[127.0.0.1]:80"
         '("[127.0.0.1]:80" "127.0.0.1" "80"))

;;-------------------------------------------------------------------------
(test-section "{n,m}")

(test-after #/a{2}/   "abaaaabaa"  "aabaa")
(test-after #/a{2,}/  "abaaaabaa"  "baa")
(test-after #/a{1,2}/ "abaaaabaa"  "baaaabaa")
(test-after #/a{2,3}/ "abaaaabaa"  "abaa")
(test-after #/a{b,c}/ "za{b,c}def" "def")
;; The following test assumes Perl compatibility.  (See the comment in
;; rc1_lex_minmax in regexp.c.
;(test-after #/a{,2}*/  "za{,2}}def" "def")
;; The above should be written in Oniguruma compatible mode.
(test-after #/a\{,2}*/ "za{,2}}def" "def")
;; The following test assumes Oniguruma compability.
(test-after #/a{,3}/   "aabaaaab"  "baaaab")

(test-after #/(ab){2}/   "babbabababba" "abba")
(test-after #/(ab){2,2}/ "babbabababba" "abba")
(test-after #/(ab){2,}/  "babbabababba" "ba")
(test-after #/(ab){1,2}/ "babbabababba" "babababba")
(test-after #/(ab){2,3}/ "babbabababba" "ba")
(test-after #/(ab){b,c}/ "zab{b,c}def"  "def")

(test-re #/(\d{2})(\d{2})/ "a12345b" '("1234" "12" "34"))
(test-re #/(\d{2,})(\d{2,})/ "a12345b" '("12345" "123" "45"))
(test-re #/(\d{2})(\d{2,})/ "a12345b" '("12345" "12" "345"))
(test-re #/(\d{1,3})(\d{2,})/ "a1234b" '("1234" "12" "34"))
(test-re #/(\d{1,3})(\d{0,2})/ "a1234b" '("1234" "123" "4"))
(test-re #/(\d{2}){2}/ "a12345b" '("1234" "34"))

(test* "{2}" (test-error)  (regexp? (string->regexp "{2}")))
(test* "{z}" #t            (regexp? (string->regexp "{z}")))
(test* "{-1}" #t           (regexp? (string->regexp "{-1}")))
(test* "{300}" (test-error) (regexp? (string->regexp "{300}")))
(test* "{3,1}" (test-error) (regexp? (string->regexp "{3,1}")))

;;-------------------------------------------------------------------------
(test-section "{n,m} (non-greedy)")

(test-after #/a{2}?/   "abaaaabaa" "aabaa")
(test-after #/a{2,}?/  "abaaaabaa" "aabaa")
(test-after #/a{1,2}?/ "abaaaabaa" "baaaabaa")
(test-after #/a{2,3}?/ "abaaaabaa" "aabaa")

(test-after #/(ab){2}?/   "babbabababba" "abba")
(test-after #/(ab){2,2}?/ "babbabababba" "abba")
(test-after #/(ab){2,}?/  "babbabababba" "abba")
(test-after #/(ab){1,2}?/ "babbabababba" "babababba")
(test-after #/(ab){2,3}?/ "babbabababba" "abba")

(test-re #/(\d{2,}?)(\d{2,}?)/ "a12345b" '("1234" "12" "34"))
(test-re #/(\d{2,})(\d{2,}?)/ "a12345b" '("12345" "123" "45"))
(test-re #/(\d{2,}?)(\d{2,})/ "a12345b" '("12345" "12" "345"))
(test-re #/(\d{1,3}?)(\d{2,}?)/ "a1234b" '("123" "1" "23"))
(test-re #/(\d{1,3}?)(\d{0,2}?)/ "a1234b" '("1" "1" ""))

;;-------------------------------------------------------------------------
(test-section "uncapturing group")

(test-substring #/a(?:b)*c(d)/    "abcdbcdefg" "d" 1)
(test-substring #/a(?:bcd)*e(f)/  "abcdbcdefg" "f" 1)
(test-substring #/a(?:bcd)*e(f)/  "aefg"       "f" 1)
(test-substring #/a(?:bcd)+e(f)/  "aefg"       #f  1)
(test-substring #/a(?:bc(de(?:fg)?hi)jk)?l/ "abcdefghijkl" "defghi" 1)
(test-substring #/a(?:bc(de(?:fg)?hi)jk)?l/ "abcdehijkl"   "dehi"   1)

(test-substring #/a(?i:bc)d/      "!aBCd!" "aBCd")
(test-substring #/a(?i:bc)d/      "!aBCD!" #f)
(test-substring #/a(?i:[a-z]+)d/  "!aBcd!" "aBcd")
(test-substring #/a(?i:[a-z]+)d/  "!ABcd!" #f)

(test-substring #/A(?-i:Bc)D/     "!ABcD!" "ABcD")
(test-substring #/A(?-i:Bc)D/     "!ABcd!" #f)
(test-substring #/A(?-i:[A-Z]+)D/ "!ABCD!" "ABCD")
(test-substring #/A(?-i:[A-Z]+)D/ "!abCD!" #f)

(test-substring #/A(?-i:Bc)D/i     "!aBcd!" "aBcd")
(test-substring #/A(?-i:Bc)D/i     "!abCd!" #f)
(test-substring #/A(?-i:[A-Z]+)D/i "!aBCd!" "aBCd")
(test-substring #/A(?-i:[A-Z]+)D/i "!abcd!" #f)

;; these tests optimizer
(test-substring #/^(?i:a*).$/      "b"      "b")
(test-substring #/^(?i:a*).$/      "a"      "a")
(test-substring #/^(?i:a*).$/      "A"      "A")
(test-substring #/^(?i:a*).$/      "Ab"     "Ab")

;;-------------------------------------------------------------------------
(test-section "backreference")
(test-re #/^(.)\1$/ "aa" '("aa" "a"))
(test-re #/^(.)\1$/ "ab" '())
(test-re #/(.+)\1/ "a123123j" '("123123" "123"))
(test-re #/(.+)\1/i "AbCaBC" '("AbCaBC" "AbC"))
(test-re #/(.+)\1/ "AbCAb1" '())
(test-re #/((.)\2)/ "aa" '("aa" "aa" "a"))
(test-re #/(a)*\1/ "aa" '("aa" "a"))
(test-re #/(aa)*\1/ "aaa" '())
(test-re #/(aa)*\1/ "aaaaa" '("aaaa" "aa"))
(test* "^\\1(.)$" (test-error) (string->regexp "^\\1(.)"))
(test* "^(\\1)$" (test-error) (string->regexp "^(\\1)$"))
(test* "(.)\\2" (test-error) (string->regexp "(.)\\2"))
(test* "((.)\\1)" (test-error) (string->regexp "((.)\\1)"))

;;-------------------------------------------------------------------------
(test-section "independent subexpression")
(test-re #/(?>.*\/)foo/ "/this/is/a/long/line/" '())
(test-re #/(?>.*\/)foo/ "/this/is/a/long/line/foo" '("/this/is/a/long/line/foo"))
(test-re #/(?>(\.\d\d[1-9]?))\d+/ "1.230003938" '(".230003938" ".23"))
(test-re #/(?>(\.\d\d[1-9]?))\d+/ "1.875000282" '(".875000282" ".875"))
(test-re #/(?>(\.\d\d[1-9]?))\d+/ "1.235" '())
(test-re #/^((?>\w+)|(?>\s+))*$/ "foo bar" '("foo bar" "bar"))
(test-re #/a*+a/ "aaa" '())
(test-re #/a*+b/ "aab" '("aab"))
(test-re #/a++a/ "aaa" '())
(test-re #/a++b/ "aab" '("aab"))
(test-re #/a?+a/ "a"   '())
(test-re #/a?+b/ "ab"  '("ab"))

;;-------------------------------------------------------------------------
(test-section "lookahead assertion")

(test-re #/^(?=ab(de))(abd)(e)/ "abde" '("abde" "de" "abd" "e"))
(test-re #/^(?!(ab)de|x)(abd)(f)/ "abdf" '("abdf" #f "abd" "f"))
(test-re #/^(?=(ab(cd)))(ab)/ "abcd" '("ab" "abcd" "cd" "ab"))
(test-re #/\w+(?=\t)/ "the quick brown\t fox" '("brown"))
(test-re #/foo(?!bar)(.*)/ "foobar is foolish see?" '("foolish see?" "lish see?"))
(test-re #/(?:(?!foo)...|^.{0,2})bar(.*)/ "foobar crowbar etc" '("rowbar etc" " etc"))
(test-re #/(?:(?!foo)...|^.{0,2})bar(.*)/ "barrel" '("barrel" "rel"))
(test-re #/(?:(?!foo)...|^.{0,2})bar(.*)/ "2barrel" '("2barrel" "rel"))
(test-re #/(?:(?!foo)...|^.{0,2})bar(.*)/ "A barrel" '("A barrel" "rel"))
(test-re #/^(\D*)(?=\d)(?!123)/ "abc456" '("abc" "abc"))
(test-re #/^(\D*)(?=\d)(?!123)/ "abc123" '())
(test-re #/(?!^)abc/ "the abc" '("abc"))
(test-re #/(?!^)abc/ "abc" '())
(test-re #/(?!^^).../ "^^abc" '("^ab"))
(test-re #/(?!^^).../ "^abc" '("abc"))
(test-re #/(?=^)abc/ "abc" '("abc"))
(test-re #/(?=^)abc/ "the abc" '())
(test-re #/(?=^^).../ "^abc" '("^ab"))
(test-re #/(?=^^).../ "^^abc" '("^^a"))
(test-re #/^(?=$).*/ "" '(""))
(test-re #/^(?!$).*/ "" '())
(test-re #/^(?=$).*/ "a" '())
(test-re #/^(?=$$).*/ "$" '("$"))
(test-re #/^(?!$).*/ "a" '("a"))

(test-re #/(\.\d\d((?=0)|\d(?=\d)))/ "1.230003938" '(".23" ".23" ""))
(test-re #/(\.\d\d((?=0)|\d(?=\d)))/ "1.875000282" '(".875" ".875" "5"))
(test-re #/(\.\d\d((?=0)|\d(?=\d)))/ "1.235" '())
(test-re #/^\D*(?!123)/ "ABC123" '("AB"))
(test-re #/^(\D*)(?=\d)(?!123)/ "ABC445" '("ABC" "ABC"))
(test-re #/^(\D*)(?=\d)(?!123)/ "ABC123" '())
(test-re #/a(?!b)./ "abad" '("ad"))
(test-re #/a(?!^b)./ "a^bad" '("a^"))
(test-re #/a(?!b)/ "abad" '("a"))
(test-re #/a(?=d)./ "abad" '("ad"))
(test-re #/a(?=^b)./ "a^bad" '())
(test-re #/a(?=c|d)./ "abad" '("ad"))

;;-------------------------------------------------------------------------
(test-section "lookbehind assertion")
(test-re #/(?<=a)b/ "b" '())
(test-re #/(?<=a)b/ "ab" '("b"))
(test-re #/(?<=a+)b/ "aab" '("b"))
(test-re #/(?<=x[yz])b/ "xzb" '("b"))
(test-re #/(?<=zyx)b/ "xyzb" '())
(test-re #/(?<=[ab]+)c/ "abc" '("c"))
(test-re #/(?<!<[^>]*)foo/ "<foo>" '())
(test-re #/(?<!<[^>]*)foo/ "<bar>foo" '("foo"))
(test-re #/(?<=^a)b/ "ab" '("b"))
(test-re #/(?<=^)b/ "ab" '())
(test-re #/(?<=^)b/ "b" '("b"))
(test-re #/.(?<=^)b/ "a^b" '())
(test-re #/(?<=^a$)/ "a" '(""))
(test-re #/(?<=^a$)b/ "a$b" '())
(test-re #/(?<=(a))b/ "ab" '("b" "a"))
(test-re #/(?<=(a)(b))c/ "abc" '("c" "a" "b"))
(test-re #/(?<=(a)|(b))c/ "bc" '("c" #f "b"))
(test-re #/(?<=(?<!foo)bar)baz/ "abarbaz" '("baz"))
(test-re #/(?<=(?<!foo)bar)baz/ "foobarbaz" '())
(test-re #/(?<=\d{3})(?<!999)foo/ "865foo" '("foo"))
(test-re #/(?<=\d{3})(?<!999)foo/ "999foo" '())
(test* "(?<=(?>a*))" (test-error) (string->regexp "(?<=(?>a*))"))
(test-re #/(abc)...(?<=\1)/ "abcabc" '("abcabc" "abc"))
(test-re #/(abC)...(?<=\1)/i "abCAbc" '("abCAbc" "abC"))

;;-------------------------------------------------------------------------
(test-section "named group")
(test-re #/(?<foo>a)/ "a" '("a" "a"))
(test* "(?<foo>a)" "a"
       (let1 m (#/(?<foo>a)/ "a")
         (m 'foo)))
(test* "(?<foo>a)" #f
       (let1 m (#/(?<foo>a)?/ "b")
         (m 'foo)))
(test* "(?<foo>a)(?<bar>.*)" '("a" "bcd")
       (let1 m (#/(?<foo>a)(?<bar>.*)/ "abcd")
         (list (m 'foo) (m 'bar))))
(test-re #/(?<foo>a)(?<bar>.*)/ "abcd" '("abcd" "a" "bcd"))
(test* "(?<foo>a)" (test-error)
       (let1 m (#/(?<foo>a)/ "abcd")
         (m 'bar)))
(test-re #/(?<foo>^a$)/ "a" '("a" "a"))
(test-re #/(?<foo>^a$)/ "ab" '())
(test-re #/(?<name-with-hyphen>a)/ "a" '("a" "a"))
(test* "(?<host>\d+.\d+.\d+.\d+)|(?<host>[\w.]+)"
       '("127.0.0.1" "127.0.0.1" #f "127.0.0.1")
       (let1 m (#/(?<host>\d+.\d+.\d+.\d+)|(?<host>[\w.]+)/ "127.0.0.1")
         (list (m 0) (m 1) (m 2) (m 'host))))
(test* "(?<host>\d+.\d+.\d+.\d+)|(?<host>[\w.]+)"
       '("foo.com" #f "foo.com" "foo.com")
       (let1 m (#/(?<host>\d+.\d+.\d+.\d+)|(?<host>[\w.]+)/ "foo.com")
         (list (m 0) (m 1) (m 2) (m 'host))))
(test-re #/(?<foo>.+)\k<foo>/ "abcabc" '("abcabc" "abc"))
(test-re #/(?<foo>.+)\k<foo>/ "abcdef" '())

(test-parse  "\\k<foo>" (test-error))
(test-before #/(?<foo>def)/ "abcdefghi" "abc" 'foo)
(test-after  #/(?<foo>def)/ "abcdefghi" "ghi" 'foo)

(test* "rxmatch-start" 3 (rxmatch-start (#/(?<foo>def)/ "abcdefghi") 'foo))
(test* "rxmatch-end" 6   (rxmatch-end (#/(?<foo>def)/ "abcdefghi") 'foo))

;;-------------------------------------------------------------------------
(test-section "conditional subexpression")
(test-re #/(a)(?(1)b)/ "ab" '("ab" "a"))
(test-re #/(a)(?(1)b)/ "aa" '())
(test-re #/(a)(?(1)b)/ "ac" '())
(test-re #/(a)?(?(1)b|c)/ "xb" '())
(test-re #/(a)?(?(1)b|c)/ "xc" '("c" #f))
(test-re #/(<)?[^<>]+(?(1)>)/ "<foo>" '("<foo>" "<"))
(test-re #/(<)?[^<>]+(?(1)>)/ "foo" '("foo" #f))
(test-re #/(?(?<=a)b)/ "ab" '(""))
(test-re #/(?(?<=a)b)/ "ac" '(""))
(test-re #/(?(?<=a)b)/ "xb" '(""))
(test-parse "(?(?a)b|c)" (test-error))
(test-re #/()(?(1))/ "" '("" ""))

(test-parse "()(?(" (test-error))
(test-parse "()(?(1" (test-error))
(test-parse "()(?(1)b|c|d)" (test-error))

;;-------------------------------------------------------------------------
(test-section "rxmatch with start/end")

(define (rxmatch->full-match re str :optional start end)
  (let ([match (rxmatch re str start end)])
    (and match
         (list (rxmatch-start match)
               (rxmatch-end match)
               (match 0)))))

(test* "abc" '(0 3 "abc") (rxmatch->full-match "abc" "abc"))
(test* "abc" '(0 3 "abc") (rxmatch->full-match "abc" "abc" 0 3))
(test* "abc" '(2 5 "abc") (rxmatch->full-match "abc" "zzabczz" 2))
(test* "abc" '(2 5 "abc") (rxmatch->full-match "^abc" "zzabczz" 2 6))
(test* "abc" '(3 6 "abc") (rxmatch->full-match "abc$" "zzzabczz" 2 6))
(test* "abc" '(5 8 "abc") (rxmatch->full-match "abc" "abczzabczz" 4))

;;-------------------------------------------------------------------------
(test-section "regexp macros")

(use gauche.regexp)

(test* "rxmatch-let" '("23:59:58" "23" "59" "58")
       (rxmatch-let (rxmatch #/(\d+):(\d+):(\d+)/
                             "Jan  1 23:59:58, 2001")
           (time hh mm ss)
         (list time hh mm ss)))
(test* "rxmatch-let" '("23" "59")
       (rxmatch-let (rxmatch #/(\d+):(\d+):(\d+)/
                             "Jan  1 23:59:58, 2001")
           (#f hh mm)
         (list hh mm)))

(test* "rxmatch-if" "time is 11:22"
       (rxmatch-if (rxmatch #/(\d+:\d+)/ "Jan 1 11:22:33")
           (time)
         (format #f "time is ~a" time)
         "unknown time"))
(test* "rxmatch-if" "unknown time"
       (rxmatch-if (rxmatch #/(\d+:\d+)/ "Jan 1 11-22-33")
           (time)
         (format #f "time is ~a" time)
         "unknown time"))

(define (test-parse-date str)
  (rxmatch-cond
    (test (not (string? str)) #f)
    ((rxmatch #/^(\d\d?)\/(\d\d?)\/(\d\d\d\d)$/ str)
     (#f mm dd yyyy)
     (map string->number (list yyyy mm dd)))
    ((rxmatch #/^(\d\d\d\d)\/(\d\d?)\/(\d\d?)$/ str)
     (#f yyyy mm dd)
     (map string->number (list yyyy mm dd)))
    ((rxmatch #/^\d+\/\d+\/\d+$/ str)
     (#f)
     (error "ambiguous:" str))
    (else (error "bogus:" str))))

(test* "rxmatch-cond" '(2001 2 3)
       (test-parse-date "2001/2/3"))
(test* "rxmatch-cond" '(1999 12 25)
       (test-parse-date "1999/12/25"))
(test* "rxmatch-cond" #f
       (test-parse-date 'abc))

(define (test-parse-date2 str)
  (rxmatch-case str
    (test (^s (not (string? s))) #f)
    (#/^(\d\d?)\/(\d\d?)\/(\d\d\d\d)$/ (#f mm dd yyyy)
        (map string->number (list yyyy mm dd)))
    (#/^(\d\d\d\d)\/(\d\d?)\/(\d\d?)$/ (#f yyyy mm dd)
        (map string->number (list yyyy mm dd)))
    (#/^\d+\/\d+\/\d+$/  (#f) (error "ambiguous:" str))
    (else (error "bogus:" str))))
(test* "rxmatch-case" '(2001 2 3)
       (test-parse-date2 "2001/2/3"))
(test* "rxmatch-case" '(1999 12 25)
       (test-parse-date2 "1999/12/25"))
(test* "rxmatch-case" #f
       (test-parse-date2 'abc))

(define (test-parse-date3 str)
  (rxmatch-case str
    (#/^(\d\d\d\d)\/(\d\d?)\/(\d\d?)$/ (#f yyyy mm dd)
        (map string->number (list yyyy mm dd)))
    (else => (cut format "bogus: ~a" <>))))
(test* "rxmatch-case (else)" "bogus: 100/2/3"
       (test-parse-date3 "100/2/3"))

(test* "regexp-replace" "abc|def|ghi"
       (regexp-replace #/def|DEF/ "abcdefghi" "|\\0|"))

(test* "regexp-replace" "abc|\\0|ghi"
       (regexp-replace #/def|DEF/ "abcdefghi" "|\\\\0|"))

(test* "regexp-replace" "abc|def|ghi"
       (regexp-replace #/(?<match>def|DEF)/ "abcdefghi" "|\\k<match>|"))

(test* "regexp-replace" (test-error)
       (regexp-replace #/(?<match>def|DEF)/ "abcdefghi" "|\\k<matchee>|"))

(test* "regexp-replace" "abraabra**brabra**brabrabracadabrabrabra"
       (regexp-replace #/a((bra)+)cadabra/
                       "abraabraabrabracadabrabrabrabracadabrabrabra"
                       "**\\1**"))

(test* "regexp-replace-all" "abraabra**brabra**br**brabra**brabra"
       (regexp-replace-all #/a((bra)+)cadabra/
                           "abraabraabrabracadabrabrabrabracadabrabrabra"
                           "**\\1**"))

(test* "regexp-replace-all" "abraabra**brabra**br**brabra**brabra"
       (regexp-replace-all #/a(?<match>(bra)+)cadabra/
                           "abraabraabrabracadabrabrabrabracadabrabrabra"
                           "**\\k<match>**"))

(test* "regexp-replace" "abfedhi"
       (regexp-replace #/c(.*)g/ "abcdefghi" 
                       (lambda (m)
                         (list->string
                          (reverse
                           (string->list (rxmatch-substring m 1)))))))

(test* "regexp-replace-all" "abraabra(bra^2)br(bra^2)brabra"
       (regexp-replace-all #/a((bra)+)cadabra/
                           "abraabraabrabracadabrabrabrabracadabrabrabra"
                           (lambda (m)
                             (format #f "(bra^~a)"
                                     (/ (string-length (rxmatch-substring m 1))
                                        3)))))

(test* "regexp-replace-all" "XaXbXcXdXeXfX"
       (regexp-replace-all #/\d*/ "abcdef" "X"))
(test* "regexp-replace-all" "XXaXbXcXXdXeXfX"
       (regexp-replace-all #/\d*/ "123abc45def" "X"))

(test* "regexp-replace*" "cbazzbc"
       (regexp-replace* "abcbabc"
                        #/abc/ "cba"
                        #/aba/ "abc"
                        #/bc/  "zz"))

(test* "regexp-replace-all*" "cbazzccbazz"
       (regexp-replace-all* "abcbacabcbc"
                            #/abc/ "cba"
                            #/aba/ "abc"
                            #/bc/  "zz"))

;;-------------------------------------------------------------------------
(test-section "regexp cimatch")

(test* "regexp/ci" "BC"
       (cond ((rxmatch #/bc/i "ABCD") => rxmatch-substring)
             (else #f)))
(test* "regexp/ci" "bC"
       (cond ((rxmatch #/Bc/i "AbCD") => rxmatch-substring)
             (else #f)))
(test* "regexp/ci" "bc"
       (cond ((rxmatch #/BC/i "AbcD") => rxmatch-substring)
             (else #f)))
(test* "regexp/ci" #f
       (cond ((rxmatch #/Bc/ "AbCD") => rxmatch-substring)
             (else #f)))
(test* "regexp/ci" #f
       (cond ((rxmatch #/BC/ "ABcD") => rxmatch-substring)
             (else #f)))
(test* "regexp/ci" "PAD"
       (cond ((rxmatch #/p[a-z]d/i "PAD") => rxmatch-substring)
             (else #f)))
(test* "regexp/ci" "pad"
       (cond ((rxmatch #/P[A-Z]D/i "pad") => rxmatch-substring)
             (else #f)))
(test* "regexp/ci" "bad"
       (cond ((rxmatch #/.[a-pQ-Z][A-Pq-z]/i "bad") => rxmatch-substring)
             (else #f)))
(test* "regexp/ci" #f
       (cond ((rxmatch #/p[a-z]d/ "PAD") => rxmatch-substring)
             (else #f)))
(test* "regexp/ci" #f
       (cond ((rxmatch #/P[A-Z]D/ "pad") => rxmatch-substring)
             (else #f)))
(test* "regexp/ci" #f
       (cond ((rxmatch #/.[a-pQ-Z][A-Pq-z]/ "bad") => rxmatch-substring)
             (else #f)))
(test* "regexp/ci" "pad"
       (cond ((rxmatch (string->regexp "p[A-Z]d" :case-fold #t) "pad")
              => rxmatch-substring)
             (else #f)))
(test* "regexp/ci" #f
       (cond ((rxmatch (string->regexp "p[A-Z]d") "pad")
              => rxmatch-substring)
             (else #f)))

;;-------------------------------------------------------------------------
(test-section "applicable regexp")

(define match #f)

(test* "object-apply regexp" #t
       (begin
         (set! match (#/(\d+)\.(\d+)/ "pi=3.14..."))
         (regmatch? match)))

(test* "object-apply regmatch (index)" '("3.14" "3" "14")
       (list (match) (match 1) (match 2)))

(test* "object-apply regmatch (symbol)" '("..." ".14..." "pi=" "pi=3.")
       (list (match 'after) (match 'after 1)
             (match 'before) (match 'before 2)))

(set! match (#/(?<int>\d+)\.(?<frac>\d+)/ "pi=3.14..."))

(test* "object-apply regmatch (index)" '("3.14" "3" "14" "3" "14")
       (list (match) (match 1) (match 2)
             (match 'int) (match 'frac)))

(test* "object-apply regmatch (symbol)"
       '("..." ".14..." ".14..." "pi=" "pi=3." "pi=3.")
       (list (match 'after) (match 'after 1) (match 'after 'int)
             (match 'before) (match 'before 2) (match 'before 'frac)))

(test* "object-apply regmatch (named submatch)"
       '("..." ".14..." ".14..." "pi=" "pi=3." "pi=3.")
       (list (match 'after) (match 'after 1) (match 'after 'int)
             (match 'before) (match 'before 2) (match 'before 'frac)))

;;-------------------------------------------------------------------------
(test-section "regexp quote")

(test* "regexp-quote" #t
       (let ((str "^(#$%#}{)\\-+?^$"))
         (regmatch? (rxmatch (string->regexp (regexp-quote str)) str))))

;;-------------------------------------------------------------------------
(test-section "regexp comparison")

(test* "equal #/abc/ #/abc/" #t
       (equal? #/abc/ #/abc/))
(test* "not equal #/abc/ #/a(bc)/" #f
       (equal? #/abc/ #/a(bc)/))
(test* "not equal #/abc/ #/abc/i" #f
       (equal? #/abc/ #/abc/i))
(test* "not equal #/abc/i #/abc/" #f
       (equal? #/abc/i #/abc/))
(test* "equal #/abc/i #/abc/i" #t
       (equal? #/abc/i #/abc/i))

;;-------------------------------------------------------------------------
(test-section "regexp printer")

(let ()
  (define (test-regexp-writer exp pat)
    (test* exp exp (write-to-string (string->regexp pat))))

  (test-regexp-writer "#/abc/" "abc")
  (test-regexp-writer "#/a[0-9]/" "a[\\d]")
  (test-regexp-writer "#/a[a-z]/" "a[a-z]")
  (test-regexp-writer "#/a\\/b/"  "a/b")
  (test-regexp-writer "#/\\\\/"  "\\\\")
  (test-regexp-writer "#/ /"  "\x20")
  (test-regexp-writer "#/ /"  "\\x20")
  (test-regexp-writer "#/\\//"  "\\/") ; single '/'
  (test-regexp-writer "#/\\\\\\//"  "\\\\/") ; backslash, then '/'
  (test-regexp-writer "#/\\\\\\//"  "\\\\\\/") ; backslash, then '/'
  )

(test* "#/(?i:abc)/" "#/(?i:abc)/"
       (write-to-string (string->regexp "abc" :case-fold #t)))

;;-------------------------------------------------------------------------
(test-section "regexp from AST")

;; A trivial cases are already covered by the preceding tests.
;; We test some edge cases.

;; empty alt clause always fails.
(test* "empty alt" #f
       (rxmatch (regexp-compile '(alt)) "a"))

;; empty alt clause can't be represented by '|' operator.  When unparsed,
;; we use negative lookahead assertion to represent "always fail" op.
(test* "empty alt unparse" "ab(?!)cd"
       (regexp->string (regexp-compile '(seq #\a #\b (alt) #\c #\d))))

;; a bit twisted way to test "always fail" pattern.  The first branch
;; matches to "abc" but fails at the empty alt, so the second branch is tried.
(test* "empty alt" "ab"
       (rxmatch->string (regexp-compile '(alt (seq #\a #\b #\c (alt))
                                              (seq #\a #\b)))
                        "abc"))

;;-------------------------------------------------------------------------
(test-section "SRE to AST transformation")

(use gauche.regexp.sre)

(define-syntax test-ast
  (syntax-rules ()
    [(_ expected regexp)
     (test* 'regexp
            '(0 #f expected)
            (regexp-parse-sre 'regexp))]))

(define-syntax test-cset
  (syntax-rules ()
    [(_ expected regexp)
     (test* 'regexp
            (list 0 #f expected)
            (regexp-parse-sre 'regexp))]))

(define-syntax test-ast-error
  (syntax-rules ()
    [(_ regexp)
     (test* 'regexp
            (test-error)
            (regexp-parse-sre 'regexp))]))

;; <cset-sre> part

(test-cset (string->char-set "a") (& ascii #\a))
(test-cset (string->char-set "a") (& ascii "a"))
(test-ast-error (& ascii "ab"))
(test-cset (string->char-set "abc") ("abc"))
(test-ast-error ("abc" "def"))
(test-cset (string->char-set "abc") (char-set "abc"))
(test-ast-error (char-set "abc" "def"))
(test* "<char-set>" `(0 #f ,char-set:ascii) (regexp-parse-sre char-set:ascii))
(test-cset (string->char-set "abc") ("abc"))
(test-ast-error ("abc" "def"))
(test-cset (string->char-set "ABC") (/ "AC"))
(test-cset (string->char-set "ABC") (char-range "AC"))
(test-cset (string->char-set "ABC") (/ "A" #\C))
(test-cset (string->char-set "") (/))
(test-cset (string->char-set "ab") (or #\a #\b))
(test-cset (string->char-set "a") (and ascii #\a))
(test-cset (string->char-set "b") (- (or #\a #\b) #\a))
(test-cset `(comp . ,(string->char-set "a")) (complement #\a))
(test-cset `(comp . ,(string->char-set "ab")) (~ #\a #\b))
(test-cset (string->char-set "bc") (and ("abc") (~ #\a)))

(test-cset (string->char-set "BC") (& ("ABC") (w/case ("aBC"))))
(test-ast-error (& ("ABC") (w/case ("aBC") "def")))
(test-cset (string->char-set "ABC") (& ("ABC") (w/nocase ("aBC"))))
(test-ast-error (& ("ABC") (w/nocase ("aBC") "def")))
(test-cset (string->char-set "BC") (& ("ABC") (w/unicode ("aBC"))))
(test-ast-error (& ("ABC") (w/unicode ("aBC") "def")))

(test-cset char-set:ascii ascii)
(test-ast any any)
(test-cset `(comp . ,(string->char-set "\n\r")) nonl)
(test-cset char-set:lower-case lower-case)
(test-cset char-set:lower-case lower)
(test-cset char-set:upper-case upper-case)
(test-cset char-set:upper-case upper)
(test-cset char-set:title-case title-case)
(test-cset char-set:title-case title)
(test-cset char-set:letter alphabetic)
(test-cset char-set:letter alpha)
(test-cset char-set:letter+digit alphanumeric)
(test-cset char-set:letter+digit alphanum)
(test-cset char-set:letter+digit alnum)
(test-cset char-set:digit numeric)
(test-cset char-set:digit num)
(test-cset char-set:punctuation punctuation)
(test-cset char-set:punctuation punct)
(test-cset char-set:symbol symbol)
(test-cset char-set:graphic graphic)
(test-cset char-set:graphic graph)
(test-cset char-set:whitespace whitespace)
(test-cset char-set:whitespace white)
(test-cset char-set:whitespace space)
(test-cset char-set:printing printing)
(test-cset char-set:printing print)
(test-cset (ucs-range->char-set 0 32) control)
(test-cset (ucs-range->char-set 0 32) cntrl)
(test-cset char-set:hex-digit hex-digit)
(test-cset char-set:hex-digit xdigit)

;; <sre> part

(test-ast (seq #\s #\t #\r) "str")
(test-ast (rep 0 #f #\a) (* #\a))
(test-ast (rep 0 #f #\a) (zero-or-more #\a))

(test-ast (rep 1 #f #\a) (+ #\a))
(test-ast (rep 1 #f #\a) (one-or-more #\a))

(test-ast (rep 0 1 #\a) (? #\a))
(test-ast (rep 0 1 #\a) (optional #\a))

(test-ast (rep 3 3 #\a) (= 3 #\a))
(test-ast (rep 3 3 #\a) (exactly 3 #\a))

(test-ast (rep 3 #f #\a) (>= 3 #\a))
(test-ast (rep 3 #f #\a) (at-least 3 #\a))

(test-ast (rep 3 5 #\a) (** 3 5 #\a))
(test-ast (rep 3 5 #\a) (repeated 3 5 #\a))

(test-ast (alt (seq #\a #\b) (seq #\c #\d))
          (|\|| "ab" "cd"))
(test-ast (alt (seq #\a #\b) (seq #\c #\d))
          (or "ab" "cd"))

(test-ast (seq (seq #\a #\b) (seq #\c #\d))
          (: "ab" "cd"))
(test-ast (seq (seq #\a #\b) (seq #\c #\d))
          (seq "ab" "cd"))

(test-ast (1 #f
	     (2 #f (seq #\a #\b #\c))
	     (3 #f (seq #\d #\e #\f))
	     (seq #\g #\h #\i))
          ($ ($ "abc") ($ "def") "ghi"))

(test-ast (1 result (seq #\a #\b #\c)) (-> result "abc"))

(test-ast (seq (seq (seq (seq #\a #\b #\c))
		    (seq (seq #\d #\e #\f))
		    (seq #\g #\h #\i)))
          (w/nocapture ($ ($ "abc") ($ "def") "ghi")))

(test-ast (seq (seq #\a #\b)) (w/case "ab"))
(test-cset `(seq (seq ,(string->char-set "Aa")
		      ,(string->char-set "Bb")))
	   (w/nocase "ab"))
(test-ast (seq (seq (seq #\a #\b))) (w/nocase (w/case "ab")))
(test-ast (seq (seq #[Aa] #[Bb]) (seq (seq #\c #\d)) (seq #[Ee] #[Ff]))
          (w/nocase "ab" (w/case "cd") "ef"))

(test-ast bol bol)
(test-ast eol eol)
(test-ast bow bow)
(test-ast eow eow)
(test-ast nwb nwb)

(test-ast (seq bow (seq #\a #\b #\c) eow) (word "abc"))
(test-ast (seq bow (rep 1 #f #[a-c]) eow) (word+ ("a**bc")))
(test-cset `(seq bow
                 (rep 1 #f
                      ,(char-set-union char-set:letter+digit
                                       (string->char-set "_")))
                 eow)
           word)

(test-ast (rep-min 0 1 #\a) (?? #\a))
(test-ast (rep-min 0 1 #\a) (non-greedy-optional #\a))

(test-ast (rep-min 0 #f #\a) (*? #\a))
(test-ast (rep-min 0 #f #\a) (non-greedy-zero-or-more #\a))

(test-ast (rep-min 3 5 #\a) (**? 3 5 #\a))
(test-ast (rep-min 3 5 #\a) (non-greedy-repeated 3 5 #\a))

(test-ast (assert #\a #\b #\c) (look-ahead #\a #\b #\c))
(test-ast (assert (lookbehind #\a #\b #\c)) (look-behind #\a #\b #\c))

(test-ast (nassert #\a #\b #\c) (neg-look-ahead #\a #\b #\c))
(test-ast (nassert (lookbehind #\a #\b #\c)) (neg-look-behind #\a #\b #\c))

(test-ast #\a #\a)
(test-ast (seq #\a #\b #\c) "abc")

(test* "atomic sre -> once ast"
       '(0 #f (once (seq #\f #\o) (seq #\o)))
       (regexp-parse-sre '(atomic "fo" "o")))

(test* "once ast -> atomic sre"
       '(seq (atomic #\f #\o #\o))
       (regexp-unparse-sre (regexp-ast #/(?>foo)/)))


(test* "if-look-ahead sre -> cpat/assert ast"
       '(0 #f (cpat (assert (seq #\t #\e #\s #\t))
                    ((seq #\t #\h #\e #\n))
                    ((seq #\e #\l #\s #\e))))
       (regexp-parse-sre '(if-look-ahead "test" "then" "else")))

(test* "cpat/assert ast -> if-look-ahead sre"
       '(seq (if-look-ahead (seq #\t #\e #\s #\t)
                            (seq (seq #\t #\h #\e #\n))
                            (seq (seq #\e #\l #\s #\e))))
       (regexp-unparse-sre (regexp-ast #/(?(?=test)then|else)/)))

(test* "if-look-ahead sre -> cpat/assert ast, no else pattern"
       '(0 #f (cpat (assert (seq #\t #\e #\s #\t))
                    ((seq #\t #\h #\e #\n))
                    ()))
       (regexp-parse-sre '(if-look-ahead "test" "then")))

(test* "cpat/assert ast -> if-look-ahead sre, no else pattern"
       '(seq (if-look-ahead (seq #\t #\e #\s #\t)
                            (seq #\t #\h #\e #\n)
                            (seq)))
       (regexp-unparse-sre (regexp-ast #/(?(?=test)then)/)))

(test* "if-neg-look-ahead sre -> cpat/nassert ast"
       '(0 #f (cpat (nassert (seq #\t #\e #\s #\t))
                    ((seq #\t #\h #\e #\n))
                    ((seq #\e #\l #\s #\e))))
       (regexp-parse-sre '(if-neg-look-ahead "test" "then" "else")))

(test* "cpat/nassert ast -> if-neg-look-ahead sre"
       '(seq (if-neg-look-ahead (seq #\t #\e #\s #\t)
                                (seq (seq #\t #\h #\e #\n))
                                (seq (seq #\e #\l #\s #\e))))
       (regexp-unparse-sre (regexp-ast #/(?(?!test)then|else)/)))

(test* "if-neg-look-ahead sre -> cpat/nassert ast, no else pattern"
       '(0 #f (cpat (nassert (seq #\t #\e #\s #\t))
                    ((seq #\t #\h #\e #\n))
                    ()))
       (regexp-parse-sre '(if-neg-look-ahead "test" "then")))

(test* "cpat/nassert ast -> if-neg-look-ahead sre, no else pattern"
       '(seq (if-neg-look-ahead (seq #\t #\e #\s #\t)
                                (seq #\t #\h #\e #\n)
                                (seq)))
       (regexp-unparse-sre (regexp-ast #/(?(?!test)then)/)))

(test* "if-look-behind sre -> cpat/lookbehind ast"
       '(0 #f (cpat (assert (lookbehind (seq #\t #\e #\s #\t)))
                    ((seq #\t #\h #\e #\n))
                    ((seq #\e #\l #\s #\e))))
       (regexp-parse-sre '(if-look-behind "test" "then" "else")))

(test* "cpat/lookbehind ast -> if-look-behind sre"
       '(seq (if-look-behind (seq #\t #\e #\s #\t)
                             (seq (seq #\t #\h #\e #\n))
                             (seq (seq #\e #\l #\s #\e))))
       (regexp-unparse-sre (regexp-ast #/(?(?<=test)then|else)/)))

(test* "if-neg-look-behind sre -> cpat/lookbehind ast"
       '(0 #f (cpat (nassert (lookbehind (seq #\t #\e #\s #\t)))
                    ((seq #\t #\h #\e #\n))
                    ((seq #\e #\l #\s #\e))))
       (regexp-parse-sre '(if-neg-look-behind "test" "then" "else")))

(test* "cpat/lookbehind ast -> if-neg-look-behind sre"
       '(seq (if-neg-look-behind (seq #\t #\e #\s #\t)
                                 (seq (seq #\t #\h #\e #\n))
                                 (seq (seq #\e #\l #\s #\e))))
       (regexp-unparse-sre (regexp-ast #/(?(?<!test)then|else)/)))

(test* "if-backref -> cpat/num ast"
       '(0 #f (cpat 1
                    ((seq #\t #\h #\e #\n))
                    ((seq #\e #\l #\s #\e))))
       (regexp-parse-sre '(if-backref 1 "then" "else")))

(test* "cpat/num ast -> if-backref sre"
       '(seq (if-backref 1
                         (seq (seq #\t #\h #\e #\n))
                         (seq (seq #\e #\l #\s #\e))))
       (regexp-unparse-sre (regexp-ast #/(?(1)then|else)/)))


;;-------------------------------------------------------------------------
(test-section "regexp-unparse-sre")

(define (test-regexp-unparse-sre src)
  (let1 ast (regexp-optimize (regexp-parse-sre src))
    (test* (format "regexp-unparse-sre ~s" src) ast
           (regexp-optimize (regexp-parse-sre (regexp-unparse-sre ast)))
           equal?)))

(for-each test-regexp-unparse-sre
          '(;; simple ones
            "" "a" "ab" (: #\a #\b) (* #\a) (+ "a") (? "a") (: "a" (* "b"))
            (: "a" (+ "b")) (: "a" (? "b")) (: (* "a") "b") (: (+ "a") "b")
            (: (? "a") "b") (= 3 "a") (** 2 4 "a") (>= 3 #\a) (or "a" "b")
            (or "a" "b" "c") (: bol "ab") (: bol (or #\a #\b)) (or (: bol "a") "b")
            (: bol (or "a" "b") eol) any #\. (: (* #\\) #\*) (seq bow "foo" eow)
            ;; charset
            (/ "az") (~ (/ #\a #\z)) (+ (/ "az")) ("a^ef-") ("a") (".")
            ;; grouping
            (: ($ "a") ($ "b")) (: ($ (or "a" "b")) "c" ($ "d"))
            (: ($ "a" ($ "b" ($ (or "c" "d")) "e")) "f")
            (* (w/nocapture (: "abc")))
            (w/nocase "ab" (w/case "cd") "ef")
            ;; backref
            (: ($ "a") "bc" (backref 1))
            (: "ab" (-> foo "c") ($ "d") (backref foo))
            ($ ($ ".") (backref 2))
            ;; lookahead, lookbehind
            (look-ahead (: (* "a") (* #\b) "c"))
            (neg-look-ahead (* #\a) (* #\b) #\c)
            (look-behind #\a ("bc"))
            (neg-look-behind #\a ("bc"))))

;;-------------------------------------------------------------------------
(test-section "SRE")		    ; based on chibi's regexp-test.sld

(define (test-sre expected sre input)
  (test* sre
         expected
         (let ([result (rxmatch (sre->regexp sre) input)])
           (and result
                (map (cut result <>)
                     (iota (rxmatch-num-matches result)))))))

(define (test-sre-named expected sre input)
  (test* sre
         expected
         (let ([result (rxmatch (sre->regexp sre) input)])
           (and result (rxmatch-named-groups result)))))

(test-sre '("ababc" "abab")
          '(: ($ (* "ab")) "c")
          "ababc")

(test-sre '("y") '(: "y") "xy")

(test-sre #f
          '(: (* any) ($ "foo" (* any)) ($ "bar" (* any)))
          "fooxbafba")

(test-sre '("fooxbarfbar" "fooxbarf" "bar")
          '(: (* any) ($ "foo" (* any)) ($ "bar" (* any)))
          "fooxbarfbar")

(test-sre '("abcd" "abcd")
          '($ (* (or "ab" "cd")))
          "abcd")

(test* '(or (-> foo "ab") (-> foo "cd"))
       "ab"
       ((rxmatch
         (sre->regexp
          '(or (-> foo "ab") (-> foo "cd")))
         "ab")
        'foo))

(test* '(or (-> foo "ab") (-> foo "cd"))
       "cd"
       ((rxmatch
         (sre->regexp
          '(or (-> foo "ab") (-> foo "cd")))
         "cd")
        'foo))

(test-sre '("ababc" "abab")
          '(: bos ($ (* "ab")) "c")
          "ababc")
(test-sre '("ababc" "abab")
          '(: ($ (* "ab")) "c" eos)
          "ababc")
(test-sre '("ababc" "abab")
          '(: bos ($ (* "ab")) "c" eos)
          "ababc")
(test-sre #f
          '(: bos ($ (* "ab")) eos "c")
          "ababc")
(test-sre #f
          '(: ($ (* "ab")) bos "c" eos)
          "ababc")

(test-sre '("ababc" "abab")
          '(: bol ($ (* "ab")) "c")
          "ababc")
(test-sre '("ababc" "abab")
          '(: ($ (* "ab")) "c" eol)
          "ababc")
(test-sre '("ababc" "abab")
          '(: bol ($ (* "ab")) "c" eol)
          "ababc")
(test-sre #f
          '(: bol ($ (* "ab")) eol "c")
          "ababc")
(test-sre #f
          '(: ($ (* "ab")) bol "c" eol)
          "ababc")
(test-sre '(".\nabc\n" "abc")
          '(: #\. (* #\newline) bol ($ (* alpha)) eol (* #\newline))
          ".\nabc\n")
;; note that chibi scheme has similar tests, but due to the
;; non-tracking nature (?) of chibi regex engine, the results are both
;; false (without the literal dot) instead of matching here.
(test-sre #f
          '(: #\. (* #\newline) bol ($ (* alpha)) eol (* #\newline))
          ".\n'abc\n")
(test-sre '(".\n" "\n" "")
          '(: #\. ($ (* #\newline)) bol ($ (* alpha)) eol (* #\newline))
          ".\nabc.\n")

(test-sre '("ababc" "abab")
          '(: bow ($ (* "ab")) "c")
          "ababc")
(test-sre '("ababc" "abab")
          '(: ($ (* "ab")) "c" eow)
          "ababc")
(test-sre '("ababc" "abab")
          '(: bow ($ (* "ab")) "c" eow)
          "ababc")
(test-sre #f
          '(: bow ($ (* "ab")) eow "c")
          "ababc")
(test-sre #f
          '(: ($ (* "ab")) bow "c" eow)
          "ababc")
(test-sre '("  abc  " "abc")
          '(: (* space) bow ($ (* alpha)) eow (* space))
          "  abc  ")

(test-sre '("abc  " "abc")
          '(: ($ (* alpha)) (* any))
          "abc  ")
(test-sre '("abc  " "")
          '(: ($ (*? alpha)) (* any))
          "abc  ")
(test-sre '("<em>Hello World</em>" "em>Hello World</em")
          '(: "<" ($ (* any)) ">" (* any))
          "<em>Hello World</em>")
(test-sre '("<em>Hello World</em>" "em")
          '(: "<" ($ (*? any)) ">" (* any))
          "<em>Hello World</em>")

(test-sre '("foo") '(: "foo") " foo ")
(test-sre #f '(: nwb "foo" nwb) " foo ")
(test-sre '("foo") '(: nwb "foo" nwb) "xfoox")

(test-sre '("beef")
          '(* (/"af"))
          "beef")

(test-sre '("12345beef" "beef")
          '(: (* numeric) ($ (* (/"af"))))
          "12345beef")

(let ((number '($ (+ numeric))))
  (test-sre '("555-867-5309" "555" "867" "5309")
            `(: ,number "-" ,number "-" ,number)
            "555-867-5309")

  (test-sre '("555-867-5309" "555" "5309")
            `(: ,number "-" (w/nocapture ,number) "-" ,number)
            "555-867-5309"))

(test-sre '("12345BeeF" "BeeF")
          '(: (* numeric) (w/nocase ($ (* (/ "af")))))
          "12345BeeF")

(cond-expand
 [gauche.ces.utf8 (include "include/regexp-sre-utf8")]
 [else])

(test-end)
