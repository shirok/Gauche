;;
;; testing regexp
;;

;; $Id: regexp.scm,v 1.19 2004-09-12 21:42:40 shirok Exp $

(use gauche.test)
(use srfi-1)

(test-start "regexp")

;; Some test examples are taken from the test suite of Henry Spencer's
;; regexp package.

(define (match&list rx input nmatch)
  (cond ((rxmatch rx input)
         => (lambda (match)
              (map (lambda (n) (rxmatch-substring match n))
                   (iota nmatch))))
        (else #f)))

;;-------------------------------------------------------------------------
(test-section "basics")

(test* "a" '("a")
       (match&list #/a/ "a" 1))
(test* "a" #f
       (match&list #/a/ "A" 1))
(test* "a" '("a")
       (match&list #/a/ "ba" 1))
(test* "a" '("a")
       (match&list #/a/ "bac" 1))
(test* "a" #f      ;; input null str
       (match&list #/a/ "" 1))
(test* "a" '("a")  ;; input includes NUL character
       (match&list #/a/ (string (integer->char 0) #\a #\b) 1))
(test* "abc" '("abc")
       (match&list #/abc/ "abc" 1))
(test* "abc" #f
       (match&list #/abc/ "abbc" 1))
(test* "abc" '("abc")
       (match&list #/abc/ "babcd" 1))
(test* "abc|de" '("abc")
       (match&list #/abc|de/ "dabce" 1))
(test* "abc|de" '("de")
       (match&list #/abc|de/ "abdec" 1))
(test* "abc|de" #f
       (match&list #/abc|de/ "abe" 1))
(test* "a|b|c" '("a")
       (match&list #/a|b|c/ "abc" 1))
(test* "a|b|c" '("b")
       (match&list #/a|b|c/ "bac" 1))
(test* "a|b|c" #f
       (match&list #/a|b|c/ "def" 1))
(test* "|abc" '("")
       (match&list #/|abc/ "abc" 1))
(test* "abc|" '("abc")
       (match&list #/abc|/ "abc" 1))
(test* "abc|" '("")
       (match&list #/abc|/ "abd" 1))

;;-------------------------------------------------------------------------
(test-section "parens")

(test* "a(b)c" '("abc" "b")
       (match&list #/a(b)c/ "abc" 2))
(test* "a((b)(c))" '("abc" "bc" "b" "c")
       (match&list #/a((b)(c))/ "abc" 4))
(test* "a((((b))))c" '("abc" "b" "b" "b" "b")
       (match&list #/a((((b))))c/ "abc" 5))
(test* "a((((b))))c" '#f
       (match&list #/a((((b))))c/ "a(b)c" 5))
(test* "a\\(" '("a(")
       (match&list #/a\(/ "a(" 1))
(test* "a()b" '("ab" "")
       (match&list #/a()b/ "ab" 2))
(test* "a()()b" '("ab" "" "")
       (match&list #/a()()b/ "ab" 3))
(test* "(we|wee|week|frob)(knights|night|day)"
       '("weeknights" "wee" "knights")
       (match&list #/(we|wee|week|frob)(knights|night|day)/
                   "weeknights" 3))
(test* "aa|(bb)|cc" '("aa" #f)
       (match&list #/aa|(bb)|cc/ "aabb" 2))
(test* "aa|(bb)|cc" '("bb" "bb")
       (match&list #/aa|(bb)|cc/ "abbaa" 2))
(test* "aa|(bb)|cc" '("cc" #f)
       (match&list #/aa|(bb)|cc/ "bccaa" 2))
(test* "aa|a(b)|cc" '("ab" "b")
       (match&list #/aa|a(b)|cc/ "abaab" 2))
(test* "aa|a(b)" '("ab" "b")
       (match&list #/aa|a(b)/ "abaab" 2))
(test* "aa|(a(b))|ac" '("ab" "ab" "b")
       (match&list #/aa|(a(b))|cc/ "abaabcc" 3))
(test* "(ab)|ac" '("ab" "ab")
       (match&list #/(ab)|ac/ "aaaabcc" 2))
(test* "(a(b))|ac" '("ab" "ab" "b")
       (match&list #/(a(b))|ac/ "abaabcc" 3))
(test* "ab|(ac)" '("ab" #f)
       (match&list #/ab|(ac)/ "aaaabcc" 2))
(test* "ab|(ac)" '("ac" "ac")
       (match&list #/ab|(ac)/ "aaaacbc" 2))
(test* "aa|(ab|(ac))|ad" '("ac" "ac" "ac")
       (match&list #/aa|(ab|(ac))|ad/ "cac" 3))
(test* "(aa|(a(b)|a(c))|ad)" '("ac" "ac" "ac" #f "c")
       (match&list #/(aa|(a(b)|a(c))|ad)/ "cac" 5))
(test* "b|()|a" '("" "")
       (match&list #/b|()|a/ "cac" 2))

;;-------------------------------------------------------------------------
(test-section "simple meta")

(test* "a.c" '("abc")
       (match&list #/a.c/ "abc" 1))
(test* "a.." '("abc")
       (match&list #/a../ "abc" 1))
(test* "a.." #f
       (match&list #/a../ "ab" 1))
(test* "..." #f
       (match&list #/.../ "ab" 1))
(test* "." '("a")
       (match&list #/./ "abc" 1))
(test* "." #f
       (match&list #/./ "" 1))

;;-------------------------------------------------------------------------
(test-section "anchors")

(test* "^abc" '("abc")
       (match&list #/^abc/ "abcd" 1))
(test* "^abc" #f
       (match&list #/^abc/ "aabcd" 1))
(test* "^^" '("^")
       (match&list #/^^/ "^^abc" 1))
(test* "^^" #f
       (match&list #/^^/ "a^^c" 1))
(test* "^abc|def" '("abc")
       (match&list #/^abc|def/ "abc" 1))
(test* "^abc|def" #f
       (match&list #/^abc|def/ "zabc" 1))
(test* "^abc|def" '("def")
       (match&list #/^abc|def/ "zabcdef" 1))
(test* "abc|^def" '("def")
       (match&list #/abc|^def/ "defabc" 1))
(test* "abc|^def" '("abc")
       (match&list #/abc|^def/ "abcdef" 1))
(test* "abc|^def" '("def")
       (match&list #/abc|^def/ "defabbc" 1))
(test* "abc|^def" #f
       (match&list #/abc|^def/ "adefbc" 1))
(test* "^(abc|def)" '("abc" "abc")
       (match&list #/^(abc|def)/ "abc" 2))
(test* "^(abc|def)" #f
       (match&list #/^(abc|def)/ "aabc" 2))
(test* "(^abc|def)" '("abc" "abc")
       (match&list #/(^abc|def)/ "abcdef" 2))
(test* "(^abc|def)" '("def" "def")
       (match&list #/(^abc|def)/ "^abcdef" 2))
(test* "a(^bc|def)" '("a^bc" "^bc")
       (match&list #/a(^bc|def)/ "a^bcdef" 2))
(test* "a(^bc|def)" #f
       (match&list #/a(^bc|def)/ "abcdef" 2))
(test* "^" '("")
       (match&list #/^/ "hoge" 1))
(test* "$" '("")
       (match&list #/$/ "hoge" 1))
(test* "abc$" '("abc")
       (match&list #/abc$/ "bcabc" 1))
(test* "abc$" #f
       (match&list #/abc$/ "abcab" 1))
(test* "^abc$" '("abc")
       (match&list #/^abc$/ "abc" 1))
(test* "abc$$" #f
       (match&list #/abc$$/ "abc" 1))
(test* "abc$$" '("abc$")
       (match&list #/abc$$/ "abc$" 1))
(test* "$$" '("$")
       (match&list #/$$/ "abc$" 1))
(test* "^$" '("")
       (match&list #/^$/ "" 1))
(test* "^$" #f
       (match&list #/^$/ "a" 1))
(test* "^^$$" '("^$")
       (match&list #/^^$$/ "^$" 1))
(test* "abc$|def" '("abc")
       (match&list #/abc$|def/ "abc" 1))
(test* "abc$|def" '("def")
       (match&list #/abc$|def/ "defabc" 1))
(test* "^abc|def$" '("abc")
       (match&list #/^abc|def$/ "abcdef" 1))
(test* "^abc|def$" #f
       (match&list #/^abc|def$/ "defabc" 1))
(test* "^abc|def$" #f
       (match&list #/^abc|def$/ "defabc" 1))
(test* "(^abc|def$)" '("def" "def")
       (match&list #/(^abc|def$)/ "aaadef" 2))
(test* "(^abc|def$)$" #f
       (match&list #/(^abc|def$)$/ "aaadef" 1))
(test* "(^abc|def$)$" '("def$")
       (match&list #/(^abc|def$)$/ "aaadef$" 1))
(test* "(abc$|def)$" #f
       (match&list #/(abc$|def)$/ "aaabc" 1))
(test* "(abc$|def)$" '("abc$")
       (match&list #/(abc$|def)$/ "aaabc$" 1))
(test* "a$b" '("a$b")
       (match&list #/a$b/ "aa$bb" 1))
(test* "ab\\$" '("ab$")
       (match&list #/ab\$/ "ab$cd" 1))

;;-------------------------------------------------------------------------
(test-section "backslash escape")

(test* "a\\*c" '("a*c")
       (match&list #/a\*c/ "a*c" 1))
(test* "a\\.c" '("a.c")
       (match&list #/a\.c/ "a.c" 1))
(test* "a\\.c" #f
       (match&list #/a\.c/ "abc" 1))
(test* "a\\\\b" '("a\\b")
       (match&list #/a\\b/ "a\\b" 1))
(test* "a\\\\\\*b" '("a\\*b")
       (match&list #/a\\\*b/ "a\\*b" 1))
(test* "a\\jc" '("ajc")
       (match&list #/a\jc/ "ajc" 1))
(test* "a\\\\bc" '("a\\bc")
       (match&list #/a\\bc/ "a\\bc" 1))
(test* "a\\[b" '("a[b")
       (match&list #/a\[b/ "a[b" 1))

;;-------------------------------------------------------------------------
(test-section "word boundary")

(test* ".z\\b" '("oz")
       (match&list #/.z\b/ "bzbazoz ize" 1))
(test* "\\b.z" '("iz")
       (match&list #/\b.z/ "brzbazoz ize" 1))
(test* ".z\\B" '("iz")
       (match&list #/.z\B/ "bz baz oz ize" 1))
(test* "\\B.z" '("az")
       (match&list #/\B.z/ "bz baz oz ize" 1))

;;-------------------------------------------------------------------------
(test-section "repetitions")

(test* "ab*c" '("abc")
       (match&list #/ab*c/ "abc" 1))
(test* "ab*c" '("ac")
       (match&list #/ab*c/ "ac" 1))
(test* "ab*c" '("abbbc")
       (match&list #/ab*c/ "abbbc" 1))
(test* "ab*c" '("abbc")
       (match&list #/ab*c/ "abbabaabbc" 1))
(test* "ab+c" '("abc")
       (match&list #/ab+c/ "abc" 1))
(test* "ab+c" '("abbc")
       (match&list #/ab+c/ "abbc" 1))
(test* "ab+c" '("abbc")
       (match&list #/ab+c/ "abbabaabbc" 1))
(test* "ab?c" '("abc")
       (match&list #/ab?c/ "abc" 1))
(test* "ab?c" '("ac")
       (match&list #/ab?c/ "abbaac" 1))
(test* "a.*c" '("abc")
       (match&list #/a.*c/ "abc" 1))
(test* "a.*c" '("aabcabcabcabcc")
       (match&list #/a.*c/ "zaabcabcabcabcczab" 1))
(test* "a(b*|c)d" '("abbd" "bb")
       (match&list #/a(b*|c)d/ "abbd" 2))
(test* "a(b*|c)d" '("ad" "")
       (match&list #/a(b*|c)d/ "ad" 2))
(test* "a(b*|c)d" '("acd" "c")
       (match&list #/a(b*|c)d/ "acd" 2))
(test* "a(b*|c)d" #f
       (match&list #/a(b*|c)d/ "abcd" 2))
(test* "a.*c" '("ac")
       (match&list #/a.*c/ "bacbababbbbadbaba" 1))
(test* "a.*c" #f
       (match&list #/a.*c/ "abaaaabababbadbabdba" 1))

;;-------------------------------------------------------------------------
(test-section "repetitions (non-greedy)")

(test* "ab*?." '("ab")
       (match&list #/ab*?./ "abc" 1))
(test* "ab*?." '("ac")
       (match&list #/ab*?./ "ac" 1))
(test* "a.*?c" '("abbbc")
       (match&list #/a.*?c/ "abbbc" 1))
(test* "a.*?a" '("abba")
       (match&list #/a.*?a/ "abbabaabbc" 1))
(test* "<.*?>" '("<tag1>")
       (match&list #/<.*?>/ "<tag1><tag2><tag3>" 1))

(test* "ab+?." '("abc")
       (match&list #/ab+?./ "abc" 1))
(test* "ab+?." '("abb")
       (match&list #/ab+?./ "abbc" 1))
(test* "a.+?a" '("abba")
       (match&list #/a.+?a/ "abbabaabbc" 1))
(test* "<.+?>" '("<><tag1>")
       (match&list #/<.+?>/ " <><tag1><tag2>" 1))

(test* "ab??c" '("abc")
       (match&list #/ab??c/ "abc" 1))
(test* "ab??c" '("ac")
       (match&list #/ab??c/ "abbaac" 1))
(test* "ab??." '("ab")
       (match&list #/ab??./ "abbaac" 1))
(test* "a(hoge)??hoge" '("ahoge")
       (match&list #/a(hoge)??hoge/ "ahogehoge" 1))

;;-------------------------------------------------------------------------
(test-section "character class")

(test* "a[bc]d" '("abd")
       (match&list #/a[bc]d/ "abd" 1))
(test* "a[bc]d" '("acd")
       (match&list #/a[bc]d/ "acd" 1))
(test* "a[bc]d" #f
       (match&list #/a[bc]d/ "aed" 1))
(test* "a[a-z]d" '("aed")
       (match&list #/a[a-z]d/ "aed" 1))
(test* "a[a-z]d" #f
       (match&list #/a[a-z]d/ "aEd" 1))
(test* "a[]]d" '("a]d")
       (match&list #/a[]]d/ "a]d" 1))
(test* "a[]-]d" '("a-d")
       (match&list #/a[]-]d/ "a-d" 1))
(test* "a[]-^]d" #f
       (match&list #/a[]-^]d/ "a-d" 1))
(test* "a[]-^]d" '("a]d")
       (match&list #/a[]-^]d/ "a]d" 1))
(test* "a[a-z-]d" '("a-d")
       (match&list #/a[a-z-]d/ "a-d" 1))
(test* "a[a-z-]d" '("afd")
       (match&list #/a[a-z-]d/ "afd" 1))
(test* "a[az-]d" '("a-d")
       (match&list #/a[az-]d/ "a-d" 1))
(test* "a[a-]d" '("a-d")
       (match&list #/a[a-]d/ "a-d" 1))
(test* "a[az-]d" #f
       (match&list #/a[az-]d/ "afd" 1))
(test* "a[az-]d" '("azd")
       (match&list #/a[az-]d/ "azd" 1))
(test* "a[^ab]c" '("acc")
       (match&list #/a[^ab]c/ "abacc" 1))
(test* "a[^]]c" '("abc")
       (match&list #/a[^]]c/ "abc" 1))
(test* "a[^]]c" #f
       (match&list #/a[^]]c/ "a]c" 1))
(test* "a[^^]c" '("abc")
       (match&list #/a[^^]c/ "abc" 1))
(test* "a[^^]c" #f
       (match&list #/a[^^]c/ "a^c" 1))
(test* "a[Bc]*d" '("aBccBd")
       (match&list #/a[Bc]*d/ "aBccBd" 1))
(test* "[a]b[c]" '("abc")
       (match&list #/[a]b[c]/ "abc" 1))
(test* "[abc]b[abc]" '("abc")
       (match&list #/[abc]b[abc]/ "abc" 1))
(test* "a[bc]d" '("abd")
       (match&list #/a[bc]d/ "xyzaaabcaababdacd" 1))
(test* "a[ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab](wee|week)(knights|night)"
       '("aaaaabaaaabaaaabaaaabweeknights" "wee" "knights")
       (match&list #/a[ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab](wee|week)(knights|night)/
                   "aaaaabaaaabaaaabaaaabweeknights" 3))
(test* "[ab][cd][ef][gh][ij][kl][mn]"
       '("acegikm")
       (match&list #/[ab][cd][ef][gh][ij][kl][mn]/
                   "xacegikmoq" 1))
(test* "[ab][cd][ef][gh][ij][kl][mn][op]"
       '("acegikmo")
       (match&list #/[ab][cd][ef][gh][ij][kl][mn][op]/
                   "xacegikmoq" 1))
(test* "[ab][cd][ef][gh][ij][kl][mn][op][qr]"
       '("acegikmoq")
       (match&list #/[ab][cd][ef][gh][ij][kl][mn][op][qr]/
                   "xacegikmoqy" 1))
(test* "[ab][cd][ef][gh][ij][kl][mn][op][q]"
       '("acegikmoq")
       (match&list #/[ab][cd][ef][gh][ij][kl][mn][op][q]/
                   "xacegikmoqy" 1))

;; this tests optimizer
(test* "[^a]*." '("b")
       (match&list #/[^a]*./ "b" 1))

(test* "\\s" '(" ")  (match&list #/\s/ "  " 1))
(test* "\\s" '("  ")  (match&list #/\s\s/ "  " 1))
(test* "\\s" '("\t")  (match&list #/\s/ "\t " 1))
(test* "\\s" #f  (match&list #/\s\s/ "\\s" 1))
(test* "\\\\s" '("\\s ")  (match&list #/\\s\s/ "\\s " 1))
(test* "\\s\\S" '("\txyz   " "\t" "xyz" "   ")
       (match&list #/(\s*)(\S+)(\s*)/ "\txyz   abc" 4))
(test* "\\d\\D" '("1234) 5678-9012" "1234" ") " "5678" "-" "9012")
       (match&list #/(\d+)(\D+)(\d+)(\D+)(\d+)/
                   " (1234) 5678-9012 " 6))
(test* "\\w\\W" '("three o" "three" " " "o")
       (match&list #/(\w+)(\W+)(\w+)/
                   "three o'clock" 4))

(test* "[ab\\sc]+" '(" ba ") (match&list #/[ab\sc]+/ "d ba e" 1))
(test* "[\\sa-c]+" '(" ba ") (match&list #/[\sabc]+/ "d ba e" 1))
(test* "[\\S\\t]+" '("\tab") (match&list #/[\S\t]+/ "\tab cd" 1))
(test* "[\\s\\d]+" '(" 1 2 3 ")
       (match&list #/[\s\d]+/ "a 1 2 3 b " 1))
(test* "[\\s\\D]+" '("a ")
       (match&list #/[\s\D]+/ "a 1 2 3 b " 1))

;; this tests optimizer
(test* "^\\[?([^\\]]*)\\]?:(\\d+)$" '("127.0.0.1:80" "127.0.0.1" "80")
       (match&list #/^\[?([^\]]*)\]?:(\d+)$/ "127.0.0.1:80" 3))
(test* "^\\[?([^\\]]*)\\]?:(\\d+)$" '("[127.0.0.1:80" "127.0.0.1" "80")
       (match&list #/^\[?([^\]]*)\]?:(\d+)$/ "[127.0.0.1:80" 3))
(test* "^\\[?([^\\]]*)\\]?:(\\d+)$" '("[127.0.0.1]:80" "127.0.0.1" "80")
       (match&list #/^\[?([^\]]*)\]?:(\d+)$/ "[127.0.0.1]:80" 3))

;;-------------------------------------------------------------------------
(test-section "{n,m}")

(test* "a{2}"   "aabaa"    (rxmatch-after (#/a{2}/ "abaaaabaa")))
(test* "a{2,}"  "baa"      (rxmatch-after (#/a{2,}/ "abaaaabaa")))
(test* "a{1,2}" "baaaabaa" (rxmatch-after (#/a{1,2}/ "abaaaabaa")))
(test* "a{2,3}" "abaa"     (rxmatch-after (#/a{2,3}/ "abaaaabaa")))
(test* "a{b,c}" "def"      (rxmatch-after (#/a{b,c}/ "za{b,c}def")))
(test* "a{,2}*" "def"       (rxmatch-after (#/a{,2}*/  "za{,2}}def")))

(test* "(ab){2}"   "abba"      (rxmatch-after (#/(ab){2}/ "babbabababba")))
(test* "(ab){2,2}" "abba"      (rxmatch-after (#/(ab){2,2}/ "babbabababba")))
(test* "(ab){2,}"  "ba"        (rxmatch-after (#/(ab){2,}/ "babbabababba")))
(test* "(ab){1,2}" "babababba" (rxmatch-after (#/(ab){1,2}/ "babbabababba")))
(test* "(ab){2,3}" "ba"        (rxmatch-after (#/(ab){2,3}/ "babbabababba")))
(test* "(ab){b,c}" "def"       (rxmatch-after (#/(ab){b,c}/ "zab{b,c}def")))

(test* "(\\d{2})(\\d{2})" '("1234" "12" "34")
       (match&list #/(\d{2})(\d{2})/ "a12345b" 3))
(test* "(\\d{2,})(\\d{2,})" '("12345" "123" "45")
       (match&list #/(\d{2,})(\d{2,})/ "a12345b" 3))
(test* "(\\d{2})(\\d{2,})" '("12345" "12" "345")
       (match&list #/(\d{2})(\d{2,})/ "a12345b" 3))
(test* "(\\d{1,3})(\\d{2,})" '("1234" "12" "34")
       (match&list #/(\d{1,3})(\d{2,})/ "a1234b" 3))
(test* "(\\d{1,3})(\\d{0,2})" '("1234" "123" "4")
       (match&list #/(\d{1,3})(\d{0,2})/ "a1234b" 3))
(test* "(\\d{2}){2}" '("1234")
       (match&list #/(\d{2}){2}/ "a12345b" 1))

(test* "{2}" *test-error*  (regexp? (string->regexp "{2}")))
(test* "{z}" #t            (regexp? (string->regexp "{z}")))
(test* "{-1}" #t           (regexp? (string->regexp "{-1}")))
(test* "{300}" *test-error* (regexp? (string->regexp "{300}")))
(test* "{3,1}" *test-error* (regexp? (string->regexp "{3,1}")))

;;-------------------------------------------------------------------------
(test-section "{n,m} (non-greedy)")

(test* "a{2}?"   "aabaa"    (rxmatch-after (#/a{2}?/ "abaaaabaa")))
(test* "a{2,}?"  "aabaa"    (rxmatch-after (#/a{2,}?/ "abaaaabaa")))
(test* "a{1,2}?" "baaaabaa" (rxmatch-after (#/a{1,2}?/ "abaaaabaa")))
(test* "a{2,3}?" "aabaa"    (rxmatch-after (#/a{2,3}?/ "abaaaabaa")))

(test* "(ab){2}?"   "abba"      (rxmatch-after (#/(ab){2}?/ "babbabababba")))
(test* "(ab){2,2}?" "abba"      (rxmatch-after (#/(ab){2,2}?/ "babbabababba")))
(test* "(ab){2,}?"  "abba"      (rxmatch-after (#/(ab){2,}?/ "babbabababba")))
(test* "(ab){1,2}?" "babababba" (rxmatch-after (#/(ab){1,2}?/ "babbabababba")))
(test* "(ab){2,3}?" "abba"      (rxmatch-after (#/(ab){2,3}?/ "babbabababba")))

(test* "(\\d{2,}?)(\\d{2,}?)" '("1234" "12" "34")
       (match&list #/(\d{2,}?)(\d{2,}?)/ "a12345b" 3))
(test* "(\\d{2,})(\\d{2,}?)" '("12345" "123" "45")
       (match&list #/(\d{2,})(\d{2,}?)/ "a12345b" 3))
(test* "(\\d{2,}?)(\\d{2,})" '("12345" "12" "345")
       (match&list #/(\d{2,}?)(\d{2,})/ "a12345b" 3))
(test* "(\\d{1,3}?)(\\d{2,}?)" '("123" "1" "23")
       (match&list #/(\d{1,3}?)(\d{2,}?)/ "a1234b" 3))
(test* "(\\d{1,3}?)(\\d{0,2}?)" '("1" "1" "")
       (match&list #/(\d{1,3}?)(\d{0,2}?)/ "a1234b" 3))

;;-------------------------------------------------------------------------
(test-section "uncapturing group")

(test* "a(?:b)*c(d)"  "d"
       (rxmatch-substring (#/a(?:b)*c(d)/ "abcdbcdefg") 1))
(test* "a(?:bcd)*e(f)"  "f"
       (rxmatch-substring (#/a(?:bcd)*e(f)/ "abcdbcdefg") 1))
(test* "a(?:bcd)*e(f)"  "f"
       (rxmatch-substring (#/a(?:bcd)*e(f)/ "aefg") 1))
(test* "a(?:bcd)+e(f)"  #f
       (rxmatch-substring (#/a(?:bcd)+e(f)/ "aefg") 1))
(test* "a(?:bc(de(?:fg)?hi)jk)?l" "defghi"
       (rxmatch-substring (#/a(?:bc(de(?:fg)?hi)jk)?l/ "abcdefghijkl") 1))
(test* "a(?:bc(de(?:fg)?hi)jk)?l" "dehi"
       (rxmatch-substring (#/a(?:bc(de(?:fg)?hi)jk)?l/ "abcdehijkl") 1))

(test* "a(?i:bc)d" "aBCd"
       (rxmatch-substring (#/a(?i:bc)d/ "!aBCd!")))
(test* "a(?i:bc)d" #f
       (rxmatch-substring (#/a(?i:bc)d/ "!aBCD!")))
(test* "a(?i:[a-z]+)d" "aBcd"
       (rxmatch-substring (#/a(?i:[a-z]+)d/ "!aBcd!")))
(test* "a(?i:[a-z]+)d" #f
       (rxmatch-substring (#/a(?i:[a-z]+)d/ "!ABcd!")))

(test* "A(?-i:Bc)D" "ABcD"
       (rxmatch-substring (#/A(?-i:Bc)D/ "!ABcD!")))
(test* "A(?-i:Bc)D" #f
       (rxmatch-substring (#/A(?-i:Bc)D/ "!ABcd!")))
(test* "A(?-i:[A-Z]+)D" "ABCD"
       (rxmatch-substring (#/A(?-i:[A-Z]+)D/ "!ABCD!")))
(test* "A(?-i:[A-Z]+)D" #f
       (rxmatch-substring (#/A(?-i:[A-Z]+)D/ "!abCD!")))

(test* "A(?-i:Bc)D/i" "aBcd"
       (rxmatch-substring (#/A(?-i:Bc)D/i "!aBcd!")))
(test* "A(?-i:Bc)D/i" #f
       (rxmatch-substring (#/A(?-i:Bc)D/i "!abCd!")))
(test* "A(?-i:[A-Z]+)D/i" "aBCd"
       (rxmatch-substring (#/A(?-i:[A-Z]+)D/i "!aBCd!")))
(test* "A(?-i:[A-Z]+)D/i" #f
       (rxmatch-substring (#/A(?-i:[A-Z]+)D/i "!abcd!")))

;; these test optimizer
(test* "^(?i:a*).$" "b"
       (rxmatch-substring (#/^(?i:a*).$/ "b")))
(test* "^(?i:a*).$" "a"
       (rxmatch-substring (#/^(?i:a*).$/ "a")))
(test* "^(?i:a*).$" "A"
       (rxmatch-substring (#/^(?i:a*).$/ "A")))
(test* "^(?i:a*).$" "Ab"
       (rxmatch-substring (#/^(?i:a*).$/ "Ab")))

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
    (test (lambda (s) (not (string? s))) #f)
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

(test* "regexp-replace" "abc|def|ghi"
       (regexp-replace #/def|DEF/ "abcdefghi" "|\\0|"))

(test* "regexp-replace" "abc|\\0|ghi"
       (regexp-replace #/def|DEF/ "abcdefghi" "|\\\\0|"))

(test* "regexp-replace" "abraabra**brabra**brabrabracadabrabrabra"
       (regexp-replace #/a((bra)+)cadabra/
                       "abraabraabrabracadabrabrabrabracadabrabrabra"
                       "**\\1**"))

(test* "regexp-replace-all" "abraabra**brabra**br**brabra**brabra"
       (regexp-replace-all #/a((bra)+)cadabra/
                           "abraabraabrabracadabrabrabrabracadabrabrabra"
                           "**\\1**"))

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

(test* "regexp-replace-all" *test-error*
       (regexp-replace-all #/\d*/ "abcdef" "X"))
(test* "regexp-replace-all" *test-error*
       (regexp-replace-all #/\d*/ "123abcdef" "X"))

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

(test-end)
