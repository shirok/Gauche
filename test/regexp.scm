;;
;; testing regexp
;;

;; $Id: regexp.scm,v 1.11 2002-09-21 03:00:13 shirok Exp $

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

(test "a" '("a")
      (lambda () (match&list #/a/ "a" 1)))
(test "a" #f
      (lambda () (match&list #/a/ "A" 1)))
(test "a" '("a")
      (lambda () (match&list #/a/ "ba" 1)))
(test "a" '("a")
      (lambda () (match&list #/a/ "bac" 1)))
(test "a" #f      ;; input null str
      (lambda () (match&list #/a/ "" 1)))
(test "a" '("a")  ;; input includes NUL character
      (lambda () (match&list #/a/ (string (integer->char 0) #\a #\b) 1)))
(test "abc" '("abc")
      (lambda () (match&list #/abc/ "abc" 1)))
(test "abc" #f
      (lambda () (match&list #/abc/ "abbc" 1)))
(test "abc" '("abc")
      (lambda () (match&list #/abc/ "babcd" 1)))
(test "abc|de" '("abc")
      (lambda () (match&list #/abc|de/ "dabce" 1)))
(test "abc|de" '("de")
      (lambda () (match&list #/abc|de/ "abdec" 1)))
(test "abc|de" #f
      (lambda () (match&list #/abc|de/ "abe" 1)))
(test "a|b|c" '("a")
      (lambda () (match&list #/a|b|c/ "abc" 1)))
(test "a|b|c" '("b")
      (lambda () (match&list #/a|b|c/ "bac" 1)))
(test "a|b|c" #f
      (lambda () (match&list #/a|b|c/ "def" 1)))
(test "|abc" '("")
      (lambda () (match&list #/|abc/ "abc" 1)))
(test "abc|" '("abc")
      (lambda () (match&list #/abc|/ "abc" 1)))
(test "abc|" '("")
      (lambda () (match&list #/abc|/ "abd" 1)))

;;-------------------------------------------------------------------------
(test-section "parens")

(test "a(b)c" '("abc" "b")
      (lambda () (match&list #/a(b)c/ "abc" 2)))
(test "a((b)(c))" '("abc" "bc" "b" "c")
      (lambda () (match&list #/a((b)(c))/ "abc" 4)))
(test "a((((b))))c" '("abc" "b" "b" "b" "b")
      (lambda () (match&list #/a((((b))))c/ "abc" 5)))
(test "a((((b))))c" '#f
      (lambda () (match&list #/a((((b))))c/ "a(b)c" 5)))
(test "a\\(" '("a(")
      (lambda () (match&list #/a\(/ "a(" 1)))
(test "a()b" '("ab" "")
      (lambda () (match&list #/a()b/ "ab" 2)))
(test "a()()b" '("ab" "" "")
      (lambda () (match&list #/a()()b/ "ab" 3)))
(test "(we|wee|week|frob)(knights|night|day)"
      '("weeknights" "wee" "knights")
      (lambda () (match&list #/(we|wee|week|frob)(knights|night|day)/
                             "weeknights" 3)))
(test "aa|(bb)|cc" '("aa" #f)
      (lambda () (match&list #/aa|(bb)|cc/ "aabb" 2)))
(test "aa|(bb)|cc" '("bb" "bb")
      (lambda () (match&list #/aa|(bb)|cc/ "abbaa" 2)))
(test "aa|(bb)|cc" '("cc" #f)
      (lambda () (match&list #/aa|(bb)|cc/ "bccaa" 2)))
(test "aa|a(b)|cc" '("ab" "b")
      (lambda () (match&list #/aa|a(b)|cc/ "abaab" 2)))
(test "aa|a(b)" '("ab" "b")
      (lambda () (match&list #/aa|a(b)/ "abaab" 2)))
(test "aa|(a(b))|ac" '("ab" "ab" "b")
      (lambda () (match&list #/aa|(a(b))|cc/ "abaabcc" 3)))
(test "(ab)|ac" '("ab" "ab")
      (lambda () (match&list #/(ab)|ac/ "aaaabcc" 2)))
(test "(a(b))|ac" '("ab" "ab" "b")
      (lambda () (match&list #/(a(b))|ac/ "abaabcc" 3)))
(test "ab|(ac)" '("ab" #f)
      (lambda () (match&list #/ab|(ac)/ "aaaabcc" 2)))
(test "ab|(ac)" '("ac" "ac")
      (lambda () (match&list #/ab|(ac)/ "aaaacbc" 2)))
(test "aa|(ab|(ac))|ad" '("ac" "ac" "ac")
      (lambda () (match&list #/aa|(ab|(ac))|ad/ "cac" 3)))
(test "(aa|(a(b)|a(c))|ad)" '("ac" "ac" "ac" #f "c")
      (lambda () (match&list #/(aa|(a(b)|a(c))|ad)/ "cac" 5)))
(test "b|()|a" '("" "")
      (lambda () (match&list #/b|()|a/ "cac" 2)))

;;-------------------------------------------------------------------------
(test-section "simple meta")

(test "a.c" '("abc")
      (lambda () (match&list #/a.c/ "abc" 1)))
(test "a.." '("abc")
      (lambda () (match&list #/a../ "abc" 1)))
(test "a.." #f
      (lambda () (match&list #/a../ "ab" 1)))
(test "..." #f
      (lambda () (match&list #/.../ "ab" 1)))
(test "." '("a")
      (lambda () (match&list #/./ "abc" 1)))
(test "." #f
      (lambda () (match&list #/./ "" 1)))

;;-------------------------------------------------------------------------
(test-section "anchors")

(test "^abc" '("abc")
      (lambda () (match&list #/^abc/ "abcd" 1)))
(test "^abc" #f
      (lambda () (match&list #/^abc/ "aabcd" 1)))
(test "^^" '("^")
      (lambda () (match&list #/^^/ "^^abc" 1)))
(test "^^" #f
      (lambda () (match&list #/^^/ "a^^c" 1)))
(test "^abc|def" '("abc")
      (lambda () (match&list #/^abc|def/ "abc" 1)))
(test "^abc|def" #f
      (lambda () (match&list #/^abc|def/ "zabc" 1)))
(test "^abc|def" '("def")
      (lambda () (match&list #/^abc|def/ "zabcdef" 1)))
(test "abc|^def" '("def")
      (lambda () (match&list #/abc|^def/ "defabc" 1)))
(test "abc|^def" '("abc")
      (lambda () (match&list #/abc|^def/ "abcdef" 1)))
(test "abc|^def" '("def")
      (lambda () (match&list #/abc|^def/ "defabbc" 1)))
(test "abc|^def" #f
      (lambda () (match&list #/abc|^def/ "adefbc" 1)))
(test "^(abc|def)" '("abc" "abc")
      (lambda () (match&list #/^(abc|def)/ "abc" 2)))
(test "^(abc|def)" #f
      (lambda () (match&list #/^(abc|def)/ "aabc" 2)))
(test "(^abc|def)" '("abc" "abc")
      (lambda () (match&list #/(^abc|def)/ "abcdef" 2)))
(test "(^abc|def)" '("def" "def")
      (lambda () (match&list #/(^abc|def)/ "^abcdef" 2)))
(test "a(^bc|def)" '("a^bc" "^bc")
      (lambda () (match&list #/a(^bc|def)/ "a^bcdef" 2)))
(test "a(^bc|def)" #f
      (lambda () (match&list #/a(^bc|def)/ "abcdef" 2)))
(test "^" '("")
      (lambda () (match&list #/^/ "hoge" 1)))
(test "$" '("")
      (lambda () (match&list #/$/ "hoge" 1)))
(test "abc$" '("abc")
      (lambda () (match&list #/abc$/ "bcabc" 1)))
(test "abc$" #f
      (lambda () (match&list #/abc$/ "abcab" 1)))
(test "^abc$" '("abc")
      (lambda () (match&list #/^abc$/ "abc" 1)))
(test "abc$$" #f
      (lambda () (match&list #/abc$$/ "abc" 1)))
(test "abc$$" '("abc$")
      (lambda () (match&list #/abc$$/ "abc$" 1)))
(test "$$" '("$")
      (lambda () (match&list #/$$/ "abc$" 1)))
(test "^$" '("")
      (lambda () (match&list #/^$/ "" 1)))
(test "^$" #f
      (lambda () (match&list #/^$/ "a" 1)))
(test "^^$$" '("^$")
      (lambda () (match&list #/^^$$/ "^$" 1)))
(test "abc$|def" '("abc")
      (lambda () (match&list #/abc$|def/ "abc" 1)))
(test "abc$|def" '("def")
      (lambda () (match&list #/abc$|def/ "defabc" 1)))
(test "^abc|def$" '("abc")
      (lambda () (match&list #/^abc|def$/ "abcdef" 1)))
(test "^abc|def$" #f
      (lambda () (match&list #/^abc|def$/ "defabc" 1)))
(test "^abc|def$" #f
      (lambda () (match&list #/^abc|def$/ "defabc" 1)))
(test "(^abc|def$)" '("def" "def")
      (lambda () (match&list #/(^abc|def$)/ "aaadef" 2)))
(test "(^abc|def$)$" #f
      (lambda () (match&list #/(^abc|def$)$/ "aaadef" 1)))
(test "(^abc|def$)$" '("def$")
      (lambda () (match&list #/(^abc|def$)$/ "aaadef$" 1)))
(test "(abc$|def)$" #f
      (lambda () (match&list #/(abc$|def)$/ "aaabc" 1)))
(test "(abc$|def)$" '("abc$")
      (lambda () (match&list #/(abc$|def)$/ "aaabc$" 1)))

;;-------------------------------------------------------------------------
(test-section "backslash escape")

(test "a\\*c" '("a*c")
      (lambda () (match&list #/a\*c/ "a*c" 1)))
(test "a\\.c" '("a.c")
      (lambda () (match&list #/a\.c/ "a.c" 1)))
(test "a\\.c" #f
      (lambda () (match&list #/a\.c/ "abc" 1)))
(test "a\\\\b" '("a\\b")
      (lambda () (match&list #/a\\b/ "a\\b" 1)))
(test "a\\\\\\*b" '("a\\*b")
      (lambda () (match&list #/a\\\*b/ "a\\*b" 1)))
(test "a\\bc" '("abc")
      (lambda () (match&list #/a\bc/ "abc" 1)))
(test "a\\\\bc" '("a\\bc")
      (lambda () (match&list #/a\\bc/ "a\\bc" 1)))
(test "a\\[b" '("a[b")
      (lambda () (match&list #/a\[b/ "a[b" 1)))

;;-------------------------------------------------------------------------
(test-section "repetitions")

(test "ab*c" '("abc")
      (lambda () (match&list #/ab*c/ "abc" 1)))
(test "ab*c" '("ac")
      (lambda () (match&list #/ab*c/ "ac" 1)))
(test "ab*c" '("abbbc")
      (lambda () (match&list #/ab*c/ "abbbc" 1)))
(test "ab*c" '("abbc")
      (lambda () (match&list #/ab*c/ "abbabaabbc" 1)))
(test "ab+c" '("abc")
      (lambda () (match&list #/ab+c/ "abc" 1)))
(test "ab+c" '("abbc")
      (lambda () (match&list #/ab+c/ "abbc" 1)))
(test "ab+c" '("abbc")
      (lambda () (match&list #/ab+c/ "abbabaabbc" 1)))
(test "ab?c" '("abc")
      (lambda () (match&list #/ab?c/ "abc" 1)))
(test "ab?c" '("ac")
      (lambda () (match&list #/ab?c/ "abbaac" 1)))
(test "a.*c" '("abc")
      (lambda () (match&list #/a.*c/ "abc" 1)))
(test "a.*c" '("aabcabcabcabcc")
      (lambda () (match&list #/a.*c/ "zaabcabcabcabcczab" 1)))
(test "a(b*|c)d" '("abbd" "bb")
      (lambda () (match&list #/a(b*|c)d/ "abbd" 2)))
(test "a(b*|c)d" '("ad" "")
      (lambda () (match&list #/a(b*|c)d/ "ad" 2)))
(test "a(b*|c)d" '("acd" "c")
      (lambda () (match&list #/a(b*|c)d/ "acd" 2)))
(test "a(b*|c)d" #f
      (lambda () (match&list #/a(b*|c)d/ "abcd" 2)))
(test "a.*c" '("ac")
      (lambda () (match&list #/a.*c/ "bacbababbbbadbaba" 1)))
(test "a.*c" #f
      (lambda () (match&list #/a.*c/ "abaaaabababbadbabdba" 1)))

;;-------------------------------------------------------------------------
(test-section "character class")

(test "a[bc]d" '("abd")
      (lambda () (match&list #/a[bc]d/ "abd" 1)))
(test "a[bc]d" '("acd")
      (lambda () (match&list #/a[bc]d/ "acd" 1)))
(test "a[bc]d" #f
      (lambda () (match&list #/a[bc]d/ "aed" 1)))
(test "a[a-z]d" '("aed")
      (lambda () (match&list #/a[a-z]d/ "aed" 1)))
(test "a[a-z]d" #f
      (lambda () (match&list #/a[a-z]d/ "aEd" 1)))
(test "a[]]d" '("a]d")
      (lambda () (match&list #/a[]]d/ "a]d" 1)))
(test "a[]-]d" '("a-d")
      (lambda () (match&list #/a[]-]d/ "a-d" 1)))
(test "a[]-^]d" #f
      (lambda () (match&list #/a[]-^]d/ "a-d" 1)))
(test "a[]-^]d" '("a]d")
      (lambda () (match&list #/a[]-^]d/ "a]d" 1)))
(test "a[a-z-]d" '("a-d")
      (lambda () (match&list #/a[a-z-]d/ "a-d" 1)))
(test "a[a-z-]d" '("afd")
      (lambda () (match&list #/a[a-z-]d/ "afd" 1)))
(test "a[az-]d" '("a-d")
      (lambda () (match&list #/a[az-]d/ "a-d" 1)))
(test "a[a-]d" '("a-d")
      (lambda () (match&list #/a[a-]d/ "a-d" 1)))
(test "a[az-]d" #f
      (lambda () (match&list #/a[az-]d/ "afd" 1)))
(test "a[az-]d" '("azd")
      (lambda () (match&list #/a[az-]d/ "azd" 1)))
(test "a[^ab]c" '("acc")
      (lambda () (match&list #/a[^ab]c/ "abacc" 1)))
(test "a[^]]c" '("abc")
      (lambda () (match&list #/a[^]]c/ "abc" 1)))
(test "a[^]]c" #f
      (lambda () (match&list #/a[^]]c/ "a]c" 1)))
(test "a[^^]c" '("abc")
      (lambda () (match&list #/a[^^]c/ "abc" 1)))
(test "a[^^]c" #f
      (lambda () (match&list #/a[^^]c/ "a^c" 1)))
(test "a[Bc]*d" '("aBccBd")
      (lambda () (match&list #/a[Bc]*d/ "aBccBd" 1)))
(test "[a]b[c]" '("abc")
      (lambda () (match&list #/[a]b[c]/ "abc" 1)))
(test "[abc]b[abc]" '("abc")
      (lambda () (match&list #/[abc]b[abc]/ "abc" 1)))
(test "a[bc]d" '("abd")
      (lambda () (match&list #/a[bc]d/ "xyzaaabcaababdacd" 1)))
(test "a[ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab](wee|week)(knights|night)"
      '("aaaaabaaaabaaaabaaaabweeknights" "wee" "knights")
      (lambda ()
        (match&list #/a[ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab](wee|week)(knights|night)/
                    "aaaaabaaaabaaaabaaaabweeknights" 3)))
(test "[ab][cd][ef][gh][ij][kl][mn]"
      '("acegikm")
      (lambda ()
        (match&list #/[ab][cd][ef][gh][ij][kl][mn]/
                    "xacegikmoq" 1)))
(test "[ab][cd][ef][gh][ij][kl][mn][op]"
      '("acegikmo")
      (lambda ()
        (match&list #/[ab][cd][ef][gh][ij][kl][mn][op]/
                    "xacegikmoq" 1)))
(test "[ab][cd][ef][gh][ij][kl][mn][op][qr]"
      '("acegikmoq")
      (lambda ()
        (match&list #/[ab][cd][ef][gh][ij][kl][mn][op][qr]/
                    "xacegikmoqy" 1)))
(test "[ab][cd][ef][gh][ij][kl][mn][op][q]"
      '("acegikmoq")
      (lambda ()
        (match&list #/[ab][cd][ef][gh][ij][kl][mn][op][q]/
                    "xacegikmoqy" 1)))

(test "\\s" '(" ")  (lambda () (match&list #/\s/ "  " 1)))
(test "\\s" '("  ")  (lambda () (match&list #/\s\s/ "  " 1)))
(test "\\s" '("\t")  (lambda () (match&list #/\s/ "\t " 1)))
(test "\\s" #f  (lambda () (match&list #/\s\s/ "\\s" 1)))
(test "\\\\s" '("\\s ")  (lambda () (match&list #/\\s\s/ "\\s " 1)))
(test "\\s\\S" '("\txyz   " "\t" "xyz" "   ")
      (lambda () (match&list #/(\s*)(\S+)(\s*)/ "\txyz   abc" 4)))
(test "\\d\\D" '("1234) 5678-9012" "1234" ") " "5678" "-" "9012")
      (lambda () (match&list #/(\d+)(\D+)(\d+)(\D+)(\d+)/
                             " (1234) 5678-9012 " 6)))
(test "\\w\\W" '("three o" "three" " " "o")
      (lambda () (match&list #/(\w+)(\W+)(\w+)/
                             "three o'clock" 4)))

(test "[ab\\sc]+" '(" ba ") (lambda () (match&list #/[ab\sc]+/ "d ba e" 1)))
(test "[\\sa-c]+" '(" ba ") (lambda () (match&list #/[\sabc]+/ "d ba e" 1)))
(test "[\\S\\t]+" '("\tab") (lambda () (match&list #/[\S\t]+/ "\tab cd" 1)))
(test "[\\s\\d]+" '(" 1 2 3 ")
      (lambda () (match&list #/[\s\d]+/ "a 1 2 3 b " 1)))
(test "[\\s\\D]+" '("a ")
      (lambda () (match&list #/[\s\D]+/ "a 1 2 3 b " 1)))

;;-------------------------------------------------------------------------
(test-section "regexp macros")

(use gauche.regexp)

(test "rxmatch-let" '("23:59:58" "23" "59" "58")
      (lambda ()
        (rxmatch-let (rxmatch #/(\d+):(\d+):(\d+)/
                              "Jan  1 23:59:58, 2001")
            (time hh mm ss)
          (list time hh mm ss))))
(test "rxmatch-let" '("23" "59")
      (lambda ()
        (rxmatch-let (rxmatch #/(\d+):(\d+):(\d+)/
                              "Jan  1 23:59:58, 2001")
            (#f hh mm)
          (list hh mm))))

(test "rxmatch-if" "time is 11:22"
      (lambda ()
        (rxmatch-if (rxmatch #/(\d+:\d+)/ "Jan 1 11:22:33")
            (time)
          (format #f "time is ~a" time)
          "unknown time")))
(test "rxmatch-if" "unknown time"
      (lambda ()
        (rxmatch-if (rxmatch #/(\d+:\d+)/ "Jan 1 11-22-33")
            (time)
          (format #f "time is ~a" time)
          "unknown time")))

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

(test "rxmatch-cond" '(2001 2 3)
      (lambda () (test-parse-date "2001/2/3")))
(test "rxmatch-cond" '(1999 12 25)
      (lambda () (test-parse-date "1999/12/25")))
(test "rxmatch-cond" #f
      (lambda () (test-parse-date 'abc)))

(define (test-parse-date2 str)
  (rxmatch-case str
    (test (lambda (s) (not (string? s))) #f)
    (#/^(\d\d?)\/(\d\d?)\/(\d\d\d\d)$/ (#f mm dd yyyy)
     (map string->number (list yyyy mm dd)))
    (#/^(\d\d\d\d)\/(\d\d?)\/(\d\d?)$/ (#f yyyy mm dd)
     (map string->number (list yyyy mm dd)))
    (#/^\d+\/\d+\/\d+$/  (#f) (error "ambiguous:" str))
    (else (error "bogus:" str))))

(test "rxmatch-case" '(2001 2 3)
      (lambda () (test-parse-date2 "2001/2/3")))
(test "rxmatch-case" '(1999 12 25)
      (lambda () (test-parse-date2 "1999/12/25")))
(test "rxmatch-case" #f
      (lambda () (test-parse-date2 'abc)))

(test "regexp-replace" "abc|def|ghi"
      (lambda () (regexp-replace #/def|DEF/ "abcdefghi" "|\\0|")))

(test "regexp-replace" "abc|\\0|ghi"
      (lambda () (regexp-replace #/def|DEF/ "abcdefghi" "|\\\\0|")))

(test "regexp-replace" "abraabra**brabra**brabrabracadabrabrabra"
      (lambda ()
        (regexp-replace #/a((bra)+)cadabra/
                        "abraabraabrabracadabrabrabrabracadabrabrabra"
                        "**\\1**")))

(test "regexp-replace-all" "abraabra**brabra**br**brabra**brabra"
      (lambda ()
        (regexp-replace-all #/a((bra)+)cadabra/
                            "abraabraabrabracadabrabrabrabracadabrabrabra"
                            "**\\1**")))

(test "regexp-replace" "abfedhi"
      (lambda ()
        (regexp-replace #/c(.*)g/ "abcdefghi" 
                        (lambda (m)
                          (list->string
                           (reverse
                            (string->list (rxmatch-substring m 1))))))))

(test "regexp-replace-all" "abraabra(bra^2)br(bra^2)brabra"
      (lambda ()
        (regexp-replace-all #/a((bra)+)cadabra/
                            "abraabraabrabracadabrabrabrabracadabrabrabra"
                            (lambda (m)
                              (format #f "(bra^~a)"
                                      (/ (string-length (rxmatch-substring m 1))
                                         3))))))

;;-------------------------------------------------------------------------
(test-section "regexp cimatch")

(test "regexp/ci" "BC"
      (lambda ()
        (cond ((rxmatch #/bc/i "ABCD") => rxmatch-substring)
              (else #f))))
(test "regexp/ci" "bC"
      (lambda ()
        (cond ((rxmatch #/Bc/i "AbCD") => rxmatch-substring)
              (else #f))))
(test "regexp/ci" "bc"
      (lambda ()
        (cond ((rxmatch #/BC/i "AbcD") => rxmatch-substring)
              (else #f))))
(test "regexp/ci" #f
      (lambda ()
        (cond ((rxmatch #/Bc/ "AbCD") => rxmatch-substring)
              (else #f))))
(test "regexp/ci" #f
      (lambda ()
        (cond ((rxmatch #/BC/ "ABcD") => rxmatch-substring)
              (else #f))))
(test "regexp/ci" "PAD"
      (lambda ()
        (cond ((rxmatch #/p[a-z]d/i "PAD") => rxmatch-substring)
              (else #f))))
(test "regexp/ci" "pad"
      (lambda ()
        (cond ((rxmatch #/P[A-Z]D/i "pad") => rxmatch-substring)
              (else #f))))
(test "regexp/ci" "bad"
      (lambda ()
        (cond ((rxmatch #/.[a-pQ-Z][A-Pq-z]/i "bad") => rxmatch-substring)
              (else #f))))
(test "regexp/ci" #f
      (lambda ()
        (cond ((rxmatch #/p[a-z]d/ "PAD") => rxmatch-substring)
              (else #f))))
(test "regexp/ci" #f
      (lambda ()
        (cond ((rxmatch #/P[A-Z]D/ "pad") => rxmatch-substring)
              (else #f))))
(test "regexp/ci" #f
      (lambda ()
        (cond ((rxmatch #/.[a-pQ-Z][A-Pq-z]/ "bad") => rxmatch-substring)
              (else #f))))
(test "regexp/ci" "pad"
      (lambda ()
        (cond ((rxmatch (string->regexp "p[A-Z]d" :case-fold #t) "pad")
               => rxmatch-substring)
              (else #f))))
(test "regexp/ci" #f
      (lambda ()
        (cond ((rxmatch (string->regexp "p[A-Z]d") "pad")
               => rxmatch-substring)
              (else #f))))

(test-end)
