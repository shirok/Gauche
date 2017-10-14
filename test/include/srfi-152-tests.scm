;; (import (except (scheme base)
;;                 string=? string<? string>? string<=? string>=?)
;;         (except (scheme char)
;;                 string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?)
;;         (srfi-152))

;; (cond-expand
;;   ((library (chibi test))
;;    (import (chibi test)))
;;   ((library (srfi 64))
;;    (import (srfi 64))
;;    (define-syntax test
;;      (syntax-rules ()
;;        ((_ arg ...) (test-equal arg ...))))
;;    (define-syntax test-exit
;;      (syntax-rules ()
;;        ((_) (test-end))))
;;    (test-begin "srfi-152 top"))
;;   (else
;;    (error "no suitable test framework available")))

(define (complement proc) (lambda (x) (not (proc x))))
(define (char-newline? ch) (eqv? ch #\newline))
(define (char-is-r? ch) (eqv? ch #\r))
(define (char-is-colon? ch) (eqv? ch #\:))
(define (char-is-a? ch) (eqv? ch #\a))
(define (char-is-space? ch) (eq? ch #\space))

;; artefact of converting from cursors to indexes and back
(define (dummy-index string index) index)

(define ABC "abc")

(test-group "srfi-152"
(test-group "srfi-152:gauche"
(test-group "srfi-152:gauche:predicates"
(test "string-null?" #f (string-null? "abc"))
(test "string-null?" #t (string-null? ""))
(test "string-every" #t (string-every char-is-a? ""))
(test "string-every" #t (string-every char-is-a? "aaaa"))
(test "string-every" #f (string-every char-is-a? "aaba"))
(test "string-every" #t (string-every char-lower-case? "aaba"))
(test "string-every" #f (string-every char-lower-case? "aAba"))
(test "string-every" #t (string-every char-lower-case? ""))
(test "string-every" #t (string-every (lambda (x) (char-ci=? x #\a)) "aAaA"))
(test "string-every" #f (string-every (lambda (x) (char-ci=? x #\a)) "aAbA"))
(test "string-every" (char->integer #\A)
       (string-every (lambda (x) (char->integer x)) "aAbA"))
(test "string-every" #t
       (string-every (lambda (x) (error "hoge")) ""))
(test "string-any" #t (string-any char-is-a? "aaaa"))
(test "string-any" #f (string-any char-is-a? "Abcd"))
(test "string-any" #f (string-any #\a ""))
(test "string-any" #t (string-any char-lower-case? "ABcD"))
(test "string-any" #f (string-any char-lower-case? "ABCD"))
(test "string-any" #f (string-any char-lower-case? ""))
(test "string-any" #t (string-any (lambda (x) (char-ci=? x #\a)) "CAaA"))
(test "string-any" #f (string-any (lambda (x) (char-ci=? x #\a)) "ZBRC"))
(test "string-any" #f (string-any (lambda (x) (char-ci=? x #\a)) ""))
(test "string-any" (char->integer #\a)
       (string-any (lambda (x) (char->integer x)) "aAbA"))
)
(test-group "srfi-152:gauche:constructors"
(test "string-tabulate" "0123456789"
       (string-tabulate (lambda (code)
                          (integer->char (+ code (char->integer #\0))))
                        10))
(test "string-tabulate" ""
       (string-tabulate (lambda (code)
                          (integer->char (+ code (char->integer #\0))))
                        0))
(test "reverse-list->string" "cBa"
       (reverse-list->string '(#\a #\B #\c)))
(test "reverse-list->string" ""
       (reverse-list->string '()))
(test "string-join" "foo+bar+baz"
      (string-join '("foo" "bar" "baz") "+"))
(test "string-join" "foo bar baz"
      (string-join '("foo" "bar" "baz")))
(test "string-join" "/foo/bar/baz"
      (string-join '("foo" "bar" "baz") "/" 'prefix))
(test "string-join" "foo;bar;baz;"
      (string-join '("foo" "bar" "baz") ";" 'suffix))
)
(test-group "srfi-152:gauche:selectors"
(test "substring" "cde" (substring "abcde" 2 5))
(test "substring" "cd"  (substring "abcde" 2 4))
(test "string-copy!" "abCDEfg"
       (let ((x (string-copy "abcdefg")))
         (string-copy! x 2 "CDE")
         x))
(test "string-copy!" "abCDEfg"
       (let ((x (string-copy "abcdefg")))
         (string-copy! x 2 "ZABCDE" 3)
         x))
(test "string-copy!" "abCDEfg"
       (let ((x (string-copy "abcdefg")))
         (string-copy! x 2 "ZABCDEFG" 3 6)
         x))
(test "string-take" "Pete S"  (string-take "Pete Szilagyi" 6))
(test "string-take" ""        (string-take "Pete Szilagyi" 0))
(test "string-take" "Pete Szilagyi" (string-take "Pete Szilagyi" 13))
(test "string-drop" "zilagyi" (string-drop "Pete Szilagyi" 6))
(test "string-drop" "Pete Szilagyi" (string-drop "Pete Szilagyi" 0))
(test "string-drop" ""        (string-drop "Pete Szilagyi" 13))

(test "string-take-right" "rules" (string-take-right "Beta rules" 5))
(test "string-take-right" ""      (string-take-right "Beta rules" 0))
(test "string-take-right" "Beta rules" (string-take-right "Beta rules" 10))
(test "string-drop-right" "Beta " (string-drop-right "Beta rules" 5))
(test "string-drop-right" "Beta rules" (string-drop-right "Beta rules" 0))
(test "string-drop-right" ""      (string-drop-right "Beta rules" 10))

(test "string-pad" "  325" (string-pad "325" 5))
(test "string-pad" "71325" (string-pad "71325" 5))
(test "string-pad" "71325" (string-pad "8871325" 5))
(test "string-pad" "~~325" (string-pad "325" 5 #\~))
(test "string-pad" "~~~25" (string-pad "325" 5 #\~ 1))
(test "string-pad" "~~~~2" (string-pad "325" 5 #\~ 1 2))
(test "string-pad-right" "325  " (string-pad-right "325" 5))
(test "string-pad-right" "71325" (string-pad-right "71325" 5))
(test "string-pad-right" "88713" (string-pad-right "8871325" 5))
(test "string-pad-right" "325~~" (string-pad-right "325" 5 #\~))
(test "string-pad-right" "25~~~" (string-pad-right "325" 5 #\~ 1))
(test "string-pad-right" "2~~~~" (string-pad-right "325" 5 #\~ 1 2))

(test "string-trim"  "a b c d  \n"
       (string-trim "  \t  a b c d  \n"))
(test "string-trim"  "\t  a b c d  \n"
       (string-trim "  \t  a b c d  \n" char-is-space?))
(test "string-trim"  "a b c d  \n"
       (string-trim "4358948a b c d  \n" char-numeric?))

(test "string-trim-right"  "  \t  a b c d"
       (string-trim-right "  \t  a b c d  \n"))
(test "string-trim-right"  "  \t  a b c d  "
       (string-trim-right "  \t  a b c d  \n" char-newline?))
(test "string-trim-right"  "349853a b c d"
       (string-trim-right "349853a b c d03490" char-numeric?))

(test "string-trim-both"  "a b c d"
       (string-trim-both "  \t  a b c d  \n"))
(test "string-trim-both"  "  \t  a b c d  "
       (string-trim-both "  \t  a b c d  \n" char-newline?))
(test "string-trim-both"  "a b c d"
       (string-trim-both "349853a b c d03490" char-numeric?))

)
(test-group "srfi-152:gauche:replacement"
(test "string-replace" "-ab01234cdefghi"
      (string-replace "-abcdefghi" "01234" 3 3))
(test "string-replace" "-ab012cdefghi"
      (string-replace "-abcdefghi" "01234" 3 3 0 3))
(test "string-replace" "-ab01234fghi"
      (string-replace "-abcdefghi" "01234" 3 6))
(test "string-replace" "-ab34fghi"
      (string-replace "-abcdefghi" "01234" 3 6 3 5))
(test "string-replace" "abcdXYZghi"
       (string-replace "abcdefghi" "XYZ" 4 6))
(test "string-replace" "abcdZghi"
       (string-replace "abcdefghi" "XYZ" 4 6 2))
(test "string-replace" "abcdZefghi"
       (string-replace "abcdefghi" "XYZ" 4 4 2))
(test "string-replace" "abcdefghi"
       (string-replace "abcdefghi" "XYZ" 4 4 1 1))
(test "string-replace" "abcdhi"
       (string-replace "abcdefghi" "" 4 7))


)

;; NB: This isn't in srfi document
'(test-group "srfi-152:extended-comparisons"
  (test "base cases for extended string comparisons"
    '(#t #t #t #t #t #t #t #t #t #t)
    (map (lambda (f) (and (f) (f "foo")))
         (list string=? string<? string>? string<=? string>=?
               string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?))))

(test-group "srfi-152:gauche:comparison"
(test "string=?" #t (string=? "foo" "foo"))


(test "string<=?" #t (string<=? "fol" "foo"))

(test "string<?" #t (string<? "fol" "foo"))

(test "string>=?" #t (string>=? "foo" "fol"))

(test "string>?" #t (string>? "foo" "fol"))

(test "string-ci=?" #t (string-ci=? "Foo" "foO"))


(test "string-ci<=?" #t (string-ci<=? "FOL" "foo"))

(test "string-ci<?" #t (string-ci<? "fol" "FOO"))

(test "string-ci>=?" #t (string-ci>=? "FOO" "fol"))

(test "string-ci>?" #t (string-ci>? "FOO" "fol"))

(test "string=?" #t (string=? "abcd" (string-append "a" "b" "c" "d")))

)

(test-group "srfi-152:gauche:presuffixes"

(test "string-prefix-length" 5
       (string-prefix-length "cancaNCAM" "cancancan"))
(test "string-suffix-length" 2
       (string-suffix-length "CanCan" "cankancan"))

(test "string-prefix?" #t    (string-prefix? "abcd" "abcdefg"))
(test "string-prefix?" #f    (string-prefix? "abcf" "abcdefg"))
(test "string-suffix?" #t    (string-suffix? "defg" "abcdefg"))
(test "string-suffix?" #f    (string-suffix? "aefg" "abcdefg"))
)
(test-group "srfi-152:gauche:searching"

(test "string-index #1" 4
       (string-index "abcd:efgh:ijkl" char-is-colon?))
(test "string-index #2" 4
       (string-index "abcd:efgh;ijkl" (complement char-alphabetic?)))
(test "string-index #3" #f
       (string-index "abcd:efgh;ijkl" char-numeric?))
(test "string-index #4" 9
       (string-index "abcd:efgh:ijkl" char-is-colon? 5))
(test "string-index-right #1" 4
       (string-index-right "abcd:efgh;ijkl" char-is-colon?))
(test "string-index-right #2" 9
       (string-index-right "abcd:efgh;ijkl" (complement char-alphabetic?)))
(test "string-index-right #3" #f
       (string-index-right "abcd:efgh;ijkl" char-numeric?))
(test "string-index-right #4" 4
       (string-index-right "abcd:efgh;ijkl" (complement char-alphabetic?) 2 5))
(test "string-contains" 3
       (string-contains "Ma mere l'oye" "mer"))
(test "string-contains" #f
       (string-contains "Ma mere l'oye" "Mer"))
)
(test-group "srfi-152:gauche:append"
(test "string-append" #f
       (let ((s "test")) (eq? s (string-append s))))
(test "string-concatenate" #f
       (let ((s "test")) (eq? s (string-concatenate (list s)))))
(test "string-concatenate" "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
       (string-concatenate
        '("A" "B" "C" "D" "E" "F" "G" "H"
          "I" "J" "K" "L" "M" "N" "O" "P"
          "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
          "a" "b" "c" "d" "e" "f" "g" "h"
          "i" "j" "k" "l" "m" "n" "o" "p"
          "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))
(test "string-concatenate-reverse" "zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLKJIHGFEDCBA"
       (string-concatenate-reverse
        '("A" "B" "C" "D" "E" "F" "G" "H"
          "I" "J" "K" "L" "M" "N" "O" "P"
          "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
          "a" "b" "c" "d" "e" "f" "g" "h"
          "i" "j" "k" "l" "m" "n" "o" "p"
          "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))
(test "string-concatenate-reverse" #f
       (let ((s "test"))
         (eq? s (string-concatenate-reverse (list s)))))
)
(test-group "srfi-152:gauche:foldmap"
(test "string-map" "svool"
       (string-map (lambda (c)
                     (integer->char (- 219 (char->integer c))))
                   "hello"))

(test "string-fold" '(#\o #\l #\l #\e #\h . #t)
       (string-fold cons #t "hello"))
(test "string-fold" '(#\l #\e . #t)
       (string-fold cons #t "hello" 1 3))
(test "string-fold-right" '(#\h #\e #\l #\l #\o . #t)
       (string-fold-right cons #t "hello"))
(test "string-fold-right" '(#\e #\l . #t)
       (string-fold-right cons #t "hello" 1 3))

(test "string-unfold" "hello"
       (string-unfold null? car cdr '(#\h #\e #\l #\l #\o)))
(test "string-unfold" "hi hello"
       (string-unfold null? car cdr '(#\h #\e #\l #\l #\o) "hi "))
(test "string-unfold" "hi hello ho"
       (string-unfold null? car cdr
                      '(#\h #\e #\l #\l #\o) "hi "
                      (lambda (x) " ho")))

(test "string-unfold-right" "olleh"
       (string-unfold-right null? car cdr '(#\h #\e #\l #\l #\o)))
(test "string-unfold-right" "olleh hi"
       (string-unfold-right null? car cdr '(#\h #\e #\l #\l #\o) " hi"))
(test "string-unfold-right" "ho olleh hi"
       (string-unfold-right null? car cdr
                            '(#\h #\e #\l #\l #\o) " hi"
                            (lambda (x) "ho ")))

(test "string-for-each" "CLtL"
       (let ((out (open-output-string))
             (prev #f))
         (string-for-each (lambda (c)
                            (if (or (not prev)
                                    (char-whitespace? prev))
                                (write-char c out))
                            (set! prev c))
                          "Common Lisp, the Language")

         (get-output-string out)))

#;(test "string-for-each-index" '(4 3 2 1 0)
       (let ((r '()))
         (string-for-each-index (lambda (i) (set! r (cons i r))) "hello")
         r))
#;(test "string-for-each-index" '(4 3 2 1)
       (let ((r '()))
         (string-for-each-index (lambda (i) (set! r (cons i r))) "hello" 1)
         r))
#;(test "string-for-each-index" '(2 1)
       (let ((r '()))
         (string-for-each-index (lambda (i) (set! r (cons i r))) "hello" 1 3)
         r))
(test "string-count #1" 2
       (string-count "abc def\tghi jkl" char-is-space?))
(test "string-count #2" 3
       (string-count "abc def\tghi jkl" char-whitespace?))
(test "string-count #3" 2
       (string-count "abc def\tghi jkl" char-whitespace? 4))
(test "string-count #4" 1
       (string-count "abc def\tghi jkl" char-whitespace? 4 9))

(test "string-filter" "rrrr"
       (string-filter char-is-r? "Help make programs run, run, RUN!"))
(test "string-filter" "HelpmakeprogramsrunrunRUN"
       (string-filter char-alphabetic? "Help make programs run, run, RUN!"))

(test "string-filter" "programsrunrun"
       (string-filter (lambda (c) (char-lower-case? c))
                      "Help make programs run, run, RUN!"
                      10))
(test "string-filter" ""
       (string-filter (lambda (c) (char-lower-case? c)) ""))
(test "string-remove" "Help make pogams un, un, RUN!"
       (string-remove char-is-r? "Help make programs run, run, RUN!"))
(test "string-remove" "   , , !"
       (string-remove char-alphabetic? "Help make programs run, run, RUN!"))
(test "string-remove" " , , RUN!"
       (string-remove (lambda (c) (char-lower-case? c))
                      "Help make programs run, run, RUN!"
                      10))
(test "string-remove" ""
       (string-remove (lambda (c) (char-lower-case? c)) ""))

)
(test-group "srfi-152:gauche:replisplit"

(test "string-replicate" "cdefab"
       (string-replicate "abcdef" 2 8))
(test "string-replicate" "efabcd"
       (string-replicate "abcdef" -2 4))
(test "string-replicate" "abcabca"
       (string-replicate "abc" 0 7))
;; (test "string-replicate" "abcabca"
;;        (string-replicate "abc"
;;                    30000000000000000000000000000000
;;                    30000000000000000000000000000007))
(test "string-replicate" "defdefd"
       (string-replicate "abcdefg" 0 7 3 6))
(test "string-replicate" ""
       (string-replicate "abcdefg" 9 9 3 6))

(test "string-segment" '("ab" "cd" "ef")
  (string-segment "abcdef" 2))
(test "string-segment" '("ab" "cd" "ef" "g")
  (string-segment "abcdefg" 2))
(test "string-segment" '()
  (string-segment "" 2))
(test "string-split" '("Help" "make" "programs" "run," "run," "RUN!")
       (string-split "Help make programs run, run, RUN!" " "))
(test "string-split" '("Help" "make" "programs run, run, RUN!")
       (string-split "Help make programs run, run, RUN!" " " 'infix 2))
(test "string-split" '("usr" "local" "bin")
       (string-split "/usr/local/bin" "/" 'prefix))
(test "string-split" '("be()" "here()" "now()")
       (string-split "be(); here(); now(); " "; " 'suffix))

)
(test-group "srfi-152:gauche:regression"
;;; Regression tests: check that reported bugs have been fixed

; From: Matthias Radestock <matthias@sorted.org>
; Date: Wed, 10 Dec 2003 21:05:22 +0100
;
; Chris Double has found the following bug in the reference implementation:
;
;  (string-contains "xabc" "ab") => 1    ;good
;  (string-contains "aabc" "ab") => #f   ;bad
;
; Matthias.

(test "string-contains" 1 (string-contains "aabc" "ab"))

(test "string-contains" 5 (string-contains "ababdabdabxxas" "abdabx"))


; (message continues)
;
; PS: There is also an off-by-one error in the bounds check of the
; unoptimized version of string-contains that is included as commented out
; code in the reference implementation. This breaks things like
; (string-contains "xab" "ab") and (string-contains "ab" "ab").

; This off-by-one bug has been fixed in the comments of the version
; of SRFI-13 shipped with Larceny.  In a version of the code without
; the fix the following test will catch the bug:

(test "string-contains" 0 (string-contains "ab" "ab"))

; From: dvanhorn@emba.uvm.edu
; Date: Wed, 26 Mar 2003 08:46:41 +0100
;
; The SRFI document gives,
;
;   string-filter s char/char-set/pred [start end] -> string
;   string-remove s char/char-set/pred [start end] -> string
;
; Yet the reference implementation switches the order giving,
;
;   ;;; string-remove char/char-set/pred string [start end]
;   ;;; string-filter char/char-set/pred string [start end]
;   ...
;   (define (string-remove criterion s . maybe-start+end)
;   ...)
;   (define (string-filter criterion s . maybe-start+end)
;   ...)
;
; I reviewed the SRFI-13 mailing list and c.l.scheme, but found no mention of
; this issue.  Apologies if I've missed something.

(test "ADR" (string-filter char-upper-case? "abrAcaDabRa"))

(test "abrcaaba" (string-remove char-upper-case? "abrAcaDabRa"))
))
(test-group "srfi-152:larceny"
;;; Predicates
(test-group "srfi-152:larceny:predicates"

(test-assert (string-null? ""))
(test-assert (not (string-null? "abc")))
(test #t (string-every (lambda (c) (if (char? c) c #f)) ""))
(test #\c (string-every (lambda (c) (if (char? c) c #f)) "abc"))
(test #f (string-every (lambda (c) (if (char>? c #\b) c #f)) "abc"))
(test #\c (string-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 2))
(test #t (string-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 1 1))
(test #f (string-any (lambda (c) (if (char? c) c #f)) ""))
(test #\a (string-any (lambda (c) (if (char? c) c #f)) "abc"))
(test #\c (string-any (lambda (c) (if (char>? c #\b) c #f)) "abc"))
(test #\c (string-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 2))
(test #f (string-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 0 2))
)
;;; Constructors
(test-group "srfi-152:larceny:constructors"
(test ""
            (string-tabulate (lambda (i)
                               (integer->char (+ i (char->integer #\a))))
                             0))
(test "abc"
            (string-tabulate (lambda (i)
                               (integer->char (+ i (char->integer #\a))))
                             3))
(test "abc"
            (let ((p (open-input-string "abc")))
              (string-unfold eof-object?
                             values
                             (lambda (x) (read-char p))
                             (read-char p))))
(test "" (string-unfold null? car cdr '()))
(test "abc" (string-unfold null? car cdr (string->list "abc")))
(test "def" (string-unfold null? car cdr '() "def"))
(test "defabcG"
            (string-unfold null?
                           car
                           cdr
                           (string->list "abc")
                           "def"
                           (lambda (x) (and (null? x) "G"))))
(test "" (string-unfold-right null? car cdr '()))
(test "cba" (string-unfold-right null? car cdr (string->list "abc")))
(test "def" (string-unfold-right null? car cdr '() "def"))
(test "Gcbadef"
            (string-unfold-right null?
                                 car
                                 cdr
                                 (string->list "abc")
                                 "def"
                                 (lambda (x) (and (null? x) "G"))))
)
;;; Conversion
(test-group "srfi-152:larceny:conversion"
(test '() (string->list ""))
(test '() (string->list "" 0))
(test '() (string->list "" 0 0))
(test '(#\a #\b #\c) (string->list "abc"))
(test '() (string->list "abc" 3))
(test '(#\b #\c) (string->list "abc" 1 3))
(test '(#\b #\c)
            (string->list "abc"
                                  (dummy-index "abc" 1)
                                  (dummy-index "abc" 3)))
(test '#() (string->vector ""))
(test '#() (string->vector "" 0))
(test '#() (string->vector "" 0 0))
(test '#(#\a #\b #\c) (string->vector "abc"))
(test '#() (string->vector "abc" 3))
(test '#(#\b #\c) (string->vector "abc" 1 3))
(test '#(#\b #\c)
            (string->vector "abc"
                                  (dummy-index "abc" 1)
                                  (dummy-index "abc" 3)))
(test "" (reverse-list->string '()))
(test "cba" (reverse-list->string '(#\a #\b #\c)))
(test "" (string-join '()))
(test " ab cd  e f "
            (string-join '("" "ab" "cd" "" "e" "f" "")))
(test "" (string-join '() ""))
(test "abcdef"
            (string-join '("" "ab" "cd" "" "e" "f" "") ""))
(test "" (string-join '() "xyz"))
(test "xyzabxyzcdxyzxyzexyzfxyz"
            (string-join '("" "ab" "cd" "" "e" "f" "") "xyz"))
(test "" (string-join '() "" 'infix))
(test "abcdef"
            (string-join '("" "ab" "cd" "" "e" "f" "") "" 'infix))
(test "" (string-join '() "xyz" 'infix))
(test "xyzabxyzcdxyzxyzexyzfxyz"
            (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'infix))
(test-error
             (string-join '() "" 'strict-infix))
(test "abcdef"
            (string-join '("" "ab" "cd" "" "e" "f" "") "" 'strict-infix))
(test-error 
             (string-join '() "xyz" 'strict-infix))
(test "xyzabxyzcdxyzxyzexyzfxyz"
            (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'strict-infix))
(test "" (string-join '() "" 'suffix))
(test "abcdef"
            (string-join '("" "ab" "cd" "" "e" "f" "") "" 'suffix))
(test "" (string-join '() "xyz" 'suffix))
(test "xyzabxyzcdxyzxyzexyzfxyzxyz"
            (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'suffix))
(test "" (string-join '() "" 'prefix))
(test "abcdef"
            (string-join '("" "ab" "cd" "" "e" "f" "") "" 'prefix))
(test "" (string-join '() "xyz" 'prefix))
(test "xyzxyzabxyzcdxyzxyzexyzfxyz"
            (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'prefix))
)
;;; Selection
(test-group "srfi-152:larceny:selection"
(test #\a (string-ref "abc" 0))
(test #\c (string-ref "abc" 2))
(test #\a (string-ref "abc" (dummy-index "abc" 0)))
(test #\c (string-ref "abc" (dummy-index "abc" 2)))
(test "" (substring "" 0 0))
(test "" (substring "abc" 0 0))
(test "" (substring "abc" 3 3))
(test ABC (substring ABC 0 3))
(test ABC
              (substring ABC
                                 (dummy-index "abc" 0)
                                 (dummy-index "abc" 3)))
(test "b" (substring "abc" 1 2))
(test "" (string-copy ""))
(test "abc" (string-copy "abc"))
(test "" (string-copy "abc" 3))
(test "c" (string-copy "abc" 2))
(test "abc" (string-copy "abc" 0))
(test "b" (string-copy "abc" 1 2))
(test "" (string-copy "" 0 0))
(test "" (string-copy "abc" 0 0))
(test "" (string-copy "abc" 3 3))
(test "abc" (string-copy "abc" 0 3))
(test "b" (string-copy "abc" 1 2))
(test (substring ABC 1 2)
              (string-copy ABC
                                   (dummy-index "abc" 1)
                                   (dummy-index "abc" 2)))
(test "" (string-take "" 0))
(test "" (string-take "abcdef" 0))
(test "ab" (string-take "abcdef" 2))
(test "" (string-drop "" 0))
(test "abcdef" (string-drop "abcdef" 0))
(test "cdef" (string-drop "abcdef" 2))
(test "" (string-take-right "" 0))
(test "" (string-take-right "abcdef" 0))
(test "ef" (string-take-right "abcdef" 2))
(test "" (string-drop-right "" 0))
(test "abcdef" (string-drop-right "abcdef" 0))
(test "abcd" (string-drop-right "abcdef" 2))
(test "" (string-pad "" 0))
(test "     " (string-pad "" 5))
(test "  325" (string-pad "325" 5))
(test "71325" (string-pad "71325" 5))
(test "71325" (string-pad "8871325" 5))
(test "" (string-pad "" 0 #\*))
(test "*****" (string-pad "" 5 #\*))
(test "**325" (string-pad "325" 5 #\*))
(test "71325" (string-pad "71325" 5 #\*))
(test "71325" (string-pad "8871325" 5 #\*))
(test "" (string-pad "" 0 #\* 0))
(test "*****" (string-pad "" 5 #\* 0))
(test "**325" (string-pad "325" 5 #\* 0))
(test "71325" (string-pad "71325" 5 #\* 0))
(test "71325" (string-pad "8871325" 5 #\* 0))
(test "***25" (string-pad "325" 5 #\* 1))
(test "*1325" (string-pad "71325" 5 #\* 1))
(test "71325" (string-pad "8871325" 5 #\* 1))
(test "" (string-pad "" 0 #\* 0 0))
(test "*****" (string-pad "" 5 #\* 0 0))
(test "**325" (string-pad "325" 5 #\* 0 3))
(test "**713" (string-pad "71325" 5 #\* 0 3))
(test "**887" (string-pad "8871325" 5 #\* 0 3))
(test "***25" (string-pad "325" 5 #\* 1 3))
(test "**132" (string-pad "71325" 5 #\* 1 4))
(test "*8713" (string-pad "8871325" 5 #\* 1 5))
(test "" (string-pad-right "" 0))
(test "     " (string-pad-right "" 5))
(test "325  " (string-pad-right "325" 5))
(test "71325" (string-pad-right "71325" 5))
(test "88713" (string-pad-right "8871325" 5))
(test "" (string-pad-right "" 0 #\*))
(test "*****" (string-pad-right "" 5 #\*))
(test "325**" (string-pad-right "325" 5 #\*))
(test "71325" (string-pad-right "71325" 5 #\*))
(test "88713" (string-pad-right "8871325" 5 #\*))
(test "" (string-pad-right "" 0 #\* 0))
(test "*****" (string-pad-right "" 5 #\* 0))
(test "325**" (string-pad-right "325" 5 #\* 0))
(test "71325" (string-pad-right "71325" 5 #\* 0))
(test "88713" (string-pad-right "8871325" 5 #\* 0))
(test "25***" (string-pad-right "325" 5 #\* 1))
(test "1325*" (string-pad-right "71325" 5 #\* 1))
(test "87132" (string-pad-right "8871325" 5 #\* 1))
(test "" (string-pad-right "" 0 #\* 0 0))
(test "*****" (string-pad-right "" 5 #\* 0 0))
(test "325**" (string-pad-right "325" 5 #\* 0 3))
(test "713**" (string-pad-right "71325" 5 #\* 0 3))
(test "887**" (string-pad-right "8871325" 5 #\* 0 3))
(test "25***" (string-pad-right "325" 5 #\* 1 3))
(test "132**" (string-pad-right "71325" 5 #\* 1 4))
(test "8713*" (string-pad-right "8871325" 5 #\* 1 5))
(test "" (string-trim ""))
(test "a  b  c  " (string-trim "  a  b  c  "))
(test "" (string-trim "" char-whitespace?))
(test "a  b  c  " (string-trim "  a  b  c  " char-whitespace?))
(test "" (string-trim "  a  b  c  " char?))
(test "" (string-trim "" char-whitespace? 0))
(test "a  b  c  " (string-trim "  a  b  c  " char-whitespace? 0))
(test "" (string-trim "  a  b  c  " char? 0))
(test "b  c  " (string-trim "  a  b  c  " char-whitespace? 3))
(test "" (string-trim "  a  b  c  " char? 3))
(test "" (string-trim "  a  b  c  " char? 0 11))
(test "b  c  " (string-trim "  a  b  c  " char-whitespace? 3 11))
(test "" (string-trim "  a  b  c  " char? 3 11))
(test "" (string-trim "  a  b  c  " char? 0 8))
(test "b  " (string-trim "  a  b  c  " char-whitespace? 3 8))
(test "" (string-trim "  a  b  c  " char? 3 8))
(test "" (string-trim-right ""))
(test "  a  b  c" (string-trim-right "  a  b  c  "))
(test "" (string-trim-right "" char-whitespace?))
(test "  a  b  c" (string-trim-right "  a  b  c  " char-whitespace?))
(test "" (string-trim-right "  a  b  c  " char?))
(test "" (string-trim-right "" char-whitespace? 0))
(test "  a  b  c" (string-trim-right "  a  b  c  " char-whitespace? 0))
(test "" (string-trim-right "  a  b  c  " char? 0))
(test "  b  c" (string-trim-right "  a  b  c  " char-whitespace? 3))
(test "" (string-trim-right "  a  b  c  " char? 3))
(test "" (string-trim-right "  a  b  c  " char? 0 11))
(test "  b  c" (string-trim-right "  a  b  c  " char-whitespace? 3 11))
(test "" (string-trim-right "  a  b  c  " char? 3 11))
(test "" (string-trim-right "  a  b  c  " char? 0 8))
(test "  b" (string-trim-right "  a  b  c  " char-whitespace? 3 8))
(test "" (string-trim-right "  a  b  c  " char? 3 8))
(test "" (string-trim-both ""))
(test "a  b  c" (string-trim-both "  a  b  c  "))
(test "" (string-trim-both "" char-whitespace?))
(test "a  b  c" (string-trim-both "  a  b  c  " char-whitespace?))
(test "" (string-trim-both "  a  b  c  " char?))
(test "" (string-trim-both "" char-whitespace? 0))
(test "a  b  c" (string-trim-both "  a  b  c  " char-whitespace? 0))
(test "" (string-trim-both "  a  b  c  " char? 0))
(test "b  c" (string-trim-both "  a  b  c  " char-whitespace? 3))
(test "" (string-trim-both "  a  b  c  " char? 3))
(test "" (string-trim-both "  a  b  c  " char? 0 11))
(test "b  c" (string-trim-both "  a  b  c  " char-whitespace? 3 11))
(test "" (string-trim-both "  a  b  c  " char? 3 11))
(test "" (string-trim-both "  a  b  c  " char? 0 8))
(test "b" (string-trim-both "  a  b  c  " char-whitespace? 3 8))
(test "" (string-trim-both "  a  b  c  " char? 3 8))
(test 0 (string-prefix-length "" ""))
(test 0 (string-prefix-length "" "aabbccddee"))
(test 0 (string-prefix-length "aisle" ""))
(test 0 (string-prefix-length "" "aabbccddee"))
(test 1 (string-prefix-length "aisle" "aabbccddee"))
(test 0 (string-prefix-length "bail" "aabbccddee"))
(test 4 (string-prefix-length "prefix" "preface"))
(test 0 (string-prefix-length "" "" 0))
(test 0 (string-prefix-length "" "aabbccddee" 0))
(test 0 (string-prefix-length "aisle" "" 0))
(test 1 (string-prefix-length "aisle" "aabbccddee" 0))
(test 0 (string-prefix-length "bail" "aabbccddee" 0))
(test 4 (string-prefix-length "prefix" "preface" 0))
(test 0 (string-prefix-length "aisle" "" 1))
(test 0 (string-prefix-length "aisle" "aabbccddee" 1))
(test 1 (string-prefix-length "bail" "aabbccddee" 1))
(test 0 (string-prefix-length "prefix" "preface" 1))
(test 0 (string-prefix-length "" "" 0 0))
(test 0 (string-prefix-length "" "aabbccddee" 0 0))
(test 0 (string-prefix-length "aisle" "" 0 4))
(test 1 (string-prefix-length "aisle" "aabbccddee" 0 4))
(test 0 (string-prefix-length "bail" "aabbccddee" 0 1))
(test 0 (string-prefix-length "aisle" "" 1 4))
(test 0 (string-prefix-length "aisle" "aabbccddee" 1 4))
(test 1 (string-prefix-length "bail" "aabbccddee" 1 4))
(test 0 (string-prefix-length "prefix" "preface" 1 5))
(test 0 (string-prefix-length "" "" 0 0 0))
(test 0 (string-prefix-length "" "aabbccddee" 0 0 0))
(test 0 (string-prefix-length "aisle" "" 0 4 0))
(test 0 (string-prefix-length "aisle" "aabbccddee" 0 4 2))
(test 1 (string-prefix-length "bail" "aabbccddee" 0 1 2))
(test 0 (string-prefix-length "prefix" "preface" 0 5 1))
(test 0 (string-prefix-length "aisle" "" 1 4 0))
(test 0 (string-prefix-length "aisle" "aabbccddee" 1 4 3))
(test 0 (string-prefix-length "bail" "aabbccddee" 1 4 3))
(test 3 (string-prefix-length "prefix" "preface" 1 5 1))
(test 0 (string-prefix-length "" "" 0 0 0 0))
(test 0 (string-prefix-length "" "aabbccddee" 0 0 0 0))
(test 0 (string-prefix-length "aisle" "" 0 4 0 0))
(test 0 (string-prefix-length "aisle" "aabbccddee" 0 4 2 10))
(test 1 (string-prefix-length "bail" "aabbccddee" 0 1 2 10))
(test 0 (string-prefix-length "prefix" "preface" 0 5 1 6))
(test 0 (string-prefix-length "aisle" "" 1 4 0 0))
(test 0 (string-prefix-length "aisle" "aabbccddee" 1 4 3 3))
(test 0 (string-prefix-length "bail" "aabbccddee" 1 4 3 6))
(test 3 (string-prefix-length "prefix" "preface" 1 5 1 7))
(test 0 (string-suffix-length "" ""))
(test 0 (string-suffix-length "" "aabbccddee"))
(test 0 (string-suffix-length "aisle" ""))
(test 0 (string-suffix-length "" "aabbccddee"))
(test 1 (string-suffix-length "aisle" "aabbccddee"))
(test 0 (string-suffix-length "bail" "aabbccddee"))
(test 3 (string-suffix-length "place" "preface"))
(test 0 (string-suffix-length "" "" 0))
(test 0 (string-suffix-length "" "aabbccddee" 0))
(test 0 (string-suffix-length "aisle" "" 0))
(test 1 (string-suffix-length "aisle" "aabbccddee" 0))
(test 0 (string-suffix-length "bail" "aabbccddee" 0))
(test 3 (string-suffix-length "place" "preface" 0))
(test 0 (string-suffix-length "aisle" "" 1))
(test 1 (string-suffix-length "aisle" "aabbccddee" 1))
(test 0 (string-suffix-length "bail" "aabbccddee" 1))
(test 3 (string-suffix-length "place" "preface" 1))
(test 0 (string-suffix-length "" "" 0 0))
(test 0 (string-suffix-length "" "aabbccddee" 0 0))
(test 0 (string-suffix-length "aisle" "" 0 4))
(test 0 (string-suffix-length "aisle" "aabbccddee" 0 4))
(test 0 (string-suffix-length "bail" "aabbccddee" 0 1))
(test 0 (string-suffix-length "aisle" "" 1 4))
(test 0 (string-suffix-length "aisle" "aabbccddee" 1 4))
(test 1 (string-suffix-length "aisle" "aabbccddee" 1 5))
(test 0 (string-suffix-length "bail" "aabbccddee" 1 4))
(test 3 (string-suffix-length "place" "preface" 1 5))
(test 0 (string-suffix-length "" "" 0 0 0))
(test 0 (string-suffix-length "" "aabbccddee" 0 0 0))
(test 0 (string-suffix-length "aisle" "" 0 4 0))
(test 0 (string-suffix-length "aisle" "aabbccddee" 0 4 2))
(test 0 (string-suffix-length "bail" "aabbccddee" 0 1 2))
(test 3 (string-suffix-length "place" "preface" 0 5 1))
(test 0 (string-suffix-length "aisle" "" 1 4 0))
(test 0 (string-suffix-length "aisle" "aabbccddee" 1 4 3))
(test 0 (string-suffix-length "bail" "aabbccddee" 1 4 3))
(test 3 (string-suffix-length "place" "preface" 1 5 1))
(test 0 (string-suffix-length "" "" 0 0 0 0))
(test 0 (string-suffix-length "" "aabbccddee" 0 0 0 0))
(test 0 (string-suffix-length "aisle" "" 0 4 0 0))
(test 1 (string-suffix-length "aisle" "aabbccddee" 0 5 2 10))
(test 1 (string-suffix-length "bail" "aabbccddee" 0 1 2 4))
(test 0 (string-suffix-length "place" "preface" 0 5 1 6))
(test 2 (string-suffix-length "place" "preface" 0 4 1 6))
(test 0 (string-suffix-length "aisle" "" 1 4 0 0))
(test 0 (string-suffix-length "aisle" "aabbccddee" 1 4 3 3))
(test 0 (string-suffix-length "bail" "aabbccddee" 1 4 3 6))
(test 3 (string-suffix-length "place" "preface" 1 5 1 7))
(test #t (string-prefix? "" ""))
(test #t (string-prefix? "" "abc"))
(test #t (string-prefix? "a" "abc"))
(test #f (string-prefix? "c" "abc"))
(test #t (string-prefix? "ab" "abc"))
(test #f (string-prefix? "ac" "abc"))
(test #t (string-prefix? "abc" "abc"))
(test #t (string-suffix? "" ""))
(test #t (string-suffix? "" "abc"))
(test #f (string-suffix? "a" "abc"))
(test #t (string-suffix? "c" "abc"))
(test #f (string-suffix? "ac" "abc"))
(test #t (string-suffix? "bc" "abc"))
(test #t (string-suffix? "abc" "abc"))
(test #t (string-prefix? "" "" 0))
(test #t (string-prefix? "" "abc" 0))
(test #t (string-prefix? "a" "abc" 0))
(test #f (string-prefix? "c" "abc" 0))
(test #t (string-prefix? "ab" "abc" 0))
(test #f (string-prefix? "ac" "abc" 0))
(test #t (string-prefix? "abc" "abc" 0))
(test #t (string-suffix? "" "" 0))
(test #t (string-suffix? "" "abc" 0))
(test #f (string-suffix? "a" "abc" 0))
(test #t (string-suffix? "c" "abc" 0))
(test #f (string-suffix? "ac" "abc" 0))
(test #t (string-suffix? "bc" "abc" 0))
(test #t (string-suffix? "abc" "abc" 0))
(test #t (string-prefix? "ab" "abc" 2))
(test #t (string-prefix? "ac" "abc" 2))
(test #f (string-prefix? "abc" "abc" 2))
(test #t (string-suffix? "ac" "abc" 2))
(test #t (string-suffix? "bc" "abc" 2))
(test #t (string-suffix? "abc" "abc" 2))
(test #t (string-prefix? "" "" 0 0))
(test #t (string-prefix? "" "abc" 0 0))
(test #t (string-prefix? "a" "abc" 0 0))
(test #f (string-prefix? "c" "abc" 0 1))
(test #t (string-prefix? "ab" "abc" 0 1))
(test #t (string-prefix? "ab" "abc" 0 2))
(test #f (string-prefix? "ac" "abc" 0 2))
(test #t (string-prefix? "abc" "abc" 0 3))
(test #t (string-suffix? "" "" 0 0))
(test #t (string-suffix? "" "abc" 0 0))
(test #f (string-suffix? "a" "abc" 0 1))
(test #t (string-suffix? "c" "abc" 0 1))
(test #t (string-suffix? "ac" "abc" 1 2))
(test #f (string-suffix? "ac" "abc" 0 2))
(test #t (string-suffix? "bc" "abc" 0 2))
(test #t (string-suffix? "abc" "abc" 0 3))
(test #t (string-prefix? "ab" "abc" 2 2))
(test #t (string-prefix? "ac" "abc" 2 2))
(test #f (string-prefix? "abc" "abc" 2 3))
(test #t (string-suffix? "ac" "abc" 2 2))
(test #t (string-suffix? "bc" "abc" 2 2))
(test #t (string-suffix? "abc" "abc" 2 3))
(test #t (string-prefix? "" "" 0 0 0))
(test #t (string-prefix? "" "abc" 0 0 0))
(test #t (string-prefix? "a" "abc" 0 0 0))
(test #f (string-prefix? "c" "abc" 0 1 0))
(test #t (string-prefix? "ab" "abc" 0 1 0))
(test #t (string-prefix? "ab" "abc" 0 2 0))
(test #f (string-prefix? "ac" "abc" 0 2 0))
(test #t (string-prefix? "abc" "abc" 0 3 0))
(test #t (string-suffix? "" "" 0 0 0))
(test #t (string-suffix? "" "abc" 0 0 0))
(test #f (string-suffix? "a" "abc" 0 1 0))
(test #t (string-suffix? "c" "abc" 0 1 0))
(test #t (string-suffix? "ac" "abc" 1 2 0))
(test #f (string-suffix? "ac" "abc" 0 2 0))
(test #t (string-suffix? "bc" "abc" 0 2 0))
(test #t (string-suffix? "abc" "abc" 0 3 0))
(test #t (string-prefix? "ab" "abc" 2 2 0))
(test #t (string-prefix? "ac" "abc" 2 2 0))
(test #f (string-prefix? "abc" "abc" 2 3 0))
(test #t (string-suffix? "ac" "abc" 2 2 0))
(test #t (string-suffix? "bc" "abc" 2 2 0))
(test #t (string-suffix? "abc" "abc" 2 3 0))
(test #t (string-prefix? "" "abc" 0 0 1))
(test #t (string-prefix? "a" "abc" 0 0 1))
(test #t (string-prefix? "c" "abc" 0 1 2))
(test #f (string-prefix? "ab" "abc" 0 1 2))
(test #f (string-prefix? "ab" "abc" 0 2 1))
(test #f (string-prefix? "ac" "abc" 0 2 1))
(test #f (string-prefix? "abc" "abc" 0 3 1))
(test #f (string-suffix? "a" "abc" 0 1 2))
(test #t (string-suffix? "c" "abc" 0 1 1))
(test #t (string-suffix? "ac" "abc" 1 2 2))
(test #t (string-suffix? "bc" "abc" 0 2 1))
(test #f (string-suffix? "bc" "abc" 0 2 2))
(test #t (string-prefix? "" "" 0 0 0 0))
(test #t (string-prefix? "" "abc" 0 0 0 3))
(test #t (string-prefix? "a" "abc" 0 0 0 3))
(test #f (string-prefix? "c" "abc" 0 1 0 3))
(test #t (string-prefix? "ab" "abc" 0 1 0 3))
(test #t (string-prefix? "ab" "abc" 0 2 0 3))
(test #f (string-prefix? "ac" "abc" 0 2 0 3))
(test #t (string-prefix? "abc" "abc" 0 3 0 3))
(test #t (string-suffix? "" "abc" 0 0 0 3))
(test #f (string-suffix? "a" "abc" 0 1 0 3))
(test #t (string-suffix? "c" "abc" 0 1 0 3))
(test #t (string-suffix? "ac" "abc" 1 2 0 3))
(test #f (string-suffix? "ac" "abc" 0 2 0 3))
(test #t (string-suffix? "bc" "abc" 0 2 0 3))
(test #t (string-suffix? "abc" "abc" 0 3 0 3))
(test #t (string-prefix? "ab" "abc" 2 2 0 3))
(test #t (string-prefix? "ac" "abc" 2 2 0 3))
(test #f (string-prefix? "abc" "abc" 2 3 0 3))
(test #t (string-suffix? "ac" "abc" 2 2 0 3))
(test #t (string-suffix? "bc" "abc" 2 2 0 3))
(test #t (string-suffix? "abc" "abc" 2 3 0 3))
(test #t (string-prefix? "" "abc" 0 0 1 3))
(test #t (string-prefix? "a" "abc" 0 0 1 3))
(test #t (string-prefix? "c" "abc" 0 1 2 3))
(test #f (string-prefix? "ab" "abc" 0 1 2 3))
(test #f (string-prefix? "ab" "abc" 0 2 1 3))
(test #f (string-prefix? "ac" "abc" 0 2 1 3))
(test #f (string-prefix? "abc" "abc" 0 3 1 3))
(test #f (string-suffix? "a" "abc" 0 1 2 3))
(test #t (string-suffix? "c" "abc" 0 1 1 3))
(test #t (string-suffix? "ac" "abc" 1 2 2 3))
(test #t (string-suffix? "bc" "abc" 0 2 1 3))
(test #f (string-suffix? "bc" "abc" 0 2 2 3))
(test #t (string-prefix? "" "abc" 0 0 0 2))
(test #t (string-prefix? "a" "abc" 0 0 0 2))
(test #f (string-prefix? "c" "abc" 0 1 0 2))
(test #t (string-prefix? "ab" "abc" 0 1 0 2))
(test #f (string-prefix? "abc" "abc" 0 3 0 2))
(test #t (string-suffix? "" "abc" 0 0 0 2))
(test #f (string-suffix? "c" "abc" 0 1 0 2))
(test #f (string-suffix? "ac" "abc" 1 2 0 2))
)
;;; Searching
(test-group "srfi-152:larceny:searching"
(test #f
       (dummy-index ""
                             (string-index "" char?)))
(test 0
       (dummy-index "abcdef"
                             (string-index "abcdef" char?)))
(test 4
       (dummy-index "abcdef"
                             (string-index "abcdef"
                                           (lambda (c) (char>? c #\d)))))
(test #f
       (dummy-index "abcdef"
                             (string-index "abcdef" char-whitespace?)))
(test #f
       (dummy-index "abcdef"
                             (string-index-right "" char?)))
(test 5
       (dummy-index "abcdef"
                             (string-index-right "abcdef" char?)))
(test 5
       (dummy-index "abcdef"
                             (string-index-right "abcdef"
                                                 (lambda (c) (char>? c #\d)))))
(test #f
       (dummy-index "abcdef"
                             (string-index-right "abcdef" char-whitespace?)))
(test #f
       (dummy-index "" (string-skip "" string?)))
(test 0
       (dummy-index "abcdef"
                             (string-skip "abcdef" string?)))
(test 4
       (dummy-index "abcdef"
                             (string-skip "abcdef"
                                          (lambda (c) (char<=? c #\d)))))
(test #f
       (dummy-index "abcdef"
                             (string-skip "abcdef" char?)))
(test #f
       (dummy-index "" (string-skip-right "" string?)))
(test 5
       (dummy-index "abcdef"
                             (string-skip-right "abcdef" string?)))
(test 5
       (dummy-index "abcdef"
                             (string-skip-right "abcdef"
                                                (lambda (c) (char<=? c #\d)))))
(test #f
       (dummy-index "abcdef"
                             (string-skip-right "abcdef" char?)))
(test 2
       (dummy-index "abcdef"
                             (string-index "abcdef" char? 2)))
(test 4
       (dummy-index "abcdef"
                             (string-index "abcdef"
                                           (lambda (c) (char>? c #\d)) 2)))
(test #f
       (dummy-index "abcdef"
                             (string-index "abcdef" char-whitespace? 2)))
(test 5
       (dummy-index "abcdef"
                             (string-index-right "abcdef" char? 2)))
(test 5
       (dummy-index "abcdef"
                             (string-index-right "abcdef"
                                                 (lambda (c)
                                                   (char>? c #\d)) 2)))
(test #f
       (dummy-index "abcdef"
                             (string-index-right "abcdef" char-whitespace? 2)))
(test 2
       (dummy-index "abcdef"
                             (string-skip "abcdef" string? 2)))
(test 4
       (dummy-index "abcdef"
                             (string-skip "abcdef"
                                          (lambda (c)
                                            (char<=? c #\d)) 2)))
(test #f
       (dummy-index "abcdef"
                             (string-skip "abcdef" char? 2)))
(test 5
       (dummy-index "abcdef"
                             (string-skip-right "abcdef" string? 2)))
(test 5
       (dummy-index "abcdef"
                             (string-skip-right "abcdef"
                                                (lambda (c)
                                                  (char<=? c #\d)) 2)))
(test #f
       (dummy-index "abcdef"
                             (string-skip-right "abcdef" char? 2)))
(test 2
       (dummy-index "abcdef"
                             (string-index "abcdef" char? 2 5)))
(test 4
       (dummy-index "abcdef"
                             (string-index "abcdef"
                                           (lambda (c) (char>? c #\d)) 2 5)))
(test #f
       (dummy-index "abcdef"
                             (string-index "abcdef" char-whitespace? 2 5)))
(test 4
       (dummy-index "abcdef"
                             (string-index-right "abcdef" char? 2 5)))
(test 4
       (dummy-index "abcdef"
                             (string-index-right "abcdef"
                                                 (lambda (c)
                                                   (char>? c #\d)) 2 5)))
(test #f
       (dummy-index "abcdef"
                             (string-index-right "abcdef"
                                                 char-whitespace? 2 5)))
(test 2
       (dummy-index "abcdef"
                             (string-skip "abcdef" string? 2 5)))
(test 4
       (dummy-index "abcdef"
                             (string-skip "abcdef"
                                          (lambda (c) (char<=? c #\d)) 2 5)))
(test #f
       (dummy-index "abcdef"
                             (string-skip "abcdef" char? 2 5)))
(test 4
       (dummy-index "abcdef"
                             (string-skip-right "abcdef" string? 2 5)))
(test 4
       (dummy-index "abcdef"
                             (string-skip-right "abcdef"
                                                (lambda (c)
                                                  (char<=? c #\d)) 2 5)))
(test #f
       (dummy-index "abcdef"
                             (string-skip-right "abcdef" char? 2 5)))
(test 0
          (dummy-index ""
                                (string-contains "" "")))
(test 0
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "")))
(test 0
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "a")))
(test 5
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "ff")))
(test 4
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "eff")))
(test 8
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "foo")))
(test #f
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "efffoo")))
(test 0
          (dummy-index ""
                                (string-contains-right "" "")))
(test 11
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo" "")))
(test 0
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo" "a")))
(test 7
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo" "ff")))
(test 4
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo" "eff")))
(test 8
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo" "foo")))
(test #f
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "efffoo")))
(test 0
          (dummy-index ""
                                (string-contains "" "" 0)))
(test 2
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "" 2)))
(test #f
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "a" 2)))
(test 5
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "ff" 2)))
(test 4
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "eff" 2)))
(test 8
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "foo" 2)))
(test #f
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "efffoo" 2)))
(test 0
          (dummy-index ""
                                (string-contains-right "" "" 0)))
(test 11
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "" 2)))
(test #f
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "a" 2)))
(test 7
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "ff" 2)))
(test 4
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "eff" 2)))
(test 8
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "foo" 2)))
(test #f
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "efffoo" 2)))
(test 0
          (dummy-index ""
                                (string-contains "" "" 0 0)))
(test 2
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "" 2 10)))
(test #f
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "a" 2 10)))
(test 5
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "ff" 2 10)))
(test 4
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "eff" 2 10)))
(test #f
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "foo" 2 10)))
(test #f
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "efffoo" 2 10)))
(test 0
          (dummy-index ""
                                (string-contains-right "" "" 0 0)))
(test 10
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "" 2 10)))
(test #f
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "a" 2 10)))
(test 7
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "ff" 2 10)))
(test 4
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "eff" 2 10)))
(test #f
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "foo" 2 10)))
(test #f
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "efffoo" 2 10)))
(test 0
          (dummy-index ""
                                (string-contains "" "" 0 0 0)))
(test 2
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "" 2 10 0)))
(test 2
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "a" 2 10 1)))
(test 5
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "ff" 2 10 1)))
(test 5
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "eff" 2 10 1)))
(test #f
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "foo" 2 10 1)))
(test #f
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "efffoo" 2 10 1)))
(test 0
          (dummy-index ""
                                (string-contains-right "" "" 0 0 0)))
(test 10
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "" 2 10 0)))
(test 10
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "a" 2 10 1)))
(test 8
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "ff" 2 10 1)))
(test 7
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "eff" 2 10 1)))
(test #f
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "foo" 2 10 1)))
(test #f
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "efffoo" 2 10 1)))
(test 0
          (dummy-index ""
                                (string-contains "" "" 0 0 0 0)))
(test 2
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "" 2 10 0 0)))
(test 2
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "a" 2 10 1 1)))
(test 5
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "ff" 2 10 1 2)))
(test 5
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "eff" 2 10 1 2)))
(test 9
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "foo" 2 10 1 2)))
(test 4
          (dummy-index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "efffoo" 2 10 0 2)))
(test 0
          (dummy-index ""
                                (string-contains-right "" "" 0 0 0 0)))
(test 10
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "" 2 10 0 0)))
(test 10
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "a" 2 10 1 1)))
(test 8
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "ff" 2 10 1 2)))
(test 8
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "eff" 2 10 1 2)))
(test 9
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "foo" 2 10 1 2)))
(test 7
          (dummy-index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "efffoo" 2 10 1 3)))
)
;;; The whole string
(test-group "srfi-152:larceny:wholestring"
(test "" (string-concatenate '()))
(test "abcdef" (string-concatenate '("" "a" "bcd" "" "ef" "" "")))
(test "" (string-concatenate-reverse '()))
(test "efbcda"
              (string-concatenate-reverse '("" "a" "bcd" "" "ef" "" "")))
(test "huh?" (string-concatenate-reverse '() "huh?"))
(test "efbcdaxy"
              (string-concatenate-reverse '("" "a" "bcd" "" "ef" "" "") "xy"))
(test "huh" (string-concatenate-reverse '() "huh?" 3))
(test "efbcdax"
              (string-concatenate-reverse '("" "a" "bcd" "" "ef" "" "") "x" 1))
(test 8
       (string-fold (lambda (c count)
                      (if (char-whitespace? c)
                          (+ count 1)
                          count))
                    0
                    " ...a couple of spaces in this one... "))
(test 7
       (string-fold (lambda (c count)
                      (if (char-whitespace? c)
                          (+ count 1)
                          count))
                    0
                    " ...a couple of spaces in this one... "
                    1))
(test 6
       (string-fold (lambda (c count)
                      (if (char-whitespace? c)
                          (+ count 1)
                          count))
                    0
                    " ...a couple of spaces in this one... "
                    1
                    32))
(test (string->list "abcdef")
            (string-fold-right cons '() "abcdef"))
(test (string->list "def")
            (string-fold-right cons '() "abcdef" 3))
(test (string->list "cde")
            (string-fold-right cons '() "abcdef" 2 5))
(test "aabraacaadaabraa"
              (let* ((s "abracadabra")
                     (ans-len (string-fold (lambda (c sum)
                                             (+ sum (if (char=? c #\a) 2 1)))
                                           0 s))
                     (ans (make-string ans-len)))
                (string-fold (lambda (c i)
                               (let ((i (if (char=? c #\a)
                                            (begin (string-set! ans i #\a)
                                                   (+ i 1))
                                                   i)))
                                 (string-set! ans i c)
                             (+ i 1)))
                             0 s)
                ans))
(test '(101 100 99 98 97)
            (let ((s "abcde") (v '()))
              (string-for-each
               (lambda (char)
                 (set! v (cons (char->integer char) v)))
               s)
              v))
(test "cdefabcdefabcd"
              (string-replicate "abcdef" -4 10))
(test "bcdefbcdefbcd"
              (string-replicate "abcdef" 90 103 1))
(test "ecdecdecde"
              (string-replicate "abcdef" -13 -3 2 5))
(test 6 (string-count "abcdef" char?))
(test 4 (string-count "counting  whitespace, again " char-whitespace? 5))
(test 3 (string-count "abcdefwxyz"
                       (lambda (c) (odd? (char->integer c)))
                       2 8))
(test "It's lots of fun to code it up in Scheme."
              (string-replace "It's easy to code it up in Scheme."
                              "lots of fun"
                              5 9))
(test "The miserable perl programmer endured daily ridicule."
              (string-replace "The TCL programmer endured daily ridicule."
                              "another miserable perl drone"
                              4 7 8 22))
(test "It's really easy to code it up in Scheme."
              (string-replace "It's easy to code it up in Scheme."
                              "really "
                              5 5))
(test-assert (null? (string-split "" "")))
(test '("a" "b" "c") (string-split "abc" ""))
(test '() (string-split "" "" 'infix))
(test '("a" "b" "c") (string-split "abc" "" 'infix))
(test '("a" "b" "c") (string-split "abc" "" 'strict-infix))
(test '() (string-split "" "" 'prefix))
(test '("a" "b" "c") (string-split "abc" "" 'prefix))
(test '() (string-split "" "" 'suffix))
(test '("a" "b" "c") (string-split "abc" "" 'suffix))
(test '() (string-split "" "" 'infix #f))
(test '("a" "b" "c") (string-split "abc" "" 'infix #f))
(test-error
             (string-split "" "" 'strict-infix))
(test '("a" "b" "c") (string-split "abc" "" 'strict-infix 3))
(test '() (string-split "" "" 'prefix 3))
(test '("a" "b" "c") (string-split "abc" "" 'prefix 3))
(test '() (string-split "" "" 'suffix 3))
(test '("a" "b" "c") (string-split "abc" "" 'suffix 3))
(test '("b" "c") (string-split "abc" "" 'strict-infix 3 1))
(test '() (string-split "" "" 'prefix 3 0))
(test '("b" "c") (string-split "abc" "" 'prefix 3 1))
(test '("b") (string-split "abc" "" 'strict-infix 3 1 2))
(test '() (string-split "" "" 'prefix 3 0 0))
(test '("b") (string-split "abc" "" 'prefix 3 1 2))
(test '() (string-split "" "" 'suffix 3 0 0))
(test '("b") (string-split "abc" "" 'suffix 3 1 2))
(test "aiueaaaoi"
              (string-filter (lambda (c) (memv c (string->list "aeiou")))
                             "What is number, that man may know it?"))
(test "And wmn, tht sh my knw nmbr?"
              (string-remove (lambda (c) (memv c (string->list "aeiou")))
                             "And woman, that she may know number?"))
(test "iueaaaoi"
              (string-filter (lambda (c) (memv c (string->list "aeiou")))
                             "What is number, that man may know it?"
                             4))
(test "mn, tht sh my knw nmbr?"
              (string-remove (lambda (c) (memv c (string->list "aeiou")))
                             "And woman, that she may know number?"
                             6))
(test "aaao"
              (string-filter (lambda (c) (memv c (string->list "aeiou")))
                             "What is number, that man may know it?"
                             16 32))
(test "And woman, that sh may know"
              (string-remove (lambda (c) (memv c (string->list "eiu")))
                             "And woman, that she may know number?"
                             0 28))
)
)
(test-group "srfi-152:residual"
  (test #t (string? "abc"))
  (test #f (string? 32))
  (test "$$$" (make-string 3 #\$))
  (test "$$$" (string #\$ #\$ #\$))
  (test '(#\b #\c) (string->list "abcde" 1 3))
  (test "abcde" (list->string '(#\a #\b #\c #\d #\e)))
  (test "abcde" (vector->string '#(#\a #\b #\c #\d #\e)))
  (test '("12345" "abcde")
    (call-with-values (lambda () (string-span "12345abcde" char-numeric?)) list))
  (test '("12345" "abcde")
    (call-with-values (lambda () (string-break "12345abcde" char-alphabetic?)) list))
  (test "abcde" (string-take-while "abcde12345" char-alphabetic?))
  (test "abcde" (string-take-while-right "12345abcde" char-alphabetic?))
  (test "abcde" (string-drop-while "   abcde" char-whitespace?))
  (test "abcde" (string-drop-while-right "abcde  " char-whitespace?))
  (test 5 (string-length "abcde"))
  (test "ab!"
    (let ((abc (string-copy "abc")))
      (string-set! abc 2 #\!)
      abc))
  (test "ab!"
    (let ((abc (string-copy "abc")))
      (string-set! abc 2 #\!)
      abc))
  (test "!!!"
    (let ((abc (string-copy "abc")))
      (string-fill! abc #\!)
      abc))
  (test "a!c"
    (let ((abc (string-copy "abc")))
      (string-fill! abc #\! 1 2)
      abc))
)
)
;(test-exit)
