;;
;; testing regexp
;;

;; $Id: regexp.scm,v 1.2 2001-04-16 09:33:25 shiro Exp $

(use gauche.test)
(use srfi-1)

(test-start "regexp")

;; In order to do thorough test, I need a standard mechanism to catch
;; an error so that I can test error conditions as well.  Gauche doesn't
;; have it yet.
;; Some test examples are taken from the test suite of Henry Spencer's
;; regexp package.

(define (match&list rx input nmatch)
  (map (lambda (n) (rxmatch-substring (rxmatch rx input) n))
       (iota nmatch)))

;;-------------------------------------------------------------------------
(test-section "basics")

(test "a" '("a")
      (lambda () (match&list #/a/ "a" 1)))
(test "abc" '("abc")
      (lambda () (match&list #/abc/ "abc" 1)))
(test "abc" '("abc")
      (lambda () (match&list #/abc|de/ "abc" 1)))
(test "abc" '("a")
      (lambda () (match&list #/a|b|c/ "abc" 1)))

;;-------------------------------------------------------------------------
(test-section "parens")

(test "abc" '("abc" "b")
      (lambda () (match&list #/a(b)c/ "abc" 2)))
(test "abc" '("a(")
      (lambda () (match&list #/a\(/ "a(" 1)))
(test "abc" '("ab" "")
      (lambda () (match&list #/a()b/ "ab" 2)))

;;-------------------------------------------------------------------------
(test-section "anchors")

;; not supported yet

;;-------------------------------------------------------------------------
(test-section "backslash escape")

(test "a.c" '("abc")
      (lambda () (match&list #/a.c/ "abc" 1)))
(test "a[bc]d" '("abd")
      (lambda () (match&list #/a[bc]d/ "abd" 1)))
(test "a\\*c" '("a*c")
      (lambda () (match&list #/a\*c/ "a*c" 1)))
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

(test-end)
