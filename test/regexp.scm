;;
;; testing regexp
;;

;; $Id: regexp.scm,v 1.1 2001-04-15 22:12:37 shiro Exp $

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


(test-end)
