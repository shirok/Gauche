;;
;; Test binary.pack
;; Author: Alex Shinn
;;

(use gauche.test)

(test-start "binary.pack")

;; kludge to make it work before installation
(when (file-exists? "../ext/uvector/uvector.scm")
  (eval '(begin (add-load-path "../ext/uvector/")
                (load "uvector"))
        (interaction-environment))
  (eval '(import gauche.uvector) (interaction-environment)))
(use binary.pack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test helpers

(define (test-error title expected thunk)
  (test title #t
    (lambda ()
      (with-error-handler
          (lambda (err)
            (and (cond ((regexp? expected)
                        (rxmatch expected (slot-ref err 'message)))
                       ((procedure? expected)
                        (expected err))
                       (else
                        (equal? expected err)))
                 #t))
        thunk))))

(define (unpack-pack fmt ls)
  (unpack fmt :from-string (pack fmt ls :to-string? #t)))

(define (test-unpack-pack name fmt ls)
  (test name ls (cut unpack-pack fmt ls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start the perl tests

(test-section "Perl pack tests")

(test "ascii integer medley" #t
  (lambda ()
    (let ((fmt "c2 x5 C C x s d i l a6")
          (ls `(1 -100 127 128 32767 ,(/ 987.654321098 100.0) 12345 123456 "abcdef")))
      (equal? ls (unpack fmt :from-string (pack fmt ls :to-string? #t))))))

;; % is unpack only, so it probably doesn't belong

; (for-each
;  (lambda (x)
;    (test "counting bits" (caddr x)
;      (lambda () (unpack (car x) :from-string (cadr x)))))
;  '(("%32B*" "\001\002\004\010\020\040\100\200\377" 16)
;    ("%32b69" "\001\002\004\010\020\040\100\200\017" 12)
;    ("%32B69" "\001\002\004\010\020\040\100\200\017" 9)))

(test-unpack-pack "maxint" "I" '(#xFFFFFFFF))

(define x-ls `(5 130 256 560 32000 3097152 268435455 1073741844
                 ,(expt 2 33) 4503599627365785
                 23728385234614992549757750638446))

(define x (string-complete->incomplete (pack "w*" x-ls :to-string? #t)))

(define y (string-complete->incomplete
           (pack "H*" (list (string-append "0581028200843081fa0081bd8440ffffff7f8480808014A0808"
                                           "0800087ffffffffffdb19caefe8e1eeeea0c2e1e3e8ede1ee6e"))
                 :to-string? #t)))

(test "ber pack" x (lambda () y))

(for-each
 (lambda (a b) (test "ber unpack" a (lambda () b)))
 x-ls (unpack "w*" :from-string y))

(for-each
 (lambda (template)
   (test (string-append "unpack-pack lengths: " template) 2
     (lambda ()
       (length (unpack-pack (string-append template "*") '(12 34))))))
 '("c" "C" "i" "I" "s" "S" "l" "L" "n" "N" "v" "V" "f" "d" "q" "Q"))

;; test the ascii template types (A, a, Z)

(for-each
 (lambda (x)
   (test (format #f "ascii ~a template: ~a"
                 (if (eq? (car x) 'p) "pack" "unpack") (cadr x))
       (cadddr x)
     (lambda ()
       (if (equal? (car x) 'u)
         (car (unpack (cadr x) :from-string (caddr x)))
         (pack (cadr x) (list (caddr x)) :to-string? #t)
         ))))
 '((p "A*"  "foo\0bar\0 " "foo\0bar\0 ")
   (p "A11" "foo\0bar\0 " "foo\0bar\0   ")
   (u "A*"  "foo\0bar \0" "foo\0bar")
   (u "A8"  "foo\0bar \0" "foo\0bar")
   (p "a*"  "foo\0bar\0 " "foo\0bar\0 ")
   (p "a11" "foo\0bar\0 " "foo\0bar\0 \0\0")
   (u "a*"  "foo\0bar \0" "foo\0bar \0")
   (u "a8"  "foo\0bar \0" "foo\0bar ")
   (p "Z*"  "foo\0bar\0 " "foo\0bar\0 \0")
   (p "Z11" "foo\0bar\0 " "foo\0bar\0 \0\0")
   (p "Z3"  "foo"         "fo\0")
   (u "Z*"  "foo\0bar \0" "foo")
   (u "Z8"  "foo\0bar \0" "foo")
   ;; added more basic tests (cross-referenced with Perl)
   (p "A*"  "foo-bar"     "foo-bar")
   (p "A11" "foo-bar"     "foo-bar    ")
   (u "A*"  "foo-bar"     "foo-bar")
   (u "A8"  "foo-bar"     "foo-bar")
   (p "a*"  "foo-bar"     "foo-bar")
   (p "a11" "foo-bar"     "foo-bar\0\0\0\0")
   (u "a*"  "foo-bar"     "foo-bar")
   (u "a8"  "foo-bar"     "foo-bar\0")
   (p "Z*"  "foo-bar"     "foo-bar\0")
   (p "Z11" "foo-bar"     "foo-bar\0\0\0\0")
   (p "Z3"  "foo"         "fo\0")
   (u "Z*"  "foo"         "foo")
   (u "Z8"  "foo"         "foo")
   ;; added binary and hex tests (cross-referenced with Perl)
   (p "B8"  "01000001"    "A")
   (u "B8"  "A"           "01000001")
   (p "b8"  "10000010"    "A")
   (u "b8"  "A"           "10000010")
   (p "B*"  "0100001001000011"   "BC")
   (u "B*"  "BC"                 "0100001001000011")
   (p "b*"  "1100001001000010"   "CB")
   (u "b*"  "CB"                 "1100001001000010")
   (p "B*"  "01000010010000110"  "BC\0")
   (p "b*"  "11000010010000100"  "CB\0")
   (p "H2"  "41"          "A")
   (u "H2"  "A"           "41")
   (p "h2"  "14"          "A")
   (u "h2"  "A"           "14")
   (p "H*"  "78797a"      "xyz")
   (u "H*"  "xyz"         "78797a")
   (p "h*"  "8797a7"      "xyz")
   (u "h*"  "xyz"         "8797a7")
   ))

;; pack nvNV byteorders

(for-each
 (lambda (x)
   (test (string-append "pack " (car x) " byteorders") (cadr x)
     (lambda ()
       (string-complete->incomplete (pack (car x) (cddr x) :to-string? #t)))))
 '(("n" #*"\xde\xad" #xdead)
   ("v" #*"\xad\xde" #xdead)
   ("N" #*"\xde\xad\xbe\xef" #xdeadbeef)
   ("V" #*"\xef\xbe\xad\xde" #xdeadbeef)))

;; object tests

(test* "unpack o" '((1 (2 (3))))
  (unpack "o" :from-string "(1 (2 (3)))"))

(test* "pack o" "(1 (2 (3)))"
  (pack "o" '((1 (2 (3)))) :to-string? #t))

;; interesting, equivalent regexps aren't equal? even if they print the
;; same
;;(test* "unpack o" '((1 (2 (3))) "string" #/re?ge[x]p/)
;;  (unpack "o*" :from-string "(1 (2 (3)))\"string\"#/re?ge[x]p/"))

(test* "pack o" "(1 (2 (3)))\"string\"#/re?ge[x]p/"
  (pack "o*" '((1 (2 (3))) "string" #/re?ge[x]p/) :to-string? #t))

;; grouping tests

(test* "pack ()" "AB"
  (pack "(C C)" '(65 66) :to-string? #t))

(test* "unpack ()" '(65 66)
  (unpack "(C C)" :from-string "AB"))

(test* "pack <>" "AB"
  (pack "<C C>" '((65 66)) :to-string? #t))

(test* "unpack <>" '((65 66))
  (unpack "<C C>" :from-string "AB"))

(test* "pack <>" "\x02ABCD"
  (pack "C/<C C>" '((65 66) (67 68)) :to-string? #t))

(test* "unpack <>" '((65 66) (67 68))
  (unpack "C/<C C>" :from-string "\x02ABCD"))

(test* "pack <>" "\x01AB"
  (pack "C/<C C>" '((65 66)) :to-string? #t))

(test* "unpack <>" '((65 66))
  (unpack "C/<C C>" :from-string "\x01AB"))

(test* "pack <>" "ABCD"
  (pack "<C C> 2" '((65 66) (67 68)) :to-string? #t))

(test* "unpack <>" '((65 66) (67 68))
  (unpack "<C C> 2" :from-string "ABCD"))

;; / tests

(test-error "unpack / with no numeric type"
    #/unknown pack template/
    (cut unpack "/a*" :from-string "hello"))

(test* "unpack C/a*" '("yes")
  (unpack "C/a*" :from-string #*"\x03yes"))

(test* "pack C/a*" "\x03yes"
  (pack "C/a*" '("yes") :to-string? #t))

(test* "unpack C/A*" '("yes")
  (unpack "C/A*" :from-string #*"\x03yes"))

(test* "pack C/A*" "\x03yes"
  (pack "C/A*" '("yes") :to-string? #t))

(test* "unpack C/C*" '(65 66 67)
  (unpack "C/C*" :from-string "\x03ABC"))

(test* "pack C/C*" "\x03ABC" 
  (pack "C/C*" '(65 66 67) :to-string? #t))

(test* "unpack w/C*" '(65 66 67)
  (unpack "w/C*" :from-string "\x03ABC"))

(test* "pack w/C*" "\x03ABC" 
  (pack "w/C*" '(65 66 67) :to-string? #t))

;; not sure about supporting implicit type conversions
;;(test* "pack / medley" "\x01A\x01B003ok \x03yes\x04z\x00"
;;  (pack "C/B* C/H* a3/A C/a* C/Z" '("01000001" "42" "ok" "yes" "z") :to-string? #t))

(test* "unpack / medley" '("01000001" "42" "ok" "yes" "z")
  (unpack "C/B* C/H* a3/A C/a* C/Z" :from-string "\x01A\x01B003ok \x03yes\x04z\x00abc"))

;; @ tests

(test* "pack fixed @" "a\0\0\0z"
  (pack "a1@4a1" '("a" "z") :to-string? #t))

(test* "pack fixed @ again" "a\0\0\0z"
  (pack "a1@4a1" '("a" "z") :to-string? #t))

(test* "unpack fixed @" '("a" "z")
  (unpack "a1@4a1" :from-string "a\0\0\0z"))

(test* "pack variable @" "a\0\0\0z"
  (pack "a*@4a1" '("a" "z") :to-string? #t))

(test* "unpack fixed @" '("a" "z")
  (unpack "C/a @4 a1" :from-string "\x01a\0\0z"))

(test* "pack twice @" "a\0\0\0mn\0\0\0z"
  (pack "a1@4a1a1@9a1" '("a" "m" "n" "z") :to-string? #t))

(test* "unpack twice @" '("a" "m" "n" "z")
  (unpack "a1@4a1a1@9a1" :from-string "a\0\0\0mn\0\0\0z"))

(test-end)

