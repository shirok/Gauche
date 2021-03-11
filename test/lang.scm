;;;
;;; tests for lang.*
;;;
;;;   For now, these are experimental modules so we only test basic
;;;   stuff.  Eventually we need comprehensive tests for each lang.* modules.
;;;

(use gauche.test)
(test-start "lang.*")

;;----------------------------------------------------------------------
(test-section "lang.asm")
(use lang.asm.x86_64)
(test-module 'lang.asm.x86_64)

;;----------------------------------------------------------------------
(test-section "lang.c")
(use lang.c.lexer)
(use parser.peg :only (<parse-error>))
(test-module 'lang.c.lexer)

(let ()
  ;; originally contributed from Shigenobu Kimura
  (define (t str expect)
    (test* (write-to-string str)
           (if (regexp? expect)
             (test-error <parse-error> expect)
             expect)
           (c-tokenize (string->list str))))

  (t "forwhile;" '((ident forwhile) |\;|))
  (t "forx;"     '((ident forx) |\;|))
  (t "foox;"     '((ident foox) |\;|))
  (t "xfor;"     '((ident xfor) |\;|))
  (t "foo;"      '((ident foo) |\;|))
  (t "b>x;"      '((ident b) > (ident  x) |\;|))
  (t "b>>x;"     '((ident b) >> (ident  x) |\;|))
  (t "b->x;"     '((ident b) -> (ident x) |\;|))
  (t "b--x;"     '((ident b) -- (ident x) |\;|))
  (t "b---x;"    '((ident b) --  - (ident x) |\;|))

  (t "0L;"      '((const long      "0L") |\;|))
  (t "0l;"      '((const long      "0l") |\;|))
  (t "0U;"      '((const uint      "0U") |\;|))
  (t "0u;"      '((const uint      "0u") |\;|))
  (t "0uL;"     '((const ulong     "0uL") |\;|))
  (t "0ul;"     '((const ulong     "0ul") |\;|))
  (t "0lU;"     '((const ulong     "0lU") |\;|))
  (t "0ll;"     '((const longlong  "0ll") |\;|))
  (t "0LL;"     '((const longlong  "0LL") |\;|))
  (t "0ull;"    '((const ulonglong "0ull") |\;|))
  (t "0uLL;"    '((const ulonglong "0uLL") |\;|))
  (t "0llU;"    '((const ulonglong "0llU") |\;|))
  (t "0LLu;"    '((const ulonglong "0LLu") |\;|))
  (t "1234;"    '((const int       "1234") |\;|))
  (t "012;"     '((const int       "012" ) |\;|))
  (t "0x12;"    '((const int       "0x12") |\;|))
  (t "0xbeef;"  '((const int       "0xbeef") |\;|))
  (t "0xallu;"  '((const ulonglong "0xallu") |\;|))
  (t "0in;"     #/malformed integer constant suffix: in/)

  (t "'a';"     '((const char "a")        |\;|))
  (t "'A';"     '((const char "A")        |\;|))
  (t "' ';"     '((const char " ")        |\;|))
  (t "'?';"     '((const char "?")        |\;|))
  (t "'\\r';"   '((const char "\r")       |\;|))
  (t "'\\0';"   '((const char "\0")       |\;|))
  (t "'\"';"    '((const char "\"")       |\;|))
  (t "'\\377';" '((const char "\xff;")    |\;|))
  (t "'%';"     '((const char "%")        |\;|))
  (t "'\\23';"  '((const char "\x13;")    |\;|))
  (t "'8';"     '((const char "8")        |\;|))
  (t "'\\\\';"  '((const char "\\")       |\;|))
  (t "'ABCD';"  '((const char "ABCD")     |\;|))
  (t "L'8';"    '((const wchar "8")       |\;|))

  (t "0.;"       '((const double "0."     )       |\;|))
  (t "3e1;"      '((const double "3e1"    )       |\;|))
  (t "3.14159;"  '((const double "3.14159")       |\;|))
  (t ".0;"       '((const double ".0"     )       |\;|))
  (t "1.0E-3;"   '((const double "1.0E-3" )       |\;|))
  (t "1e-3;"     '((const double "1e-3"   )       |\;|))
  (t "1.0;"      '((const double "1.0"    )       |\;|))
  (t "0.00034;"  '((const double "0.00034")       |\;|))
  (t "2e+9;"     '((const double "2e+9"   )       |\;|))
  (t "1.0f;"     '((const float  "1.0f"   )       |\;|))
  (t "1.0e67L;"  '((const longdouble "1.0e67L")   |\;|))
  (t "1.37E+6L;" '((const longdouble "1.37E+6L")  |\;|))
  (t "0E1L;"     '((const longdouble "0E1L"   )   |\;|))
  (t "0x1.0p1;"  '((const double "0x1.0p1")       |\;|))
  (t "0x1.0;"    '((const double "0x1.0")         |\;|))

  (t "\"abra\""       '((string "abra")))
  (t "\"\";"          '((string "")     |\;|))
  (t "\"\\\"\";"      '((string "\"")   |\;|))
  (t "\"Copyright 2000 \\nTexas Instruments. \""
                   '((string "Copyright 2000 \nTexas Instruments. ")))
  (t "L\"abc\";" '((wstring "abc") |\;|))

  (t "X++Y;"     '((ident X) ++ (ident Y)  |\;|))
  (t "-12ul;"    '(- (const ulong  "12ul") |\;|))
  (t "x**2;"     '((ident x) * * (const int  "2") |\;|))
  (t "A*=B;"     '((ident A) *= (ident B)  |\;|))
  )

(use lang.c.parser)
(test-module 'lang.c.parser)

;; We haven't fixed the format of the semantic value, so for now we just
;; see if it parses.
(let ()
  (define (t-succ expect code)
    (test* code expect (boolean (c-parse-string code))))

  ;; simple declaration
  (t-succ #t "int x;")
  (t-succ #t "const int x;")
  (t-succ #t "int const x;")
  (t-succ #t "int x, y, z;")
  (t-succ #t "int *x, **y, * const * volatile z;")

  ;; typedef
  (t-succ #t "typedef int N; N x;")

  ;; typedef scoped
  (t-succ #t "int N; { typedef double N; (N)3; } N = 3;")
  (t-succ #t "typedef int N; N x; { int N; return N; }")
  (t-succ #t "typedef int N; N x; { int N; return N; } N y;")
  )


(test-end)
