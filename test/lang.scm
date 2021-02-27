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
(test-module 'lang.c.lexer)

;; contributed from Shigenobu Kimura
(define (test-c-tokenize str expect)
  (test* (write-to-string str) expect (c-tokenize (string->list str))))

(test-c-tokenize "forwhile;" '((ident forwhile) |\;|))
(test-c-tokenize "forx;"     '((ident forx) |\;|))
(test-c-tokenize "foox;"     '((ident foox) |\;|))
(test-c-tokenize "xfor;"     '((ident xfor) |\;|))
(test-c-tokenize "foo;"      '((ident foo) |\;|))
(test-c-tokenize "b>x;"      '((ident b) > (ident  x) |\;|))
(test-c-tokenize "b->x;"     '((ident b) -> (ident x) |\;|))
(test-c-tokenize "b--x;"     '((ident b) -- (ident x) |\;|))
(test-c-tokenize "b---x;"    '((ident b) --  - (ident x) |\;|))

(test-c-tokenize "0L;"      '((const long      "0L") |\;|))
(test-c-tokenize "0l;"      '((const long      "0l") |\;|))
(test-c-tokenize "0U;"      '((const uint      "0U") |\;|))
(test-c-tokenize "0u;"      '((const uint      "0u") |\;|))
(test-c-tokenize "0uL;"     '((const ulong     "0uL") |\;|))
(test-c-tokenize "0ul;"     '((const ulong     "0ul") |\;|))
(test-c-tokenize "0lU;"     '((const ulong     "0lU") |\;|))
(test-c-tokenize "0ll;"     '((const longlong  "0ll") |\;|))
(test-c-tokenize "0LL;"     '((const longlong  "0LL") |\;|))
(test-c-tokenize "0ull;"    '((const ulonglong "0ull") |\;|))
(test-c-tokenize "0uLL;"    '((const ulonglong "0uLL") |\;|))
(test-c-tokenize "0llU;"    '((const ulonglong "0llU") |\;|))
(test-c-tokenize "0LLu;"    '((const ulonglong "0LLu") |\;|))
(test-c-tokenize "1234;"    '((const int       "1234") |\;|))
(test-c-tokenize "012;"     '((const int       "012" ) |\;|))
(test-c-tokenize "0x12;"    '((const int       "0x12") |\;|))

(test-c-tokenize "'a';"     '((const char "a")        |\;|))
(test-c-tokenize "'A';"     '((const char "A")        |\;|))
(test-c-tokenize "' ';"     '((const char " ")        |\;|))
(test-c-tokenize "'?';"     '((const char "?")        |\;|))
(test-c-tokenize "'\\r';"   '((const char "\r")       |\;|))
(test-c-tokenize "'\\0';"   '((const char "\0")       |\;|))
(test-c-tokenize "'\"';"    '((const char "\"")       |\;|))
(test-c-tokenize "'\\377';" '((const char "\xff;")    |\;|))
(test-c-tokenize "'%';"     '((const char "%")        |\;|))
(test-c-tokenize "'\\23';"  '((const char "\x13;")    |\;|))
(test-c-tokenize "'8';"     '((const char "8")        |\;|))
(test-c-tokenize "'\\\\';"  '((const char "\\")       |\;|))
(test-c-tokenize "'ABCD';"  '((const char "ABCD")     |\;|))

(test-c-tokenize "0.;"       '((const double "0."     )       |\;|))
(test-c-tokenize "3e1;"      '((const double "3e1"    )       |\;|))
(test-c-tokenize "3.14159;"  '((const double "3.14159")       |\;|))
(test-c-tokenize ".0;"       '((const double ".0"     )       |\;|))
(test-c-tokenize "1.0E-3;"   '((const double "1.0E-3" )       |\;|))
(test-c-tokenize "1e-3;"     '((const double "1e-3"   )       |\;|))
(test-c-tokenize "1.0;"      '((const double "1.0"    )       |\;|))
(test-c-tokenize "0.00034;"  '((const double "0.00034")       |\;|))
(test-c-tokenize "2e+9;"     '((const double "2e+9"   )       |\;|))
(test-c-tokenize "1.0f;"     '((const float  "1.0f"   )       |\;|))
(test-c-tokenize "1.0e67L;"  '((const longdouble "1.0e67L")   |\;|))
(test-c-tokenize "1.37E+6L;" '((const longdouble "1.37E+6L")  |\;|))
(test-c-tokenize "0E1L;"     '((const longdouble "0E1L"   )   |\;|))
(test-c-tokenize "0x1.0p1;"  '((const double "0x1.0p1")       |\;|))
(test-c-tokenize "0x1.0;"    '((const double "0x1.0")         |\;|))

(test-c-tokenize "\"abra\""       '((string "abra")))
(test-c-tokenize "\"\";"          '((string "")     |\;|))
(test-c-tokenize "\"\\\"\";"      '((string "\"")   |\;|))
(test-c-tokenize "\"Copyright 2000 \\nTexas Instruments. \""
           '((string "Copyright 2000 \nTexas Instruments. ")))

(test-c-tokenize "X++Y;"     '((ident X) ++ (ident Y)  |\;|))
(test-c-tokenize "-12ul;"    '(- (const ulong  "12ul") |\;|))
(test-c-tokenize "x**2;"     '((ident x) * * (const int  "2") |\;|))
(test-c-tokenize "A*=B;"     '((ident A) *= (ident B)  |\;|))


(use lang.c.parser)
(test-module 'lang.c.parser)

(test-end)
