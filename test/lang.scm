;;;
;;; tests for lang.*
;;;
;;;   For now, these are experimental modules so we only test basic
;;;   stuff.  Eventually we need comprehensive tests for each lang.* modules.
;;;

(use gauche.test)
(test-start "lang.*")
(use util.match)

;;----------------------------------------------------------------------
(test-section "lang.asm")
(use lang.asm.x86_64)
(test-module 'lang.asm.x86_64)

;;----------------------------------------------------------------------
(test-section "lang.c")

(use lang.c.type)
(test-module 'lang.c.type)

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

  (t "0L;"      '((const long        "0L") |\;|))
  (t "0l;"      '((const long        "0l") |\;|))
  (t "0U;"      '((const u-int       "0U") |\;|))
  (t "0u;"      '((const u-int       "0u") |\;|))
  (t "0uL;"     '((const u-long      "0uL") |\;|))
  (t "0ul;"     '((const u-long      "0ul") |\;|))
  (t "0lU;"     '((const u-long      "0lU") |\;|))
  (t "0ll;"     '((const long-long   "0ll") |\;|))
  (t "0LL;"     '((const long-long   "0LL") |\;|))
  (t "0ull;"    '((const u-long-long "0ull") |\;|))
  (t "0uLL;"    '((const u-long-long "0uLL") |\;|))
  (t "0llU;"    '((const u-long-long "0llU") |\;|))
  (t "0LLu;"    '((const u-long-long "0LLu") |\;|))
  (t "1234;"    '((const int         "1234") |\;|))
  (t "012;"     '((const int         "012" ) |\;|))
  (t "0x12;"    '((const int         "0x12") |\;|))
  (t "0xbeef;"  '((const int         "0xbeef") |\;|))
  (t "0xallu;"  '((const u-long-long "0xallu") |\;|))
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
  (t "1.0e67L;"  '((const long-double "1.0e67L")   |\;|))
  (t "1.37E+6L;" '((const long-double "1.37E+6L")  |\;|))
  (t "0E1L;"     '((const long-double "0E1L"   )   |\;|))
  (t "0x1.0p1;"  '((const double "0x1.0p1")       |\;|))
  (t "0x1.0;"    '((const double "0x1.0")         |\;|))

  (t "\"abra\""       '((string "abra")))
  (t "\"\";"          '((string "")     |\;|))
  (t "\"\\\"\";"      '((string "\"")   |\;|))
  (t "\"Copyright 2000 \\nTexas Instruments. \""
                   '((string "Copyright 2000 \nTexas Instruments. ")))
  (t "L\"abc\";" '((wstring "abc") |\;|))

  (t "X++Y;"     '((ident X) ++ (ident Y)  |\;|))
  (t "-12ul;"    '(- (const u-long  "12ul") |\;|))
  (t "x**2;"     '((ident x) * * (const int  "2") |\;|))
  (t "A*=B;"     '((ident A) *= (ident B)  |\;|))
  )

(use lang.c.parser)
(test-module 'lang.c.parser)

;; grokking type
(let ()
  (define (t-type expect code)
    (test* code expect
           (let loop ((decls (c-parse-string code)))
             (match decls
               [(('decl ('x _ type _)) . _) type]
               [(_ . r) (loop r)]))))

  (t-type '(int ()) "int x;")
  (t-type '(int (const)) "const int x;")
  (t-type '(int (const)) "int const x;")
  (t-type '(int (volatile restrict const)) "volatile int restrict const x;")

  (t-type '(char ()) "char x;")
  (t-type '(u-char ()) "unsigned char x;")
  (t-type '(u-char ()) "char unsigned x;")
  (t-type '(s-char ()) "signed char x;")
  (t-type '(s-char ()) "char signed x;")

  (t-type '(short ()) "short x;")
  (t-type '(short ()) "short int x;")
  (t-type '(short ()) "int short x;")
  (t-type '(u-short ()) "unsigned short x;")
  (t-type '(u-short ()) "unsigned int short x;")
  (t-type '(short ()) "signed short x;")
  (t-type '(short ()) "signed int short x;")
  (t-type '(short (volatile const)) "signed volatile int const short x;")

  (t-type '(long ()) "long x;")
  (t-type '(long ()) "long int x;")
  (t-type '(long ()) "int long x;")
  (t-type '(u-long ()) "unsigned long x;")
  (t-type '(u-long ()) "unsigned int long x;")
  (t-type '(long ()) "signed long x;")
  (t-type '(long ()) "signed int long x;")
  (t-type '(long-long ()) "long long x;")
  (t-type '(long-long ()) "long int long x;")
  (t-type '(long-long ()) "int long long x;")
  (t-type '(u-long-long ()) "int long unsigned long x;")
  (t-type '(u-long-long ()) "unsigned long long x;")
  (t-type '(long-long ()) "signed int long long x;")
  (t-type '(long-long (restrict const)) "restrict long signed int const long x;")

  (t-type '(bool ()) "_Bool x;")
  (t-type '(bool (const volatile)) "const _Bool volatile x;")

  (t-type '(float ()) "float x;")
  (t-type '(double ()) "double x;")
  (t-type '(long-double ()) "long double x;")
  (t-type '(long-double ()) "double long x;")
  (t-type '(float-complex ()) "_Complex float x;")
  (t-type '(float-complex ()) "float _Complex x;")
  (t-type '(double-complex ()) "_Complex double x;")
  (t-type '(long-double-complex ()) "long _Complex double x;")

  (t-type '(.type T () (int ())) "typedef int T; T x;")
  (t-type '(.type T (const) (int ())) "typedef int T; const T x;")
  (t-type '(.type T (const) (int ())) "typedef int T; T const x;")
  (t-type '(.type T () (int (const))) "typedef const int T; T x;")
  (t-type '(.type T (volatile) (int (const))) "typedef const int T; T volatile x;")

  (t-type '(.pointer () (int ())) "int *x;")
  (t-type '(.pointer () (int (const))) "const int *x;")
  (t-type '(.pointer (const) (int ())) "int * const x;")
  (t-type '(.pointer (const) (int (const))) "int const * const x;")
  (t-type '(.pointer () (.pointer () (int ()))) "int **x;")
  (t-type '(.pointer (volatile) (.pointer () (int ()))) "int *volatile*x;")

  (t-type '(.array (int ()) () 3) "int x[3];")
  (t-type '(.array (int ()) () 12) "int x[3*4];")
  (t-type '(.array (.array (int ()) () 2) () 3) "int x[3][2];")
  (t-type '(.array (.pointer () (int ())) () (ident N)) "int *x[N];")

  (t-type '(.function () (int ()) ()) "int x(void);")
  (t-type '(.function () (void ()) unknown-args) "void x();")
  (t-type '(.function () (int ()) ((#f (int ()))
                                   (#f (.pointer () (char  ())))))
          "int x(int, char*);")
  (t-type '(.function () (int ()) ((#f (int ()))
                                   (#f (.pointer () (char  ())))
                                   ...))
          "int x(int, char*, ...);")
  (t-type '(.function () (int ()) ((x #f)
                                   (y #f)
                                   (z #f)))
          "int x(x, y, z);")
  (t-type '(.function () (int ()) ((x (int ()))
                                   (y (int ()))
                                   (z (double ()))))
          "int x(int x, int y, double z);")

  (t-type '(.function () (.pointer () (int (const))) ())
          "const int *x(void);")
  (t-type '(.pointer () (.function () (int (const)) ()))
          "const int (*x)(void);")
  )

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
