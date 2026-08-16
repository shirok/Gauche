;;;
;;; tests for lang.*
;;;
;;;   For now, these are experimental modules so we only test basic
;;;   stuff.  Eventually we need comprehensive tests for each lang.* modules.
;;;

(use gauche.test)
(use gauche.uvector)
(test-start "lang.*")
(use util.match)

;;----------------------------------------------------------------------
(test-section "lang.asm")
(use lang.asm.regset)
(test-module 'lang.asm.regset)
(use lang.asm.linker)
(test-module 'lang.asm.linker)
(use lang.asm.fragment)
(test-module 'lang.asm.fragment)

(let ()
  (define (link-single-frag frag)
    (receive (_ lbls)
        (link-templates (list (make-obj-template (list frag) 'little-endian 8)) '())
      lbls))

  (test* "make-obj-template-labels" #t
         (obj-template-labels? (make-obj-template-labels)))

  (test* "obj-template-labels->alist empty" '()
         (obj-template-labels->alist (make-obj-template-labels)))

  (test* "link-templates returns <obj-template-labels>" #t
         (obj-template-labels?
          (link-single-frag (make-obj-fragment #u8(0) '((start . 0)) '() 'text))))

  (test* "obj-template-labels->alist after link-templates" '((start . 0))
         (obj-template-labels->alist
          (link-single-frag (make-obj-fragment #u8(0) '((start . 0)) '() 'text))))

  (test* "linked-label-offset" 0
         (linked-label-offset
          (link-single-frag (make-obj-fragment #u8(0) '((entry . 0)) '() 'text))
          'entry))

  (test* "link-templates duplicate label error" (test-error)
         (let* ([f1 (make-obj-fragment #u8(0) '((dup . 0)) '() 'text)]
                [f2 (make-obj-fragment #u8(0) '((dup . 0)) '() 'text)]
                [tmpl (make-obj-template (list f1 f2) 'little-endian 8)])
           (link-templates (list tmpl) '())))
  )

(let ()
  ;; prelink-template tests

  (test* "prelink-template returns <obj-template>" #t
         (let* ([frag (make-obj-fragment #u8(0 0 0 0) '() '() 'text)]
                [tmpl (make-obj-template (list frag) 'little-endian 8)])
           (obj-template? (prelink-template tmpl '()))))

  (test* "prelink-template fills typed patch from params" #u8(42 0 0 0)
         (let* ([frag (make-obj-fragment #u8(0 0 0 0) '() '((:val 0 4)) 'text)]
                [tmpl (make-obj-template (list frag) 'little-endian 8)]
                [result (prelink-template tmpl `((:val ,<integer> 42)))])
           (~ (car (~ result 'fragments)) 'bytes)))

  (test* "prelink-template removes applied patch" '()
         (let* ([frag (make-obj-fragment #u8(0 0 0 0) '() '((:val 0 4)) 'text)]
                [tmpl (make-obj-template (list frag) 'little-endian 8)]
                [result (prelink-template tmpl `((:val ,<integer> 42)))])
           (~ (car (~ result 'fragments)) 'patches)))

  (test* "prelink-template defers typed patch with no matching param - bytes" #u8(0 0 0 0)
         (let* ([frag (make-obj-fragment #u8(0 0 0 0) '() '((:val 0 4)) 'text)]
                [tmpl (make-obj-template (list frag) 'little-endian 8)]
                [result (prelink-template tmpl '())])
           (~ (car (~ result 'fragments)) 'bytes)))

  (test* "prelink-template defers typed patch with no matching param - patches" '((:val 0 4))
         (let* ([frag (make-obj-fragment #u8(0 0 0 0) '() '((:val 0 4)) 'text)]
                [tmpl (make-obj-template (list frag) 'little-endian 8)]
                [result (prelink-template tmpl '())])
           (~ (car (~ result 'fragments)) 'patches)))

  (test* "prelink-template always defers label-rel patches" '((target 0 label-rel 4))
         (let* ([frag (make-obj-fragment #u8(0 0 0 0 0)
                                         '((target . 4))
                                         '((target 0 label-rel 4))
                                         'text)]
                [tmpl (make-obj-template (list frag) 'little-endian 8)]
                ;; Even with a spurious param, label-rel is never applied by prelink
                [result (prelink-template tmpl `((:val ,<integer> 99)))])
           (~ (car (~ result 'fragments)) 'patches)))

  (test* "prelink-template applies handler patch" #xf3  ; #xf3 = movss prefix byte
         (let* ([frag (make-obj-fragment #u8(0) '() '((:pfx 0 x86_64-movs_)) 'text)]
                [tmpl (make-obj-template (list frag) 'little-endian 8)]
                [result (prelink-template tmpl '((:pfx #f movss)))])
           (u8vector-ref (~ (car (~ result 'fragments)) 'bytes) 0)))

  (test* "prelink-template preserves fragment count" 2
         (let* ([f1 (make-obj-fragment #u8(0 0) '() '() 'text)]
                [f2 (make-obj-fragment #u8(0 0) '() '() 'data)]
                [tmpl (make-obj-template (list f1 f2) 'little-endian 8)]
                [result (prelink-template tmpl '())])
           (length (~ result 'fragments))))

  (test* "prelink-template does not mutate original template" #u8(0 0 0 0)
         (let* ([frag (make-obj-fragment #u8(0 0 0 0) '() '((:val 0 4)) 'text)]
                [tmpl (make-obj-template (list frag) 'little-endian 8)])
           (prelink-template tmpl `((:val ,<integer> 42)))
           (~ (car (~ tmpl 'fragments)) 'bytes)))

  ;; Composability: prelink fills :val1, link-templates fills :val2 in the same fragment.
  (test* "prelink fills one param, link-templates fills another"
         #u8(11 0 0 0  22 0 0 0)
         (let* ([frag (make-obj-fragment #u8(0 0 0 0  0 0 0 0)
                                         '()
                                         '((:val1 0 4) (:val2 4 4))
                                         'text)]
                [tmpl (make-obj-template (list frag) 'little-endian 8)]
                [prelinked (prelink-template tmpl `((:val1 ,<integer> 11)))])
           (receive (bytes _)
               (link-templates (list prelinked) `((:val2 ,<integer> 22)))
             bytes)))

  ;; Composability: prelink fills :val early; link-templates later resolves label-rel.
  ;; text frag (8 bytes): :val at [0..3], label-rel to 'target with end-off=8 at [4..7]
  ;; data frag (4 bytes): label 'target at offset 0
  ;; Section order: text (0..7) then data (8..11), so 'target absolute = 8
  ;; label-rel disp = 8 - 8 = 0, written as s32 at bytes[4]
  (test* "prelink-template then link-templates"
         #u8(42 0 0 0  0 0 0 0  #xde #xad #xbe #xef)
         (let* ([tf (make-obj-fragment #u8(0 0 0 0  0 0 0 0)
                                       '()
                                       '((:val 0 4) (target 4 label-rel 8))
                                       'text)]
                [df (make-obj-fragment #u8(#xde #xad #xbe #xef)
                                       '((target . 0))
                                       '()
                                       'data)]
                [tmpl (make-obj-template (list tf df) 'little-endian 8)]
                [prelinked (prelink-template tmpl `((:val ,<integer> 42)))])
           (receive (bytes _)
               (link-templates (list prelinked) '())
             bytes)))
  )

;; lang.asm.x86_64 is tested in ext/lang

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
  (define c-parser (make <c-parser>))
  (define (t-type expect code)
    (test* code expect
           (let loop ((decls (c-parse-string c-parser code)))
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
  (t-type '(.function (inline) (int ()) ()) "inline int x(void);")
  (t-type '(.function (inline) (.pointer () (int ())) ()) "inline int *x(void);")

  (t-type '(.struct foo () ()) "struct foo {} x;")
  (t-type '(.struct foo () ((a (int ()))
                            (b (.pointer () (int ())))
                            (c (double ()))))
          "struct foo {int a, *b; double c;} x;")
  (t-type '(.struct #f (const volatile) ()) "const struct {} volatile x;")
  (t-type '(.union foo () ()) "union foo {} x;")

  (t-type '(.union #f () ((n (.struct #f () ((a (int ()))
                                             (b (double ())))))
                          (b (.array (u-char ()) () 16))))
          "union { struct { int a; double b; } n; unsigned char b[16];} x;")

  (t-type '(.enum e () ((X #f) (Y #f) (Z #f)))
          "enum e { X, Y, Z } x;")
  (t-type '(.enum #f (const) ((X 1) (Y 3) (Z #f)))
          "enum { X=1, Y=3, Z } const x;")
  (t-type '(.type T () (.enum e () ()))
          "typedef enum e T; T x;")
  )

;; We haven't fixed the format of the semantic value, so for now we just
;; see if it parses.
(let ()
  (define c-parser (make <c-parser>))
  (define (t-succ expect code)
    (test* code expect (boolean (c-parse-string c-parser code))))

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
