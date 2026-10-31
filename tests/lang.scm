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

(use lang.c.toolchain)
(test-module 'lang.c.toolchain)

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
  ;; A type can be circular through struct members.  To compare, we
  ;; omit members of a struct/union that's already being expanded.
  (define (expand type)
    (let loop ([x type] [seen '()])
      (match x
        [((and (or '.struct '.union '.enum) k) tag qs . cell)
         (if (memq cell seen)
           `(,k ,tag ,qs)
           `(,k ,tag ,qs ,(loop (car cell) (cons cell seen))))]
        [(a . d) (cons (loop a seen) (loop d seen))]
        [_ x])))
  (define (t-type expect code)
    (test* code expect
           (let loop ((decls (c-parse-string (c-toplevel-parser) code)))
             (match decls
               [(('decl 'x _ type _) . _) (expand type)]
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

  (t-type '(.array (int ()) () (const int "3")) "int x[3];")
  (t-type '(.array (int ()) () (* (const int "3") (const int "4")))
          "int x[3*4];")
  (t-type '(.array (.array (int ()) () (const int "2")) () (const int "3"))
          "int x[3][2];")
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
                          (b (.array (u-char ()) () (const int "16")))))
          "union { struct { int a; double b; } n; unsigned char b[16];} x;")

  (t-type '(.enum e () ((X #f) (Y #f) (Z #f)))
          "enum e { X, Y, Z } x;")
  (t-type '(.enum #f (const) ((X (const int "1")) (Y (const int "3")) (Z #f)))
          "enum { X=1, Y=3, Z } const x;")
  (t-type '(.enum #f () ((A (const int "0x10"))
                         (B (+ (ident A) (const int "1")))))
          "enum { A = 0x10, B = A + 1 } x;")
  (t-type '(.array (int ()) () (const int "010")) "int x[010];")
  (t-type '(.type T () (.enum e () #f))
          "typedef enum e T; T x;")

  ;; tags
  (t-type '(.struct foo () #f) "struct foo x;")
  (t-type '(.pointer () (.struct foo () ((a (int ())))))
          "struct foo *x; struct foo { int a; };")
  (t-type '(.struct node () ((v (int ()))
                             (next (.pointer () (.struct node ())))))
          "struct node { int v; struct node *next; } x;")
  (t-type '(.struct foo () ((a (int ()))))
          "struct foo { int a; }; struct foo x;")
  (t-type '(.union u () ((a (int ()))))
          "union u { int a; }; union u x;")
  (t-type '(.enum e () ((A #f))) "enum e { A }; enum e x;")
  (t-type '(.struct a () ((b (.struct b () ((c (int ())))))))
          "struct a { struct b { int c; } b; }; struct b *y; struct a x;")
  )

;; tag scoping
(let ()
  ;; Returns a list of types of declarations of identifier 'x', in order.
  (define (xtypes code)
    (let loop ([forms (c-parse-string (c-toplevel-parser) code)])
      (append-map (^f (match f
                        [('decl 'x _ type _) (list type)]
                        [('def _ _ _ ('begin . body)) (loop body)]
                        [('begin . body) (loop body)]
                        [_ '()]))
                  forms)))
  (define (eq?* x . ys) (every (cut eq? x <>) ys))
  ;; Returns the last pair of struct/union/enum, which is its identity.
  (define (tag-of type)
    (match type
      [((or '.struct '.union '.enum) _ _ . cell) cell]
      [('.pointer _ t) (tag-of t)]))

  (test* "tag: same tag, same type" #t
         (apply eq? (map tag-of (xtypes "struct foo *x; struct foo { int a; } x;"))))
  (test* "tag: anonymous structs differ" #f
         (apply eq? (map tag-of (xtypes "struct { int a; } x; struct { int a; } x;"))))
  (test* "tag: shared by declarators" #t
         (match (c-parse-string (c-toplevel-parser) "struct { int a; } x, y;")
           [(('decl _ _ t1 _) ('decl _ _ t2 _)) (eq? (tag-of t1) (tag-of t2))]))
  (test* "tag: inner scope refers outer" #t
         (apply eq? (map tag-of (xtypes "struct foo { int a; } x; \
                                         void f(void) { struct foo x; }"))))
  (test* "tag: inner definition shadows" '(#f (a) (b) (a))
         (let1 ts (xtypes "struct foo { int a; } x; \
                           void f(void) { struct foo x; \
                                          { struct foo { int b; } x; } \
                                          struct foo x; }")
           (cons (eq? (tag-of (car ts)) (tag-of (caddr ts)))
                 (map (^t (map car (car (tag-of t)))) (cdr ts)))))
  (test* "tag: struct foo; declares new one" '(#f #f #t)
         (match-let1 (t0 t1 t2)
             (xtypes "struct foo { int a; } x; \
                      void f(void) { struct foo; struct foo *x; \
                                     struct foo { int b; }; struct foo x; }")
           (list (eq? (tag-of t0) (tag-of t1))
                 (eq? (tag-of t0) (tag-of t2))
                 (eq? (tag-of t1) (tag-of t2)))))
  (test* "tag: inner reference to undeclared tag is local" #f
         (match-let1 (t0 t1)
             (xtypes "void f(void) { struct foo *x; } struct foo x;")
           (eq? (tag-of t0) (tag-of t1))))
  (test* "tag: redefinition" #f
         (guard (e [else #f])
           (c-parse-string (c-toplevel-parser)
                           "struct foo { int a; }; struct foo { int a; };")))
  (test* "tag: wrong kind" #f
         (guard (e [else #f])
           (c-parse-string (c-toplevel-parser)
                           "struct foo { int a; }; union foo x;")))
  (test* "tag: different kind in inner scope" '.union
         (car (cadr (xtypes "struct foo { int a; } x; \
                             void f(void) { union foo { int b; } x; }"))))
  (test* "tag: qualified type shares identity" #t
         (apply eq?* (map tag-of (xtypes "struct foo *x; const struct foo x; \
                                         struct foo { int a; } volatile x;"))))
  (test* "tag: write-shared/read roundtrip" '(#t 16)
         (let* ([t (car (xtypes "struct node { int v; struct node *next; } x;"))]
                [t2 (read-from-string (write-to-string t write-shared))])
           (match t2
             [('.struct 'node () . cell)
              (match (car cell)
                [(_ (_ ('.pointer _ ('.struct _ _ . cell2))))
                 (list (eq? cell cell2)
                       (~ (c-type->native-type t2) 'size))])])))
  )

;; c-type -> native-type
(let ()
  (define (type-of code)
    (let loop ((decls (c-parse-string (c-toplevel-parser) code)))
      (match decls
        [(('decl 'x _ type _) . _) type]
        [(_ . r) (loop r)])))
  ;; We only need to evaluate integer literals here.
  (define (eval-const expr)
    (match expr
      [('const 'int s) (string->number s)]
      [_ #f]))
  (define (nt code) (c-type->native-type (type-of code) eval-const))
  (define (t-prim expect code)
    (test* #"native-type: ~code" expect (nt code)))
  (define (t-shape expect code)
    (test* #"native-type: ~code" expect
           (and-let1 t (nt code)
             (list (class-name (class-of t)) (~ t'size) (~ t'alignment)))))
  (define (field-offsets t)
    (let ([names (with-module gauche.native-type c-struct/union-type-field-names)]
          [offset (with-module gauche.native-type c-struct/union-type-field-offset)])
      (map (^n (cons n (offset t n))) (names t))))

  (t-prim <c-char> "char x;")
  (t-prim <int8> "signed char x;")
  (t-prim <uint8> "unsigned char x;")
  (t-prim <short> "short x;")
  (t-prim <ushort> "unsigned short x;")
  (t-prim <int> "const int x;")
  (t-prim <uint> "unsigned x;")
  (t-prim <long> "long x;")
  (t-prim <ulong> "unsigned long x;")
  (t-prim <longlong> "long long x;")
  (t-prim <ulonglong> "unsigned long long x;")
  (t-prim <c-bool> "_Bool x;")
  (t-prim <float> "float x;")
  (t-prim <double> "double x;")
  (t-prim <longdouble> "long double x;")
  (t-prim <uint> "typedef unsigned int mode_t; mode_t x;")
  (t-prim <void*> "void *x;")
  (t-prim <void*> "const void *x;")
  (t-prim <float-complex> "float _Complex x;")
  (t-prim <double-complex> "_Complex double x;")
  (t-prim <longdouble-complex> "_Complex long double x;")

  (t-shape `(<c-pointer-type> ,(~ <void*>'size) ,(~ <void*>'alignment))
           "char *x;")
  (test* "native-type: char *x; pointee" <c-char>
         (~ (nt "char *x;")'pointee-type))
  (t-shape `(<c-pointer-type> ,(~ <void*>'size) ,(~ <void*>'alignment))
           "struct foo *x;")
  (t-shape '(<c-array-type> 24 4) "int x[3][2];")
  (test* "native-type: int x[3][2]; dimensions" '(3 2)
         (~ (nt "int x[3][2];")'dimensions))
  (test* "native-type: int x[]; dimensions" '(*)
         (~ (nt "int x[];")'dimensions))
  (t-prim #f "int x[N];")
  (t-shape '(<c-struct-type> 24 8) "struct s { char a; int b; double c[2]; } x;")
  (test* "native-type: struct field offsets" '((a . 0) (b . 4) (c . 8))
         (field-offsets (nt "struct s { char a; int b; double c[2]; } x;")))
  (t-shape '(<c-union-type> 8 8) "union u { char a; int b; double c; } x;")
  (t-prim #f "struct foo x;")             ;incomplete
  (t-shape '(<c-struct-type> 16 8)
           "struct node { int v; struct node *next; } x;")
  (t-shape '(<c-struct-type> 4 4)
           "struct foo *y; struct foo { int a; }; struct foo x;")
  (t-prim #f "struct s { int a : 3; } x;") ;bit field
  (t-shape `(<c-struct-type> ,(~ <longdouble>'size) ,(~ <longdouble>'alignment))
           "struct s { long double a; } x;")
  (test* "native-type: function" '(<int> (<int> <c-char*>) #t)
         (let1 t (nt "int x(int, char *, ...);")
           (list (~ t'return-type'name)
                 (map (cut ~ <>'name) (~ t'argument-types))
                 (~ t'variadic?))))
  (test* "native-type: function (void)" '(() #f)
         (let1 t (nt "int x(void);")
           (list (~ t'argument-types) (~ t'variadic?))))
  (test* "native-type: function ()" '(() #t)
         (let1 t (nt "int x();")
           (list (~ t'argument-types) (~ t'variadic?))))
  (t-shape '(<c-enum-type> 4 4) "enum e { A, B = 5, C } x;")
  (test* "native-type: enumerators" '((A . 0) (B . 5) (C . 6))
         (~ (nt "enum e { A, B = 5, C } x;")'enumerator-alist))
  (t-shape '(<c-enum-type> 4 4) "enum e x;") ;incomplete
  (t-prim #f "enum e { A = N } x;")
  )

;; We haven't fixed the format of the semantic value, so for now we just
;; see if it parses.
(let ()
  (define (t-succ expect code)
    (test* code expect (boolean (c-parse-string (c-toplevel-parser) code))))

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

;; Binary operators: operand order and associativity
(let ()
  (define (t-expr expect code)
    (test* code expect (c-parse-string (c-expression-parser) code)))

  (t-expr '(- (ident a) (ident b)) "a - b")
  (t-expr '(- (- (ident a) (ident b)) (ident c)) "a - b - c")
  (t-expr '(% (/ (* (ident a) (ident b)) (ident c)) (ident d))
          "a * b / c % d")
  (t-expr '(- (+ (ident a) (* (ident b) (ident c))) (ident d))
          "a + b * c - d")
  (t-expr '(>> (<< (ident a) (ident b)) (ident c)) "a << b >> c")
  (t-expr '(> (ident z) (const int "0")) "z > 0")
  (t-expr '(!= (== (ident a) (ident b)) (ident c)) "a == b != c")
  )

;; Function definition.
(let ()
  (test* "function definition, ANSI style"
         '(def f #f
               (.function () (int ())
                          ((i (int ()))
                           (j (double ()))))
               (begin (return (ident i))))
         (car (c-parse-string (c-toplevel-parser)
                              "int f(int i, double j) { return i; }")))
  (test* "function definition, K&R style"
         '(def f #f
               (.function () (int ())
                          ((i (int ()))
                           (j (double ()))))
               (begin (return (ident i))))
         (car (c-parse-string (c-toplevel-parser)
                              "int f(i, j) int i; double j; { return i; }")))
  )

(use lang.c.grokker)
(test-module 'lang.c.grokker)

(let ()
  (define input
    (string-join
     '("int i, j=1;"
       "static int k;"
       "extern double d;"
       "int f(int c, char **v) {"
       " return c;"
       "}")))

  ;; Note that grokker API is still undetermined.  The tests will evolve
  ;; as the code evolves.
  (test* "grokker global names" '(d f i j k)
         (let1 ctx (c-grok-string input)
           (sort (hash-table-keys (~ ctx'globals)))))
  )

(let ()
  (define (t expected init)
    (test* #"initial value: ~init" expected
           (let1 ctx (c-grok-string #"const long x = ~|init|;")
             (c-variable-initial-value ctx 'x))))
  ;; literals
  (t 22 "22")
  (t 0 "0")
  (t 64 "0100")
  (t #x5413 "0x5413")
  (t #xff "0XFFu")
  (t 1 "1L")
  (t 18446744073709551615 "18446744073709551615UL")
  (t #x7fffffffffffffff "0x7fffffffffffffffLL")
  (t 1.5 "1.5f")
  (t 0.5 ".5")
  (t 100.0 "1e2")
  (t 12.0 "0x1.8p3")
  (t 1.0 "0x.8p1")
  (t 0.25 "0x1p-2")
  (t 12.0 "0X1.8P+3L")
  ;; operators
  (t 448 "(0400|0200|0100)")
  (t 2 "7 & 3 & 2")
  (t 5 "1 ^ 2 ^ 6")
  (t -9223372036854775808 "(-0x7fffffffffffffffL - 1L)")
  (t 3 "7 / 2")
  (t -3 "-7 / 2")
  (t 3.5 "7.0 / 2")
  (t 1 "7 % 3")
  (t -1 "-7 % 3")
  (t 16 "1 << 4")
  (t 1 "16 >> 4")
  (t -1 "~0")
  (t 1 "!0")
  (t 0 "1 && 2 && 0")
  (t 1 "1 && 2 && 3")
  (t 1 "0 || 0 || 5")
  (t 0 "0 || 0")
  ;; references
  (test* "initial value: reference" 3
         (let1 ctx (c-grok-string "const int y = 1; const int x = y + 2;")
           (c-variable-initial-value ctx 'x)))
  ;; nonexistent variable, or no initializer
  (test* "initial value: no such variable" #f
         (let1 ctx (c-grok-string "const int x = 1;")
           (c-variable-initial-value ctx 'y)))
  (test* "initial value: no initializer" #f
         (let1 ctx (c-grok-string "extern const int x;")
           (c-variable-initial-value ctx 'x)))
  ;; unevaluatable
  (let ()
    (define (t-fail expected init)
      (let1 ctx (c-grok-string #"const int y; const int x = ~|init|;")
        (test* #"initial value failure: ~init" expected
               ($ c-variable-initial-value ctx 'x
                  :on-failure (^e (list (~ e'initializer)
                                        (~ e'expression)))))
        (test* #"initial value failure: ~init (:on-failure #f)" #f
               (c-variable-initial-value ctx 'x :on-failure #f))
        (test* #"initial value failure: ~init (:on-failure :error)"
               (test-error <c-unevaluatable-initializer>)
               (c-variable-initial-value ctx 'x :on-failure :error))
        (test* #"initial value failure: ~init (default)"
               (test-error <c-unevaluatable-initializer>)
               (c-variable-initial-value ctx 'x))))
    (t-fail '((- (ident undefined_id)) (ident undefined_id))
            "- undefined_id")
    (t-fail '((- (ident undefined_id) (const int "1")) (ident undefined_id))
            "undefined_id - 1")
    (t-fail '((logior (const int "1") (ident undefined_id) (const int "2"))
              (ident undefined_id))
            "1 | undefined_id | 2")
    (t-fail '((+ (ident y) (const int "1")) (ident y))
            "y + 1")
    (t-fail '((/ (const int "1") (const int "0"))
              (/ (const int "1") (const int "0")))
            "1 / 0")
    (t-fail '((<< (const double "1.0") (const int "2"))
              (<< (const double "1.0") (const int "2")))
            "1.0 << 2"))
  (test* "initial value: short-circuit" 0
         (let1 ctx (c-grok-string "const int x = 0 && undefined_id;")
           (c-variable-initial-value ctx 'x)))
  (test* "initial value: nested failure" '(ident undefined_id)
         (let1 ctx (c-grok-string
                    "const int y = undefined_id; const int x = y + 1;")
           ($ c-variable-initial-value ctx 'x
              :on-failure (cut ~ <>'expression))))
  (test* "initial value: invalid on-failure" (test-error)
         (let1 ctx (c-grok-string "const int x = undefined_id;")
           (c-variable-initial-value ctx 'x :on-failure 'oops)))
  )

(test-end)
