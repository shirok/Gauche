;;;
;;;  gen-unicode.scm - generate unicode-handling tables
;;;
;;;    Originally written by Shiro Kawai, 2011
;;;    Public Domain - use as you like.
;;;

;; Reads Unicode data tables and generates various source files.

;; This script can serve two operations.
;;
;; (1) Generate unicode-data.scm from Unicode character database
;;   It is only necessary when new version of Unicode is published,
;;   and the resulting unicode-data.scm is checked in to the source
;;   tree so that other developers don't need UCD.
;;
;;    gosh ./gen-unicode.scm --import <unicode-database-directory> unicode-data.scm
;;
;; (2) Generate source files from unicode-data.scm
;;   To reduce runtime overhead, unicode properties are saved in binary
;;   tables, within the following two generated source files:
;;
;;   char_attr.c                - General category and case mappings.
;;   ../ext/text/unicode_attr.h - Grapheme break, word break, normalization.
;;
;;   This is done when you build from git source tree.
;;
;;    gosh ./gen-unicode.scm --compile unicode-data.scm
;;

(use srfi-1)
(use srfi-13)
(use srfi-42)
(use text.csv)
(use util.match)
(use file.util)
(use gauche.dictionary)
(use gauche.sequence)
(use gauche.record)
(use gauche.charconv)
(use gauche.uvector)
(use text.unicode.ucd)

;; We generate four kind of lookup structures.  Each structure consists
;; of various types of tables in order to reduce the size.
;;
;; * General categories and case bits
;;
;;    This structure maps Unicode codepoint to a byte containing the
;;    following info:
;;
;;    bit 7-6 - Alphabetic and case info
;;                00 : non-alphabetic char
;;                01 : lowercase alphabetic char
;;                10 : uppercase alphabetic char
;;                11 : caseless or titlecase alphabetic char
;;    bit 5   - Reserved
;;    bit 4-0 - General category (Lu, Nd, Cc, etc.)
;;              See ucd-general-categories
;;              and the enum in src/gauche/char_attr.h
;;
;;    For U+0000 to U+1ffff, we have a single table that directly maps
;;    the codepoint to the above byte.
;;
;;      unsigned char ucs_general_category_00000[0x20000]  (131072 bytes)
;;
;;    Above U+20000 the mappings are sparse and mostly contiguous, so we
;;    generate a C code that performs a binary search.
;;
;;      unsigned char ucs_general_category_20000(ScmChar code)
;;
;; * Case mapping
;;
;;    Almost all characters that needs case mappings are below #xffff.  In
;;    Unicode 6.0, the only exceptions are 80 characters in U+104xx range
;;    (Deseret).
;;
;;    Among case-mapped characters, almost all of them requires a simple
;;    rule---that is, either a character can be lowercased by adding some
;;    small value to the codepoint, or a character can be upcased and
;;    titlecased by adding some small value to the codepoint.  These have
;;    simple casemap entries.  Characters that requires more complex handling
;;    have extended casemap entries.
;;
;;    The tables first maps a codepoint into 16bit entry.  The entry
;;    represents either a simple casemap entry, or an index to an
;;    extended casemap entry.
;;
;;    bit15 == 0:
;;      This is a simple entry.  uppercase and titlecase is the same,
;;      no special case mappings, and conversion between
;;      (upper,title) <-> lower is simply done by adding the given offset.
;;
;;      If bit14 is 0, this letter is uppercase.  Converting to uppercase and
;;                     titlecase is noop.  Converting to lowercase is to add
;;                    the offset.
;;      If bit14 is 1, this letter is lowercase.  Converting to lowercase is
;;                     noop.  Converting to uppercase and titlecase is to add
;;                     the offset.
;;      bit13-bit0, singned integer offset [-8192, 8191]
;;
;;    bit15 == 1:
;;      This is an extended entry (except #xffff).
;;      bit14-bit0 is an index to an extended entry table.
;;
;;    #xffff indicates empty entry.
;;
;;    Characters that require case mapping tend to cluster, so we use two-staged
;;    table to lookup the 16-bit entry from the codepoint below U+10000.
;;
;;    The bit 15-8 is the index of this table:
;;
;;       static unsiged char casemap_000[256]
;;
;;    If the value is 255, the character doesn't have case mappings.
;;    Otherwise, let V be the value of the above lookup, the entry
;;    can be looked up by the following table:
;;
;;       static unsiged char casemap_subtable[V][<lower 8 bit of codepoint>];
;;
;;    We only need to have 18 subtables.
;;
;; * Digit values
;;
;;    Characters with categrory Nd has associated numeric value 0..9.
;;    A set of decimal numeric characters for 0..9 are always contiguous.
;;    As of Unicode 6.2, we have 42 of such sets.
;;
;; * Break properties
;;
;;    This structure maps codepoint to the Grapheme_Cluster_Break,
;;    Word_Break and Sentence_Break properties.
;;
;;    See http://www.unicode.org/reports/tr29/
;;    http://www.unicode.org/Public/UNIDATA/auxiliary/GraphemeBreakProperty.txt
;;    http://www.unicode.org/Public/UNIDATA/auxiliary/WordBreakProperty.txt
;;    http://www.unicode.org/Public/UNIDATA/auxiliary/SentenceBreakProperty.txt
;;
;;    An entry is 8-bit, indicating the character's Word_Break and
;;    Grapheme_Break properties.  In ext/unicode_attr.h, they are
;;    prefixed with WB_ and GB_, respectively.
;;    Note that WB_CR, WB_LF, WB_Single_Quote, WB_Double_Quote,
;;    GB_CR and GB_LF values are *not* stored in the table.  Each
;;    property value is assigned to single character and we check
;;    them separately.
;;
;;    bit7-4:  Word_Break property
;;             10    CR     (not stored in the table)
;;             11    LF     (not stored in the table)
;;              0    Newline
;;              1    Extend
;;              2    Regional_Indicator
;;              3    Format
;;              4    Katakana
;;              5    Hebrew_Letter
;;              6    ALetter
;;             12    Single_Quote (not stored in the table)
;;             13    Double_Quote (not stored in the table)
;;              7    MidLetter
;;              8    MidNum
;;              9    MidNumLet
;;              a    Numeric
;;              b    ExtendNumLet
;;              c    Other
;;    bit3-0:  Graphene_Break property
;;             10    CR (not stored in the table)
;;             11    LF (not stored in the table)
;;              0    Control
;;              1    Extend
;;              2    Regional_Indicator
;;              3    Prepend
;;              4    SpacingMark
;;              5    L
;;              6    V
;;              7    T
;;              8    LV
;;              9    LVT
;;              a    Other
;;
;;   Codepoints below U+20000 are looked up by two-staged tables.
;;   First, look up this table with (codepoint >> 8).
;;
;;     static unsigned char break_table[0x200]
;;
;;   If the value is 255, both properties are 'Other'.
;;   Otherwise, the value is an index to the secondary table.
;;
;;     static unsigned char break_subtable[index][256]
;;
;;   The value of this table encodes WB and GB properties.
;;
;;   Codepoints on or above U+20000 are all 'Other', except the following
;;   ranges.  They are handled specially in the lookup procedure.
;;
;;    E0001          GB_Control, WB_Format
;;    E0020..E007F   GB_Control, WB_Format
;;    E0100..E01EF   GB_Extend, WB_Extend
;;

;;;
;;;  Main entry
;;;

(define (main args)
  (match (cdr args)
    [("--import" dir ucdfile)
     (unless (file-is-directory? dir)
       (exit 1 "Directory required, but got: ~a" dir))
     (with-output-to-file ucdfile
       (cut ucd-save-db (ucd-parse-files dir)))]
    [("--compile" ucdfile)
     (unless (file-exists? ucdfile)
       (exit 1 "Couldn't open unicode data file: ~a" ucdfile))
     (generate-tables (call-with-input-file ucdfile ucd-load-db))]
    [else
     (exit 1 "Usage:\n\
              gen-unicode.scm --import <unicode-database-dir>\n\
              gen-unicode.scm --compile <unicode-data.scm>")])
  0)

;;;
;;;  Generate source file
;;;

(define (generate-tables db)
  (with-output-to-file "char_attr.c"
    (^() (preamble db)
      (generate-category-tables db)
      (generate-case-tables db)
      (generate-digit-value-tables db)))
  (with-output-to-file "../ext/text/unicode_attr.h"
    (^() (preamble db)
      (generate-break-tables db))))

(define (preamble db)
  (print "/* Generated automatically from Unicode character database */")
  (print #"/* Unicode version ~(ucd-version db).  Do not edit. */")
  )

;; NB: The caracter category tables are generated for each supported
;; internal encodings.  The lookup function is defined in gauche/char_*.h,
;; and must be in sync with the tables generated here.
(define (generate-category-tables db)
  (define (emit-entry e)
    (if e
      (cond [(ucd-entry-uppercase e)  (format #t " U(~a)," (ucd-entry-category e))]
            [(ucd-entry-lowercase e)  (format #t " L(~a)," (ucd-entry-category e))]
            [(ucd-entry-alphabetic e) (format #t " A(~a)," (ucd-entry-category e))]
            [else                 (format #t "    ~a," (ucd-entry-category e))])
      (format #t "    Cn,")))
  (define (emit-columns start end p)
    (do-ec (:parallel (: k start end)
                      (:integers i))
           (begin
             (cond [(zero? (mod i 256)) (format #t "\n /* ~5,'0x - */\n" k)]
                   [(zero? (mod i 8))  (newline)])
             (p k)))
    (print))
  (define (ucs->entry ucs) (ucd-get-entry db ucs))

  ;; Setup
  (dolist [c (ucd-general-categories)]
    (format #t "#define ~a SCM_CHAR_CATEGORY_~a\n" c c))
  (print "#undef A")
  (print "#undef U")
  (print "#undef L")
  (print "#define A(x) ((x)|SCM_CHAR_ALPHABETIC_BITS)")
  (print "#define U(x) ((x)|SCM_CHAR_UPPERCASE_BITS)")
  (print "#define L(x) ((x)|SCM_CHAR_LOWERCASE_BITS)")

  ;; utf-8 ('none' is also here, for we treat it as latin-1.)
  (print "#if !defined(GAUCHE_CHAR_ENCODING_EUC_JP) && !defined(GAUCHE_CHAR_ENCODING_SJIS)")
  ;;   U+0000 - U+1ffff : direct table lookup
  (display "static unsigned char ucs_general_category_00000[] = {")
  (emit-columns 0 #x20000 (^i (emit-entry (ucs->entry i))))
  (print "};")
  ;;   Over U+20000 - binary search
  (print)
  (print "static unsigned char ucs_general_category_20000(ScmChar code)")
  (print "{")
  (print "  /*")
  (for-each (^e (format #t "    ~6x ~a\n" (car e) (cdr e)))
            (ucd-get-category-ranges db))
  (print "  */")
  (generate-bisect (coerce-to <vector> (ucd-get-category-ranges db))
                   (^e (format "return ~a;"
                               (case (cdr e)
                                 [(Lo) "A(Lo)"]
                                 [(#f) "Cn"]
                                 [else => identity]))))
  (print "}")
  (print "#endif /*defined(GAUCHE_CHAR_ENCODING_UTF_8)*/")
  (print)

  ;; euc-jp
  (print "#if defined(GAUCHE_CHAR_ENCODING_EUC_JP)")
  (display "static unsigned char eucjp_general_category_G0[] = {")
  (emit-columns 0 #x80 (^i (emit-entry (eucjp->ucd-entry db i))))
  (print "};")
  (display "static unsigned char eucjp_general_category_G1[] = {")
  (dotimes [z (- #xff #xa1)]
    (emit-columns (+ (ash (+ z #xa1) 8) #xa1)
                  (+ (ash (+ z #xa1) 8) #xff)
                  (^i (emit-entry (eucjp->ucd-entry db i)))))
  (print "};")
  (display "static unsigned char eucjp_general_category_G2[] = {")
  (emit-columns #x8ea1 #x8ee0 (^i (emit-entry (eucjp->ucd-entry db i))))
  (print "};")
  (display "static unsigned char eucjp_general_category_G3[] = {")
  (dotimes [z (- #xff #xa1)]
    (emit-columns (+ (ash (+ z #xa1) 8) #x8f00a1)
                  (+ (ash (+ z #xa1) 8) #x8f00ff)
                  (^i (emit-entry (eucjp->ucd-entry db i)))))
  (print "};")
  (print "#endif /*defined(GAUCHE_CHAR_ENCODING_EUC_JP)*/")
  (print)

  ;; sjis
  (print "#if defined(GAUCHE_CHAR_ENCODING_SJIS)")
  (display "static unsigned char sjis_general_category_00[] = {")
  (emit-columns 0 #x80 (^i (emit-entry (sjis->ucd-entry db i))))
  (print "};")
  (display "static unsigned char sjis_general_category_a0[] = {")
  (emit-columns #xa0 #xdf (^i (emit-entry (sjis->ucd-entry db i))))
  (print "};")
  (display "static unsigned char sjis_general_category_8000[] = {")
  (do-ec (: z #x8000 #xa000 256)
         (emit-columns (+ z #x40)
                       (+ z #xfd)
                       (^i (emit-entry (sjis->ucd-entry db i)))))
  (print "};")
  (display "static unsigned char sjis_general_category_e000[] = {")
  (do-ec (: z #xe000 #x10000 256)
         (emit-columns (+ z #x40)
                       (+ z #xfd)
                       (^i (emit-entry (sjis->ucd-entry db i)))))
  (print "};")
  (print "#endif /*defined(GAUCHE_CHAR_ENCODING_SJIS)*/")
  (print)

  ;; Teardown
  (dolist [c (ucd-general-categories)]
    (format #t "#undef ~a\n" c))
  (format #t "#undef A\n")
  (format #t "#undef U\n")
  (format #t "#undef L\n")
  (print))

(define (generate-case-tables db)
  (define subtables '())
  (define extended '())
  ;; returns a list of (code . ucd-entry-case-map) with U+HHHxx where HHH
  ;; is given to the hi arg.
  (define (gather-entries hb)
    (fold-ec '() (: lb 256) (ucd-get-entry db (+ (* hb 256) lb))
             (^(e seed) (if (or (not e) (not (ucd-entry-case-map e)))
                          seed
                          (acons (+ (* hb 256) lb) (ucd-entry-case-map e) seed)))))
  ;; ucd-entry-alist :: ((code . ucd-entry-case-map) ...)
  ;; subtable :: (start-code . #(....))
  ;; returns (start-code . subtable-number)
  (define (gen-subtable start-code ucd-entry-alist)
    (if (null? ucd-entry-alist)
      `(,start-code . 255)
      (let* ([table-num (length subtables)]
             [vec   (make-vector 256 #f)])
        (dolist [p ucd-entry-alist]
          (when (ucd-extended-case-map? (cdr p))
            (push! extended (cdr p)))
          (set! (~ vec (logand (car p) #xff)) (cdr p)))
        (push! subtables (cons start-code vec))
        (cons start-code table-num))))
  ;;
  (define (gather-toptable)
    (list-ec (: hi 256) (gen-subtable (* hi 256) (gather-entries hi))))

  ;;
  (define (emit-subtable subtable)
    (format #t "  /* ~4,'0x - ~4,'0x */\n"
            (car subtable)
            (+ (car subtable) 255))
    (format #t "  {\n")
    (dotimes [n 256]
      (format #t "    ~20a,   /* 0x~4,'0x */\n"
              (if-let1 cmap (~ (cdr subtable) n)
                (if (ucd-simple-case-map? cmap)
                  (format "~a(~a)"
                          (if (eq? (ucd-simple-case-map-case cmap) 'upper)
                            "TOLOWER"
                            "TOUPPER")
                          (ucd-simple-case-map-offset cmap))
                  (format "EXTENDED(~a)"
                          (find-index (cut eq? cmap <>) extended)))
                "NO_CASE_MAPPING")
              (+ (car subtable) n)))
    (format #t "  },\n"))

  (define (emit-subtables)
    (format #t "static unsigned short casemap_subtable[][256] = {\n")
    (for-each emit-subtable (reverse subtables))
    (format #t "};\n\n"))

  (define (emit-toptable subtable-num-alist)
    (let1 vec (make-vector 256 255)
      (dolist [s subtable-num-alist]
        (set! (~ vec (div (car s) 256)) (cdr s)))
      (format #t "static unsigned char casemap_000[] = {")
      (dotimes [i 256]
        (when (zero? (mod i 16)) (newline))
        (format #t " ~3d," (~ vec i)))
      (format #t "\n};\n")))

  (define (emit-extended-case-maps)
    (format #t "static ScmCharCaseMap extended_casemaps[] = {\n")
    (dolist [e extended]
      (let ([simple  (ucd-extended-case-map-simple-map e)]
            [special (ucd-extended-case-map-special-map e)])
        (format #t "  { ~s,~s,~s,"
                (or (~ simple 0) 0)
                (or (~ simple 1) 0)
                (or (~ simple 2) 0))
        (if special
          (format #t "~a,~a,~a"
                  (extended-special-array (~ special 0))
                  (extended-special-array (~ special 1))
                  (extended-special-array (~ special 2)))
          (format #t "{-1},{-1},{-1}"))
        (format #t "}, /* ~4,'0x */\n" (ucd-extended-case-map-code e))))
    (format #t "};\n\n"))
  (define (extended-special-array lis)
    (apply format "{~a,~a,~a,-1}" (map (^i (list-ref lis i -1)) '(0 1 2))))

  ;; body of generate-case-tables
  (print "#define NO_CASE_MAPPING SCM_CHAR_NO_CASE_MAPPING")
  (print "#define TOLOWER(x) SCM_CHAR_CASEMAP_TOLOWER(x)")
  (print "#define TOUPPER(x) SCM_CHAR_CASEMAP_TOUPPER(x)")
  (print "#define EXTENDED(x) SCM_CHAR_CASEMAP_EXTENDED(x)")
  (let1 subtable-num-alist (gather-toptable)
    (emit-extended-case-maps)
    (emit-subtables)
    (emit-toptable subtable-num-alist))
  )

(define (generate-bisect entries gen-value)
  (define (bisect lo hi indent)
    (if (= (+ lo 1) hi)
      (format #t "~v,a~a\n" (* 2 indent) " " (gen-value (~ entries lo)))
      (let1 mid (div (+ lo hi) 2)
        (format #t "~v,aif (code < 0x~x) {\n"
                (* 2 indent) " " (car (~ entries mid)))
        (bisect lo mid (+ indent 1))
        (format #t "~v,a} else {\n" (* 2 indent) " ")
        (bisect mid hi (+ indent 1))
        (format #t "~v,a}\n" (* 2 indent) " "))))
  (bisect 0 (size-of entries) 1))

;; Digit-value tables.  Note that we'll have a shortcut for the first
;; chunk [0x30, 0x39], so we only generate for the second chunk and after
(define (generate-digit-value-tables db)
  (let1 ranges ($ reverse
                  $ fold (^[p s] (match s
                                   [() `(,p (0 . #f))]
                                   [((_ . last) . rest)  
                                    (if (= (car p) last)
                                      `(,p ,@s)
                                      `(,p (,last . #f) ,@s))]))
                  '()
                  $ cdr $ (cut sort-by <> car) $ filter identity
                  $ ucd-map-entries db
                  (^[code entry]
                    (and (eq? (ucd-entry-category entry) 'Nd)
                         (zero? (ucd-entry-digit-value entry))
                         (cons code (+ code 10)))))

    (print)
    (print "static int ucs_digit_value(ScmChar code)")
    (print "{")
    (dolist [r ranges]
      (format #t "  /* ~5,'0x- ~a */\n" (car r) (if (cdr r) "Nd" "* ")))
    (generate-bisect (coerce-to <vector> ranges)
                     (^e (if (cdr e)
                           (format "return (code - 0x~x);" (car e))
                           "return -1;")))
    (print "}")))

;; Break property values
(define (generate-break-tables db)
  (define subtable-count 0)

  (define (gen-entry code)
    (let1 e (ucd-get-break-property db code)
      (if (not e)
        (format #t "    BREAK_ENTRY(GB_Other, WB_Other),\n")
        (format #t "    BREAK_ENTRY(GB_~a, WB_~a),\n"
                (let1 gp (ucd-break-property-grapheme e)
                  (if (memq gp '(CR LF))
                    'Other
                    gp))
                (let1 wp (ucd-break-property-word e)
                  (if (memq wp '(CR LF Single_Quote Double_Quote))
                    'Other
                    wp))))))

  ;; returns subtable-number
  (define (gen-subtable start-code)
    (if (any?-ec (: lb 256) (ucd-get-break-property db (+ start-code lb)))
      (rlet1 subtable-num subtable-count
        (format #t "  {\n")
        (do-ec (: lb 256) (gen-entry (+ start-code lb)))
        (format #t "  },\n")
        (inc! subtable-count))
      255))

  ;; Generate table of symbols
  (define (gen-symbol-table constants prefix)
    (receive (normals specials) (break not constants)
      (for-each-with-index (^(i c) (format #t "#define ~a_~a ~a\n" prefix c i))
                           normals)
      (for-each-with-index (^(i c) (format #t "#define ~a_~a ~a\n" prefix c
                                           (+ 16 i)))
                           (cdr specials)))
    (format #t "static void init_~a_symbols(ScmModule *mod) {\n" prefix)
    (for-each (^c
               (and c
                    (format #t
                            "Scm_DefineConst(mod, \
                          SCM_SYMBOL(SCM_INTERN(\"~a_~a\")),\
                          SCM_MAKE_INT(~a_~a));\n"
                            prefix c prefix c)))
              constants)
    (print "}"))

  (print)
  (gen-symbol-table (ucd-grapheme-break-properties) "GB")
  (gen-symbol-table (ucd-word-break-properties) "WB")
  (print)
  (print "#define BREAK_ENTRY(g, w)  (((g)<<4)|(w))")
  (print)
  (print "static unsigned char break_subtable[][256] = {")
  (let1 nlist (list-ec (: n 512) (gen-subtable (* n 256)))
    (print "};")
    (print)
    (format #t "static unsigned char break_table[] = {")
    (do-ec (:parallel (: n nlist) (:integers i))
           (begin (when (zero? (mod i 8)) (format #t "\n   "))
                  (format #t " ~3d," n)))
    (format #t "\n};\n"))
  )
