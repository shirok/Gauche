;;;
;;;  gen-unicode.scm - generate unicode-handling tables
;;;
;;;    Originally written by Shiro Kawai, 2011
;;;    Public Domain - use as you like.
;;;

;; Reads Unicode data tables and generates various source files.
;; The directory containing Unicode character database files must
;; be provided.

;; This script generates those files:
;;   char_attr.c                - General category and case mappings.
;;   ../ext/text/unicode_attr.h - Grapheme break, word break, normalization.

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
;;              See +general-categories+ constant,
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
;;    Grapheme_Break properties.
;;
;;    bit7-4:  Word_Break property
;;              0    CR
;;              1    LF
;;              2    Newline
;;              3    Extend
;;              4    Format
;;              5    Katakana
;;              6    ALetter
;;              7    MidLetter
;;              8    MidNum
;;              9    MidNumLet
;;              a    Numeric
;;              b    ExtendNumLet
;;              c    Other
;;    bit3-0:  Graphene_Break property
;;              0    CR
;;              1    LF
;;              2    Control
;;              3    Extend
;;              4    Prepend
;;              5    SpacingMark
;;              6    L
;;              7    V
;;              8    T
;;              9    LV
;;              a    LVT
;;              b    Other
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


;; UnicodeData.txt format
;;  code
;;  name
;;  category
;;  combining-class
;;  bidi-class
;;  decomposition
;;  numeric-value-1 (for decimal only)
;;  numeric-value-2 (for decimal & digits)
;;  numeric-value-3 (for decimal, digits & numeric)
;;  bidi-mirrored
;;  old-name
;;  notes
;;  simple-uppercase
;;  simple-lowercase
;;  simple-titlecase


(define-record-type unichar-db %make-unichar-db #f
  table                ; hashtable of codepoint -> entry
  ranges               ; treemap start-codepoint -> Maybe category
                       ;   only used for char category over U+20000
  break-table          ; treemap codepoint -> break-entry
  )

(define (make-unichar-db)
  (%make-unichar-db (make-hash-table 'eqv?) (make-tree-map = <)
                    (make-tree-map = <)))

(define-record-type entry %make-entry #f
  (category)
  (case-info)          ; (upper lower title) codepoints
  (special-case-info)  ; ((upper ...) (lower ...) (title ...)) codepoints
  (case-map)           ; either simple-case-map or extended-case-map
                       ;   set by assign-case-mapping
  (alphabetic)         ; #t if Alphabetic
  (uppercase)          ; #t if Uppercase
  (lowercase)          ; #t if Lowercase
  (digit-value)        ; 0..9 for Nd chars, #f otherwise
  )

(define-record-type break-entry %make-break-entry #f
  (grapheme)           ; Grapheme_Break category
  (word)               ; Word_Break category
  )

(define (make-entry category case-info digit-value)
  (%make-entry category case-info #f #f #f #f #f digit-value))

(define (make-break-entry)
  (%make-break-entry 'Other 'Other))

(define-record-type simple-case-map #t #t
  case      ; upper or lower, for this character itself (title==upper)
  offset)   ; offset to convert to the opposite case

(define-record-type extended-case-map #t #t
  code                 ; original code
  simple-map           ; (upper-off lower-off title-off) or #f
  special-map)         ; ((upper ... ) (lower ...) (title ...)) or #f

;; This order must match with the enum in src/gauche/char_attr.h
(define-constant +general-categories+
  '(Lu Ll Lt Lm Lo Mn Mc Me Nd Nl No Pc Pd Ps Pe Pi Pf Po
    Sm Sc Sk So Zs Zl Zp Cc Cf Cs Co Cn))

(define-constant +grapheme-break-categories+
  '(CR LF Control Extend Prepend SpacingMark L V T LV LVT Other))

(define-constant +word-break-categories+
  '(CR LF Newline Extend Format Katakana ALetter MidLetter MidNum MidNumLet
    Numeric ExtendNumLet Other))

(define-constant +required-files+
  '("UnicodeData.txt" "SpecialCasing.txt" "PropList.txt"
    "auxiliary/GraphemeBreakProperty.txt" "auxiliary/WordBreakProperty.txt"))

;; We don't have utf8->ucs4 yet in 0.9.1, so we need to roll our own.
(define (utf8vec->ucs4 vec)
  (let1 b0 (~ vec 0)
    (cond [(< b0 #x80) b0]
          [(< b0 #xe0) (+ (ash (logand b0 #x1f) 6)
                          (logand (~ vec 1) #x3f))]
          [(< b0 #xf0) (+ (ash (logand b0 #x0f) 12)
                          (ash (logand (~ vec 1) #x3f) 6)
                          (logand (~ vec 2) #x3f))]
          [else        (+ (ash (logand b0 #x07) 18)
                          (ash (logand (~ vec 1) #x3f) 12)
                          (ash (logand (~ vec 2) #x3f) 6)
                          (logand (~ vec 3) #x3f))])))

(define (get-char/ces vec ces)
  (utf8vec->ucs4
   (string->u8vector
    (ces-convert (u8vector->string vec) ces 'utf-8))))

(define (2b->u8vector code)
  (u8vector (ash code -8) (logand code #xff)))
(define (3b->u8vector code)
  (u8vector (ash code -16) (logand (ash code -8) #xff) (logand code #xff)))

(define (eucjp->ucs code)
  ;; NB: The codes in *jisx0213-extras* should be filtered before
  ;; calling this.
  (and-let* ([v (cond [(< code #x80) (u8vector code)]
                      [(<= #x8ea1 code #x8ef2) (2b->u8vector code)]
                      [(<= #xa1a1 code #xfefe) (2b->u8vector code)]
                      [(<= #x8fa1a1 code #x8ffefe) (3b->u8vector code)]
                      [else #f])])
    (get-char/ces v 'euc-jp)))

(define (sjis->ucs code)
  ;; NB: The codes in *jisx0213-extras* should be filtered before
  ;; calling this.
  (and-let* ([v (cond [(< code #x80)   (u8vector code)]
                      [(<= #xa0 code #xdf) (u8vector code)]
                      [(<= #x8040 code #x8ffc) (2b->u8vector code)]
                      [(<= #x9040 code #x9ffc) (2b->u8vector code)]
                      [(<= #xe040 code #xeffc) (2b->u8vector code)]
                      [(<= #xf040 code #xfffc) (2b->u8vector code)]
                      [else #f])])
    (get-char/ces v 'sjis)))

;; NB: JISX0213:2004 has 25 letters that aren't included in Unicode 6.0.
;; We define general categories of them by our own.
(define *jisx0213-extras*
  '(;; sjis   euc-jp  category
    (#x82F5   #xA4F7  Lo) ; U+304B U+309A  Kana+semi voice mark
    (#x82F6   #xA4F8  Lo) ; U+304D U+309A
    (#x82F7   #xA4F9  Lo) ; U+304F U+309A
    (#x82F8   #xA4FA  Lo) ; U+3051 U+309A
    (#x82F9   #xA4FB  Lo) ; U+3053 U+309A
    (#x8397   #xA5F7  Lo) ; U+30AB U+309A
    (#x8398   #xA5F8  Lo) ; U+30AD U+309A
    (#x8399   #xA5F9  Lo) ; U+30AF U+309A
    (#x839A   #xA5FA  Lo) ; U+30B1 U+309A
    (#x839B   #xA5FB  Lo) ; U+30B3 U+309A
    (#x839C   #xA5FC  Lo) ; U+30BB U+309A
    (#x839D   #xA5FD  Lo) ; U+30C4 U+309A
    (#x839E   #xA5FE  Lo) ; U+30C8 U+309A
    (#x83F6   #xA6F8  Lo) ; U+31F7 U+309A
    (#x8663   #xABC4  Ll) ; U+00E6 U+0300  IPA+accent
    (#x8667   #xABC8  Ll) ; U+0254 U+0300
    (#x8668   #xABC9  Ll) ; U+0254 U+0301
    (#x8669   #xABCA  Ll) ; U+028C U+0300
    (#x866A   #xABCB  Ll) ; U+028C U+0301
    (#x866B   #xABCC  Ll) ; U+0259 U+0300
    (#x866C   #xABCD  Ll) ; U+0259 U+0301
    (#x866D   #xABCE  Ll) ; U+025A U+0300
    (#x866E   #xABCF  Ll) ; U+025A U+0301
    (#x8685   #xABE5  Sk) ; U+02E9 U+02E5  Modifier
    (#x8686   #xABE6  Sk) ; U+02E5 U+02E9  Modifier
    ))

(define (xxx->entry db code get-extra-code ->ucs)
  (cond [(find (^l (eqv? code (get-extra-code l))) *jisx0213-extras*)
         => (^l (let1 cat (caddr l)
                  (rlet1 e (make-entry cat '(#f #f #f) #f)
                    (when (eq? cat 'Ll) (set! (entry-lowercase e) #t))
                    (when (eq? cat 'Lo) (set! (entry-alphabetic e) #t)))))]
        [else
         (dict-get (unichar-db-table db) (->ucs code) #f)]))

(define (eucjp->entry db code)
  (xxx->entry db code cadr eucjp->ucs))

(define (sjis->entry db code)
  (xxx->entry db code car sjis->ucs))

;;;
;;;  Main entry
;;;

(define (main args)
  (match (cdr args)
    [(dir)
     (dolist [f +required-files+]
       (unless (file-exists? (build-path dir f))
         (exit 1 "~a: Couldn't find datafile `~a' in ~a; the directory should \
                  contain these files: ~a" (car args) f dir +required-files+)))
     (generate-tables (parse-data-files dir))]
    [else
     (exit 1 "Usage: ~a <data-file-directory>" (car args))])
  0)

;;;
;;;  Parsing UnicodeData and other tables.
;;;

(define (parse-data-files datadir)
  (let1 db (unicode-data (build-path datadir "UnicodeData.txt"))
    (check-assumptions db)
    (special-case-mapping (build-path datadir "SpecialCasing.txt") db)
    (additional-case-entries (build-path datadir "PropList.txt") db)
    (assign-case-mapping db)
    (grapheme-break-property
     (build-path datadir "auxiliary/GraphemeBreakProperty.txt") db)
    (word-break-property
     (build-path datadir "auxiliary/WordBreakProperty.txt") db)
    db))

;; Process UnicodeData
(define (unicode-data file)
  (define db (make-unichar-db))
  ;; state := (prev-code prev-cat range?)
  (call-with-input-file file
    (^p (port-fold (unicode-data-1 db) '(#f #f #f)
                   (cut (make-csv-reader #\;) p))))
  db)

(define (dict-fill! dic start end ent)
  (dotimes [i (- end start)]
    (dict-put! dic (+ start i) ent)))

(define (unicode-data-1 db)
  (define table  (unichar-db-table db))
  (define ranges (unichar-db-ranges db))
  (^[entry state]
    (match-let ([(scode name scat _ _ _ snum _ _ _ _ _ sup slo sti) entry]
                [(prev-code prev-cat range?) state])
      (let* ([code (parse-code scode)]
             [range-start? (#/First>/ name)]
             [cat (string->symbol scat)]
             [case-info `(,(parse-code sup) ,(parse-code slo) ,(parse-code sti))]
             [entry (make-entry cat case-info (string->number snum))])
        ;; register table entry
        (cond
         [(not prev-code)           ; initial loop
          (dict-put! table code entry)]
         [(and range? (not (= code (+ prev-code 1)))) ;gap
          (dict-fill! table (+ prev-code 1) code entry)]
         [else
          (dict-put! table code entry)])
        ;; insert category range over U+20000
        (cond [(= code #x20000) (dict-put! ranges code cat)] ; start
              [(= code #x10fffd) (dict-put! ranges #x10fffe #f)] ;end
              [(> code #x20000)
               (cond [(not (= code (+ prev-code 1)))
                      (unless range?
                        (dict-put! ranges (+ prev-code 1) #f)
                        (dict-put! ranges code cat))]
                     [(not (eq? cat prev-cat))
                      (dict-put! ranges code cat)])])
        `(,code ,cat ,range-start?)))))

(define (check-assumptions db)
  (define table (unichar-db-table db))
  ;; Check if U+2xxxx range only has Lo and no case mappings.
  (dotimes [i #x10000]
    (let1 e (dict-get table (+ i #x20000) #f)
      (when (and e (or (not (eq? (entry-category e) 'Lo))
                       (not (equal? (entry-case-info e) '(#f #f #f)))))
        (errorf "Unicode assumption failed: U+~x ~s ~s ~s"
                (+ i #x20000)
                (entry-category e)
                (entry-case-info e)
                (entry-special-case-info e)))))
  )

(define (parse-code str) (string->number str 16))

;; Augument the db with SpecialCasing.txt.
;; We ignore entries with conditional mappings.  The only language-insensitive
;; conditional mapping is the final form of sigma:
;;  03A3; 03C2; 03A3; 03A3; Final_Sigma; # GREEK CAPITAL LETTER SIGMA
;; We handle this one explicitly.
(define (special-case-mapping file db)
  (define table (unichar-db-table db))
  (define (handle line)
    (if-let1 sp (rxmatch->string #/^([0-9a-fA-F][^#]*)/ line 1)
      (match-let1 (scode slo sti sup sconditions . _) (string-split sp #/\;\s*/)
        (when (string-null? sconditions)
          (if-let1 code (parse-code scode)
            (let ([lo (map parse-code (string-split slo #[\s]))]
                  [ti (map parse-code (string-split sti #[\s]))]
                  [up (map parse-code (string-split sup #[\s]))]
                  [e  (dict-get table code)])
              (set! (entry-special-case-info e) (list up lo ti))))))))
  (with-input-from-file file
    (cut generator-for-each handle read-line)))

;; Assign case-map slot by either simple-case-map or extended-case-map
(define (assign-case-mapping db)
  (define (handle code entry)
    (set! (entry-case-map entry)
          (if (entry-special-case-info entry)
            (make-extended-case-map code
                                    (map (^s (and s (- s code)))
                                         (entry-case-info entry))
                                    (entry-special-case-info entry))
            (match (entry-case-info entry)
              [#f #f]
              [(#f #f #f) #f]
              [(#f (? number? lower) #f) (=> fail)
               (if (<= -8192 (- lower code) 8191)
                 (make-simple-case-map 'upper (- lower code))
                 (fail))]
              [((? number? upper) #f title) (=> fail)
               (if (and (eqv? upper title)
                        (<= -8192 (- upper code) 8191))
                 (make-simple-case-map 'lower (- upper code))
                 (fail))]
              [_ (make-extended-case-map code
                                         (map (^s (and s (- s code)))
                                              (entry-case-info entry))
                                         (entry-special-case-info entry))]))))
  (hash-table-for-each (unichar-db-table db) handle))

(define (additional-case-entries file db)
  (define table (unichar-db-table db))
  (define (parse line)
    (rxmatch-case line
      [#/^([0-9A-Fa-f]+)(\.\.([0-9A-Fa-f]+))?\s+\;\s+(\w+)/ (#f s #f e p)
       (list (string->symbol p)
             (parse-code s)
             (if e (parse-code e) (parse-code s)))]
      [else #f]))
  (define (flag start end proc)
    (do-ec (: c start (+ end 1)) (set! (proc (dict-get table c)) #t)))
  (define (handle line)
    (match (parse line)
      [('Other_Alphabetic s e) (flag s e entry-alphabetic)]
      [('Other_Uppercase  s e) (flag s e entry-uppercase)]
      [('Other_Lowercase  s e) (flag s e entry-lowercase)]
      [_ #f]))

  (dict-for-each table
                 (^(k v)
                   (case (entry-category v)
                     [(Lu) (set! (entry-alphabetic v) #t)
                           (set! (entry-uppercase v) #t)]
                     [(Ll) (set! (entry-alphabetic v) #t)
                           (set! (entry-lowercase v) #t)]
                     [(Lt Lm Lo Nl) (set! (entry-alphabetic v) #t)])))
  (with-input-from-file file
    (cut generator-for-each handle read-line)))

(define (get-break-entry db code)
  (let1 table (unichar-db-break-table db)
    (or (dict-get table code #f)
        (rlet1 e (make-break-entry)
          (dict-put! table code e)))))

(define ((break-property accessor) file db)
  (define (handle line)
    (rxmatch-case line
      [#/^([0-9a-fA-F]{4,6})\.\.([0-9a-fA-F]{4,6})\s+\;\s+(\w+)/ (_ ss ee cc)
       (do-ec (: c (parse-code ss) (+ (parse-code ee) 1))
              (let1 e (get-break-entry db c)
                (set! (accessor e) (string->symbol cc))))]
      [#/^([0-9a-fA-F]{4,6})\s+\;\s+(\w+)/ (_ ss cc)
       (let1 e (get-break-entry db (parse-code ss))
         (set! (accessor e) (string->symbol cc)))]))
  (with-input-from-file file
    (cut generator-for-each handle read-line)))

(define grapheme-break-property (break-property break-entry-grapheme))
(define word-break-property     (break-property break-entry-word))

;;;
;;;  Generate source file
;;;

(define (generate-tables db)
  (with-output-to-file "char_attr.c"
    (^() (preamble)
      (generate-category-tables db)
      (generate-case-tables db)
      (generate-digit-value-tables db)))
  (with-output-to-file "../ext/text/unicode_attr.h"
    (^() (preamble)
      (generate-break-tables db))))

(define (preamble)
  (print "/* Generated automatically from Unicode character database */")
  (print "/* and src/gen-unicode.scm script.  Do not edit.           */")
  )

;; NB: The caracter category tables are generated for each supported
;; internal encodings.  The lookup function is defined in gauche/char_*.h,
;; and must be in sync with the tables generated here.
(define (generate-category-tables db)
  (define (emit-entry e)
    (if e
      (cond [(entry-uppercase e)  (format #t " U(~a)," (entry-category e))]
            [(entry-lowercase e)  (format #t " L(~a)," (entry-category e))]
            [(entry-alphabetic e) (format #t " A(~a)," (entry-category e))]
            [else                 (format #t "    ~a," (entry-category e))])
      (format #t "    ~Cn,")))
  (define (emit-columns start end p)
    (do-ec (:parallel (: k start end)
                      (:integers i))
           (begin
             (cond [(zero? (mod i 256)) (format #t "\n /* ~5,'0x - */\n" k)]
                   [(zero? (mod i 8))  (newline)])
             (p k)))
    (print))
  (define (ucs->entry ucs) (dict-get (unichar-db-table db) ucs #f))

  ;; Setup
  (dolist [c +general-categories+]
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
            (unichar-db-ranges db))
  (print "  */")
  (generate-bisect (coerce-to <vector> (unichar-db-ranges db))
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
  (emit-columns 0 #x80 (^i (emit-entry (eucjp->entry db i))))
  (print "};")
  (display "static unsigned char eucjp_general_category_G1[] = {")
  (dotimes [z (- #xff #xa1)]
    (emit-columns (+ (ash (+ z #xa1) 8) #xa1)
                  (+ (ash (+ z #xa1) 8) #xff)
                  (^i (emit-entry (eucjp->entry db i)))))
  (print "};")
  (display "static unsigned char eucjp_general_category_G2[] = {")
  (emit-columns #x8ea1 #x8ee0 (^i (emit-entry (eucjp->entry db i))))
  (print "};")
  (display "static unsigned char eucjp_general_category_G3[] = {")
  (dotimes [z (- #xff #xa1)]
    (emit-columns (+ (ash (+ z #xa1) 8) #x8f00a1)
                  (+ (ash (+ z #xa1) 8) #x8f00ff)
                  (^i (emit-entry (eucjp->entry db i)))))
  (print "};")
  (print "#endif /*defined(GAUCHE_CHAR_ENCODING_EUC_JP)*/")
  (print)

  ;; sjis
  (print "#if defined(GAUCHE_CHAR_ENCODING_SJIS)")
  (display "static unsigned char sjis_general_category_00[] = {")
  (emit-columns 0 #x80 (^i (emit-entry (sjis->entry db i))))
  (print "};")
  (display "static unsigned char sjis_general_category_a0[] = {")
  (emit-columns #xa0 #xdf (^i (emit-entry (sjis->entry db i))))
  (print "};")
  (display "static unsigned char sjis_general_category_8000[] = {")
  (do-ec (: z #x8000 #xa000 256)
         (emit-columns (+ z #x40)
                       (+ z #xfd)
                       (^i (emit-entry (sjis->entry db i)))))
  (print "};")
  (display "static unsigned char sjis_general_category_e000[] = {")
  (do-ec (: z #xe000 #x10000 256)
         (emit-columns (+ z #x40)
                       (+ z #xfd)
                       (^i (emit-entry (sjis->entry db i)))))
  (print "};")
  (print "#endif /*defined(GAUCHE_CHAR_ENCODING_SJIS)*/")
  (print)

  ;; Teardown
  (dolist [c +general-categories+]
    (format #t "#undef ~a\n" c))
  (format #t "#undef A\n")
  (format #t "#undef U\n")
  (format #t "#undef L\n")
  (print))

(define (generate-case-tables db)
  (define table (unichar-db-table db))
  (define subtables '())
  (define extended '())
  ;; returns a list of (code . entry-case-map) with U+HHHxx where HHH is given
  ;; to the hi arg.
  (define (gather-entries hb)
    (fold-ec '() (: lb 256) (dict-get table (+ (* hb 256) lb) #f)
             (^(e seed) (if (or (not e) (not (entry-case-map e)))
                          seed
                          (acons (+ (* hb 256) lb) (entry-case-map e) seed)))))
  ;; entry-alist :: ((code . entry-case-map) ...)
  ;; subtable :: (start-code . #(....))
  ;; returns (start-code . subtable-number)
  (define (gen-subtable start-code entry-alist)
    (if (null? entry-alist)
      `(,start-code . 255)
      (let* ([table-num (length subtables)]
             [vec   (make-vector 256 #f)])
        (dolist [p entry-alist]
          (when (extended-case-map? (cdr p))
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
                (if (simple-case-map? cmap)
                  (format "~a(~a)"
                          (if (eq? (simple-case-map-case cmap) 'upper)
                            "TOLOWER"
                            "TOUPPER")
                          (simple-case-map-offset cmap))
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
      (let ([simple  (extended-case-map-simple-map e)]
            [special (extended-case-map-special-map e)])
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
        (format #t "}, /* ~4,'0x */\n" (extended-case-map-code e))))
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
                  $ dict-map (unichar-db-table db)
                             (^[code entry]
                               (and (eq? (entry-category entry) 'Nd)
                                    (zero? (entry-digit-value entry))
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
  (define table (unichar-db-break-table db))
  (define subtable-count 0)

  (define (gen-entry code)
    (let1 e (dict-get table code #f)
      (if (not e)
        (format #t "    BREAK_ENTRY(GB_Other, WB_Other),\n")
        (format #t "    BREAK_ENTRY(GB_~a, WB_~a),\n"
                (break-entry-grapheme e)
                (break-entry-word e)))))

  ;; returns subtable-number
  (define (gen-subtable start-code)
    (if (any?-ec (: lb 256) (dict-get table (+ start-code lb) #f))
      (rlet1 subtable-num subtable-count
        (format #t "  {\n")
        (do-ec (: lb 256) (gen-entry (+ start-code lb)))
        (format #t "  },\n")
        (inc! subtable-count))
      255))

  ;; Generate table of symbols
  (define (gen-symbol-table constants prefix)
    (for-each-with-index (^(i c) (format #t "#define ~a_~a ~a\n" prefix c i))
                         constants)
    (format #t "static void init_~a_symbols(ScmModule *mod) {\n" prefix)
    (for-each-with-index (^(i c)
                           (format #t
                                   "Scm_DefineConst(mod, \
                                       SCM_SYMBOL(SCM_INTERN(\"~a_~a\")),\
                                       SCM_MAKE_INT(~a));\n"
                                   prefix c i))
                         constants)
    (print "}"))

  (print)
  (gen-symbol-table +grapheme-break-categories+ "GB")
  (gen-symbol-table +word-break-categories+ "WB")
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
