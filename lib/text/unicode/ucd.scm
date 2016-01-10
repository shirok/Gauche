;;;
;;;  text.unicode.ucd - Unicode character database library
;;;
;;;    Originally written by Shiro Kawai, 2011
;;;    Public Domain - use as you like.
;;;

;; Read Unicode data tables and generates Scheme data structures
;; for ease of use.
;; This data structures are not intended to be lightweight for
;; in-application use; they are intermediate format to generate
;; more efficient structure suitable for applications.

(define-module text.unicode.ucd
  (use gauche.record)
  (use gauche.dictionary)
  (use gauche.uvector)
  (use gauche.charconv)
  (use gauche.generator)
  (use srfi-13)
  (use srfi-42)
  (use util.match)
  (use file.util)
  (use text.csv)
  (export ucd-parse-files
          ucd-get-entry ucd-get-category-ranges ucd-get-break-property
          ucd-map-entries

          ucd-save-db ucd-load-db ucd-version

          ucd-entry ucd-entry-category
          ucd-entry-case-info ucd-entry-special-case-info
          ucd-entry-case-map ucd-entry-alphabetic
          ucd-entry-uppercase ucd-entry-lowercase
          ucd-entry-digit-value

          ucd-simple-case-map?
          ucd-simple-case-map-case
          ucd-simple-case-map-offset
          ucd-extended-case-map?
          ucd-extended-case-map-simple-map
          ucd-extended-case-map-special-map
          ucd-extended-case-map-code
          
          ucd-break-property
          ucd-break-property-grapheme
          ucd-break-property-word

          ucd-general-categories
          ucd-grapheme-break-properties
          ucd-word-break-properties

          eucjp->ucd-entry sjis->ucd-entry
          )
  )
(select-module text.unicode.ucd)

;;;
;;; The database record
;;;

;; We can't simply have codepoint -> entry table for all the codepoints,
;; since it'll be too huge.  We use the fact that the region above
;; U+20000 has many consecutive characters that shares some of the
;; properties.
;;
;; The slots of unichar-db should be considered private.  They may be
;; changed in future versions.

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
  ;; Unicode version, in string, e.g. "6.0.3"
  (version)
  ;; codepoint -> ucd-entry
  ;; The 'table' maps characters below U+20000 to the ucd-entry record.
  ;; It is a hashtable keyed by codepoint.
  table
  ;; start-codepoint -> Maybe category
  ;; This is only used for chars over U+20000.
  ranges
  ;; codepoint -> ucd-break-property
  break-table
  )

(define (make-unichar-db)
  (%make-unichar-db #f
                    (make-hash-table 'eqv?)
                    (make-hash-table 'eqv?)
                    (make-hash-table 'eqv?)))

;; equal? method is mainly for testing
(define-method object-equal? ((a unichar-db) (b unichar-db))
  (define (dict-equal? ta tb)
    (and (= (size-of ta) (size-of tb))
         (every (^k (equal? (dict-get ta k) (dict-get tb k)))
                (dict-keys ta))))
  (and (dict-equal? (unichar-db-table a) (unichar-db-table b))
       (dict-equal? (unichar-db-ranges a) (unichar-db-ranges b))
       (dict-equal? (unichar-db-break-table a) (unichar-db-break-table b))))

(define-record-type ucd-entry %make-ucd-entry #f
  (category)
  (case-info)          ; (upper lower title) codepoints
  (special-case-info)  ; ((upper ...) (lower ...) (title ...)) codepoints
  (case-map)           ; either ucd-simple-case-map or ucd-extended-case-map
                       ;   set by assign-case-mapping
  (alphabetic)         ; #t if Alphabetic
  (uppercase)          ; #t if Uppercase
  (lowercase)          ; #t if Lowercase
  (digit-value)        ; 0..9 for Nd chars, #f otherwise
  )

(define-method object-equal? ((a ucd-entry) (b ucd-entry))
  (and (eq? (ucd-entry-category a) (ucd-entry-category b))
       (equal? (ucd-entry-case-map a) (ucd-entry-case-map b))
       (eq? (ucd-entry-alphabetic a) (ucd-entry-alphabetic b))
       (eq? (ucd-entry-uppercase a) (ucd-entry-uppercase b))
       (eq? (ucd-entry-lowercase a) (ucd-entry-lowercase b))
       (eq? (ucd-entry-digit-value a) (ucd-entry-digit-value b))))

(define-record-type ucd-break-property %make-ucd-break-property #f
  (grapheme)           ; Grapheme_Break category
  (word)               ; Word_Break category
  )

(define-method object-equal? ((a ucd-break-property) (b ucd-break-property))
  (and (eq? (ucd-break-property-grapheme a) (ucd-break-property-grapheme b))
       (eq? (ucd-break-property-word a) (ucd-break-property-word b))))

(define (make-ucd-entry category case-info digit-value)
  (%make-ucd-entry category case-info #f #f #f #f #f digit-value))

(define (make-ucd-break-property)
  (%make-ucd-break-property 'Other 'Other))

;; API
(define (ucd-get-entry db ucs)
  (dict-get (unichar-db-table db) ucs #f))

;; API
;; Call (proc code ucd-entry) for all entries in unichar-db-table
;; NB: The db table only contains entries up to U+20000.  Higher entries
;; has to be dealt with separately.
(define (ucd-map-entries db proc)
  (dict-map (unichar-db-table db) proc))

;; API
;; ((<start-codepoint> . <category-symbol>) ...)
(define (ucd-get-category-ranges db)
  (sort (coerce-to <list> (unichar-db-ranges db)) < car))

;; API
;; return break property
(define (ucd-get-break-property db code)
  (dict-get (unichar-db-break-table db) code #f))

(define-record-type ucd-simple-case-map #t #t
  case      ; upper or lower, for this character itself (title==upper)
  offset)   ; offset to convert to the opposite case

(define-method object-equal? ((a ucd-simple-case-map)
                              (b ucd-simple-case-map))
  (and (eq? (ucd-simple-case-map-case a) (ucd-simple-case-map-case b))
       (eqv? (ucd-simple-case-map-offset a) (ucd-simple-case-map-offset b))))

(define-record-type ucd-extended-case-map #t #t
  code                 ; original code
  simple-map           ; (upper-off lower-off title-off) or #f
  special-map)         ; ((upper ... ) (lower ...) (title ...)) or #f

(define-method object-equal? ((a ucd-extended-case-map)
                              (b ucd-extended-case-map))
  (and (eqv? (ucd-extended-case-map-code a)
             (ucd-extended-case-map-code b))
       (equal? (ucd-extended-case-map-simple-map a)
               (ucd-extended-case-map-simple-map b))
       (equal? (ucd-extended-case-map-special-map a)
               (ucd-extended-case-map-special-map b))))

;; This order must match with the enum in src/gauche/char_attr.h
(define (ucd-general-categories)
  '(Lu Ll Lt Lm Lo Mn Mc Me Nd Nl No Pc Pd Ps Pe Pi Pf Po
    Sm Sc Sk So Zs Zl Zp Cc Cf Cs Co Cn))

;; Break properties.  NB: In order to squeeze grapheme and word break
;; properties into a single byte, we treat GB_CR, GB_LF, WB_CR, WB_LF,
;; WB_Single_Quote and WB_Double_Quote specially; they have precisely
;; one character each, so the lookup procedure recognizes those characters
;; directly instead of table lookup.  We don't store those properties
;; in the table.  The '#f' in the list marks boundary of these propetries.
(define (ucd-grapheme-break-properties)
  '(Control Extend Regional_Indicator Prepend SpacingMark
    L V T LV LVT Other #f CR LF))

(define (ucd-word-break-properties)
  '(Newline Extend Regional_Indicator Format Katakana Hebrew_Letter ALetter
    MidLetter MidNum MidNumLet Numeric ExtendNumLet Other
    #f CR LF Single_Quote Double_Quote))

;;
;; CES encoding conversions.
;;

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
                  (rlet1 e (make-ucd-entry cat '(#f #f #f) #f)
                    (when (eq? cat 'Ll) (set! (ucd-entry-lowercase e) #t))
                    (when (eq? cat 'Lo) (set! (ucd-entry-alphabetic e) #t)))))]
        [else
         (dict-get (unichar-db-table db) (->ucs code) #f)]))

;; The following APIs are needed since we have to handle jisx0213 extra
;; characters.

;; API
(define (eucjp->ucd-entry db code)
  (xxx->entry db code cadr eucjp->ucs))

;; API
(define (sjis->ucd-entry db code)
  (xxx->entry db code car sjis->ucs))

;;;
;;;  Parsing UnicodeData and other tables.
;;;

(define (ucd-parse-files datadir)
  (check-data-files datadir)
  (let1 db (unicode-data (build-path datadir "UnicodeData.txt"))
    (check-assumptions db)
    (set-unicode-version! db (build-path datadir "SpecialCasing.txt"))
    (special-case-mapping (build-path datadir "SpecialCasing.txt") db)
    (additional-case-entries (build-path datadir "PropList.txt") db)
    (assign-case-mapping db)
    (grapheme-break-property
     (ucd-grapheme-break-properties)
     (build-path datadir "auxiliary/GraphemeBreakProperty.txt") db)
    (word-break-property
     (ucd-word-break-properties)
     (build-path datadir "auxiliary/WordBreakProperty.txt") db)
    db))

(define-constant +required-files+
  '("UnicodeData.txt" "SpecialCasing.txt" "PropList.txt"
    "auxiliary/GraphemeBreakProperty.txt" "auxiliary/WordBreakProperty.txt"))

(define (check-data-files dir)
  (dolist [f +required-files+ #t]
    (unless (file-exists? (build-path dir f))
      (errorf "Couldn't find datafile `~a' in ~a; the directory should \
               contain these files: ~a" f dir +required-files+))))

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
             [entry (make-ucd-entry cat case-info (string->number snum))])
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
      (when (and e (or (not (eq? (ucd-entry-category e) 'Lo))
                       (not (equal? (ucd-entry-case-info e) '(#f #f #f)))))
        (errorf "Unicode assumption failed: U+~x ~s ~s ~s"
                (+ i #x20000)
                (ucd-entry-category e)
                (ucd-entry-case-info e)
                (ucd-entry-special-case-info e)))))
  )

(define (parse-code str) (string->number str 16))

;; Get Unicode version number from a data file
;; UCD data files (except UnicodeData.txt) begins with a comment line
;; like this:
;;  # FileName-X.X.X.txt
;; where the actual file name is FileName.txt, and X.X.X is the version
;; number.   We use it to determine unicode version heuristically.
(define (get-unicode-version file)
  (rxmatch-let (#/^(\w+)\.txt$/ (sys-basename file)) [_ base]
    (let* ([rx (string->regexp #"~|base|-(\\d+\\.\\d+\\.\\d+)\\.txt")]
           [line (with-input-from-file file read-line)]
           [m (rx line)])
      (and m (m 1)))))

;; API
(define (ucd-version db) (unichar-db-version db))

(define (set-unicode-version! db . files)
  (set! (unichar-db-version db) (any get-unicode-version files)))


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
              (set! (ucd-entry-special-case-info e) (list up lo ti))))))))
  (with-input-from-file file
    (cut generator-for-each handle read-line)))

;; Assign case-map slot by either simple-case-map or extended-case-map
(define (assign-case-mapping db)
  (define (handle code entry)
    (set! (ucd-entry-case-map entry)
          (if (ucd-entry-special-case-info entry)
            (make-ucd-extended-case-map code
                                        (map (^s (and s (- s code)))
                                             (ucd-entry-case-info entry))
                                        (ucd-entry-special-case-info entry))
            (match (ucd-entry-case-info entry)
              [#f #f]
              [(#f #f #f) #f]
              [(#f (? number? lower) #f) (=> fail)
               (if (<= -8192 (- lower code) 8191)
                 (make-ucd-simple-case-map 'upper (- lower code))
                 (fail))]
              [((? number? upper) #f title) (=> fail)
               (if (and (eqv? upper title)
                        (<= -8192 (- upper code) 8191))
                 (make-ucd-simple-case-map 'lower (- upper code))
                 (fail))]
              [_ ($ make-ucd-extended-case-map code
                    (map (^s (and s (- s code)))
                         (ucd-entry-case-info entry))
                    (ucd-entry-special-case-info entry))]))))
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
      [('Other_Alphabetic s e) (flag s e ucd-entry-alphabetic)]
      [('Other_Uppercase  s e) (flag s e ucd-entry-uppercase)]
      [('Other_Lowercase  s e) (flag s e ucd-entry-lowercase)]
      [_ #f]))

  (dict-for-each table
                 (^(k v)
                   (case (ucd-entry-category v)
                     [(Lu) (set! (ucd-entry-alphabetic v) #t)
                           (set! (ucd-entry-uppercase v) #t)]
                     [(Ll) (set! (ucd-entry-alphabetic v) #t)
                           (set! (ucd-entry-lowercase v) #t)]
                     [(Lt Lm Lo Nl) (set! (ucd-entry-alphabetic v) #t)])))
  (with-input-from-file file
    (cut generator-for-each handle read-line)))

(define (ensure-ucd-break-property db code)
  (let1 table (unichar-db-break-table db)
    (or (dict-get table code #f)
        (rlet1 e (make-ucd-break-property)
          (dict-put! table code e)))))

(define ((break-property accessor) symbols file db)
  (define (check str)
    (rlet1 sym (string->symbol str)
      (unless (memq sym symbols)
        (errorf "Unrecognized break property `~a'.  This must be a newer \
                 version of UnicodeData.  The code needs to be updated." sym))))
  (define (handle line)
    (rxmatch-case line
      [#/^([0-9a-fA-F]{4,6})\.\.([0-9a-fA-F]{4,6})\s+\;\s+(\w+)/ (_ ss ee cc)
       (do-ec (: c (parse-code ss) (+ (parse-code ee) 1))
              (let1 e (ensure-ucd-break-property db c)
                (set! (accessor e) (check cc))))]
      [#/^([0-9a-fA-F]{4,6})\s+\;\s+(\w+)/ (_ ss cc)
       (let1 e (ensure-ucd-break-property db (parse-code ss))
         (set! (accessor e) (check cc)))]))
  (with-input-from-file file
    (cut generator-for-each handle read-line)))

(define grapheme-break-property (break-property ucd-break-property-grapheme))
(define word-break-property     (break-property ucd-break-property-word))

;;;
;;;  Dumping and restoring database
;;;

;; Dump format should be regarded as internal - we may change it at any
;; tiem as we like.

;; For now, we dump it in three parts, correspoinding to the unichar-db
;; slots.  To suppress the size of the dump, we use simple run-length
;; compressing at the S-expr level.
;;
;; <RLE-compressed-list> : <RLE-compressed-entry> ...
;; <RLE-compressed-entry> : <entry>
;;                        | (rep <count> <entry> ...)
;;
;; 1. The low codepoint table part
;;
;;  RLE compressed list of <ucd-entry> 
;;
;;  <ucd-entry>  : <basic-entry>
;;  <basic-entry> : #f ; unassigned
;;               | (<category> [<case-map>] [A] [L] [U] [<digit>])
;;  <category>   : <symbol>  ; general category symbol
;;  <case-map>   | ( {upper|lower} <offset> )   ; for simple-case-map
;;               | (<code> <simple-map-info> <extended-map-info>)
;;  <simple-map-info> : #f
;;                    | (<maybe-offset> <maybe-offset> <maybe-offset>)
;;  <extended-map-info> : #f
;;                      | ((<code> ...) (<code> ...) (<code> ...))
;;
;; 2. The upper category ranges
;;
;;  List of (<start-code> <category>).
;;
;; 3. Break properties
;;
;;  RLE compressed list of (<grapheme-break-property> <word-break-property>).
;;


(define (ucd-save-db db)
  (define (convert-case-map cm)
    (cond [(ucd-simple-case-map? cm)
           `(,(ucd-simple-case-map-case cm)
             ,(ucd-simple-case-map-offset cm))]
          [(ucd-extended-case-map? cm)
           `(,(ucd-extended-case-map-code cm)
             ,(ucd-extended-case-map-simple-map cm)
             ,(ucd-extended-case-map-special-map cm))]
          [else #f]))
  (define (format-ucd-entry e)
    (and e
         `(,(ucd-entry-category e)
           ,@(cond-list
              [(convert-case-map (ucd-entry-case-map e))]
              [(ucd-entry-alphabetic e) 'A]
              [(ucd-entry-uppercase e) 'U]
              [(ucd-entry-lowercase e) 'L]
              [(ucd-entry-digit-value e)]))))
  (define (format-break-property-entry e)
    (and e
         `(,(ucd-break-property-grapheme e)
           ,(ucd-break-property-word e))))

  (print ";; Generated by text.unicode.ucd from Unicode Character Database")
  (print ";; unicode version")
  (write (unichar-db-version db))
  (print)
  
  (print ";; unichar-db-table")
  (print "(")
  (let1 tab (unichar-db-table db)
    ($ generator-for-each print
       $ rle-compressing-generator
       $ gmap (^c (format-ucd-entry (dict-get tab c #f)))
       $ giota $ + 1 $ apply max $ dict-keys tab))
  (print ")")

  (print ";; unichar-db-high-ranges")
  (print "(")
  (let1 tab (unichar-db-ranges db)
    (dolist [k (sort (dict-keys tab))]
      (print (list k (dict-get tab k)))))
  (print ")")

  (print ";; unichar-db-break-table")
  (print "(")
  (let1 tab (unichar-db-break-table db)
    ($ generator-for-each print
       $ rle-compressing-generator
       $ gmap (^c (format-break-property-entry (dict-get tab c #f)))
       $ giota $ + 1 $ apply max $ dict-keys tab))
  (print ")")
  )

(define (ucd-load-db port)
  (rlet1 db (make-unichar-db)

    (define (recover-case-map e)
      (match e
        [#f #f]
        [(case offset) (make-ucd-simple-case-map case offset)]
        [(code simple ext) (make-ucd-extended-case-map code simple ext)]))

    (define (add-entry! e code)
      (and e
           (receive (category case-map flags)
               (match e
                 [(c) (values c #f '())]
                 [(c (? pair? m) . flags) (values c m flags)]
                 [(c . flags) (values c #f flags)])
             (hash-table-put! (unichar-db-table db)
                              code
                              (%make-ucd-entry category #f #f
                                               (recover-case-map case-map)
                                               (boolean (memq 'A flags))
                                               (boolean (memq 'U flags))
                                               (boolean (memq 'L flags))
                                               (find number? flags))))))

    (define (add-break-property! e code)
      (match e
        [#f #f]
        [(g w) (let1 r (ensure-ucd-break-property db code)
                 (ucd-break-property-grapheme-set! r g)
                 (ucd-break-property-word-set! r w))]))

    ;; version
    (set! (unichar-db-version db) (read port))
    ;; ucd-entries
    (let1 entries (read port)
      (generator-for-each add-entry!
                          ($ rle-decompressiong-generator
                             $ list->generator entries)
                          (giota)))
    ;; high ranges
    (dolist [range (read port)]
      (match-let1 (start val) range
        (dict-put! (unichar-db-ranges db) start val)))
    ;; break properties
    (let1 break-properties (read port)
      (generator-for-each add-break-property!
                          ($ rle-decompressiong-generator
                             $ list->generator break-properties)
                          (giota)))
    ))

;; Run-length compressor/decompressor
(define (rle-compressing-generator input-gen)
  (define (compress in seed)
    (match seed
      [#f                  ; initial state
       (values '() `(,in 0))]
      [(i0 c)              ; may be repeating 1 item
       (cond [(equal? in i0) (values '() `(,i0 ,(+ c 1)))] ;repeat
             [(zero? c) (values '() `(,i0 ,in 0))] ; may be repeating 2
             [else (values `((rep ,(+ c 1) ,i0)) `(,in 0))])]
      [(i0 i1 c)
       (cond [(equal? in i0) (values '() `(,i0 ,i1 ,c 1))] ; may be repeating 2
             [(equal? in i1)
              (if (zero? c)
                (values `(,i0) `(,i1 1))
                (values `((rep ,(+ c 1) ,i0 ,i1)) `(,in 0)))]
             [(zero? c) (values `(,i0) `(,i1 ,in 0))]
             [else (values `((rep ,(+ c 1) ,i0 ,i1)) `(,in 0))])]
      [(i0 i1 c 1) ; we saw previous input == i0
       (cond [(equal? in i1) (values '() `(,i0 ,i1 ,(+ c 1)))] ; repeating
             [(equal? in i0) ; we saw [i0 i1 i0 i0]
              (if (zero? c)
                (values `(,i0 ,i1) `(,i0 1))
                (values `((rep ,(+ c 1) ,i0 ,i1)) `(,i0 1)))]
             [(zero? c) (values `(,i0 ,i1) `(,i0 ,in 0))]
             [else (values `((rep ,(+ c 1) ,i0 ,i1)) `(,i0 ,in 0))])]
      ))
  (define (finish seed)
    (match seed
      [#f '()]
      [(i0 0) `(,i0)]
      [(i0 c) `((rep ,(+ c 1) ,i0))]
      [(i0 i1 0) `(,i0 ,i1)]
      [(i0 i1 c) `((rep ,(+ c 1) ,i0 ,i1))]
      [(i0 i1 c 1) `((rep ,(+ c 1) ,i0 ,i1) ,i0)]))
  (gbuffer-filter compress #f input-gen finish))

(define (rle-decompressiong-generator input-gen)
  ;; Compressed a -> Generator a
  (define (entry->gen entry)
    (match entry
      [('rep c . items) (gflatten (gmap (^_ items) (giota c)))]
      [item (list->generator (list item))]))

  ;; TRANSIENT: gflatten is in gauche.generator, but it was added after
  ;; 0.9.4 release and we need our own definition in order to build
  ;; 0.9.5 with 0.9.4.  Remove this after 0.9.5 release.
  (define (gflatten gen)
    (let ([current '()])
      (rec (g)
        (cond [(eof-object? current) current]
              [(pair? current) (pop! current)]
              [else (set! current (gen)) (g)]))))
  
  ($ gconcatenate $ gmap entry->gen input-gen))
