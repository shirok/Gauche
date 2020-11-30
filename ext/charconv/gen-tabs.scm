;;;
;;;  Generating conversion state machine
;;;

;; For now, we just generate conversion matrix and some other tables.
;; Eventually, we'll define DSL from which convertion C routines are generated.

(use gauche.cgen)
(use gauche.mop.instance-pool)
(use srfi-42)
(use util.match)

;;
;; Encoding Scheme
;;

(define-class <encoding-scheme> (<instance-pool-mixin>)
  ((name :init-keyword :name)           ;symbol, short Scheme name
   (c-enum :init-keyword :c-enum)       ;symbol, C-enum name
   (aliases :init-keyword :aliases)     ;list of strings, alias names.
                                        ; we ignore '_' and '-' for match.
   (reset :init-keyword :reset          ;string name of c reset procedure
          :init-value #f)               ; reset proedure is to reset the
                                        ; output state.
   (states :init-keyword :states        ;If stateful encoding, list of states
           :init-value '())             ; (suitable for C enums)
   (initial-state :init-keyword :initial-state
                  :init-value #f)
   ))
(define-method write-object ((obj <encoding-scheme>) port)
  (format port "#<encoding ~a>" (~ obj'name)))

(define-syntax define-encoding-scheme 
  (syntax-rules ()
    [(_ name c-enum aliases . opts)
     (define name (make <encoding-scheme>
                    :name 'name
                    :c-enum 'c-enum
                    :aliases 'aliases
                    . opts))]))

(define (encoding-schemes) (instance-pool->list <encoding-scheme>))

(define (find-encoding-scheme name)
  (instance-pool-find <encoding-scheme> (^e (eq? name (~ e'name)))))

(define (emit-encoding-tables)
  (cgen-extern "/* Encodings */"
               "enum {")
  (do-ec (: e (encoding-schemes))
         (cgen-extern #"    ~(~ e'c-enum),"))
  (cgen-extern "    NUM_JCODES"
               "};"
               "")

  (do-ec (: states (delete-duplicates (map (cut ~ <>'states)
                                           (encoding-schemes))))
         (if (not (null? states)))
         (begin
           (cgen-extern "enum {")
           (do-ec (: s states) (cgen-extern #"    ~|s|,"))
           (cgen-extern "};" "")))

  (cgen-body "static struct conv_support_rec conv_supports[] = {")
  (do-ec (: e (encoding-schemes))
         (: a (~ e'aliases))
         (cgen-body (format "    { ~15a, ~15a },"
                            (cgen-safe-string a)
                            (~ e 'c-enum))))
  (cgen-body "    { NULL, 0 }"
             "};"))

;;
;; Conversion
;;

;; Conversion is defined with one or two procedure name(s).

(define-class <conversion> (<instance-pool-mixin>)
  ((procs :init-keyword :procs)          ;name(s) of conversion routinens
   (from  :init-keyword :from)           ;list of encodings
   (to    :init-keyword :to)             ;list of encodings
   ))

(define-syntax define-conversion
  (syntax-rules ()
    [(_ (procs ...) from to)
     (make <conversion> :procs '(procs ...) :from 'from :to 'to)]
    [(_ proc from to)
     (make <conversion> :procs '(proc) :from 'from :to 'to)]))     

(define (find-conversion from to)
  ($ instance-pool-find <conversion>
     (^c (and (memq (~ from'name) (~ c'from))
              (memq (~ to'name) (~ c'to))))))

(define (emit-conversion-matrix)
  (define (fmt-entry oname reset istate ostate :optional (f1 'NULL) (f2 'NULL))
    ($ format
       "~60a /* ~a */"
       (format "    { ~a, ~a, ~a, ~a, ~a }," f1 f2 (or reset 'NULL)
               (or istate 0) (or ostate 0) )
       oname))

  ;; Generate declartions in the header file
  (let1 protos '()
    (do-ec (: f (instance-pool->list <conversion>))
           (: p (~ f'procs))
           (if (not (memq p protos)))
           (begin
             (cgen-extern #"static ScmSize ~p(ScmConvInfo*, const char*,"
                          "   ScmSize, char*, ScmSize, ScmSize*);")
             (push! protos p))))

  (cgen-body "static struct conv_converter_rec"
             " conv_converter[NUM_JCODES][NUM_JCODES] = {")

  (dolist [f (encoding-schemes)]
    (cgen-body #"  /* ~(~ f'name) => */"
               #"  {")
    (dolist [t (encoding-schemes)]
      (if-let1 c (find-conversion f t)
        (cgen-body (apply fmt-entry 
                          (~ t'name) (~ t'reset) 
                          (~ f'initial-state) (~ t'initial-state)
                          (~ c'procs)))
        (if (or (eq? f t)
                (eq? (~ f'name) 'none)
                (eq? (~ t'name) 'none))
          (cgen-body (format "~60a /* ~a */"
                             "    { ident, NULL, NULL, 0, 0},"
                             (~ t'name)))
          (cgen-body (format "~60a /* ~a */"
                             "    { NULL, NULL, NULL, 0, 0},"
                             (~ t'name))))))
    (cgen-body #"  },"))
  (cgen-body "};")
  )

(define (main args)
  (match (cdr args)
    [(outfile)
     (parameterize ([cgen-current-unit (make <cgen-unit> :name outfile)])
       (emit-encoding-tables)
       (emit-conversion-matrix)
       (cgen-emit-c (cgen-current-unit))
       (cgen-emit-h (cgen-current-unit)))])
  0)

;;;
;;; Definitions
;;;

(define-encoding-scheme ascii JCODE_ASCII
  ("ascii" "usascii"
   "isoir6" "iso646us" "us" "ibm367" "cp367" "csascii"))

(define-encoding-scheme eucj JCODE_EUCJ
  ("eucjp" "eucj" "eucjisx0213"))

(define-encoding-scheme sjis JCODE_SJIS
  ("sjis" "shiftjis"))

(define-encoding-scheme utf8 JCODE_UTF8
  ("utf8"))

(define-encoding-scheme utf16 JCODE_UTF16
  ("utf16")
  :states '(UTF_DEFAULT UTF_BE UTF_LE)
  :initial-state 'UTF_DEFAULT)

(define-encoding-scheme utf16be JCODE_UTF16BE
  ("utf16be")
  :states '(UTF_DEFAULT UTF_BE UTF_LE)
  :initial-state 'UTF_BE)

(define-encoding-scheme utf16le JCODE_UTF16LE
  ("utf16le")
  :states '(UTF_DEFAULT UTF_BE UTF_LE)
  :initial-state 'UTF_LE)

(define-encoding-scheme iso2022jp JCODE_ISO2022JP
  ("iso2022jp" "csiso2022jp" "iso2022jp1" "iso2022jp2" "iso2022jp3")
  :states '(JIS_ASCII JIS_ROMAN JIS_KANA JIS_78 JIS_0212 JIS_0213_1 JIS_0213_2
            JIS_UNKNOWN)
  :initial-state 'JIS_ASCII
  :reset "jis_reset")

(define-encoding-scheme iso8859-1 JCODE_ISO8859_1
  ("iso88591" "latin1"))

(define-encoding-scheme none JCODE_NONE
  ("none"))



(define-conversion ascii_x (ascii) (eucj sjis utf8 iso2022jp iso8859-1))
(define-conversion ascii_utf16 (ascii) (utf16 utf16be utf16le))

(define-conversion eucj_ascii (eucj) (ascii))
(define-conversion eucj_sjis  (eucj) (sjis))
(define-conversion eucj_utf8  (eucj) (utf8))
(define-conversion eucj_utf16 (eucj) (utf16 utf16be utf16le))
(define-conversion eucj_jis   (eucj) (iso2022jp))
(define-conversion eucj_lat1  (eucj) (iso8859-1))

(define-conversion sjis_ascii (sjis) (ascii))
(define-conversion sjis_eucj  (sjis) (eucj))
(define-conversion sjis_utf8  (sjis) (utf8))
(define-conversion (sjis_utf8 utf8_utf16) (sjis) (utf16 utf16be utf16le))
(define-conversion (sjis_eucj eucj_jis) (sjis) (iso2022jp))
(define-conversion (sjis_eucj eucj_lat1) (sjis) (iso8859-1))

(define-conversion utf8_ascii (utf8) (ascii))
(define-conversion utf8_eucj  (utf8) (eucj))
(define-conversion utf8_sjis  (utf8) (sjis))
(define-conversion utf8_utf16 (utf8) (utf16 utf16be utf16le))
(define-conversion (utf8_eucj eucj_jis) (utf8) (iso2022jp))
(define-conversion utf8_lat1  (utf8) (iso8859-1))

(define-conversion (utf16_utf8 utf8_ascii) (utf16 utf16be utf16le) (ascii))
(define-conversion utf16_eucj (utf16 utf16be utf16le) (eucj))
(define-conversion (utf16_eucj eucj_sjis) (utf16 utf16be utf16le) (sjis))
(define-conversion utf16_utf8 (utf16 utf16be utf16le) (utf8))
(define-conversion utf16_utf16 (utf16 utf16be utf16le) (utf16 utf16be utf16le))
(define-conversion (utf16_eucj eucj_jis) (utf16 utf16be utf16le) (iso2022jp))
(define-conversion (utf16_utf8 utf8_lat1) (utf16 utf16be utf16le) (iso8859-1))

(define-conversion (jis_eucj eucj_ascii) (iso2022jp) (ascii))
(define-conversion jis_eucj (iso2022jp) (eucj))
(define-conversion (jis_eucj eucj_sjis) (iso2022jp) (sjis))
(define-conversion (jis_eucj eucj_utf8) (iso2022jp) (utf8))
(define-conversion (jis_eucj eucj_utf16) (iso2022jp) (utf16 utf16be utf16le))
(define-conversion (jis_eucj eucj_lat1) (iso2022jp) (iso8859-1))

(define-conversion lat1_ascii (iso8859-1) (ascii))
(define-conversion lat1_eucj (iso8859-1) (eucj))
(define-conversion (lat1_eucj eucj_sjis) (iso8859-1) (sjis))
(define-conversion lat1_utf8 (iso8859-1) (utf8))
(define-conversion (lat1_utf8 utf8_utf16) (iso8859-1) (utf16 utf16be utf16le))
(define-conversion (lat1_eucj eucj_jis) (iso8859-1) (iso2022jp))


  
