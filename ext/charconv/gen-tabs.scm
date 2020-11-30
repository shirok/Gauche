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
  ((procs :init-keyword :procs)          ;name(s) of conversion routines
   (from  :init-keyword :from)           ;list of encodings
   (to    :init-keyword :to)             ;list of encodings
   (proc-name :init-value #f)            ;final proc name (automatically set)
   ))

(define-method initialize ((c <conversion>) initargs)
  (define (ssplit sym) (string-split (x->string sym) #\_))
  (next-method)
  (match (~ c 'procs)
    [(f0) (set! (~ c'proc-name) f0)]
    ;; autogenerate fused function name
    [(f0 f1) 
     (set! (~ c'proc-name) #"~(car (ssplit f0))_~(cadr (ssplit f1))")]
    [(f0 f1 f2) 
     (set! (~ c'proc-name) #"~(car (ssplit f0))_~(cadr (ssplit f2))")]))

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
  (define (fmt-entry oname conv reset istate ostate)
    ($ format
       "~60a /* ~a */"
       (format "    { ~a, ~a, ~a, ~a }," conv (or reset 'NULL)
               (or istate 0) (or ostate 0) )
       oname))
  (define (emit-fused-proc proc-name procs)
    (cgen-body #"static ScmSize ~proc-name(ScmConvInfo *cinfo,"
               #"    const char *inptr, ScmSize inroom,"
               #"    char *outptr, ScmSize outroom, ScmSize *outchars)"
               #"{")
    (match-let1 (f0 f1) procs
      (cgen-body #"    char buf[INTERMEDIATE_BUF_SIZE];"
                 #"    ScmSize bufcount;"
                 #"    ScmSize r0 = ~f0(cinfo, inptr, inroom,"
                 #"       buf, INTERMEDIATE_BUF_SIZE, &bufcount);"
                 #"    if (r0 < 0) return r0;"
                 #"    if (bufcount == 0) { *outchars = 0; return r0; }"
                 #"    ScmSize r1 = ~f1(cinfo, buf, bufcount,"
                 #"       outptr, outroom, outchars);"
                 #"    if (r1 < 0) return r1;"
                 #"    return r0;"))
    (cgen-body "}" ""))

  ;; Generate declartions in the header file
  (let1 protos '()
    (do-ec (: f (instance-pool->list <conversion>))
           (if (not (member (~ f'proc-name) protos)))
           (begin
             (cgen-extern #"static ScmSize ~(~ f'proc-name)(ScmConvInfo*,"
                          #"    const char*, ScmSize,"
                          #"    char*, ScmSize, ScmSize*);")
             (when (= (length (~ f'procs)) 2)
               (emit-fused-proc (~ f'proc-name) (~ f'procs)))
             (push! protos (~ f'proc-name)))))

  (cgen-body "static struct conv_converter_rec"
             " conv_converter[NUM_JCODES][NUM_JCODES] = {")

  (dolist [f (encoding-schemes)]
    (cgen-body #"  /* ~(~ f'name) => */"
               #"  {")
    (dolist [t (encoding-schemes)]
      (if-let1 c (find-conversion f t)
        (cgen-body (fmt-entry (~ t'name) (~ c'proc-name) (~ t'reset) 
                              (~ f'initial-state) (~ t'initial-state)))
        (if (or (eq? f t)
                (eq? (~ f'name) 'none)
                (eq? (~ t'name) 'none))
          (cgen-body (fmt-entry (~ t'name) "ident" #f 0 0))
          (cgen-body (fmt-entry (~ t'name) 'NULL #f 0 0)))))
    (cgen-body #"  },"))
  (cgen-body "};")
  )

(define (main args)
  (match (cdr args)
    [(outfile)
     (parameterize ([cgen-current-unit (make <cgen-unit> 
                                         :name outfile
                                         :init-prologue ""
                                         :init-epilogue "")])
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

(define-encoding-scheme utf32 JCODE_UTF32
  ("utf32")
  :states '(UTF_DEFAULT UTF_BE UTF_LE)
  :initial-state 'UTF_DEFAULT)

(define-encoding-scheme utf32be JCODE_UTF32BE
  ("utf32be")
  :states '(UTF_DEFAULT UTF_BE UTF_LE)
  :initial-state 'UTF_BE)

(define-encoding-scheme utf32le JCODE_UTF32LE
  ("utf32le")
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
(define-conversion ascii_utf32 (ascii) (utf32 utf32be utf32le))

(define-conversion eucj_ascii (eucj) (ascii))
(define-conversion eucj_sjis  (eucj) (sjis))
(define-conversion eucj_utf8  (eucj) (utf8))
(define-conversion eucj_utf16 (eucj) (utf16 utf16be utf16le))
(define-conversion eucj_utf32 (eucj) (utf32 utf32be utf32le))
(define-conversion eucj_jis   (eucj) (iso2022jp))
(define-conversion eucj_lat1  (eucj) (iso8859-1))

(define-conversion sjis_ascii (sjis) (ascii))
(define-conversion sjis_eucj  (sjis) (eucj))
(define-conversion sjis_utf8  (sjis) (utf8))
(define-conversion (sjis_utf8 utf8_utf16) (sjis) (utf16 utf16be utf16le))
(define-conversion (sjis_utf8 utf8_utf32) (sjis) (utf32 utf32be utf32le))
(define-conversion (sjis_eucj eucj_jis) (sjis) (iso2022jp))
(define-conversion (sjis_eucj eucj_lat1) (sjis) (iso8859-1))

(define-conversion utf8_ascii (utf8) (ascii))
(define-conversion utf8_eucj  (utf8) (eucj))
(define-conversion utf8_sjis  (utf8) (sjis))
(define-conversion utf8_utf16 (utf8) (utf16 utf16be utf16le))
(define-conversion utf8_utf32 (utf8) (utf32 utf32be utf32le))
(define-conversion (utf8_eucj eucj_jis) (utf8) (iso2022jp))
(define-conversion utf8_lat1  (utf8) (iso8859-1))

(define-conversion (utf16_utf8 utf8_ascii) (utf16 utf16be utf16le) (ascii))
(define-conversion utf16_eucj (utf16 utf16be utf16le) (eucj))
(define-conversion (utf16_eucj eucj_sjis) (utf16 utf16be utf16le) (sjis))
(define-conversion utf16_utf8 (utf16 utf16be utf16le) (utf8))
(define-conversion utf16_utf16 (utf16 utf16be utf16le) (utf16 utf16be utf16le))
(define-conversion (utf16_utf8 utf8_utf32) (utf16 utf16be utf16le) (utf32 utf32be utf32le))
(define-conversion (utf16_eucj eucj_jis) (utf16 utf16be utf16le) (iso2022jp))
(define-conversion (utf16_utf8 utf8_lat1) (utf16 utf16be utf16le) (iso8859-1))

(define-conversion (utf32_utf8 utf8_ascii) (utf32 utf32be utf32le) (ascii))
(define-conversion utf32_eucj (utf32 utf32be utf32le) (eucj))
(define-conversion (utf32_eucj eucj_sjis) (utf32 utf32be utf32le) (sjis))
(define-conversion utf32_utf8 (utf32 utf32be utf32le) (utf8))
(define-conversion (utf32_utf8 utf8_utf16) (utf32 utf32be utf32le) (utf16 utf16le utf16be))
(define-conversion utf32_utf32 (utf32 utf32be utf32le) (utf32 utf32be utf32le))
(define-conversion (utf32_eucj eucj_jis) (utf32 utf32be utf32le) (iso2022jp))
(define-conversion (utf32_utf8 utf8_lat1) (utf32 utf32be utf32le) (iso8859-1))

(define-conversion (jis_eucj eucj_ascii) (iso2022jp) (ascii))
(define-conversion jis_eucj (iso2022jp) (eucj))
(define-conversion (jis_eucj eucj_sjis) (iso2022jp) (sjis))
(define-conversion (jis_eucj eucj_utf8) (iso2022jp) (utf8))
(define-conversion (jis_eucj eucj_utf16) (iso2022jp) (utf16 utf16be utf16le))
(define-conversion (jis_eucj eucj_utf32) (iso2022jp) (utf32 utf32be utf32le))
(define-conversion (jis_eucj eucj_lat1) (iso2022jp) (iso8859-1))

(define-conversion lat1_ascii (iso8859-1) (ascii))
(define-conversion lat1_eucj (iso8859-1) (eucj))
(define-conversion (lat1_eucj eucj_sjis) (iso8859-1) (sjis))
(define-conversion lat1_utf8 (iso8859-1) (utf8))
(define-conversion (lat1_utf8 utf8_utf16) (iso8859-1) (utf16 utf16be utf16le))
(define-conversion (lat1_utf8 utf8_utf32) (iso8859-1) (utf32 utf32be utf32le))
(define-conversion (lat1_eucj eucj_jis) (iso8859-1) (iso2022jp))


  
