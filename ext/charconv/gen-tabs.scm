;;;
;;;  Generating conversion state machine
;;;

;; For now, we just generate conversion matrix and some other tables.
;; Eventually, we'll define DSL from which convertion C routines are generated.

(use gauche.cgen)
(use gauche.mop.instance-pool)
(use srfi-42)
(use util.match)

;;;
;;; Encoding Scheme
;;;

(define-class <encoding-scheme> (<instance-pool-mixin>)
  ((name :init-keyword :name)           ;symbol, short Scheme name
   (c-enum :init-keyword :c-enum)       ;symbol, C-enum name
   (aliases :init-keyword :aliases)     ;list of strings, alias names.
                                        ; we ignore '_' and '-' for match.
   (states :init-keyword :states        ;If stateful encoding, list of states
           :init-value '())             ; (suitable for C enums)
   (initial-state :init-keyword :initial-state
                  :init-value #f)
   ))

(define-syntax define-encoding-scheme 
  (syntax-rules ()
    [(_ name c-enum aliases . opts)
     (define name (make <encoding-scheme>
                    :name 'name
                    :c-enum 'c-enum
                    :aliases 'aliases
                    . opts))]))

(define (encoding-schemes) (instance-pool->list <encoding-scheme>))

(define (emit-encoding-enum)
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
         (cgen-body (format "    { ~12a, ~15a },"
                            (cgen-safe-string a)
                            (~ e 'c-enum))))
  (cgen-body "    { NULL, 0 }"
             "};"))

(define (main args)
  (match (cdr args)
    [(outfile)
     (parameterize ([cgen-current-unit (make <cgen-unit> :name outfile)])
       (emit-encoding-enum)
       (cgen-emit-c (cgen-current-unit))
       (cgen-emit-h (cgen-current-unit)))])
  0)

;;;
;;; Definitions
;;;

(define-encoding-scheme ascii JCODE_ASCII
  ("ascii" "usascii"
   "isoir6" "iso646us" "us" "ibm367" "cp367" "csascii"))

(define-encoding-scheme eucjp JCODE_EUCJ
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
  :initial-state 'JIS_ASCII)

(define-encoding-scheme iso8859-1 JCODE_ISO8859_1
  ("iso88591" "latin1"))

(define-encoding-scheme none JCODE_NONE
  ("none"))


