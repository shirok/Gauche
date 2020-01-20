;;-*- coding:utf-8 -*-
;; testing gauche.unicode
;;

(use gauche.test)
(use util.match)
(use gauche.sequence)
(test-start "gauche.unicode")

(use gauche.unicode)
(test-module 'gauche.unicode)

(test-section "transfer encodings")

;; UTF encoding test data.
;; This data can be used in two-fold; that is, for ucs4->(utf8,utf16) and
;; (utf8,utf16)->ucs4.  'Dir' field specifies the valid direction(s).
;; 'Mode' is a list of one or more of s, p, r and i, representing strictness
;; mode strict, permissive, replace and ignore, respectively.

(define *te-samples*
  '(;;mode dir ucs4     utf8                         utf16
    (spri  =  0         (0)                          (0))
    (spri  =  1         (1)                          (1))
    (spri  =  #x7f      (#x7f)                       (#x7f))
    (spri  =  #x80      (#xc2 #x80)                  (#x80))
    (spri  =  #x81      (#xc2 #x81)                  (#x81))
    (spri  =  #xff      (#xc3 #xbf)                  (#xff))
    (spri  =  #x100     (#xc4 #x80)                  (#x100))
    (spri  =  #x7ff     (#xdf #xbf)                  (#x7ff))
    (spri  =  #x800     (#xe0 #xa0 #x80)             (#x800))
    (spri  =  #xd7ff    (#xed #x9f #xbf)             (#xd7ff))
    ;; surrogated pair region
    (p     =  #xd800    (#xed #xa0 #x80)             (#xd800))
    (s     >  #xd800    error                        error)
    (r     <  #xfffd    (#xed #xa0 #x80)             (#xd800))
    (s     <  error     (#xed #xa0 #x80)             (#xd800))
    (p     =  #xdfff    (#xed #xbf #xbf)             (#xdfff))
    (s     >  #xdfff    error                        error)
    (r     <  #xfffd    (#xed #xbf #xbf)             (#xdfff))
    (s     <  error     (#xed #xbf #xbf)             (#xdfff))
    ;; end of surrogated pair region
    (spri  =  #xe000    (#xee #x80 #x80)             (#xe000))
    (spri  =  #xffff    (#xef #xbf #xbf)             (#xffff))
    (spri  =  #x10000   (#xf0 #x90 #x80 #x80)        (#xd800 #xdc00))
    (spri  =  #x103ff   (#xf0 #x90 #x8f #xbf)        (#xd800 #xdfff))
    (spri  =  #x10400   (#xf0 #x90 #x90 #x80)        (#xd801 #xdc00))
    (spri  =  #x50000   (#xf1 #x90 #x80 #x80)        (#xd900 #xdc00))
    (spri  =  #x10ffff  (#xf4 #x8f #xbf #xbf)        (#xdbff #xdfff))
    ;; end of valid unicode range.
    (s     >  #x110000  error                        error)
    (s     <  error     (#xf4 #x90 #x80 #x80)        error)
    (p     =  #x110000  (#xf4 #x90 #x80 #x80)        error)
    (r     <  #xfffd    (#xf4 #x90 #x80 #x80)        -)
    (s     <  error     (#xf7 #xbf #xbf #xbf)        error)
    (p     =  #x1fffff  (#xf7 #xbf #xbf #xbf)        error)
    (r     <  #xfffd    (#xf7 #xbf #xbf #xbf)        -)
    (s     <  error     (#xf8 #x88 #x80 #x80 #x80)   error)
    (p     =  #x200000  (#xf8 #x88 #x80 #x80 #x80)   error)
    (r     <  #xfffd    (#xf8 #x88 #x80 #x80 #x80)   -)
    (s     <  error     (#xfb #xbf #xbf #xbf #xbf)   error)
    (p     =  #x3ffffff (#xfb #xbf #xbf #xbf #xbf)   error)
    (r     <  #xfffd    (#xfb #xbf #xbf #xbf #xbf)   -)
    (s     <  error     (#xfc #x84 #x80 #x80 #x80)   error)
    (p     =  #x4000000 (#xfc #x84 #x80 #x80 #x80 #x80) error)
    (r     <  #xfffd    (#xfc #x84 #x80 #x80 #x80 #x80) -)
    (s     <  error     (#xfd #xbf #xbf #xbf #xbf #xbf) error)
    (p     =  #x7fffffff(#xfd #xbf #xbf #xbf #xbf #xbf) error)
    (r     <  #xfffd    (#xfd #xbf #xbf #xbf #xbf #xbf) -)
    (s     <  error     (#xfe #x80 #x80 #x80 #x80 #x80) error)
    (p     <  #xfe      (#xfe)                          error)
    (r     <  #xfffd    (#xfe)                          -)
    (i     <  #x01      (#xfe #x80 #x80 #x80 #x80 #x80 #x01) error)
    (s     <  error     (#xff #x80 #x80 #x80 #x80 #x80) error)
    (p     <  #xff      (#xff)                          error)
    (r     <  #xfffd    (#xff)                          -)
    (i     <  #x01      (#xff #x80 #x80 #x80 #x80 #x80 #x01) error)
    ;; redundant encodings
    (s     <  error     (#xc0 #x80)                  -)
    (p     <  #x00      (#xc0 #x80)                  -)
    (r     <  #xfffd    (#xc0 #x80)                  -)
    (i     <  #x01      (#xc0 #x80 #x01)             -)
    (s     <  error     (#xc1 #xbf)                  -)
    (p     <  #x7f      (#xc1 #xbf)                  -)
    (r     <  #xfffd    (#xc1 #xbf)                  -)
    (i     <  #x01      (#xc1 #xbf #x01)             -)
    (s     <  error     (#xe0 #x80 #x80)             -)
    (p     <  #x0       (#xe0 #x80 #x80)             -)
    (r     <  #xfffd    (#xc1 #xbf)                  -)
    (i     <  #x01      (#xe0 #x80 #x80 #x01)        -)
    (s     <  error     (#xe0 #x9f #xbf)             -)
    (p     <  #x7ff     (#xe0 #x9f #xbf)             -)
    (r     <  #xfffd    (#xe0 #x9f #xbf)             -)
    (i     <  #x01      (#xe0 #x9f #xbf #x01)        -)
    (s     <  error     (#xf0 #x80 #x80 #x80)        -)
    (p     <  #x0       (#xf0 #x80 #x80 #x80)        -)
    (r     <  #xfffd    (#xf0 #x80 #x80 #x80)        -)
    (i     <  #x01      (#xf0 #x80 #x80 #x80 #x01)   -)
    (s     <  error     (#xf0 #x8f #xbf #xbf)        -)
    (p     <  #xffff    (#xf0 #x8f #xbf #xbf)        -)
    (r     <  #xfffd    (#xf0 #x8f #xbf #xbf)        -)
    (i     <  #x01      (#xf0 #x8f #xbf #xbf #x01)   -)
    ;; invalid encodings
    (s     <  error     (#x80)                       -)
    (p     <  #x80      (#x80)                       -)
    (r     <  #xfffd    (#x80)                       -)
    (i     <  #x01      (#x80 #x01)                  -)
    (i     <  #x02      (#x80 #xbf #x02)             -)
    (sp    <  error     (#xc2 #x01)                  -)
    (r     <  #xfffd    (#xc2 #x01)                  -)
    (i     <  #x01      (#xc2 #x01)                  -)
    (i     <  #x81      (#xc2 #xc2 #x81)             -)
    (sp    <  error     (#xe0 #x01 #x80)             -)
    (r     <  #xfffd    (#xe0 #x01)                  -)
    (i     <  #x01      (#xe0 #x01)                  -)
    (sp    <  error     (#xe0 #xa0 #x02)             -)
    (r     <  #xfffd    (#xe0 #xa0 #x02)             -)
    (i     <  #x02      (#xe0 #xa0 #x02)             -)
    ;; lone surrogates
    (s     <  error     -              (#xd800))
    (p     <  #xd800    -              (#xd800))
    (r     <  #xfffd    -              (#xd800))
    (i     <  eof       -              (#xd800))
    (s     <  error     -              (#xdbff))
    (p     <  #xdbff    -              (#xdbff))
    (r     <  #xfffd    -              (#xdbff))
    (i     <  eof       -              (#xdbff))
    (s     <  error     -              (#xdc00))
    (p     <  #xdc00    -              (#xdc00))
    (r     <  #xfffd    -              (#xdc00))
    (i     <  eof       -              (#xdc00))
    (s     <  error     -              (#xdfff))
    (p     <  #xdfff    -              (#xdfff))
    (r     <  #xfffd    -              (#xdfff))
    (i     <  eof       -              (#xdfff))
    (s     <  error     -              (#xd800 #xd801))
    (p     <  #xd800    -              (#xd800 #xd801))
    (r     <  #xfffd    -              (#xd800 #xd801))
    (i     <  eof       -              (#xd800 #xd801))
    (i     <  #x10400   -              (#xd800 #xd801 #xdc00))
    ))

(define (test-utf-conv spec)
  (define (sig1 obj)
    (cond [(number? obj) (format "#x~x" obj)]
          [(list? obj) (map sig1 obj)]
          [else obj]))
  (define (t-utf8->ucs4 from strictness)
    (receive (ucs4 rest) (utf8->ucs4 (append from '(0)) strictness)
      (if (equal? rest '(0))
        ucs4
        (cons 'wrong-rest-value rest)))) ; for easier diagnostics
  (define (t-utf16->ucs4 from strictness)
    (receive (ucs4 rest) (utf16->ucs4 from strictness)
      (if (eof-object? ucs4) 'eof ucs4)))

  (let-syntax ([t (syntax-rules ()
                    [(t fn from to strictness)
                     (test* (format "~s[~a] ~a -> ~a" 'fn strictness
                                    (sig1 from) (sig1 to))
                            (if (eq? to 'error) (test-error) (sig1 to))
                            (sig1 (fn from strictness)))])])
    (match-let1 (mode dir ucs4 utf8 utf16) spec
      (dolist [strictness (map (pa$ assq-ref '((#\s . strict)
                                               (#\p . permissive)
                                               (#\r . replace)
                                               (#\i . ignore)))
                               (string->list (symbol->string mode)))]
        (when (memq dir '(= >))
          (t ucs4->utf8  ucs4 utf8 strictness)
          (t ucs4->utf16 ucs4 utf16 strictness))
        (when (and (memq dir '(= <)) (not (symbol? utf8)))
          (t t-utf8->ucs4 utf8 ucs4 strictness))
        (when (and (memq dir '(= <)) (not (symbol? utf16)))
          (t t-utf16->ucs4 utf16 ucs4 strictness))))))

(for-each test-utf-conv *te-samples*)

;; utf8->string and string->utf8
(when (memq (gauche-character-encoding) '(utf-8 sjis euc-jp))
  (let1 data '(("abc" #u8(97 98 99))
               ("λΛ"  #u8(206 187 206 155))
               (#f    #u8(97 128 128)))
    (define (conversion-tester str bvec)
      (test* "utf8->string" (or str (test-error))
             (utf8->string bvec))
      (when str
        (test* "string->utf8" bvec
               (string->utf8 str))))

    (for-each (^d (apply conversion-tester d)) data)))

;; utf16->string
(test* "utf16->string big-endian" "012"
       (utf16->string #u8(0 48 0 49 0 50) 'big))
(test* "utf16->string little-endian" "012"
       (utf16->string #u8(48 0 49 0 50 0) 'little))
(test* "utf16->string big-endian by default" "012"
       (utf16->string #u8(0 48 0 49 0 50)))
(test* "utf16->string BOM big-endian" "012"
       (utf16->string #u8(#xfe #xff 0 48 0 49 0 50)))
(test* "utf16->string BOM little-endian" "012"
       (utf16->string #u8(#xff #xfe 48 0 49 0 50 0)))
(test* "utf16->string BOM override" "012"
       (utf16->string #u8(#xff #xfe 48 0 49 0 50 0) 'big))
(test* "utf16->string ignore-bom big-endian" "\xfeff;012"
       (utf16->string #u8(#xfe #xff 0 48 0 49 0 50) 'big #t))
(test* "utf16->string ignore-bom little-endian" "\xfeff;012"
       (utf16->string #u8(#xff #xfe 48 0 49 0 50 0) 'little #t))
(test* "utf16->string start/end" "012"
       (utf16->string #u8(0 0 48 0 49 0 50) 'big #t 1))
(test* "utf16->string BOM only" ""
       (utf16->string #u8(#xfe #xff)))
(test* "utf16->string BOM only" ""
       (utf16->string #u8(#xff #xfe)))
(test* "string->utf16 default" #u8(0 48 0 49 0 50)
       (string->utf16 "012"))
(test* "string->utf16 big" #u8(0 48 0 49 0 50)
       (string->utf16 "012" 'big-endian))
(test* "string->utf16 little" #u8(48 0 49 0 50 0)
       (string->utf16 "012" 'little-endian))
(test* "string->utf16 start" #u8(48 0 49 0 50 0)
       (string->utf16 "a012" 'little-endian 1))
(test* "string->utf16 start/end" #u8(48 0 49 0 50 0)
       (string->utf16 "a012b" 'little-endian 1 4))

(test-section "word boundary")

(define (test-word-breaker sentence expected)
  (test* "string->words" expected (string->words sentence))
  (test* "codepoints->words"
         (map (^w (map char->ucs w)) expected)
         (codepoints->words (map char->ucs sentence)))
  (test* "codepoints->words"
         (map (^w (map char->ucs w)) expected)
         (codepoints->words (map-to <vector> char->ucs sentence))))

;; example given in UAX#29
(test-word-breaker
 "The quick (\"brown\") fox can't jump 32.3 feet, right?"
 '("The" " " "quick" " " "(" "\"" "brown" "\"" ")" " "
   "fox" " " "can't" " " "jump" " " "32.3" " " "feet" "," " " "right" "?"))

(when (memq (gauche-character-encoding) '(utf-8 euc-jp sjis))
  (test-word-breaker
   "Gauche(ゴーシュ)はR5RS準拠のScheme処理系"
   '("Gauche" "(" "ゴーシュ" ")" "は" "R5RS" "準" "拠" "の"
     "Scheme" "処" "理" "系")))

(test-section "case conversion")

(define (test-xcase-matrix up down title fold)
  (let-syntax ([do-tests
                (syntax-rules ()
                  [(_ upper downer titler folder u d t f)
                   (let ([u. u] [d. d] [t. t] [f. f])
                     (test* (format "~a (from down)" 'upper) u. (upper d.))
                     (test* (format "~a (from up)" 'upper) u. (upper u.))
                     (test* (format "~a (from down)" 'downer) d. (downer d.))
                     (test* (format "~a (from up)" 'downer) d. (downer u.))
                     (test* (format "~a (from down)" 'titler) t. (titler d.))
                     (test* (format "~a (from up)" 'titler) t. (titler u.))
                     (test* (format "~a (from down)" 'folder) f. (folder d.))
                     (test* (format "~a (from up)" 'folder) f. (folder u.)))])])

    (do-tests string-upcase string-downcase string-titlecase string-foldcase
              up down title fold)
    (do-tests codepoints-upcase codepoints-downcase
              codepoints-titlecase codepoints-foldcase
              (map char->ucs up)
              (map char->ucs down)
              (map char->ucs title)
              (map char->ucs fold))
    (do-tests codepoints-upcase codepoints-downcase
              codepoints-titlecase codepoints-foldcase
              (map-to <vector> char->ucs up)
              (map-to <vector> char->ucs down)
              (map-to <vector> char->ucs title)
              (map-to <vector> char->ucs fold))
    ))

(test-xcase-matrix "YOU'D SING `HUSH-A-BYE'!"
                   "you'd sing `hush-a-bye'!"
                   "You'd Sing `Hush-A-Bye'!"
                   "you'd sing `hush-a-bye'!")

(when (memq (gauche-character-encoding) '(utf-8 euc-jp sjis))
  (test-xcase-matrix "ΧΑΟΣΧΑΟΣ.ΧΑΟΣ. Σ."
                     "χαοσχαοσ.χαος. σ."
                     "Χαοσχαοσ.χαος. Σ."
                     "χαοσχαοσ.χαοσ. σ.")
  )

;; non round-trip mapping
(test* "string-upcase"   "STRASSE" (string-upcase "stra\u00dfe"))
(test* "string-downcase" "strasse" (string-downcase "STRASSE"))
(test* "string-downcase" "stra\u00dfe" (string-downcase "stra\u00dfe"))
(test* "string-titlecase" "Stra\u00dfe" (string-titlecase "stra\u00dfe"))
(test* "string-foldcase" "strasse" (string-foldcase "stra\u00dfe"))

(test-section "east asian width")

(let ([data1 '((#\a  Na)
               (#\x7e Na)
               (#\x7f N))]
      [data2 (cond-expand
              [gauche.ces.none '()]
              [else '((#\u03b1 A)
                      (#\u3000 F)
                      (#\u30a2 W))])])
  (define (width-test ch expected)
    (test* #"east asian width (~ch)" `(,expected ,expected)
           (list (char-east-asian-width ch)
                 (char-east-asian-width (char->ucs ch)))))

  (dolist [f data1] (apply width-test f))
  (dolist [f data2] (apply width-test f))
  )

(test-end)
