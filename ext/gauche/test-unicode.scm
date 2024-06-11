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
(let1 data '(("abc" #u8(97 98 99))
             ("λΛ"  #u8(206 187 206 155))
             (#f    #u8(128))
             (#f    #u8(#xc0 01)))
  (define (conversion-tester str bvec)
    (test* "utf8->string" (or str
                              (if (eq? (gauche-character-encoding) 'utf-8)
                                (string (ucs->char #xfffd))
                                (test-error)))
           (utf8->string bvec))
    (when str
      (test* "string->utf8" bvec
             (string->utf8 str))))

  (for-each (^d (apply conversion-tester d)) data))

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
       (string->utf16 "a012" 'little-endian #f 1))
(test* "string->utf16 start/end" #u8(48 0 49 0 50 0)
       (string->utf16 "a012b" 'little-endian #f 1 4))
(test* "string->utf16 bom" #u8(#xff #xfe 48 0 49 0 50 0)
       (string->utf16 "012" 'little-endian #t))
(test* "string->utf16 bom" #u8(#xfe #xff 0 48 0 49 0 50)
       (string->utf16 "012" 'big-endian #t))

;; utf32->string
(test* "utf32->string big-endian" "012"
       (utf32->string #u8(0 0 0 48 0 0 0 49 0 0 0 50) 'big))
(test* "utf32->string little-endian" "012"
       (utf32->string #u8(48 0 0 0 49 0 0 0 50 0 0 0) 'little))
(test* "utf32->string big-endian by default" "012"
       (utf32->string #u8(0 0 0 48 0 0 0 49 0 0 0 50)))
(test* "utf32->string BOM big-endian" "012"
       (utf32->string #u8(0 0 #xfe #xff 0 0 0 48 0 0 0 49 0 0 0 50)))
(test* "utf32->string BOM little-endian" "012"
       (utf32->string #u8(#xff #xfe 0 0 48 0 0 0 49 0 0 0 50 0 0 0)))
(test* "utf32->string BOM override" "012"
       (utf32->string #u8(#xff #xfe 0 0 48 0 0 0 49 0 0 0 50 0 0 0) 'big))
(test* "utf32->string ignore-bom big-endian" "\xfeff;012"
       (utf32->string #u8(0 0 #xfe #xff 0 0 0 48 0 0 0 49 0 0 0 50) 'big #t))
(test* "utf32->string ignore-bom little-endian" "\xfeff;012"
       (utf32->string #u8(#xff #xfe 0 0 48 0 0 0 49 0 0 0 50 0 0 0) 'little #t))
(test* "utf32->string start/end" "012" ; map-to path
       (utf32->string #u8(0 0 0 0 48 0 0 0 49 0 0 0 50) 'big #t 1))
(test* "utf32->string start/end" "12" ; u32vector->string path
       (utf32->string #u8(0 0 0 48 0 0 0 49 0 0 0 50) 'big #t 4))
(test* "utf32->string BOM only" ""
       (utf32->string #u8(0 0 #xfe #xff)))
(test* "utf32->string BOM only" ""
       (utf32->string #u8(#xff #xfe 0 0)))
(test* "string->utf32 default" #u8(0 0 0 48 0 0 0 49 0 0 0 50)
       (string->utf32 "012"))
(test* "string->utf32 big" #u8(0 0 0 48 0 0 0 49 0 0 0 50)
       (string->utf32 "012" 'big-endian))
(test* "string->utf32 little" #u8(48 0 0 0 49 0 0 0 50 0 0 0)
       (string->utf32 "012" 'little-endian))
(test* "string->utf32 start" #u8(48 0 0 0 49 0 0 0 50 0 0 0)
       (string->utf32 "a012" 'little-endian #f 1))
(test* "string->utf32 start/end" #u8(48 0 0 0 49 0 0 0 50 0 0 0)
       (string->utf32 "a012b" 'little-endian #f 1 4))
(test* "string->utf32 bom" #u8(#xff #xfe 0 0 48 0 0 0 49 0 0 0 50 0 0 0)
       (string->utf32 "012" 'little-endian #t))
(test* "string->utf32 bom" #u8(0 0 #xfe #xff 0 0 0 48 0 0 0 49 0 0 0 50)
       (string->utf32 "012" 'big-endian #t))

(test-section "word boundary")

(define (test-word-breaker sentence expected)
  (test* "string->words" expected (string->words sentence))
  (test* "codepoints->words"
         (map (^w (map char->ucs w)) expected)
         (codepoints->words (map char->ucs sentence)))
  (test* "codepoints->words"
         (map (^w (map-to <vector> char->ucs w)) expected)
         (codepoints->words (map-to <vector> char->ucs sentence))))

;; example given in UAX#29
(test-word-breaker
 "The quick (\"brown\") fox can't jump 32.3 feet, right?"
 '("The" " " "quick" " " "(" "\"" "brown" "\"" ")" " "
   "fox" " " "can't" " " "jump" " " "32.3" " " "feet" "," " " "right" "?"))

(test-word-breaker
 "Gauche(ゴーシュ)はR5RS準拠のScheme処理系"
 '("Gauche" "(" "ゴーシュ" ")" "は" "R5RS" "準" "拠" "の"
   "Scheme" "処" "理" "系"))

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

(test-xcase-matrix "ΧΑΟΣΧΑΟΣ.ΧΑΟΣ. Σ."
                   "χαοσχαοσ.χαος. σ."
                   "Χαοσχαοσ.χαος. Σ."
                   "χαοσχαοσ.χαοσ. σ.")

;; non round-trip mapping
(test* "string-upcase"   "STRASSE" (string-upcase "stra\u00dfe"))
(test* "string-downcase" "strasse" (string-downcase "STRASSE"))
(test* "string-downcase" "stra\u00dfe" (string-downcase "stra\u00dfe"))
(test* "string-titlecase" "Stra\u00dfe" (string-titlecase "stra\u00dfe"))
(test* "string-foldcase" "strasse" (string-foldcase "stra\u00dfe"))

(test-section "text segmentation")

(let ()
  ;; Each test data countains a list of (<count> <seq> <comment>)
  ;; where <seq> is <bool> (<code> <bool>)*
  (include "../../test/include/unicode-test-data.scm")
  (define (data->codes seq) ; extract codes
    (remove boolean? seq))
  (define (data->clusters seq) ; group unbreakable codes
    (let loop ([seq (cdr seq)] [r '()] [s `(,(car seq))])
      (match seq
        [() (reverse (cons (reverse s) r))]
        [(#t x . seq) (loop seq (cons (reverse s) r) (list x))]
        [(#f x . seq) (loop seq r (cons x s))])))
  (define (t name data segmenter)
    (let ([count (car data)]
          ;; The test data sequence us surrounded by '÷' (break), but we don't
          ;; need them.
          [seq (subseq (cadr data) 1 (- (length (cadr data)) 1))])
      (test* #"~name (~count)" (data->clusters seq)
             (segmenter (data->codes seq)))))
  (define (t* name all-data segmenter)
    (for-each (cut t name <> segmenter) all-data))

  (t* "grapheme break" *grapheme-break-tests* codepoints->grapheme-clusters)
  (t* "word break" *word-break-tests* codepoints->words)
  )

(test-section "east asian width")

(let ([data1 '((#\a  Na)
               (#\x7e Na)
               (#\x7f N))]
      [data2 '((#\u03b1 A)
               (#\u3000 F)
               (#\u30a2 W))])
  (define (width-test ch expected)
    (test* #"east asian width (~ch)" `(,expected ,expected)
           (list (char-east-asian-width ch)
                 (char-east-asian-width (char->ucs ch)))))

  (dolist [f data1] (apply width-test f))
  (dolist [f data2] (apply width-test f))
  )

(test* "string-east-asian-width" 7
       (string-east-asian-width "a\u3000b\u30a2c"))
(test* "string-east-asian-width" 5
       (string-east-asian-width "a\u3000b\u30a2c" :F 1 :W 1))
(test* "string-take-width" "a\u3000"
       (string-take-width "a\u3000\u30a2bc" 4))
(test* "string-take-width" "a\u3000\u30a2"
       (string-take-width "a\u3000\u30a2bc" 5))
(test* "string-drop-width" "\u30a2bc"
       (string-drop-width "a\u3000\u30a2bc" 4))
(test* "string-drop-width" "bc"
       (string-drop-width "a\u3000\u30a2bc" 5))

;;-------------------------------------------------------------------
;; text.fill depends on gauche.unicode, so we test it here.
;;

(test-section "text.fill")
(use text.fill)
(test-module 'text.fill)

(define fill-data-1
  (string-append
   "Lorem ipsum dolor sit amet, consectetur"
   " adipiscing elit, sed do eiusmod tempor incididunt ut labore"
   " et dolore magna aliqua.  Ut enim ad minim veniam, quis"
   " nostrud exercitation ullamco laboris nisi ut aliquip ex ea"
   " commodo consequat. Duis aute irure dolor in reprehenderit in\n"
   " voluptate velit esse cillum dolore eu fugiat nulla pariatur."
   " Excepteur sint occaecat cupidatat non proident, sunt in culpa"
   " qui officia deserunt mollit anim id est laborum."))

(define fill-data-2
  (string-append
   "これらの各メソッドは、<num-vector>クラスや<num-matrix>"
   "クラスに所属するわけではありません。"
   "メソッドがクラスに所有されているわけではないので、既に存在するクラスに対していつでも"
   "独自のメソッドを定義することができます(但し、いくつかの組込みクラスの既定のメソッドには"
   "変更できないものがあります)。上の例で既にこれは示されています。プログラマは"
   "メソッドproductを組込みクラス<number>に対して定義することが"
   "できます。これが、全てはオブジェクトであるかどうかはCLOSスタイルのオブジェクトシステム"
   "では問題にならないと言った理由です。"))

(define fill-data-3 ; paragraph break
  (string-append
   "Lorem ipsum dolor sit amet, consectetur"
   " adipiscing elit, sed do eiusmod tempor incididunt ut labore"
   " et dolore magna aliqua.\n\n"
   " Ut enim ad minim veniam, quis"
   " nostrud exercitation ullamco laboris nisi ut aliquip ex ea"
   " commodo consequat.\n\n"
   "Duis aute irure dolor in reprehenderit in\n"
   " voluptate velit esse cillum dolore eu fugiat nulla pariatur."
   " Excepteur sint occaecat cupidatat non proident, sunt in culpa"
   " qui officia deserunt mollit anim id est laborum."))

(define (test-fill name input params expect)
  (test*/diff #"text.fill ~|name| ~|params|"
              expect
              (with-output-to-string
                (^[] (apply display-filled-text input params) (newline)))))

(test-fill "en" fill-data-1 '()
           '("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do"
             "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut"
             "enim ad minim veniam, quis nostrud exercitation ullamco laboris"
             "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in"
             "reprehenderit in voluptate velit esse cillum dolore eu fugiat"
             "nulla pariatur. Excepteur sint occaecat cupidatat non proident,"
             "sunt in culpa qui officia deserunt mollit anim id est laborum."))

(test-fill "ja" fill-data-2 '()
           '("これらの各メソッドは、<num-vector>クラスや<num-matrix>クラスに所"
             "属するわけではありません。メソッドがクラスに所有されているわけで"
             "はないので、既に存在するクラスに対していつでも独自のメソッドを定"
             "義することができます(但し、いくつかの組込みクラスの既定のメソッド"
             "には変更できないものがあります)。上の例で既にこれは示されていま"
             "す。プログラマはメソッドproductを組込みクラス<number>に対して定義"
             "することができます。これが、全てはオブジェクトであるかどうかは"
             "CLOSスタイルのオブジェクトシステムでは問題にならないと言った理由"
             "です。"))

(test-fill "en" fill-data-1 '(:width 30)
           '("Lorem ipsum dolor sit amet,"
             "consectetur adipiscing elit,"
             "sed do eiusmod tempor"
             "incididunt ut labore et dolore"
             "magna aliqua. Ut enim ad minim"
             "veniam, quis nostrud"
             "exercitation ullamco laboris"
             "nisi ut aliquip ex ea commodo"
             "consequat. Duis aute irure"
             "dolor in reprehenderit in"
             "voluptate velit esse cillum"
             "dolore eu fugiat nulla"
             "pariatur. Excepteur sint"
             "occaecat cupidatat non"
             "proident, sunt in culpa qui"
             "officia deserunt mollit anim"
             "id est laborum."))

(test-fill "ja" fill-data-2 '(:width 30)
           '("これらの各メソッドは、<num-"
             "vector>クラスや<num-matrix>"
             "クラスに所属するわけではありま"
             "せん。メソッドがクラスに所有さ"
             "れているわけではないので、既に"
             "存在するクラスに対していつでも"
             "独自のメソッドを定義することが"
             "できます(但し、いくつかの組込"
             "みクラスの既定のメソッドには変"
             "更できないものがあります)。上"
             "の例で既にこれは示されていま"
             "す。プログラマはメソッド"
             "productを組込みクラス<number>"
             "に対して定義することができま"
             "す。これが、全てはオブジェクト"
             "であるかどうかはCLOSスタイルの"
             "オブジェクトシステムでは問題に"
             "ならないと言った理由です。"))

(test-fill "en" fill-data-1 '(:indent 2 :width 50)
           '("  Lorem ipsum dolor sit amet, consectetur"
             "  adipiscing elit, sed do eiusmod tempor"
             "  incididunt ut labore et dolore magna aliqua. Ut"
             "  enim ad minim veniam, quis nostrud exercitation"
             "  ullamco laboris nisi ut aliquip ex ea commodo"
             "  consequat. Duis aute irure dolor in"
             "  reprehenderit in voluptate velit esse cillum"
             "  dolore eu fugiat nulla pariatur. Excepteur sint"
             "  occaecat cupidatat non proident, sunt in culpa"
             "  qui officia deserunt mollit anim id est laborum."))

(test-fill "ja" fill-data-2 '(:indent 2 :width 50)
           '("  これらの各メソッドは、<num-vector>クラスや<num-"
             "  matrix>クラスに所属するわけではありません。"
             "  メソッドがクラスに所有されているわけではないの"
             "  で、既に存在するクラスに対していつでも独自の"
             "  メソッドを定義することができます(但し、いくつか"
             "  の組込みクラスの既定のメソッドには変更できないも"
             "  のがあります)。上の例で既にこれは示されていま"
             "  す。プログラマはメソッドproductを組込みクラス<"
             "  number>に対して定義することができます。これが、"
             "  全てはオブジェクトであるかどうかはCLOSスタイルの"
             "  オブジェクトシステムでは問題にならないと言った理"
             "  由です。"))

;; If single word in a line overflows width, we let it overflow.
(test-fill "en" fill-data-1 '(:indent 2 :hanging 10 :width 12)
           '("          Lorem"
             "  ipsum"
             "  dolor sit"
             "  amet,"
             "  consectetur"
             "  adipiscing"
             "  elit, sed"
             "  do eiusmod"
             "  tempor"
             "  incididunt"
             "  ut labore"
             "  et dolore"
             "  magna"
             "  aliqua. Ut"
             "  enim ad"
             "  minim"
             "  veniam,"
             "  quis"
             "  nostrud"
             "  exercitation"
             "  ullamco"
             "  laboris"
             "  nisi ut"
             "  aliquip ex"
             "  ea commodo"
             "  consequat."
             "  Duis aute"
             "  irure"
             "  dolor in"
             "  reprehenderit"
             "  in"
             "  voluptate"
             "  velit esse"
             "  cillum"
             "  dolore eu"
             "  fugiat"
             "  nulla"
             "  pariatur."
             "  Excepteur"
             "  sint"
             "  occaecat"
             "  cupidatat"
             "  non"
             "  proident,"
             "  sunt in"
             "  culpa qui"
             "  officia"
             "  deserunt"
             "  mollit"
             "  anim id"
             "  est"
             "  laborum."))

(test-fill "en + lead-in" fill-data-1
           '(:hanging 10 :lead-in "A" :indent 2)
           '("A         Lorem ipsum dolor sit amet, consectetur adipiscing"
             "  elit, sed do eiusmod tempor incididunt ut labore et dolore"
             "  magna aliqua. Ut enim ad minim veniam, quis nostrud"
             "  exercitation ullamco laboris nisi ut aliquip ex ea commodo"
             "  consequat. Duis aute irure dolor in reprehenderit in voluptate"
             "  velit esse cillum dolore eu fugiat nulla pariatur. Excepteur"
             "  sint occaecat cupidatat non proident, sunt in culpa qui officia"
             "  deserunt mollit anim id est laborum."))


(test-fill "en + lead-in" fill-data-1
           '(:hanging 10 :lead-in "ABCDEFGHI" :indent 2)
           '("ABCDEFGHI Lorem ipsum dolor sit amet, consectetur adipiscing"
             "  elit, sed do eiusmod tempor incididunt ut labore et dolore"
             "  magna aliqua. Ut enim ad minim veniam, quis nostrud"
             "  exercitation ullamco laboris nisi ut aliquip ex ea commodo"
             "  consequat. Duis aute irure dolor in reprehenderit in voluptate"
             "  velit esse cillum dolore eu fugiat nulla pariatur. Excepteur"
             "  sint occaecat cupidatat non proident, sunt in culpa qui officia"
             "  deserunt mollit anim id est laborum."))

(test-fill "en + lead-in" fill-data-1
           '(:hanging 10 :lead-in "ABCDEFGHIJ" :indent 2)
           '("ABCDEFGHIJ"
             "          Lorem ipsum dolor sit amet, consectetur adipiscing"
             "  elit, sed do eiusmod tempor incididunt ut labore et dolore"
             "  magna aliqua. Ut enim ad minim veniam, quis nostrud"
             "  exercitation ullamco laboris nisi ut aliquip ex ea commodo"
             "  consequat. Duis aute irure dolor in reprehenderit in voluptate"
             "  velit esse cillum dolore eu fugiat nulla pariatur. Excepteur"
             "  sint occaecat cupidatat non proident, sunt in culpa qui officia"
             "  deserunt mollit anim id est laborum."))

(test-fill "en + lead-in" fill-data-1
           '(:hanging 10 :lead-in "ABCDEFGHIJKLMN" :indent 2)
           '("ABCDEFGHIJKLMN"
             "          Lorem ipsum dolor sit amet, consectetur adipiscing"
             "  elit, sed do eiusmod tempor incididunt ut labore et dolore"
             "  magna aliqua. Ut enim ad minim veniam, quis nostrud"
             "  exercitation ullamco laboris nisi ut aliquip ex ea commodo"
             "  consequat. Duis aute irure dolor in reprehenderit in voluptate"
             "  velit esse cillum dolore eu fugiat nulla pariatur. Excepteur"
             "  sint occaecat cupidatat non proident, sunt in culpa qui officia"
             "  deserunt mollit anim id est laborum."))

(test-fill "en paragraph break" fill-data-3
           '(:hanging 8 :lead-in "START" :indent 3)
           '("START   Lorem ipsum dolor sit amet, consectetur adipiscing elit,"
             "   sed do eiusmod tempor incididunt ut labore et dolore magna"
             "   aliqua."
             "        Ut enim ad minim veniam, quis nostrud exercitation"
             "   ullamco laboris nisi ut aliquip ex ea commodo consequat."
             "        Duis aute irure dolor in reprehenderit in voluptate velit"
             "   esse cillum dolore eu fugiat nulla pariatur. Excepteur sint"
             "   occaecat cupidatat non proident, sunt in culpa qui officia"
             "   deserunt mollit anim id est laborum."))

(test-end)
