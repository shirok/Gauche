;;;
;;; Generate some of the data files
;;;

(use file.util)
(use gauche.charconv)
(use gauche.uvector)
(use srfi-42)

;; Latin-1 test dataset
(define (do-lat1 suffix emitter exclude-b5?)
  (with-output-to-file #"~(if exclude-b5? 'lat1x 'lat1).~suffix"
    (^[] (dotimes [n 256]
           (cond [(<= n #x7f) (write-byte n)]
                 [(and exclude-b5? (= n #xb5))]
                 [(<= #xa0 n) (emitter n)])
           (when (zero? (modulo (+ n 1) 16)) (newline))))))

(define (do-lat1* suffix emitter)
  (do-lat1 suffix emitter #f)
  (do-lat1 suffix emitter #t))
  
(define (lat1.iso8859-1)
  (do-lat1* "ISO8859-1" write-byte))

(define (lat1.ascii)
  (do-lat1 "ASCII" (^b (write-char #\?)) #f))

(define (lat1.utf-8)
  (do-lat1* "UTF-8"
            (^b (write-byte (+ #xc0 (ash b -6)))
                (write-byte (+ #x80 (logand b #x3f))))))

;; NB: U+00B5 (MICRO SIGN) doesn't have a corresponding JIS character.
;; We could've mapped it ot greek mu, but then we'll lose round-trip identity.
(define *lat1-eucjp-data* "
0xA9A2	U+00A0	# NO-BREAK SPACE	[2000]
0xA9A3	U+00A1	# INVERTED EXCLAMATION MARK	[2000]
0xA1F1	U+00A2	# CENT SIGN	Windows: U+FFE0
0xA1F2	U+00A3	# POUND SIGN	Windows: U+FFE1
0xA9A4	U+00A4	# CURRENCY SIGN	[2000]
0xA1EF	U+00A5	# YEN SIGN	Windows: U+FFE5
0xA9A5	U+00A6	# BROKEN BAR	[2000]
0xA1F8	U+00A7	# SECTION SIGN
0xA1AF	U+00A8	# DIAERESIS
0xA9A6	U+00A9	# COPYRIGHT SIGN	[2000]
0xA9A7	U+00AA	# FEMININE ORDINAL INDICATOR	[2000]
0xA9A8	U+00AB	# LEFT-POINTING DOUBLE ANGLE QUOTATION MARK 	[2000]
0xA2CC	U+00AC	# NOT SIGN	[1983]	Windows: U+FFE2
0xA9A9	U+00AD	# SOFT HYPHEN	[2000]
0xA9AA	U+00AE	# REGISTERED SIGN	[2000]
0xA9AB	U+00AF	# MACRON	[2000]
0xA1EB	U+00B0	# DEGREE SIGN
0xA1DE	U+00B1	# PLUS-MINUS SIGN
0xA9AC	U+00B2	# SUPERSCRIPT TWO	[2000]
0xA9AD	U+00B3	# SUPERSCRIPT THREE	[2000]
0xA1AD	U+00B4	# ACUTE ACCENT
0xA2F9	U+00B6	# PILCROW SIGN	[1983]
0xA9AE	U+00B7	# MIDDLE DOT	[2000]
0xA9AF	U+00B8	# CEDILLA	[2000]
0xA9B0	U+00B9	# SUPERSCRIPT ONE	[2000]
0xA9B1	U+00BA	# MASCULINE ORDINAL INDICATOR	[2000]
0xA9B2	U+00BB	# RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK 	[2000]
0xA9B3	U+00BC	# VULGAR FRACTION ONE QUARTER	[2000]
0xA9B4	U+00BD	# VULGAR FRACTION ONE HALF	[2000]
0xA9B5	U+00BE	# VULGAR FRACTION THREE QUARTERS	[2000]
0xA9B6	U+00BF	# INVERTED QUESTION MARK	[2000]
0xA9B7	U+00C0	# LATIN CAPITAL LETTER A WITH GRAVE	[2000]
0xA9B8	U+00C1	# LATIN CAPITAL LETTER A WITH ACUTE	[2000]
0xA9B9	U+00C2	# LATIN CAPITAL LETTER A WITH CIRCUMFLEX	[2000]
0xA9BA	U+00C3	# LATIN CAPITAL LETTER A WITH TILDE	[2000]
0xA9BB	U+00C4	# LATIN CAPITAL LETTER A WITH DIAERESIS	[2000]
0xA9BC	U+00C5	# LATIN CAPITAL LETTER A WITH RING ABOVE	[2000]
0xA9BD	U+00C6	# LATIN CAPITAL LETTER AE 	[2000]
0xA9BE	U+00C7	# LATIN CAPITAL LETTER C WITH CEDILLA	[2000]
0xA9BF	U+00C8	# LATIN CAPITAL LETTER E WITH GRAVE	[2000]
0xA9C0	U+00C9	# LATIN CAPITAL LETTER E WITH ACUTE	[2000]
0xA9C1	U+00CA	# LATIN CAPITAL LETTER E WITH CIRCUMFLEX	[2000]
0xA9C2	U+00CB	# LATIN CAPITAL LETTER E WITH DIAERESIS	[2000]
0xA9C3	U+00CC	# LATIN CAPITAL LETTER I WITH GRAVE	[2000]
0xA9C4	U+00CD	# LATIN CAPITAL LETTER I WITH ACUTE	[2000]
0xA9C5	U+00CE	# LATIN CAPITAL LETTER I WITH CIRCUMFLEX	[2000]
0xA9C6	U+00CF	# LATIN CAPITAL LETTER I WITH DIAERESIS	[2000]
0xA9C7	U+00D0	# LATIN CAPITAL LETTER ETH 	[2000]
0xA9C8	U+00D1	# LATIN CAPITAL LETTER N WITH TILDE	[2000]
0xA9C9	U+00D2	# LATIN CAPITAL LETTER O WITH GRAVE	[2000]
0xA9CA	U+00D3	# LATIN CAPITAL LETTER O WITH ACUTE	[2000]
0xA9CB	U+00D4	# LATIN CAPITAL LETTER O WITH CIRCUMFLEX	[2000]
0xA9CC	U+00D5	# LATIN CAPITAL LETTER O WITH TILDE	[2000]
0xA9CD	U+00D6	# LATIN CAPITAL LETTER O WITH DIAERESIS	[2000]
0xA1DF	U+00D7	# MULTIPLICATION SIGN
0xA9CE	U+00D8	# LATIN CAPITAL LETTER O WITH STROKE	[2000]
0xA9CF	U+00D9	# LATIN CAPITAL LETTER U WITH GRAVE	[2000]
0xA9D0	U+00DA	# LATIN CAPITAL LETTER U WITH ACUTE	[2000]
0xA9D1	U+00DB	# LATIN CAPITAL LETTER U WITH CIRCUMFLEX	[2000]
0xA9D2	U+00DC	# LATIN CAPITAL LETTER U WITH DIAERESIS	[2000]
0xA9D3	U+00DD	# LATIN CAPITAL LETTER Y WITH ACUTE	[2000]
0xA9D4	U+00DE	# LATIN CAPITAL LETTER THORN 	[2000]
0xA9D5	U+00DF	# LATIN SMALL LETTER SHARP S 	[2000]
0xA9D6	U+00E0	# LATIN SMALL LETTER A WITH GRAVE	[2000]
0xA9D7	U+00E1	# LATIN SMALL LETTER A WITH ACUTE	[2000]
0xA9D8	U+00E2	# LATIN SMALL LETTER A WITH CIRCUMFLEX	[2000]
0xA9D9	U+00E3	# LATIN SMALL LETTER A WITH TILDE	[2000]
0xA9DA	U+00E4	# LATIN SMALL LETTER A WITH DIAERESIS	[2000]
0xA9DB	U+00E5	# LATIN SMALL LETTER A WITH RING ABOVE	[2000]
0xA9DC	U+00E6	# LATIN SMALL LETTER AE 	[2000]
0xA9DD	U+00E7	# LATIN SMALL LETTER C WITH CEDILLA	[2000]
0xA9DE	U+00E8	# LATIN SMALL LETTER E WITH GRAVE	[2000]
0xA9DF	U+00E9	# LATIN SMALL LETTER E WITH ACUTE	[2000]
0xA9E0	U+00EA	# LATIN SMALL LETTER E WITH CIRCUMFLEX	[2000]
0xA9E1	U+00EB	# LATIN SMALL LETTER E WITH DIAERESIS	[2000]
0xA9E2	U+00EC	# LATIN SMALL LETTER I WITH GRAVE	[2000]
0xA9E3	U+00ED	# LATIN SMALL LETTER I WITH ACUTE	[2000]
0xA9E4	U+00EE	# LATIN SMALL LETTER I WITH CIRCUMFLEX	[2000]
0xA9E5	U+00EF	# LATIN SMALL LETTER I WITH DIAERESIS	[2000]
0xA9E6	U+00F0	# LATIN SMALL LETTER ETH 	[2000]
0xA9E7	U+00F1	# LATIN SMALL LETTER N WITH TILDE	[2000]
0xA9E8	U+00F2	# LATIN SMALL LETTER O WITH GRAVE	[2000]
0xA9E9	U+00F3	# LATIN SMALL LETTER O WITH ACUTE	[2000]
0xA9EA	U+00F4	# LATIN SMALL LETTER O WITH CIRCUMFLEX	[2000]
0xA9EB	U+00F5	# LATIN SMALL LETTER O WITH TILDE	[2000]
0xA9EC	U+00F6	# LATIN SMALL LETTER O WITH DIAERESIS	[2000]
0xA1E0	U+00F7	# DIVISION SIGN
0xA9ED	U+00F8	# LATIN SMALL LETTER O WITH STROKE	[2000]
0xA9EE	U+00F9	# LATIN SMALL LETTER U WITH GRAVE	[2000]
0xA9EF	U+00FA	# LATIN SMALL LETTER U WITH ACUTE	[2000]
0xA9F0	U+00FB	# LATIN SMALL LETTER U WITH CIRCUMFLEX	[2000]
0xA9F1	U+00FC	# LATIN SMALL LETTER U WITH DIAERESIS	[2000]
0xA9F2	U+00FD	# LATIN SMALL LETTER Y WITH ACUTE	[2000]
0xA9F3	U+00FE	# LATIN SMALL LETTER THORN 	[2000]
0xA9F4	U+00FF	# LATIN SMALL LETTER Y WITH DIAERESIS	[2000]")

(define *lat1-eucjp-table*
  ;; NB: We use U+3013 (EUC A2AE, "Geta Mark") as the replacement
  ;; character respecting the tradition.
  (rlet1 tab (make-vector (- #x100 #xa0) #xa2ae)
    (map (^[line]
           (rxmatch-case line
             [#/0x([[:xdigit:]]+)\s+U\+([[:xdigit:]]+)/ (_ e u)
              (vector-set! tab
                           (- (string->number u 16) #xa0)
                           (string->number e 16))]))
         (string-split *lat1-eucjp-data* #\newline))))

(define (lat1.eucjp)
  (do-lat1* "EUCJP"
            (^b (let1 c (vector-ref *lat1-eucjp-table* (- b #xa0))
                  (if (<= c #xff)
                    (write-byte c)
                    (begin
                      (write-byte (ash c -8))
                      (write-byte (logand c #xff))))))))

(define (lat1.sjis)
  (do-lat1* "SJIS"
            (^b (let1 c (vector-ref *lat1-eucjp-table* (- b #xa0))
                  (if (<= c #xff)
                    (write-byte c)
                    (let1 m (open-output-string)
                      (write-byte (ash c -8) m)
                      (write-byte (logand c #xff) m)
                      (display (ces-convert (get-output-string m)
                                            'eucjp 'sjis))))))))

(define (lat1.iso2022-jp)
  (with-output-to-file #"lat1.ISO2022JP"
    (^[] (display 
          (ces-convert (file->string "lat1.EUCJP") 'eucjp 'iso2022-jp)))))

(define (lat1.utf-16)
  (define (expand vec be?)
    (do-ec (: b vec)
           (if be? 
             (begin (write-byte 0) (write-byte b))
             (begin (write-byte b) (write-byte 0)))))
  (define (generate name)
    (let1 vec (call-with-input-file #"~|name|.ISO8859-1" port->uvector)
      (with-output-to-file #"~|name|.UTF-16BE"
        (cut expand vec #t))
      (with-output-to-file #"~|name|.UTF-16LE"
        (cut expand vec #f))
      (with-output-to-file #"~|name|.UTF-16"
        (^[] (write-byte #xfe) (write-byte #xff) (expand vec #t)))))
  (generate "lat1")
  (generate "lat1x"))

(define (gen-lat1)
  (lat1.iso8859-1)
  (lat1.ascii)
  (lat1.utf-8)
  (lat1.utf-16)
  (lat1.eucjp)
  (lat1.sjis)
  (lat1.iso2022-jp))

;;;

(define (main args)
  (gen-lat1))

               
