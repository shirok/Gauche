(define-module text.console.wide-char-setting
  (use gauche.unicode)
  (export <wide-char-setting> get-char-width))
(select-module text.console.wide-char-setting)

;; <wide-char-setting>
;; Initializable slots:
;;   mode    - specify the mode for determining widths of wide characters.
;;             If 'Unicode is specified, character widths are determined
;;             by East Asian Width of Unicode.
;;             If 'Surrogate is specified, character widths are determined
;;             by checking surrogate pairs of Unicode.
;;             If 'Wide is specified, character widths are determined
;;             by only character codes.
;;             Otherwise, wide character support is disabled.
;;   wide-char-width - a width of wide characters.
;;   surrogate-char-width - a width of surrogate pair characters of Unicode.
;;   ambiguous-char-width - a width of ambiguous width characters of Unicode.
;;             To the above 3 slots, specify a multiple of the width of
;;             half-width characters.
;;   emoji-char-workaround - if this is not #f, emoji characters are
;;             treated as wide characters.
;;
(define-class <wide-char-setting> ()
  ((mode :init-keyword :mode :init-value 'Unicode)
   (wide-char-width :init-keyword :wide-char-width :init-value 2)
   (surrogate-char-width :init-keyword :surrogate-char-width :init-value 2)
   (ambiguous-char-width :init-keyword :ambiguous-char-width :init-value 1)
   (emoji-char-workaround :init-keyword :emoji-char-workaround :init-value #t)
   ))

;; Get a character width
(define (get-char-width wide-char-setting ch)
  (define wide-char-mode        (~ wide-char-setting'mode))
  (define wide-char-width       (~ wide-char-setting'wide-char-width))
  (define surrogate-char-width  (~ wide-char-setting'surrogate-char-width))
  (define ambiguous-char-width  (~ wide-char-setting'ambiguous-char-width))
  (define emoji-char-workaround (~ wide-char-setting'emoji-char-workaround))
  (define chcode (char->integer ch))
  (cond
   [(<= 0 chcode #x7f)
    1]
   [else
    (cond-expand
     [gauche.ces.utf8
      (case wide-char-mode
        [(Unicode)
         (if (and emoji-char-workaround
                  ;(<= #x1f000 chcode #x1ffff))
                  (unicode-emoji? chcode))
           wide-char-width
           (case (char-east-asian-width ch)
             [(A)      ambiguous-char-width]
             [(F W)    wide-char-width]
             [(H N Na) 1]
             [else     ambiguous-char-width]))]
        [(Surrogate)
         (if (>= chcode #x10000)
           surrogate-char-width
           wide-char-width)]
        [(Wide)
         wide-char-width]
        [else
         1])]
     [else
      (case wide-char-mode
        [(Unicode Surrogate Wide)
         wide-char-width]
        [else
         1])])]))

;; unicode emoji table
;;  generated from
;;  https://unicode.org/Public/UNIDATA/emoji/emoji-data.txt
;;  (excluding less than U+1000)
;;
;; TODO: integrate into gauche.unicode
;;
(define *unicode-emoji-table*
  (vector
   '(#x0203c . #x0203c) '(#x02049 . #x02049) '(#x02122 . #x02122)
   '(#x02139 . #x02139) '(#x02194 . #x02199) '(#x021a9 . #x021aa)
   '(#x0231a . #x0231b) '(#x02328 . #x02328) '(#x023cf . #x023cf)
   '(#x023e9 . #x023f3) '(#x023f8 . #x023fa) '(#x024c2 . #x024c2)
   '(#x025aa . #x025ab) '(#x025b6 . #x025b6) '(#x025c0 . #x025c0)
   '(#x025fb . #x025fe) '(#x02600 . #x02604) '(#x0260e . #x0260e)
   '(#x02611 . #x02611) '(#x02614 . #x02615) '(#x02618 . #x02618)
   '(#x0261d . #x0261d) '(#x02620 . #x02620) '(#x02622 . #x02623)
   '(#x02626 . #x02626) '(#x0262a . #x0262a) '(#x0262e . #x0262f)
   '(#x02638 . #x0263a) '(#x02640 . #x02640) '(#x02642 . #x02642)
   '(#x02648 . #x02653) '(#x0265f . #x02660) '(#x02663 . #x02663)
   '(#x02665 . #x02666) '(#x02668 . #x02668) '(#x0267b . #x0267b)
   '(#x0267e . #x0267f) '(#x02692 . #x02697) '(#x02699 . #x02699)
   '(#x0269b . #x0269c) '(#x026a0 . #x026a1) '(#x026a7 . #x026a7)
   '(#x026aa . #x026ab) '(#x026b0 . #x026b1) '(#x026bd . #x026be)
   '(#x026c4 . #x026c5) '(#x026c8 . #x026c8) '(#x026ce . #x026cf)
   '(#x026d1 . #x026d1) '(#x026d3 . #x026d4) '(#x026e9 . #x026ea)
   '(#x026f0 . #x026f5) '(#x026f7 . #x026fa) '(#x026fd . #x026fd)
   '(#x02702 . #x02702) '(#x02705 . #x02705) '(#x02708 . #x0270d)
   '(#x0270f . #x0270f) '(#x02712 . #x02712) '(#x02714 . #x02714)
   '(#x02716 . #x02716) '(#x0271d . #x0271d) '(#x02721 . #x02721)
   '(#x02728 . #x02728) '(#x02733 . #x02734) '(#x02744 . #x02744)
   '(#x02747 . #x02747) '(#x0274c . #x0274c) '(#x0274e . #x0274e)
   '(#x02753 . #x02755) '(#x02757 . #x02757) '(#x02763 . #x02764)
   '(#x02795 . #x02797) '(#x027a1 . #x027a1) '(#x027b0 . #x027b0)
   '(#x027bf . #x027bf) '(#x02934 . #x02935) '(#x02b05 . #x02b07)
   '(#x02b1b . #x02b1c) '(#x02b50 . #x02b50) '(#x02b55 . #x02b55)
   '(#x03030 . #x03030) '(#x0303d . #x0303d) '(#x03297 . #x03297)
   '(#x03299 . #x03299) '(#x1f004 . #x1f004) '(#x1f0cf . #x1f0cf)
   '(#x1f170 . #x1f171) '(#x1f17e . #x1f17f) '(#x1f18e . #x1f18e)
   '(#x1f191 . #x1f19a) '(#x1f1e6 . #x1f1ff) '(#x1f201 . #x1f202)
   '(#x1f21a . #x1f21a) '(#x1f22f . #x1f22f) '(#x1f232 . #x1f23a)
   '(#x1f250 . #x1f251) '(#x1f300 . #x1f321) '(#x1f324 . #x1f393)
   '(#x1f396 . #x1f397) '(#x1f399 . #x1f39b) '(#x1f39e . #x1f3f0)
   '(#x1f3f3 . #x1f3f5) '(#x1f3f7 . #x1f4fd) '(#x1f4ff . #x1f53d)
   '(#x1f549 . #x1f54e) '(#x1f550 . #x1f567) '(#x1f56f . #x1f570)
   '(#x1f573 . #x1f57a) '(#x1f587 . #x1f587) '(#x1f58a . #x1f58d)
   '(#x1f590 . #x1f590) '(#x1f595 . #x1f596) '(#x1f5a4 . #x1f5a5)
   '(#x1f5a8 . #x1f5a8) '(#x1f5b1 . #x1f5b2) '(#x1f5bc . #x1f5bc)
   '(#x1f5c2 . #x1f5c4) '(#x1f5d1 . #x1f5d3) '(#x1f5dc . #x1f5de)
   '(#x1f5e1 . #x1f5e1) '(#x1f5e3 . #x1f5e3) '(#x1f5e8 . #x1f5e8)
   '(#x1f5ef . #x1f5ef) '(#x1f5f3 . #x1f5f3) '(#x1f5fa . #x1f64f)
   '(#x1f680 . #x1f6c5) '(#x1f6cb . #x1f6d2) '(#x1f6d5 . #x1f6d7)
   '(#x1f6e0 . #x1f6e5) '(#x1f6e9 . #x1f6e9) '(#x1f6eb . #x1f6ec)
   '(#x1f6f0 . #x1f6f0) '(#x1f6f3 . #x1f6fc) '(#x1f7e0 . #x1f7eb)
   '(#x1f90c . #x1f93a) '(#x1f93c . #x1f945) '(#x1f947 . #x1f978)
   '(#x1f97a . #x1f9cb) '(#x1f9cd . #x1f9ff) '(#x1fa70 . #x1fa74)
   '(#x1fa78 . #x1fa7a) '(#x1fa80 . #x1fa86) '(#x1fa90 . #x1faa8)
   '(#x1fab0 . #x1fab6) '(#x1fac0 . #x1fac2) '(#x1fad0 . #x1fad6)))

;; check if unicode emoji character
(define (unicode-emoji? chcode)
  (define min-index 0)
  (define max-index (- (vector-length *unicode-emoji-table*) 1))
  (if (or (< chcode (car (vector-ref *unicode-emoji-table* 0)))
          (> chcode (cdr (vector-ref *unicode-emoji-table* max-index))))
    #f
    (let loop ()
      (cond
       [(<= min-index max-index)
        (let1 middle-index (quotient (+ min-index max-index) 2)
          (cond
           [(< chcode (car (vector-ref *unicode-emoji-table* middle-index)))
            (set! max-index (- middle-index 1))
            (loop)]
           [(> chcode (cdr (vector-ref *unicode-emoji-table* middle-index)))
            (set! min-index (+ middle-index 1))
            (loop)]
           [else
            #t]))]
       [else
        #f]))))

