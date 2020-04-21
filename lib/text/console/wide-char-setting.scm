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
   (ambiguous-char-width :init-keyword :ambiguous-char-width :init-value 2)
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
                  (<= #x1f000 chcode #x1ffff))
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
