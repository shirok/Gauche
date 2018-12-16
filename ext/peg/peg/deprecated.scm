;; The following parsers are deprecated.  Their name tend to conflict
;; accidentally.
;; We haven't make parser.peg API public, but there may be some external
;; libraries depending on the unofficial API, so we keep them for a while
;; in a separate module.  Such libraries should use parser.peg.deprecated.

(define-module parser.peg.deprecated
  (extend parser.peg)
  (export anychar upper lower letter alphanum digit
          hexdigit newline tab space spaces eof

          $s $c $y)

  ;; for these two, use '$.'
  (define ($s x) ($string x))
  (define ($c x) ($char x))

  (define ($y x) ($symbol x))

  (define (anychar s)
    (if (pair? s)
      (return-result (car s) (cdr s))
      (return-failure/expect "character" s)))

  (define-syntax define-char-parser
    (syntax-rules ()
      ((_ proc charset expect)
       (define proc
         ($expect ($one-of charset) expect)))))

  (define-char-parser upper    #[A-Z]         "upper case letter")
  (define-char-parser lower    #[a-z]         "lower case letter")
  (define-char-parser letter   #[A-Za-z]      "letter")
  (define-char-parser alphanum #[A-Za-z0-9]   "letter or digit")
  (define-char-parser digit    #[0-9]         "digit")
  (define-char-parser hexdigit #[0-9A-Fa-f]   "hexadecimal digit")
  (define-char-parser newline  #[\n]          "newline")
  (define-char-parser tab      #[\t]          "tab")
  (define-char-parser space    #[\s]          "space")

  (define spaces ($lift make-rope ($many space)))

  (define (eof s)
    (if (pair? s)
      (return-failure/expect "end of input" s)
      (return-result (eof-object) s)))
  )

