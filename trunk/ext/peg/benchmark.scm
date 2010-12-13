(use parser.peg)
(use gauche.charconv)
(use srfi-13)
(use srfi-42)

(define data
  (let1 s (call-with-input-file "data/13tokyo.csv"
            port->string
            :encoding 'utf8)
    (string-concatenate (make-list 10 s))))

(define csv-parser
  (let ()
    (define ws     ($skip-many ($one-of #[ \t])))
    (define comma  ($seq ws ($char #\,) ws))
    (define dquote ($char #\"))
    (define double-dquote ($do [($string "\"\"")] ($return #\")))
    (define quoted-body ($many ($or ($one-of #[^\"]) double-dquote)))
    (define quoted ($between dquote quoted-body dquote))
    (define unquoted ($alternate ($many1 ($one-of #[^ \t\r\n,]))
                                 ($many  ($one-of #[ \t]))))
    (define field  ($or quoted unquoted))
    (define record ($sep-by ($->rope field) comma 1))
    ($sep-by record newline)))

;(profiler-start)
(time (peg-parse-string csv-parser data))
;(profiler-stop)

;(profiler-show)

#|
(add-load-path ".")
(use peg-orig)

(define csv-parser
  (let* ((spaces ($many ($one-of #[ \t])))
         (comma ($seq spaces ($char #\,) spaces))
         (dquote ($char #\"))
         (double-dquote ($do (($string "\"\"")) ($return #\")))
         (quoted-body ($many ($or ($one-of #[^\"]) double-dquote)))
         (quoted ($between dquote quoted-body dquote))
         (unquoted ($alternate ($one-of #[^ \t\n,]) spaces))
         (field ($or quoted unquoted))
         (record ($sep-by ($->rope field) comma 1)))
    ($sep-by record newline)))

(time (parse-string csv-parser data))
|#

;;;============================================================
(use text.csv)
(define reader (make-csv-reader #\,))

(time (port->list reader (open-input-string data)))
