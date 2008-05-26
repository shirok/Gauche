(use parser.peg)
(use gauche.charconv)
(use srfi-42)

(define data
  (call-with-input-file "data/13tokyo.csv"
    port->string
    :encoding 'utf8))

(define csv-parser
  (let* ((spaces_ ($many_ ($one-of #[ \t])))
         (spaces ($many ($one-of #[ \t])))
         (comma  ($seq spaces_ ($char #\,) spaces_))
         (dquote ($char #\"))
         (double-dquote ($do (($string "\"\"")) ($return #\")))
         (quoted-body ($many ($or ($one-of #[^\"]) double-dquote)))
         (quoted ($between dquote quoted-body dquote))
         (unquoted ($alternate ($one-of #[^ \t\n,]) spaces))
         (field ($or quoted unquoted))
         (record ($sep-by ($->rope field) comma 1)))
    ($sep-by record newline)))

(time (parse-string csv-parser data))

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

;;;============================================================
(use text.csv)
(define reader (make-csv-reader #\,))

(time (port->list reader (open-input-string data)))
