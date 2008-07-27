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
  (let* ((spaces_ ($skip-many ($one-of #[ \t])))
         (spaces ($many ($one-of #[ \t])))
         (comma  ($seq spaces_ ($char #\,) spaces_))
         (dquote ($char #\"))
         (double-dquote ($do [($string "\"\"")] ($return #\")))
         (quoted-body ($many ($or ($one-of #[^\"]) double-dquote)))
         (quoted ($between dquote quoted-body dquote))
         (unquoted ($alternate ($one-of #[^ \t\n,]) spaces))
         (field ($or quoted unquoted))
         (record ($sep-by ($->rope field) comma 1)))
    ($sep-by record newline)))

(time (peg-parse-string csv-parser data))

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
