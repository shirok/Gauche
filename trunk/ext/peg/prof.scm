(use gauche.vm.profiler)
(use parser.peg)

(define data
  (call-with-input-file "data/13tokyo.csv"
    port->string
    :encoding 'utf8))

(define csv-parser
  (let* ((spaces  ($many ($one-of #[ \t])))
         (spaces_ ($many_ ($one-of #[ \t])))
         (comma ($seq spaces_ ($char #\,) spaces_))
         (dquote ($char #\"))
         (double-dquote ($do (($string "\"\"")) ($return #\")))
         (quoted-body ($many ($or ($one-of #[^\"]) double-dquote)))
         (quoted ($between dquote quoted-body dquote))
         (unquoted ($alternate ($one-of #[^ \t\n,]) spaces))
         (field ($or quoted unquoted))
         (record ($sep-by ($->rope field) comma 1)))
    ($sep-by record newline)))

(profiler-start)
(parse-string csv-parser data)
(profiler-stop)

(profiler-show)


