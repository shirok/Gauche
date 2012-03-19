(use gauche.vm.profiler)
(use parser.peg)
(use text.csv)

(define data
  (let1 d (call-with-input-file "data/13tokyo.csv"
            port->string
            :encoding 'utf8)
    (string-append d d d d d)))

(define csv-parser
  (let* ((spaces  ($many ($one-of #[ \t])))
         (skip-spaces ($skip-many ($one-of #[ \t])))
         (comma ($seq skip-spaces ($char #\,) skip-spaces))
         (dquote ($char #\"))
         (double-dquote ($do (($string "\"\"")) ($return #\")))
         (quoted-body ($many ($or ($one-of #[^\"]) double-dquote)))
         (quoted ($between dquote quoted-body dquote))
         (unquoted ($alternate ($one-of #[^ \t\n,]) spaces))
         (field ($or quoted unquoted))
         (record ($sep-by ($->rope field) comma 1)))
    ($sep-by record newline)))

(profiler-start)
(peg-parse-string csv-parser data)
(profiler-stop)
(profiler-show)
(profiler-reset)

(define reader (make-csv-reader #\,))

(profiler-start)
(call-with-input-string data (^p (generator-for-each identity (cut reader p))))
(profiler-stop)
(profiler-show)


