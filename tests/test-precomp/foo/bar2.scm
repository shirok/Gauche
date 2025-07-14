(define-module foo.bar2
  (use util.match)
  (export bar2))
(select-module foo.bar2)

(define-syntax bar2
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ x) (quasirename r (list ',x ,x))]))))

  
