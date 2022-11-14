;;
;; srfi-236 Evaluating expressions in an unspecified order
;;

(define-module srfi-236
  (use util.match)
  (export independently))
(select-module srfi-236)

(define-syntax independently
  (er-macro-transformer
   (^[f r c]
     (quasirename r
       `(let ,(map (^[expr] (quasirename r
                              `(,(gensym) (begin ,expr #f))))
                   (cdr f))
          (undefined))))))
