;;
;; precompiling macros
;;

(define-module macros
  (use util.match)
  (export er-aif im-state))
(select-module macros)

(define-syntax er-aif
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ test then else)
        (quasirename r
          `(let ((,'it ,test))
             (if ,'it ,then ,else)))]))))

(define *internal-state* #f)

(define-syntax im-state
  (make-id-transformer
   (er-macro-transformer
    (^[f r c]
      (match f
        [('set! _ expr)
         (quasirename r
           `(set! *internal-state* ,expr))]
        [(? identifier?)
         (quasirename r `*internal-state*)])))))
