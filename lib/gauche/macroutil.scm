;;;
;;; Miscellaneous macro-related utilities
;;;

(define-module gauche.macroutil
  (use util.match)
  (export quasirename))
(select-module gauche.macroutil)

;; This should be compiled into the core in future; for now, because of
;; the dependency to util.match and er-macro-transformer, we put it here.
(define-syntax quasirename
  (er-macro-transformer
   (^[f r c]
     (define unquote. (r'unquote))
     (define (unquote? x)
       (and (or (symbol? x) (identifier? x))
            (c (r x) unquote.)))
     (define cons. (r'cons))
     (define vector. (r'vector))
     (define let. (r'let))
     (define tmp. (r'tmp))
     (match f
       [(_ rr ff)
        (define (rec ff)
          (match ff
            [((? unquote?) x) x]
            [(x (? unquote?) y) `(,cons. ,(rec x) ,y)]
            [(x . y) `(,cons. ,(rec x) ,(rec y))]
            [(? symbol?) `(,tmp. ',ff)]
            [(? identifier?) `(,tmp. ',ff)]
            [(? vector?) (cons vector. (map rec (vector->list ff)))]
            [_ ff]))
        `(,let. ((,tmp. ,rr))
           ,(rec ff))]
       [_ (error "malformed quasirename:" f)]))))
