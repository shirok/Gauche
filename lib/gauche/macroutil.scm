;;;
;;; Miscellaneous macro-related utilities
;;;

(define-module gauche.macroutil
  (use util.match)
  (export quasirename))
(select-module gauche.macroutil)

;; TRANSIENT: We'll put this into libmac.scm after 0.9.6 release.
;; 0.9.5's er-macro-transformer would emit reference to obsoleted
;; support procedures, and we don't want to depend on them.
(define-syntax quasirename
  (er-macro-transformer
   (^[f r c]
     (define unquote. (r'unquote))
     (define (unquote? x)
       (and (or (symbol? x) (identifier? x))
            (c (r x) unquote.)))
     (define unquote-splicing. (r'unquote-splicing))
     (define (unquote-splicing? x)
       (and (or (symbol? x) (identifier? x))
            (c (r x) unquote-splicing.)))
     (define cons. (r'cons))
     (define append. (r'append))
     (define vector. (r'vector))
     (define let. (r'let))
     (define tmp. (r'tmp))
     (match f
       [(_ rr ff)
        (define (rec ff)
          (match ff
            [((? unquote?) x) x]
            [(((? unquote-splicing?) x) . y)
             (if (null? y)
               x
               `(,append. ,x ,(rec y)))]
            [(x (? unquote?) y) `(,cons. ,(rec x) ,y)]
            [(x . y) `(,cons. ,(rec x) ,(rec y))]
            [(? symbol?) `(,tmp. ',ff)]
            [(? identifier?) `(,tmp. ',ff)]
            [(? vector?) (cons vector. (map rec (vector->list ff)))]
            [_ ff]))
        `(,let. ((,tmp. ,rr))
           ,(rec ff))]
       [_ (error "malformed quasirename:" f)]))))
