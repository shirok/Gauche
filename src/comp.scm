;;
;; A compiler.
;;

(define-module gauche.compile
  (use util.match)
  )
(select-module gauche.compile)

;; Entry
(define (compile program . opts)
  (compile-int program (get-optional opts #f) 'tail))

;;
(define (compile-int program env ctx)
  (match program
    ((op . args)
     (if (or (symbol? op) (identifier? op))
       (let ((var (lookup-env op env #t)))
         (compile-varref var env))))
    ((@or (? symbol?) (? identifier?) form)
     (compile-varref var env))
    (else
     (if (eq? ctx 'stmt) '() (list program)))
    ))

;;
(define (comp-test . arg)
  (warn "Comp-test!!! ~s" arg))

           
  
