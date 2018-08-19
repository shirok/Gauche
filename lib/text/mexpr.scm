;;;
;;; text.mexpr - M-expression parser
;;;
;;;   By Shiro Kawai  <shiro@acm.org>
;;;
;;;   Public domain.  This is just a little sample of parser.peg,
;;;   and I wrote it just for fun.
;;;

(define-module text.mexpr
  (use parser.peg)
  (use util.match)
  (export parse-mexpr))
(select-module text.mexpr)

;;
;; Tokenizer
;;
(define %ws ($skip-many ($one-of #[ \t\r\n])))

(define (make-word chars)
  (let1 s (list->string chars)
    (if-let1 n (string->number s)
      `(number ,n)
      (cond [(#/^[A-Z][A-Z0-9]*$/ s) `(atom ,(string->symbol s))]
            [(#/^[a-z][a-z0-9]*$/ s) `(identifier ,(string->symbol s))]
            [else (error "Invalid word: " s)]))))

(define %word ($lift make-word ($many-chars #[0-9a-zA-Z] 1)))

;; A few reserved word
(define %lambda ($seq ($s "lambda") ($return 'lambda)))
(define %label  ($seq ($s "label") ($return 'label)))
(define %-> ($seq ($or ($s "->") ($c #\u2192)) ($return '->))) ; right arrow

(define %token
  ($between %ws
            ($or %-> %lambda %label %word ($one-of #[\[\]\(\).\;=]))
            %ws))

(define (tokenize input)
  (generator->lseq (peg-parser->generator %token input)))

;;
;; Parser
;;

(define (snd x y) y)
(define (tok-atom? x)       (match x [('atom x) x] [_ #f]))
(define (tok-number? x)     (match x [('number x) x] [_ #f]))
(define (tok-identifier? x) (match x [('identifier x) x] [_ #f]))

(define %atom       ($satisfy tok-atom? 'atom snd))
(define %number     ($satisfy tok-number? 'number snd))
(define %identifier ($satisfy tok-identifier? 'identifier snd))

(define %datum ($lazy ($or %atom %number %list)))

(define %list-tail
  ($lazy ($or ($seq ($c #\)) ($return '()))
              ($between ($c #\.) %datum ($c #\)))
              ($do [x %datum]
                   [y %list-tail]
                   ($return (cons x y))))))
            
(define %list
  ($seq ($c #\()
        ($or ($seq ($c #\)) ($return '()))
             ($do [x %datum]
                  [y %list-tail]
                  ($return (cons x y))))))

(define %form
  ($lazy ($or ($do [x %datum] ($return `(quote ,x)))
              %conditional
              %funcall-or-variable)))

(define %conditional-clause
  ($do [test %form]
       [($satisfy (cut eq? <> '->) '->)]
       [expr %form]
       ($return (list test expr))))

(define %conditional
  ($do [clauses ($between ($c #\[)
                          ($sep-by %conditional-clause ($c #\;))
                          ($c #\]))]
       ($return `(cond ,@clauses))))

(define %function ($lazy ($or %lambda-form %label-form %identifier)))

(define %lambda-form
  ($do [($satisfy (cut eq? 'lambda <>) 'lambda)]
       [($c #\[)]
       [args ($between ($c #\[)
                       ($sep-by %identifier ($c #\;))
                       ($c #\]))]
       [($c #\;)]
       [body %form]
       [($c #\])]
       ($return `(lambda ,args ,body))))

(define %label-form
  ($do [($satisfy (cut eq? 'label <>) 'label)]
       [($c #\[)]
       [id %identifier]
       [($c #\;)]
       [f %function]
       [($c #\])]
       ($return `(label ,id ,f))))

(define %funcall-or-variable
  ($do [head %function]
       [args ($optional ($between ($c #\[)
                                  ($sep-by %form ($c #\;))
                                  ($c #\])))]
       [follow ($optional ($seq ($c #\=) %form))]
       ($return (let1 pre (if args (cons head args) head)
                  (if follow
                    `(define ,pre ,follow)
                    pre)))))

(define (parse-mexpr input)
  (values-ref (peg-run-parser %form (tokenize input)) 0))
