;;
;; Code in the manual section "PEG Walkthrough"
;;

(use parser.peg)

(define digits    ($many1 ($. #[\d])))
(define ws        ($many_ ($. #[\s])))
(define separator ($seq ws ($. #\,) ws))

#|
(peg-parse-string digits "12345")
;; => (#\1 #\2 #\3 #\4 #\5)
|#

(define integer
  ($let ([ds digits])
    ($return (x->integer (list->string ds)))))

#|
(peg-parse-string integer "12345")
;; => 12345
|#

(define integers1 ($seq integer
                        ($many ($seq separator integer))))

#|
(peg-parse-string integers1 "123, 456, 789")
;; => (456 789)
|#

(define integers2 ($let ([n  integer]
                         [ns ($many ($seq separator integer))])
                     ($return (cons n ns))))


#|
(peg-parse-string integers2 "123, 456, 789")
;; => (123 456 789)
|#

(define integers3 ($lift cons
                         integer
                         ($many ($seq separator integer))))

#|
(peg-parse-string integers3 "123, 456, 789")
;; => (123 456 789)
|#

(define integers4 ($or ($let ([n  integer]
                              [ns ($many ($seq separator integer))])
                         ($return (cons n ns)))
                       ($return '())))

#|
(peg-parse-string integers4 " ")
;; => ()
|#

(define (sep-by stuff separator)
  ($or ($let ([n  stuff]
              [ns ($many ($seq separator stuff))])
         ($return (cons n ns)))
       ($return '())))
(define integers5 (sep-by integer separator))

#|
(peg-parse-string integers5 "123, 456, 789, 0")
;; => (123 456 789 0)
|#

(define paren  ($. #\())
(define thesis ($. #\)))

#|
(peg-parse-string ($or ($seq paren ($."ab") thesis)
                       ($seq paren ($."cd") thesis))
                  "(cd)")
;; => *** PARSE-ERROR: expecting ab at 1, but got #\c

(peg-parse-string ($seq paren
                        ($or ($."ab") ($."cd"))
                        thesis)
                  "(cd)")
;; => #\)

(peg-parse-string ($or ($try ($seq paren ($."ab") thesis))
                       ($seq paren ($."cd") thesis))
                  "(cd)")
;; => #\)
|#

(define begin-list 
  ($seq0 ($. #[\(\[\{]) ws))

(define (end-list opener) 
  ($seq ws (case opener
            [(#\() ($. #\))]
            [(#\[) ($. #\])]
            [(#\{) ($. #\})])))

(define int-list
  ($let* ([opener begin-list]                ;*1
          [ints ($sep-by integer separator)] ;*2
          [ (end-list opener) ])             ;*3
    ($return ints)))

#|
(peg-parse-string int-list "[123, 456, 789]")
;; => (123 456 789)
(peg-parse-string int-list "{123, 456, 789}")
;; => (123 456 789)
(peg-parse-string int-list "(123, 456, 789}")
;; => *** PARSE-ERROR: expecting #\) at 14, but got #\@}
|#

(define nested-list
  ($lazy
    ($let* ([opener begin-list]
            [ints ($sep-by elem separator)]
            [ (end-list opener) ])
      ($return ints))))
(define elem  ($or integer nested-list))

#|
(peg-parse-string nested-list "(123, [456, {}, 789], 987)")
;; => (123 (456 () 789) 987)
(peg-parse-string nested-list "(123, [456, {}, 789), 987)")
;; => *** PARSE-ERROR: expecting one of (#[0-9] #\]) at 19
|#

(define (end-list2 opener) 
  (define expected
    (assv-ref '((#\( . #\)) (#\[ . #\]) (#\{ . #\})) opener))
  ($seq ws 
        ($let ([closer ($. #[\)\]\}])])
          (if (eqv? closer expected)
            ($return closer)
            ($fail (format "Mismatched closing bracket. '~c' expected, but got '~c'"
                           expected closer))))))
(define nested-list2
  ($lazy
    ($let* ([opener begin-list]
            [ints ($sep-by elem2 separator)]
            [ (end-list2 opener) ])
      ($return ints))))
(define elem2  ($or integer nested-list2))

#|
(peg-parse-string nested-list2 "(123, [456, {}, 789), 987)")
;; =>  *** PARSE-ERROR: Mismatched closing bracket. ']' expected, but got ')' at 20
|#
