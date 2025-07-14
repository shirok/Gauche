;;from reference implementation
;; https://github.com/scheme-requests-for-implementation/srfi-197/blob/master/test.scm
;;(include "./srfi-64-minimal.scm")

(define (exclamation x) (string-append x "!"))

(define (foo+bar x) (values (string-append x "foo") (string-append x "bar")))

(test-begin "Pipeline Operators")

(test-equal "chain" "bazbarfoo!"
  (chain ""
         (string-append "foo" _)
         (string-append "bar" _)
         (string-append "baz" _)
         (exclamation _)))

(test-equal "chain with mixed _ position" "barfoobaz"
  (chain ""
         (string-append _ "foo")
         (string-append "bar" _)
         (string-append _ "baz")))

(test-equal "chain with _ in operator position" 3
  (chain +
         (_ 1 2)))

(test-equal "chain without _" "barbazqux"
  (chain ""
         (string-append _ "foo")
         (string-append "bar" "baz")
         (string-append _ "qux")))

(test-equal "chain multiple _" "quxfoo/quxbar"
  (chain "qux"
         (foo+bar _)
         (string-append _ "/" _)))

(test-equal "chain _ ..." "bazquxfooquxbar"
  (chain "qux"
         (foo+bar _)
         (string-append "baz" _ ...)))

(test-equal "chain _ _ ..." "quxfoobazquxbar"
  (chain "qux"
         (foo+bar _)
         (string-append _ "baz" _ ...)))

(test-equal "chain with custom _" "bazbarfoo!"
  (chain "" <>
         (string-append "foo" <>)
         (string-append "bar" <>)
         (string-append "baz" <>)
         (exclamation <>)))

(test-equal "chain with custom ..." "bazquxfooquxbar"
  (chain "qux" - ---
         (foo+bar -)
         (string-append "baz" - ---)))

(test-equal "chain-and" "bazbarfoo!"
  (chain-and ""
             (string-append "foo" _)
             (string-append "bar" _)
             (string-append "baz" _)
             (exclamation _)))

(test-equal "chain-and with mixed _ position" "barfoobaz"
  (chain-and ""
             (string-append _ "foo")
             (string-append "bar" _)
             (string-append _ "baz")))

(test-equal "chain-and without _" "barbazqux"
  (chain-and ""
             (string-append "foo" _)
             (string-append "bar" "baz")
             (string-append _ "qux")))

(test-equal "chain-and short-circuit" #f
  (chain-and ""
             (string-append "foo" _)
             (equal? _ "bar")
             (string-append "baz" _)
             (exclamation _)))

(test-equal "chain-and short-circuit first" #f
  (chain-and #f
             (not _)))

(test-equal "chain-and with custom _" "bazbarfoo!"
  (chain-and "" <>
             (string-append "foo" <>)
             (string-append "bar" <>)
             (string-append "baz" <>)
             (exclamation <>)))

(test-equal "chain-when" "bazfoo"
  (chain-when ""
              ((= (+ 2 2) 4) (string-append "foo" _))
              ((= (+ 2 2) 5) (string-append "bar" _))
              (#t (string-append "baz" _))))

(test-equal "chain-when with mixed _ position" "barfooqux"
  (chain-when ""
              (#t (string-append _ "foo"))
              (#t (string-append "bar" _))
              (#f (string-append _ "baz"))
              (#t (string-append _ "qux"))))

(test-equal "chain-when without _" "barqux"
  (chain-when ""
              (#t (string-append _ "foo"))
              (#t (string-append "bar"))
              (#f (string-append _ "baz"))
              (#t (string-append _ "qux"))))

(test-equal "chain-when with custom _" "bazfoo"
  (chain-when "" <>
              ((= (+ 2 2) 4) (string-append "foo" <>))
              ((= (+ 2 2) 5) (string-append "bar" <>))
              (#t (string-append "baz" <>))))

(test-equal "chain-lambda" "bazbarfoo!"
  ((chain-lambda (string-append "foo" _)
                 (string-append "bar" _)
                 (string-append "baz" _)
                 (exclamation _))
   ""))

(test-equal "chain-lambda one step" "foobar"
  ((chain-lambda (string-append "foo" _)) "bar"))

(test-equal "chain-lambda with mixed _ position" "barfoobaz"
  ((chain-lambda (string-append _ "foo")
                 (string-append "bar" _)
                 (string-append _ "baz"))
   ""))

(test-equal "chain-lambda multiple _" "foobarbazqux"
  ((chain-lambda (string-append _ "bar" _)
                 (string-append _ "qux"))
   "foo"
   "baz"))

(test-equal "chain-lambda without _" "barqux"
  ((chain-lambda (string-append "bar")
                 (string-append _ "qux"))))

(test-equal "chain-lambda _ ..." "foobarbazqux"
  ((chain-lambda (string-append "foo" _ ...)
                 (string-append _ "qux"))
   "bar"
   "baz"))

(test-equal "chain-lambda _ _ ..." "foobarbazquxquux"
  ((chain-lambda (string-append _ "bar" _ ...)
                 (string-append _ "quux"))
   "foo"
   "baz"
   "qux"))

(test-equal "chain-lambda with custom _" "bazbarfoo!"
  ((chain-lambda <>
                 (string-append "foo" <>)
                 (string-append "bar" <>)
                 (string-append "baz" <>)
                 (exclamation <>))
   ""))

(test-equal "chain-lambda with custom ..." "foobarbazqux"
  ((chain-lambda - ---
                 (string-append "foo" - ---)
                 (string-append - "qux"))
   "bar"
   "baz"))

(test-equal "nest" '(1 2 (3 (4) 5))
  (nest (quote _)
        (1 2 _)
        (3 _ 5)
        (_)
        4))

(test-equal "nest with custom _" '(1 2 (3 (4) 5))
  (nest <>
        (quote <>)
        (1 2 <>)
        (3 <> 5)
        (<>)
        4))

(test-equal "nested nest" '(1 2 3 (4 5 6))
  (nest (nest _2 (quote _2) (1 2 3 _2) _ 6)
        (_ 5 _2)
        4))

(test-equal "nest-reverse" '(1 2 (3 (4) 5))
  (nest-reverse 4
                (_)
                (3 _ 5)
                (1 2 _)
                (quote _)))

(test-equal "nest-reverse with custom _" '(1 2 (3 (4) 5))
  (nest-reverse 4 <>
                (<>)
                (3 <> 5)
                (1 2 <>)
                (quote <>)))

(test-end "Pipeline Operators")
