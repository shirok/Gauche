(test-section "srfi-180")

(define-module test-srfi-180
  (use gauche.test)
  (use srfi-180)
  (use gauche.generator)

  (test-module 'srfi-180)

  (let ()
    (define (parse-with-generator str)
      (generator->list (call-with-input-string str
                         (cut json-generator <>))))

    (test* "generator basic" '(null) (parse-with-generator "  null "))
    (test* "generator basic" '(#t) (parse-with-generator "true"))
    (test* "generator basic" '(#f) (parse-with-generator " false foo foo"))
    (test* "generator basic" '(123) (parse-with-generator " 123;567"))
    (test* "generator basic" '("json") (parse-with-generator " \"json\"515"))

    (test* "generator array"
           '(array-start 1 2 3 "abc" null #t #f array-end)
           (parse-with-generator " [1,2 , 3  ,\"abc\",null , true, false] 1234"))
    (test* "generator array"
           '(array-start array-end)
           (parse-with-generator " [] "))
    (test* "generator object"
           '(object-start "a" 1 "b" 2 object-end)
           (parse-with-generator "{\"a\":1, \"b\" : 2} zzz"))
    (test* "generator object"
           '(object-start object-end)
           (parse-with-generator " {} "))

    (test* "generator nesting"
           '(array-start object-start "a" array-start 1 2 3 array-end 
                         "b" 456 object-end null
                         array-start 3 1 2 array-end array-end)
           (parse-with-generator "[ {\"a\": [1,2 ,3],\"b\":456}, null, [ 3, 1, 2]]"))
    )
  )
