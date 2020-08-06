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

  (let ([input (open-input-string "123 \"abc\" [1] {} null")])
    (test* "consecutive call to the generators"
           '((123) ("abc") (array-start 1 array-end)
                 (object-start object-end) (null))
           (let* ([a (generator->list (json-generator input))]
                  [b (generator->list (json-generator input))]
                  [c (generator->list (json-generator input))]
                  [d (generator->list (json-generator input))]
                  [e (generator->list (json-generator input))])
             (list a b c d e))))

  (test* "json-read" (eof-object)
         (call-with-input-string "" json-read))

  (test* "json-read" 12345
         (call-with-input-string "12345 6789" json-read))

  (test* "json-fold" '#(1 2 3 ((a . 4) (b . 5)) null #t #f ((z . #())))
         (let ()
           ;; our json-read doesn't use json-fold, so we roll our own
           ;; to test json-fold.
           (define (%json-read port)
             (define (array-start _) '())
             (define (array-end items) (reverse-list->vector items))
             (define (object-start _) '())
             (define (object-end plis)
               (map (^[kv] (cons (string->symbol (car kv)) (cadr kv))) 
                    (slices (reverse plis) 2)))
             (define (proc obj seed)
               (if (undefined? seed)
                 obj
                 (cons obj seed)))
             (let1 out (json-fold proc 
                                  array-start array-end 
                                  object-start object-end
                                  (undefined) port)
               (if (undefined? out)
                 (eof-object)
                 out)))
           (call-with-input-string "[ \n\
                                    1, \n\
                                    2, \n\
                                    3, \n\
                                    { \"a\" : 4, \n\
                                      \"b\" : 5 }, \n\
                                    null, true, false, \n\
                                    {\"z\":[]} ]"
             %json-read)))

  (test* "json-read" '#(1 2 3 ((a . 4) (b . 5)) null #t #f ((z . #())))
         (call-with-input-string "[ \n\
                                    1, \n\
                                    2, \n\
                                    3, \n\
                                    { \"a\" : 4, \n\
                                      \"b\" : 5 }, \n\
                                    null, true, false, \n\
                                    {\"z\":[]} ]"
           json-read))

  (test* "json-lines-read"
         '(null 123 #(1 2 3) ((a . 1) (b . ((x . #(2 3))))))
         (generator->list
          (call-with-input-string "null\n\
                                  123\n\
                                  [1, 2, 3]\n\
                                  {\"a\":1, \"b\": {\"x\": [2, 3]}}"
            json-lines-read)))

  (test* "json-sequence-read"
         '(123 #(1 2 3) #t ((a . 1) (b . ((x . #(2 3))))))
         (generator->list
          (call-with-input-string "null\n\x1e;\
                                  123 \x1e;\x1e;\
                                  [1, 2, 3]\n\
                                  true \x1e;\
                                  [true, \x1e;\
                                  {\"a\":1, \"b\": {\"x\": [2, 3]}}"
            json-sequence-read)))
  )
