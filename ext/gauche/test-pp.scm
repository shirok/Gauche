(use gauche.test)
(use gauche.parameter)
(test-start "pretty printer")

(use gauche.pp)
(test-module 'gauche.pp)

(let ([data1 '(Lorem ipsum dolor sit amet consectetur adipisicing elit
               sed do eiusmod tempor incididunt ut labore et dolore)]
      [data2 '(Lorem (ipsum #(dolor (sit (amet . consectetur)))))]
      )
  (define (t name expect data . args)
    (test* #"~|name| ~|args|" expect
           (with-output-to-string (^[] (apply pprint data args)))))
  (let-syntax
      ([t* (syntax-rules ()
             [(_ (data . args) expect)
              (t 'data expect data . args)])])

    (t* (data1)
        "(Lorem ipsum dolor sit amet consectetur adipisicing elit sed do eiusmod tempor\
       \n incididunt ut labore et dolore)")
    (t* (data1 :print-width #f)
        "(Lorem ipsum dolor sit amet consectetur adipisicing elit sed do eiusmod tempor incididunt ut labore et dolore)")
    (t* (data1 :print-width 40)
        "(Lorem ipsum dolor sit amet consectetur\
       \n adipisicing elit sed do eiusmod tempor\
       \n incididunt ut labore et dolore)")
    (t* (data1 :print-width 39)
        "(Lorem ipsum dolor sit amet consectetur\
       \n adipisicing elit sed do eiusmod tempor\
       \n incididunt ut labore et dolore)")
    (t* (data1 :print-width 38)
        "(Lorem ipsum dolor sit amet\
       \n consectetur adipisicing elit sed do\
       \n eiusmod tempor incididunt ut labore\
       \n et dolore)")
    (t* (data1 :print-length 5)
        "(Lorem ipsum dolor sit amet ....)")
    (t* (data1 :print-length 1)
        "(Lorem ....)")
    (t* (data1 :print-length 0)
        "(....)")
    (t* (data1 :print-level 1 :print-length 5)
        "(Lorem ipsum dolor sit amet ....)")
    (t* (data1 :print-level 0 :print-length 5)
        "#")
    (t* ('a :print-level 0)
        "a")

    (t* (data2 :print-level 0)
        "#")
    (t* (data2 :print-level 1)
        "(Lorem #)")
    (t* (data2 :print-level 2)
        "(Lorem (ipsum #))")
    (t* (data2 :print-level 3)
        "(Lorem (ipsum #(dolor #)))")
    (t* (data2 :print-level 4)
        "(Lorem (ipsum #(dolor (sit #))))")
    (t* (data2 :print-level 5)
        "(Lorem (ipsum #(dolor (sit (amet . consectetur)))))")
    (t* (data2 :print-level 4 :print-width 30)
        "(Lorem\
       \n (ipsum #(dolor (sit #))))")
    ))

(test-end)
