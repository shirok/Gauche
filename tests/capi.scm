;; Testing C API features that aren't accesible from Scheme by default.
;; This script nees to be run with the special "test-extra" executable
;; in ../src directory.

(use gauche.test)

(test-start "some C API features")

(test-section "foreign pointer")

(let* ([obj (cons 'a 'a)] [obj2 (cons 'b 'b)])

  ;; See KEEP_IDENTITY
  (test* "identity preservation" '(#f #t #f)
         (list (eq? (make-fptr-simple obj) (make-fptr-simple obj))
               (eq? (make-fptr-unique obj) (make-fptr-unique obj))
               (eq? (make-fptr-unique obj) (make-fptr-unique obj2))))

  ;; See MAP_NULL flag maps (void*)0 to #f, not #<fptr 0>.
  (test* "map-null" '(#t #f)
         (list (boolean (make-fptr-maybe obj))
               (boolean (make-fptr-maybe #f))))

  ;; Foreign pointer attributes
  (test* "foreign pointer attrs 1" '((a . b))
         (foreign-pointer-attributes (make-fptr-simple #f '((a . b)))))

  (test* "foreign pointer attrs get" 'b
         (let1 x (make-fptr-simple #f '((a . b)))
           (foreign-pointer-attribute-get x 'a)))
  (test* "foreign pointer attrs get (error)" (test-error)
         (let1 x (make-fptr-simple #f '((a . b)))
           (foreign-pointer-attribute-get x 'b)))
  (test* "foreign pointer attrs get (fallback)" 'z
         (let1 x (make-fptr-simple #f '((a . b)))
           (foreign-pointer-attribute-get x 'b 'z)))
  (test* "foreign pointer attrs set" '(d b)
         (let1 x (make-fptr-simple #f '())
           (foreign-pointer-attribute-set! x 'a 'b)
           (foreign-pointer-attribute-set! x 'c 'd)
           (list (foreign-pointer-attribute-get x 'c)
                 (foreign-pointer-attribute-get x 'a))))
  (test* "foreign pointer attrs replacement" 'c
         (let1 x (make-fptr-simple #f (list (cons 'a 'b)))
           (foreign-pointer-attribute-set! x 'a 'c)
           (foreign-pointer-attribute-get x 'a)))


  )

(test-section "maybe-typed arguments")

;; #f selects the default value, for the maybe types that can't represent
;; #f in C.
(test* "maybe arg, omitted" '(1 -1 3.0) (maybe-arg-fixnum 1))
(test* "maybe arg, given" '(1 2 4.0) (maybe-arg-fixnum 1 2 4.0))
(test* "maybe arg, #f" '(1 -1 4.0) (maybe-arg-fixnum 1 #f 4.0))
(test* "maybe arg, #f #f" '(1 -1 3.0) (maybe-arg-fixnum 1 #f #f))
(test* "maybe arg, type error" (test-error <error> #/supposed to be of type .*or #f/)
       (maybe-arg-fixnum 1 'a))
;; Required arg isn't maybe-typed
(test* "maybe arg, required arg" (test-error) (maybe-arg-fixnum #f))

(test* "maybe key arg, omitted" '(#\a . 10) (maybe-arg-key))
(test* "maybe key arg, given" '(#\b . 20) (maybe-arg-key :c #\b :n 20))
(test* "maybe key arg, #f" '(#\a . 20) (maybe-arg-key :c #f :n 20))

;; Pointer maybe type isn't affected---#f is passed as NULL, regardless
;; of the default value.
(test* "maybe ptr arg, omitted" "abc" (maybe-arg-ptr))
(test* "maybe ptr arg, given" "def" (maybe-arg-ptr "def"))
(test* "maybe ptr arg, #f" #f (maybe-arg-ptr #f))

(test-section "path substitution")

(test* "substitute_all" "abcXYZdefXYZ@XYZghi"
       (substitute-all "abc@^def@^@@^ghi"
                       "@^"
                       "XYZ"))
(test* "substitute_all" "XYZ@@abcXYZ"
       (substitute-all "@@@@@abc@@@"
                       "@@@"
                       "XYZ"))

(test-end)
