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

(test-end)




