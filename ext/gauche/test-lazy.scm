;; Test gauche.lazy
;;  gauche.lazy isn't precompiled (yet), but it depends on gauche.generator
;;  so we test it here.

(use gauche.test)
(use gauche.generator)
(use srfi-1)
(test-start "lazy sequence utilities")

(use gauche.lazy)
(test-module 'gauche.lazy)

(let ()
  (define (test-eager-lazy name l e . args)
    (test* #"eager vs lazy (~name)" (apply e args) (apply l args)))
  (define (map-accumx f s xs) (values-ref (map-accum f s xs) 0))
  (define (state-filter f s xs)
    (if (null? xs)
      '()
      (receive (r s.) (f (car xs) s)
        (if r
          (cons (car xs) (state-filter f s. (cdr xs)))
          (state-filter f s. (cdr xs))))))

  (test-eager-lazy "lmap" lmap map (^x (* x x)) '(1 2 3 4 5))
  (test-eager-lazy "lmap" lmap map (^(x y) (+ x y)) '(1 2 3 4 5) '(2 3 4 5))
  (test-eager-lazy "lmap-accum" lmap-accum map-accum
                   (^(x y) (values (+ x y) (+ y 1))) 1 '(1 2 3 4 5))
  (test-eager-lazy "lappend" lappend append '() '(1) '(1 2 3))
  (test-eager-lazy "lappend" lappend append)
  (test-eager-lazy "lappend" lappend append '())
  (test-eager-lazy "lfilter" lfilter filter odd? '())
  (test-eager-lazy "lfilter" lfilter filter odd? '(1 2 3 4 5))
  (test-eager-lazy "lfilter-map" lfilter-map filter-map
                   (^[x y] (and (not (zero? y)) (quotient x y)))
                   '(10 20 30 40 50) '(3 0 2 0 7))
  (test-eager-lazy "lstate-filter" lstate-filter state-filter
                   (^[x y] (values (> x y) x)) 0
                   '(1 2 3 2 1 0 1 2 3 2 1 0 1 2 3))
  (test-eager-lazy "ltake" ltake take* '(1 2 3 4 5) 2)
  (test-eager-lazy "ltake" ltake take* '(1 2 3 4 5) 10)
  (test-eager-lazy "ltake" ltake take* '(1 2 3 4 5) 10 #t 'z)
  (test-eager-lazy "ltake-while" ltake-while take-while odd? '(1 3 5 4 2))
  (test-eager-lazy "ltake-while" ltake-while take-while even? '(1 3 5 4 2))
  (test-eager-lazy "lslices" lslices slices '(1 2 3 4 5 6 7) 2)
  (test-eager-lazy "lslices" lslices slices '(1 2 3 4 5 6 7) 2 #t 'z)
  )

(test* "lazyness - coercion" '(1 2 3 4 5)
       (lmap identity '#(1 2 3 4 5)))
(test* "lazyness - coercion" '(#\a #\b #\c #\d #\e)
       (lmap identity "abcde"))

(test* "lazyness - lmap" 0
       ;; this yields an error if lmap works eagerly.
       (list-ref (lmap (^x (quotient 1 x)) '(1 2 3 4 0)) 2))

(test* "lazyness - lappend-map" 0
       ;; this yields an error if lappend-map works too eagerly.
       (car (lappend-map (^x (if (= x 1) (error "boo!") (list x x))) '(0 1))))

(test* "lazyness - lconcatenate" '(0 1 2 3 0 1 2 3 0 1)
       (letrec ([c (lcons* '(0 1 2 3) '(0 1 2 3) c)])
         (take (lconcatenate c) 10)))

(test* "lazyness - linterweave" '(#0=(1 a 2 b 3 c) #0#)
       (list (linterweave '(1 2 3 4) '(a b c))
             (linterweave '(1 2 3) '(a b c d))))

(test* "lazyness - linterweave" '(1 a A 2 b A 3 c A)
       (linterweave (lrange 1)  '(a b c) '#0=(A . #0#)))

(test-end)

