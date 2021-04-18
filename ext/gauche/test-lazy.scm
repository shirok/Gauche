;; Test gauche.lazy
;;  gauche.lazy isn't precompiled (yet), but it depends on gauche.generator
;;  so we test it here.

(use gauche.test)
(use gauche.generator)
(use gauche.sequence)
(use scheme.list)
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

(test* "lazyness - eager evaluation"
       '(#\a #\b #\c #\d #\e)
       (with-input-from-string "abcde"
         (^[] (begin0 (lseq->list (generator->lseq read-char))
                (close-input-port (current-input-port))))))

(test* "lazyness - coercion" '(1 2 3 4 5)
       (lmap identity '#(1 2 3 4 5)))
(test* "lazyness - coercion" '(#\a #\b #\c #\d #\e)
       (lmap identity "abcde"))

(test* "lazyness - literate" '(0 2 4 6 8)
       (take (literate (cut + <> 2) 0) 5))

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

(test* "lazyness - coroutine" '(0 1 4 9 16 25 36 49 64 81)
       (coroutine->lseq (^[yield] (dotimes [i 10] (yield (square i))))))

;;; lseq with input-positions

(let ()
  (define (test-positions name expected input . args)
    (let1 z (apply port->char-lseq/position (open-input-string input) args)
      (test* name expected
             (map (^[c i]
                    (let1 p (lseq-position (drop* z i))
                      (and p
                           (format "~a:~a:~a(~a) ~a"
                                   (sequence-position-source p)
                                   (sequence-position-line p)
                                   (sequence-position-column p)
                                   (sequence-position-item-count p)
                                   c))))
                  z
                  (liota)))))

  (test-positions "simple"
                  '("source.txt:1:1(0) a"
                    "source.txt:1:2(1) b"
                    "source.txt:1:3(2) c"
                    "source.txt:1:4(3) \n"
                    "source.txt:2:1(4) d"
                    "source.txt:2:2(5) e"
                    "source.txt:2:3(6) f"
                    "source.txt:2:4(7) \n"
                    "source.txt:3:1(8) g"
                    "source.txt:3:2(9) h"
                    "source.txt:3:3(10) i")
                  "abc\n\
                   def\n\
                   ghi"
                  :source-name "source.txt")

  (test-positions "cpp-line-adjuster"
                  '("foo.txt:1:1(0) a"
                    "foo.txt:1:2(1) \n"
                    "foo.txt:2:1(2) b"
                    "foo.txt:2:2(3) \n"
                    "foo.txt:10:1(4) c"
                    "foo.txt:10:2(5) \n"
                    "bar.txt:2:1(6) d"
                    "bar.txt:2:2(7) \n"
                    "bar.txt:5:1(8) #"
                    "bar.txt:5:2(9) i"
                    "bar.txt:5:3(10) f"
                    "bar.txt:5:4(11) \n"
                    "bar.txt:6:1(12) e"
                    "bar.txt:6:2(13) \n"
                    "baz.txt:1:1(14) f"
                    "baz.txt:1:2(15) \n"
                    "foo.txt:14:1(16) g")
                  "#line 1 \"foo.txt\"\n\
                   a\n\
                   b\n\
                   #line 10\n\
                   c\n\
                   #  line   2   bar.txt  \n\
                   d\n\
                   #line 5\n\
                   #if\n\
                   e\n\
                   #line 12 \"foo.txt\"\n\
                   #line 1 baz.txt\n\
                   f\n\
                   #line 14 \"foo.txt\"\n\
                   g"
                  :line-adjusters `((#\# . ,cpp-line-adjuster)))

  (test-positions "cc1-line-adjuster"
                  '("foo.txt:1:1(0) a"
                    "foo.txt:1:2(1) \n"
                    "foo.txt:2:1(2) b"
                    "foo.txt:2:2(3) \n"
                    "foo.txt:10:1(4) c"
                    "foo.txt:10:2(5) \n"
                    "bar.txt:2:1(6) d"
                    "bar.txt:2:2(7) \n"
                    "bar.txt:5:1(8) #"
                    "bar.txt:5:2(9) l"
                    "bar.txt:5:3(10) i"
                    "bar.txt:5:4(11) n"
                    "bar.txt:5:5(12) e"
                    "bar.txt:5:6(13)  "
                    "bar.txt:5:7(14) 3"
                    "bar.txt:5:8(15) \n"
                    "bar.txt:6:1(16) e"
                    "bar.txt:6:2(17) \n"
                    "baz.txt:1:1(18) f"
                    "baz.txt:1:2(19) \n"
                    "foo.txt:14:1(20) g")
                  "# 1 \"foo.txt\" 1 2\n\
                   a\n\
                   b\n\
                   # 10\n\
                   c\n\
                   # 2 bar.txt 2 \n\
                   d\n\
                   # 5\n\
                   #line 3\n\
                   e\n\
                   # 12 \"foo.txt\"\n\
                   # 1 baz.txt\n\
                   f\n\
                   # 14 \"foo.txt\"\n\
                   g"
                  :line-adjusters `((#\# . ,cc1-line-adjuster)))
  )

(test-end)
