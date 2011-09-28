;; Test gauche.generator
;;  gauche.generator isn't precompiled (yet), but it depends on gauche.sequence
;;  so we test it here.

(use gauche.test)
(use srfi-1)
(use srfi-60)
(test-start "generators")


(use gauche.generator)
(test-module 'gauche.generator)

(test-section "genreator constructors")

;; first, let's test the stuff used by the following tests.
(test* "giota + generator->list" '(0 1 2 3 4)
       (generator->list (giota 5)))
(test* "giota(start) + generator->list" '(10 11 12 13 14)
       (generator->list (giota 5 10)))
(test* "giota(start,step) + generator->list" '(10 12 14 16 18)
       (generator->list (giota 5 10 2)))
(test* "giota + generator->list(n)" '(0 1 2)
       (generator->list (giota 5) 3))
(test* "giota + generator->list(n)" '(0 1 2 3 4)
       (generator->list (giota 5) 10))

(test* "grange + generator->list" '(0 1 2 3 4)
       (generator->list (grange 0 5)))
(test* "grange + generator->list" '(2 3 4 5)
       (generator->list (grange 2 6)))
(test* "grange + generator->list" '(2 5 8 11)
       (generator->list (grange 2 14 3)))

;; converters
(let-syntax ((t (syntax-rules ()
                  [(t fn cv data)
                   (test* (format "~a" 'fn) (cv data)
                          (generator->list (fn data)))])))

  (t list->generator identity '(a b c d e))
  (t vector->generator vector->list '#(a b c d e))
  (t reverse-vector->generator (.$ reverse vector->list) '#(a b c d e))
  (t string->generator string->list "abcde")
  (t bits->generator integer->list 4395928592485)
  (t reverse-bits->generator (.$ reverse integer->list) 4395928592485)
  )

(test* "circular-generator" '(0 1 2 0 1 2 0 1 2 0)
       (generator->list (circular-generator 0 1 2) 10))

(test* "gappend" '(0 1 2 3 a b c d A B C D)
       (generator->list (gappend (giota 4)
                                 (list->generator '(a b c d))
                                 (list->generator '(A B C D)))))

(test* "gunfold" (unfold (^s (>= s 10))
                         (^s (* s 2))
                         (^s (+ s 1))
                         0
                         (^s (iota 10)))
       (generator->list (gunfold (^s (>= s 10))
                                 (^s (* s 2))
                                 (^s (+ s 1))
                                 0
                                 (giota 10))))

(define-syntax test-list-like
  (syntax-rules ()
    [(_  gfn lfn src ...)
     (dolist [s (list src ...)]
       (test* (format "~s" 'gfn) (lfn s)
              (generator->list (gfn (list->generator s)))))]))

(test-list-like (cut gmap (^x (* x 2)) <>)
                (cut map (^x (* x 2)) <>)
                '(1 2 3 4 5) '())

(test-list-like (cut gfilter odd? <>)
                (cut filter odd? <>)
                '(1 2 3 4 5) '())

(test-list-like (cut gtake <> 3)
                (cut take <> 3)
                '(1 2 3 4 5 6))

(test-list-like (cut gdrop <> 3)
                (cut drop <> 3)
                '(1 2 3 4 5 6))

(test-list-like (cut gtake-while even? <>)
                (cut take-while even? <>)
                '(2 4 0 1 3) '(1 2 4 4 8) '() '(2 2) '(3 5))

(test-list-like (cut gdrop-while even? <>)
                (cut drop-while even? <>)
                '(2 4 0 1 3) '(1 2 4 4 8) '() '(2 2) '(3 5))

(test-end)

