;; testing util.* that depend on extension modules

(use gauche.test)
(test-start "util.* modules extra")

;;;========================================================================
(test-section "util.levenshtein")
(use util.levenshtein)
(test-module 'util.levenshtein)

(let1 datasets
    '((""
       (""                          0   0   0)
       ("a"                         1   1   1)   ;1i
       ("abc"                       3   3   3))  ;3i
      ("abc"
       (""                          3   3   3)
       ("ab"                        1   1   1)   ;1d
       ("ac"                        1   1   1)   ;1d
       ("bc"                        1   1   1)   ;1d
       ("acb"                       2   1   1)   ;1t
       ("ca"                        3   3   2)   ;1d, 1t
       ("bdac"                      3   3   2))
      ("ca"
       ("abc"                       3   3   2))
      ("rcik"
       ("rick"                      2   1   1)
       ("irkc"                      4   4   3))
      ("string-filter"
       ("stirng-filter"             2   1   1)
       ("stirng-filtre"             4   2   2)
       ("string-filtrze"            3   3   2))
      ("a cat"
       ("a cat"                     0   0   0)
       ("an act"                    3   2   2)
       ("a abct"                    3   3   2))
      )
  (define (test-algo name distances result-selector)
    (dolist [set datasets]
      (test* #"~|name| distance with \"~(car set)\""
             (map result-selector (cdr set))
             (distances (car set) (map car (cdr set)))))
    (dolist [c '(0 1 2 3)]
      (dolist [set datasets]
        (test* #"~|name| distance with \"~(car set)\", cutoff ~c"
               (map (^x (let1 r (result-selector x)
                          (and (<= r c) r)))
                    (cdr set))
               (distances (car set) (map car (cdr set)) :cutoff c)))))

  (test-algo "Levenshtein" l-distances cadr)
  (test-algo "Restricted edit" re-distances caddr)
  (test-algo "Damerau-Levenshtein" dl-distances cadddr))


(test-end)
