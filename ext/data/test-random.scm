;;
;; Testing data.random here, for it depends on math.mt-random.
;;

(use gauche.test)
(test-start "data.random")
(test-section "data.random")
(use data.random)
(test-module 'data.random)

(use gauche.generator)

(define (test-regular-strings regexp)
  (let1 g (regular-strings$ regexp)
    (test* #"regular-string$ ~regexp"
           regexp
           (generator->list g 10)
           (^[rx ss] (every (^s (equal? (rxmatch->string rx s) s)) ss)))))

(test-regular-strings #/a/)
(test-regular-strings #/[a-z]/)
(test-regular-strings #/[^a-z]/)
(test-regular-strings #/abc|def/)
(test-regular-strings #/ab*/)
(test-regular-strings #/(ab|cd)+/)
(test-regular-strings #/(abc){3}/)
(test-regular-strings #/(abc){3,5}/)
(test-regular-strings #/(abc){,3}/)
(test-regular-strings #/(?i:abc)/)
(test-regular-strings #/abc/i)

(test-end)
