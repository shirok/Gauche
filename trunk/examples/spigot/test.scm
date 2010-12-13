;;
;; Test spigot module
;;

(use gauche.test)

(test-start "spigot")
(use spigot)
(test-module 'spigot)  ;; This checks the exported symbols are indeed bound.

;; Normal operation test
(test* "spigot-calculate-pi (10)"
       '#(3 1 4 1 5 9 2 6 5 3)
       (spigot-calculate-pi 10))

(test* "spigot-calculate-e (15)"
       '#(2 7 1 8 2 8 1 8 2 8 4 5 9 0 4)
       (spigot-calculate-e 15))

(test* "spigot-calculate-pi (1)"
       '#(3)
       (spigot-calculate-pi 1))

(test* "spigot-calculate-e (1)"
       '#(2)
       (spigot-calculate-e 1))

;; See if they reports error for invalid arguments.
(test* "spigot-calculate-pi (0)"
       *test-error*
       (spigot-calculate-pi 0))

(test* "spigot-calculate-e (0)"
       *test-error*
       (spigot-calculate-e 0))

(test* "spigot-calculate-pi (-1)"
       *test-error*
       (spigot-calculate-pi -1))

(test* "spigot-calculate-e (-1)"
       *test-error*
       (spigot-calculate-e -1))

(test-end)
