;;
;; Tests gauche.hook
;;

(use gauche.test)

(test-start "hook")

(use gauche.hook)
(test-module 'gauche.hook)
;;----------------------------------------------------------------

(test* "<hook> creation and run-hook" #t
       (let1 x (make-hook)
         (run-hook x)
         (hook-empty? x)))

(test* "add-hook!" "1"
       (let1 x (make-hook)
         (add-hook! x (^[] (display 1)))
         (with-output-to-string (^[] (run-hook x)))))

(test* "add-hook!" "123"
       (let1 x (make-hook)
         (add-hook! x (^[] (display 1)))
         (add-hook! x (^[] (display 2)) #t)
         (add-hook! x (^[] (display 3)) #t)
         (with-output-to-string (^[] (run-hook x)))))

(test* "add-hook!" "321"
       (let1 x (make-hook)
         (add-hook! x (^[] (display 1)))
         (add-hook! x (^[] (display 2)))
         (add-hook! x (^[] (display 3)))
         (with-output-to-string (^[] (run-hook x)))))

(test* "add-hook!" "aaa"
       (let1 x (make-hook 1)
         (add-hook! x (^y (display y)))
         (add-hook! x (^y (display y)))
         (add-hook! x (^y (display y)))
         (with-output-to-string (^[] (run-hook x 'a)))))

(test* "arity check" *test-error*
       (let1 x (make-hook 1)
         (run-hook x)))

(test* "arity check" *test-error*
       (let1 x (make-hook 1)
         (run-hook x 1 2)))

(test* "arity check" *test-error*
       (let1 x (make-hook 1)
         (add-hook! x (^[a b] #f))))

(test* "arity check" *test-error*
       (let1 x (make-hook 2)
         (add-hook! x (^[a] #f))))

(test* "arity check" 'ok
       (let1 x (make-hook 2)
         (with-error-handler (^e 'error)
           (^[] (add-hook! x (^ a #f)) 'ok))))

(test* "arity check" 'ok
       (let1 x (make-hook 2)
         (with-error-handler (^e 'error)
           (^[] (add-hook! x (^[a b . c] #f)) 'ok))))

(test* "remove hook" "31"
       (let ([x (make-hook)]
             [f (^[] (display 2))])
         (add-hook! x (^[] (display 1)))
         (add-hook! x f)
         (add-hook! x (^[] (display 3)))
         (remove-hook! x f)
         (with-output-to-string (^[] (run-hook x)))))

(test* "reset hook" ""
       (let1 x (make-hook)
         (add-hook! x (^[] (display 3)))
         (add-hook! x (^[] (display 2)))
         (add-hook! x (^[] (display 1)))
         (reset-hook! x)
         (with-output-to-string (^[] (run-hook x)))))

(test* "hook->list" "321"
       (let1 x (make-hook)
         (add-hook! x (^[] (display 1)))
         (add-hook! x (^[] (display 2)))
         (add-hook! x (^[] (display 3)))
         (with-output-to-string
           (^[] (for-each (cut apply <> '()) (hook->list x))))))

(test* "object-apply" "321"
       (let1 x (make-hook)
         (add-hook! x (^[] (display 1)))
         (add-hook! x (^[] (display 2)))
         (add-hook! x (^[] (display 3)))
         (with-output-to-string x)))

(test-end)
