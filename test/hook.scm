;;
;; Tests gauche.hook
;;

(use gauche.test)

(test-start "hook")

(use gauche.hook)
(test-module 'gauche.hook)
;;----------------------------------------------------------------

(test* "<hook> creation and run-hook" #t
       (let ((x (make-hook)))
         (run-hook x)
         (hook-empty? x)))

(test* "add-hook!" "1"
       (let ((x (make-hook)))
         (add-hook! x (lambda () (display 1)))
         (with-output-to-string (lambda () (run-hook x)))))

(test* "add-hook!" "123"
       (let ((x (make-hook)))
         (add-hook! x (lambda () (display 1)))
         (add-hook! x (lambda () (display 2)) #t)
         (add-hook! x (lambda () (display 3)) #t)
         (with-output-to-string (lambda () (run-hook x)))))

(test* "add-hook!" "321"
       (let ((x (make-hook)))
         (add-hook! x (lambda () (display 1)))
         (add-hook! x (lambda () (display 2)))
         (add-hook! x (lambda () (display 3)))
         (with-output-to-string (lambda () (run-hook x)))))

(test* "add-hook!" "aaa"
       (let ((x (make-hook 1)))
         (add-hook! x (lambda (y) (display y)))
         (add-hook! x (lambda (y) (display y)))
         (add-hook! x (lambda (y) (display y)))
         (with-output-to-string (lambda () (run-hook x 'a)))))

(test* "arity check" *test-error*
       (let ((x (make-hook 1)))
         (run-hook x)))

(test* "arity check" *test-error*
       (let ((x (make-hook 1)))
         (run-hook x 1 2)))

(test* "arity check" *test-error*
       (let ((x (make-hook 1)))
         (add-hook! x (lambda (a b) #f))))

(test* "arity check" *test-error*
       (let ((x (make-hook 2)))
         (add-hook! x (lambda (a) #f))))

(test* "arity check" 'ok
       (let ((x (make-hook 2)))
         (with-error-handler
             (lambda (e) 'error)
           (lambda () (add-hook! x (lambda a #f)) 'ok))))

(test* "arity check" 'ok
       (let ((x (make-hook 2)))
         (with-error-handler
             (lambda (e) 'error)
           (lambda () (add-hook! x (lambda (a b . c) #f)) 'ok))))

(test* "remove hook" "31"
       (let ((x (make-hook))
             (f (lambda () (display 2))))
         (add-hook! x (lambda () (display 1)))
         (add-hook! x f)
         (add-hook! x (lambda () (display 3)))
         (remove-hook! x f)
         (with-output-to-string (lambda () (run-hook x)))))

(test* "reset hook" ""
       (let ((x (make-hook)))
         (add-hook! x (lambda () (display 3)))
         (add-hook! x (lambda () (display 2)))
         (add-hook! x (lambda () (display 1)))
         (reset-hook! x)
         (with-output-to-string (lambda () (run-hook x)))))

(test* "hook->list" "321"
       (let ((x (make-hook)))
         (add-hook! x (lambda () (display 1)))
         (add-hook! x (lambda () (display 2)))
         (add-hook! x (lambda () (display 3)))
         (with-output-to-string
           (lambda ()
             (for-each (cut apply <> '()) (hook->list x))))))

(test* "object-apply" "321"
       (let ((x (make-hook)))
         (add-hook! x (lambda () (display 1)))
         (add-hook! x (lambda () (display 2)))
         (add-hook! x (lambda () (display 3)))
         (with-output-to-string x)))

(test-end)
