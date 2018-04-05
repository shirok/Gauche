(use gauche.interactive)

(define (main argc)
  (print "Welcome to my standalone REPL application!")
  (read-eval-print-loop #f #f #f (^[] (display "sample-repl> ") (flush)))
  0)
