;; echo client sample
(use gauche.net)

(define (echo-client host port)
  (define s (make-client-socket host port))
  (print "Type 'quit' or 'exit' to quit.")
  (let loop ()
    (display "> ")
    (flush)
    (let1 d (read-line)
      (cond
       [(or (eof-object? d)
            (member d '("quit" "exit")))
        (socket-shutdown s)
        (socket-close s)]
       [else
        (socket-send s d)
        (print (socket-recv s (string-size d)))
        (loop)]))))

(define (main args)
  (define host "localhost")
  (define port 3131)
  (format #t "echo client connecting to ~a:~d~%" host port)
  (echo-client host port)
  0)
