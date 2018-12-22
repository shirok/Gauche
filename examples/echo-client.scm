;; echo client sample
(use gauche.net)

(define (echo-client host port)
  (define s (make-client-socket host port))
  (let loop ()
    (display "> ")
    (flush)
    (let1 d (read-line)
      (cond
       ((member d '("q" "quit" "e" "exit" "end"))
        (socket-shutdown s))
       (else
        (socket-send s d)
        (print (socket-recv s 1000))
        (loop))))))

(define (main args)
  (define host "localhost")
  (define port 3131)
  (format #t "echo client connecting to ~a:~d~%" host port)
  (echo-client host port)
  0)
