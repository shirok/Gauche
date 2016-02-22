;; Example of gauche.termios

#!no-fold-case

(use gauche.termios)

(define (get-password prompt)
  (let* ((port (current-input-port))
         (attr (sys-tcgetattr port))
         (lflag (slot-ref attr 'lflag)))
    ;; Show prompt
    (display prompt)
    (flush)
    ;; Turn off echo during reading.
    (dynamic-wind
     (lambda ()
       (slot-set! attr 'lflag (logand lflag (lognot ECHO)))
       (sys-tcsetattr port TCSAFLUSH attr))
     (lambda ()
       (read-line port))
     (lambda ()
       (slot-set! attr 'lflag lflag)
       (sys-tcsetattr port TCSANOW attr)))))
