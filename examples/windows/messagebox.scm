;; Public domain

(use os.windows)

(define (main args)
  (guard (e [else (sys-message-box #f (format "ERROR: ~a" (~ e'message))
                                   (sys-basename *program-name*)
                                   (logior MB_OK MB_ICONERROR))])
    (messagebox-sample (sys-basename (car args))))
  0)

(define (messagebox-sample program-name)
  (sys-message-box #f "Let's start!" program-name
                   (logior MB_OK MB_ICONINFORMATION))
  (let loop ((count 0))
    (let1 r (sys-message-box
             #f (format "You've counted to ~a.\nCount more?" count)
             program-name (logior MB_YESNO MB_ICONQUESTION))
      (if (= r IDYES)
        (loop (+ count 1))
        (sys-message-box #f #"You counted up to ~|count|."
                         program-name MB_OK))))
  0)
