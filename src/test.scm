(define (x)
  (let ((path '())
        (c #f))
    (letrec ((add (lambda (s)
                 (set! path (cons s path)))))
      (dynamic-wind
       (lambda () (vm-dump) (add 'connect))
       (lambda ()
         (add (call/cc
               (lambda (c0)
                 (set! c c0)
                 'talk1)))
         )
       (lambda () (add 'disconnect)))
      (if (< (length path) 4)
          (c 'talk2)
          (reverse path)))))
     