#!/usr/bin/env gosh

(define (usage)
  (format (current-error-port)
          "Usage: ~a regexp file ...\n" *program-name*)
  (exit 2))

(define (grep rx port)
  (with-input-from-port port
    (lambda ()
      (port-for-each
       (lambda (line)
         (when (rxmatch rx line)
           (format #t "~a:~a: ~a\n"
                   (port-name port)
                   (- (port-current-line port) 1)
                   line)))
       read-line))))

(define (main args)
  (if (null? (cdr args))
      (usage)
      (let ((rx (string->regexp (cadr args))))
        (if (null? (cddr args))
            (grep rx (current-input-port))
            (for-each (lambda (f)
                        (call-with-input-file f
                          (lambda (p) (grep rx p))))
                      (cdr args)))))
  0)

