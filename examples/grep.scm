#!/usr/bin/env gosh

(define (usage program-name)
  (format (current-error-port)
          "Usage: ~a regexp file ...\n" program-name)
  (exit 2))

(define (grep rx)
  (generator-for-each
   (lambda (line)
     (when (rx line)
       (format #t "~a:~a: ~a\n"
               (port-name (current-input-port))
               (- (port-current-line (current-input-port)) 1)
               line)))
   read-line))

(define (main args)
  (when (null? (cdr args)) (usage (car args)))
  (let1 rx (string->regexp (cadr args))
    (if (null? (cddr args))
        (grep rx)
        (for-each (cut with-input-from-file <> (cut grep rx))
                  (cddr args))))
  0)

