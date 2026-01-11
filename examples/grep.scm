#!/usr/bin/env gosh
(use gauche.parseopt)
(use util.match)

(define (usage)
  (print #"Usage: ~|*program-name*| [option ...] regexp file ...")
  (print "Options:")
  (print (option-parser-help-string))
  (exit 2))

(define (grep test)
  (generator-for-each
   (^[line]
     (when (test line)
       (format #t "~a:~a: ~a\n"
               (port-name (current-input-port))
               (- (port-current-line (current-input-port)) 1)
               line)))
   read-line))

(define (main args)
  (let-args (cdr args) ([exclude-match "v"
                         ? "Exclude lines that match the regexp"]
                        . args)
    (match args
      [() (usage)]
      [(pattern . files)
       (let* ([rx (string->regexp pattern)]
              [test (if exclude-match (complement rx) rx)])
         (if (null? files)
           (grep test)
           (for-each (cut with-input-from-file <> (cut grep test))
                     files)))]))
  0)
