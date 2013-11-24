;;
;; ndbm-suffix - find out what suffix(es) is/are used for ndbm database.
;;               write out the result into a file given to the argument
;;               in C source form.

(use gauche.process)
(use util.match)

(define (main args)
  (match (cdr args)
    [(file)
     (receive (p tname) (sys-mkstemp "ndbmtest")
       (close-output-port p)
       (sys-unlink tname)
       (let1 p (run-process `("./ndbm-makedb" ,tname) :wait #t)
         (unless (zero? (process-exit-status p))
           (print "ndbm-makedb failed")
           (exit 1))
         ;; NB: we cannot use file.util now, for it is being compiled.
         (let1 sfxs (map (cut rxmatch->string #/\.[^\.]+$/ <>)
                         (glob #"~|tname|.*"))
           (with-output-to-file file
             (^[]
               (print "static const char *ndbm_suffixes[] = {")
               (for-each (cut format #t "  ~s,\n" <>) sfxs)
               (print "  NULL")
               (print "};"))))
         (for-each sys-unlink (glob #"~|tname|.*"))))]
    [else (print "Usage: gosh ndbm-suffixes.scm <outfile>") (exit 1)])
  0)





