;; Rewrite "copyright" line of the source files to make it current.
;; This is a quick "throwaway" script.  It is grossly inefficient,
;; but it does the job.  An example of something you write in half
;; an hour to finish some chores.

(use file.util)
(use util.match)
(use srfi-19)

(define (usage)
  (print "Usage: gosh copyrewrite.scm <directory> <author> <email>")
  (exit 0))

(define (main args)
  (match (cdr args)
    ((dir author email)
     (directory-fold dir
                     (lambda (path seed)
                       (when (or (#/\.(c|h|scm|stub|in|texi)$/ path)
                                 (member (sys-basename path)
                                         '("COPYING" "genstub" "geninsn")))
                         (check-file path author email)))
                     #f))
    (_ (usage)))
  0)

(define (check-file path author email)
  (define check-rx
    (string->regexp #"[cC]opyright\\s*\\([cC]\\)\\s*(\\d+)(-\\d+)?\\s+(by\\s+)?~|author|"))
  (define current-year (date-year (current-date)))
  (define (file->string-list+ path)
    (call-with-input-file path
      (lambda (in)
        (unwind-protect
            (port->string-list (open-coding-aware-port in))
          (close-input-port in)))))
  (define (rewrite line)
    (let* ((m (check-rx line))
           (start-year (x->integer (m 1)))
           (years (if (= start-year current-year)
                    start-year
                    #"~|start-year|-~|current-year|")))
      #"~(m 'before)Copyright (c) ~years  ~author  <~|email|>"))
  
  (and-let* ((input   (file->string-list+ path))
             (matched (find check-rx input)))
    (print "Rewriting " path "...")
    (receive (out tmp) (sys-mkstemp path)
      (for-each (lambda (line)
                  (display (if (eq? line matched) (rewrite line) line) out)
                  (newline out))
                input)
      (close-output-port out)
      (replace-file path tmp))))

(define (replace-file path tmp)
  (sys-chmod tmp (file-perm path))
  (sys-rename tmp path))
