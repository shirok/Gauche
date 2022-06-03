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
    [(dir author email)
     (directory-fold dir
                     (^[path seed]
                       (when (or (#/\.(c|h|scm|stub|in|texi)$/ path)
                                 (member (sys-basename path)
                                         '("COPYING" "genstub" "geninsn")))
                         (check-file path author email)))
                     #f)]
    [_ (usage)])
  0)

(define (check-file path author email)
  (define check-rx
    (string->regexp #"[cC]opyright\\s*(\\D+)?(\\d+)(-\\d+)?\\s+(by\\s+)?~|author|"))
  (define current-year (date-year (current-date)))
  (define (file->string-list+ path)
    (call-with-input-file path
      (lambda (in)
        (unwind-protect
            (port->string-list (open-coding-aware-port in))
          (close-input-port in)))))
  (define (rewrite line)
    (let* ([m (check-rx line)]
           [start-year (x->integer (m 2))]
           [copyr (or (m 1) "")]
           [by (or (m 4) " ")]
           [years (if (= start-year current-year)
                    start-year
                    #"~|start-year|-~|current-year|")]
           [em (if (equal? (path-extension path) "texi")
                 (regexp-replace #/@/ email "@@")
                 email)])
      #"~(m 'before)Copyright ~|copyr|~|years| ~|by|~|author|  <~|em|>"))

  (and-let* ([input   (file->string-list+ path)]
             [matched (find check-rx input)])
    (print "Rewriting " path "...")
    (call-with-temporary-file
     (^[out tmp]
       (for-each (lambda (line)
                   (display (if (eq? line matched) (rewrite line) line) out)
                   (newline out))
                 input)
       (replace-file path tmp))
     :directory (sys-dirname path))))

(define (replace-file path tmp)
  (sys-chmod tmp (file-perm path))
  (sys-rename tmp path))
