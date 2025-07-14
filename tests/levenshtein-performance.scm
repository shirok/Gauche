;;
;; benchmarking for util.levenshtein
;;

(use gauche.time)
(use util.match)
(use file.util)
(use util.levenshtein)

(define (main args)
  (match (cdr args)
    [()        (doit l-distances #f)]
    [("l". x)  (doit l-distances  (and (pair? x) (string->number (car x))))]
    [("re". x) (doit re-distances (and (pair? x) (string->number (car x))))]
    [("dl". x) (doit dl-distances (and (pair? x) (string->number (car x))))]
    [_ (exit 1 "Usage: ~a [type [cutoff]]\
              \n  Type: l (levenshtein), re (restricted edit) or dl (damerau-levenshtein\
              \n  Cutoff: integer cutoff"
             (car args))])
  0)

(define (doit proc cutoff)
  (define data (file->string-list "/usr/share/dict/words"))
  (time (proc "scratch" data :cutoff cutoff)))
