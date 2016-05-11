;;
;; Sample of text.line-edit
;;

;; Just reads a line at a time and echoes back.

(use text.console)
(use text.line-edit)
(use gauche.listener :only (complete-sexp?))

(define (main args)
  (let* ([con (guard (e [else (exit 1 (~ e'message))])
                (make-default-console))]
         [count 0]
         [ctx (make <line-edit-context>
                :console con
                :prompt (^[] (format #t "[~d]$ " count))
                :input-continues (^s (not (complete-sexp? s))))])
    (let loop ()
      (let1 line (read-line/edit ctx)
        (unless (eof-object? line)
          (print line)
          (inc! count)
          (loop))))))
