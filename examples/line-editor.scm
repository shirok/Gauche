;;
;; Sample of text.line-edit
;;

;; Just reads a line at a time and echoes back.

(use text.console)
(use text.line-edit)

(define (main args)
  (let* ([con (guard (e [else (exit 1 (~ e'message))])
                (make-default-console))]
         [count 0]
         [ctx (make <line-edit-context>
                :console con
                :prompt (^[] (format #t "[~d]$ " count)))])
    (let loop ()
      (let1 line (read-line/edit ctx)
        (newline)
        (print line)
        (inc! count)
        (loop)))))
