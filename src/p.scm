;;
;; very simple formatter for compiled code
;;

(define (print-code code)
  (letrec ((do-indent
            (lambda (count)
              (if (> count 0)
                  (begin (display "  ") (do-indent (- count 1))))))
           (print-code-int
            (lambda (code indent)
              (for-each (lambda (insn)
                          (if (pair? insn)
                              (if (memq (car insn) '(let let* letrec lambda))
                                  (begin
                                    (do-indent indent)
                                    (write-limited insn 60)
                                    (newline))
                                  (print-code-int insn (+ indent 1)))
                              (begin
                                (do-indent indent)
                                (write-limited insn 60)
                                (newline))))
                        code)))
           )
    (print-code-int code 0)))

                           

  