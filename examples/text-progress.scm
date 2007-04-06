;; sample program to show how to use text.progress

(use text.progress)

(define (main args)
  (define (num-format cur max)
    (format "~d/~d(~3d%)" cur max
            (round->exact (/. (* cur 100) max))))
  
  (let ((p (make-text-progress-bar :header "Example"
                                   :header-width 10
                                   :bar-char #\o
                                   :num-format num-format
                                   :num-width 13
                                   :max-value 256)))
    (do ((i 0 (+ i 1)))
        ((= i 256) (p 'finish))
      (p 'inc 1)
      (sys-select #f #f #f 50000))))

