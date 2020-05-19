;; ring-buffer
(test-section "data.ring-buffer")
(use data.ring-buffer)
(test-module 'data.ring-buffer)
(use gauche.sequence)
(use gauche.uvector)
(use srfi-1)


(define (test-ring-buffer initial-storage)
  (define c (class-name (class-of initial-storage)))
  (define (remove-all-from-front rb)
    (unfold ring-buffer-empty?
            ring-buffer-remove-front!
            identity
            rb))
  (define (remove-all-from-back rb)
    (unfold ring-buffer-empty?
            ring-buffer-remove-back!
            identity
            rb))
  (define (test-add-remove n)
    (let1 rb (make-ring-buffer initial-storage)
      (test* #"add front - remove front (~c, ~n))"
             (reverse (iota n))
             (begin
               (dotimes (i n) (ring-buffer-add-front! rb i))
               (remove-all-from-front rb))))
    (let1 rb (make-ring-buffer initial-storage)
      (test* #"add front - remove back (~c, ~n))"
             (iota n)
             (begin
               (dotimes (i n) (ring-buffer-add-front! rb i))
               (remove-all-from-back rb))))
    (let1 rb (make-ring-buffer initial-storage)
      (test* #"add back - remove front (~c, ~n))"
             (iota n)
             (begin
               (dotimes (i n) (ring-buffer-add-back! rb i))
               (remove-all-from-front rb))))
    (let1 rb (make-ring-buffer initial-storage)
      (test* #"add back - remove back (~c, ~n))"
             (reverse (iota n))
             (begin
               (dotimes (i n) (ring-buffer-add-back! rb i))
               (remove-all-from-back rb)))))

  (define (test-basic)
    (let1 rb (make-ring-buffer initial-storage)
      (test* #"empty (~c)" #t (ring-buffer-empty? rb))
      (test* #"empty (~c)" 0  (ring-buffer-num-entries rb))
      (test* #"initial (~c)" '(10 10 1 #f)
             (begin (ring-buffer-add-front! rb 10)
                    (list (ring-buffer-front rb)
                          (ring-buffer-back rb)
                          (ring-buffer-num-entries rb)
                          (ring-buffer-empty? rb))))
      (test* #"2 elemenst (~c)" '(10 11 2 #f)
             (begin (ring-buffer-add-back! rb 11)
                    (list (ring-buffer-front rb)
                          (ring-buffer-back rb)
                          (ring-buffer-num-entries rb)
                          (ring-buffer-empty? rb))))))
  (define (test-overflow)
    (let1 rb (make-ring-buffer initial-storage :overflow-handler 'error)
      (test* #"overflow - error (~c)" (test-error)
             (dotimes [i (+ (size-of initial-storage) 1)]
               (ring-buffer-add-back! rb i))))
    (let1 rb (make-ring-buffer initial-storage :overflow-handler 'overwrite)
      (test* #"overflow - overwrite forward (~c)"
             (iota (size-of initial-storage)
                   (size-of initial-storage))
             (begin
               (dotimes [i (* (size-of initial-storage) 2)]
                 (ring-buffer-add-back! rb i))
               (remove-all-from-front rb))))
    (let1 rb (make-ring-buffer initial-storage :overflow-handler 'overwrite)
      (test* #"overflow - overwrite backward (~c)"
             (reverse (iota (size-of initial-storage)
                            (size-of initial-storage)))
             (begin
               (dotimes [i (* (size-of initial-storage) 2)]
                 (ring-buffer-add-front! rb i))
               (remove-all-from-front rb)))))

  ;; body of test-ring-buffer
  (test-basic)
  (test-overflow)
  (test-add-remove 3)
  (test-add-remove 12) ; causes realloc
  )
                  
(test-ring-buffer (make-vector 4))
(test-ring-buffer (make-u8vector 5))
