(define (string-replace s1 s2 start1 end1 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (string-append (substring s1 0 start1)
                 (apply %maybe-substring s2 args)
                 (substring s1 end1 (string-length s1))))

(define (string-tokenize s . args)
  (check-arg string? s)
  (let-optional* args ((token-set #[\S]) start end)
    (letrec ((src (open-input-string (%maybe-substring s start end)))
             (in-word (lambda (ch dst r)
                        (cond ((eof-object? ch)
                               (reverse! (cons (get-output-string dst) r)))
                              ((char-set-contains? token-set ch)
                               (write-char ch dst)
                               (in-word (read-char src) dst r))
                              (else
                               (out-word (read-char src)
                                         (cons (get-output-string dst) r))))))
             (out-word (lambda (ch r)
                         (cond ((eof-object? ch) (reverse! r))
                               ((char-set-contains? token-set ch)
                                (let ((dst (open-output-string)))
                                  (write-char ch dst)
                                  (in-word (read-char src) dst r)))
                               (else
                                (out-word (read-char src) r))))))
      (out-word (read-char src) '()))))


  