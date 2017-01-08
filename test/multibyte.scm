;; Encode-specific character/string tests
(add-load-path "../test")

(use gauche.test)

;; char-set-copy to ensure we test mutable one.
(define (test-char-set-1 name expected op arg1)
  (test* #"char-set ~name" (make-list 2 expected)
         (let1 a (char-set-copy arg1)
           (list (op a)
                 (op (char-set-freeze a))))))

(define (test-char-set-2 name expected op arg1 arg2)
  (test* #"char-set ~name" (make-list 4 expected)
         (let* ([a (char-set-copy arg1)]
                [b (char-set-copy arg2)]
                [A (char-set-freeze a)]
                [B (char-set-freeze b)])
           (list (op a b)
                 (op A b)
                 (op a B)
                 (op A B)))))

(case (gauche-character-encoding)
  [(euc-jp) (load "euc-jp")]
  [(sjis)   (load "sjis")]
  [(utf-8)  (load "utf-8")]
  [(none)   #f]
  [else (format #t "No test provided for the character encoding ~s"
                (gauche-character-encoding))])
