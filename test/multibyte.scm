;; Encode-specific character/string tests
(add-load-path "../test")

(use gauche.test)

(define (test-char-set-1 name expected op arg1)
  (test* #"char-set ~name" (make-list 2 expected)
         (list (op arg1)
               (op (char-set-freeze arg1)))))

(define (test-char-set-2 name expected op arg1 arg2)
  (test* #"char-set ~name" (make-list 4 expected)
         (let ([iarg1 (char-set-freeze arg1)]
               [iarg2 (char-set-freeze arg2)])
           (list (op arg1 arg2)
                 (op iarg1 arg2)
                 (op arg1 iarg2)
                 (op iarg1 iarg2)))))

(case (gauche-character-encoding)
  [(euc-jp) (load "euc-jp")]
  [(sjis)   (load "sjis")]
  [(utf-8)  (load "utf-8")]
  [(none)   #f]
  [else (format #t "No test provided for the character encoding ~s"
                (gauche-character-encoding))])
