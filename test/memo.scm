;; Tests for memo-table
;; memo-table is not for general use, but it is used with built-in type
;; contructors, so we want to test it in the early stage.

(use gauche.test)
(test-start "memo-table")

(define make-memo-table (with-module gauche.internal make-memo-table))
(define memo-table-put! (with-module gauche.internal memo-table-put!))
(define memo-table-get2 (with-module gauche.internal memo-table-get2))

(define (memo-table-get1 tab x) (values-ref (memo-table-get2 tab x) 0))
(define (memo-table-exists? tab x) (values-ref (memo-table-get2 tab x) 1))

(define (test-get-put args data)
  (define tab)
  (test* #"build ~args" #t
         (begin (set! tab (apply make-memo-table args)) #t))
  (test* #"empty ~args" #f
         (any (cut memo-table-exists? tab <>) data))
  (test* #"put 1 ~args" 0
         (begin
           (memo-table-put! tab (car data) 0)
           (memo-table-get1 tab (car data))))
  (test* #"put 1 (rest) ~args" #f
         (any (cut memo-table-exists? tab <>) (cdr data)))
  (test* #"put 2 ~args" '(0 1)
         (begin
           (memo-table-put! tab (cadr data) 1)
           (list (memo-table-get1 tab (car data))
                 (memo-table-get1 tab (cadr data)))))
  (test* #"put 2 (rest) ~args" #f
         (any (cut memo-table-exists? tab <>) (cddr data)))
  (test* #"put all ~args" (iota (length data))
         (begin
           (for-each (^[d v] (memo-table-put! tab d v))
                     (cddr data) (iota (length (cddr data)) 2))
           (map (cut memo-table-get1 tab <>) data)))
  )

(test-get-put '(16 1)
              '(#(a) #(#t) #(4) #("abc") #(4.35)))

(test-get-put '(16 2)
              '(#(a b) #(#t #f) #(4 9) #("abc" "def") #(4.35 9.15)))

(test-get-put '(16 0)
              '(#((a b)) #((#t)) #((4)) #(("abc" "def")) #((4.35))))

(test-get-put '(16 -1)
              '(#(a (b)) #(#t ()) #(4 (8 7)) #("abc" ("def")) #(4.35 (1.0 -i))))

(test-end)
