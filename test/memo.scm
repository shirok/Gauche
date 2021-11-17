;; Tests for memo-table
;; memo-table is not for general use, but it is used with built-in type
;; contructors, so we want to test it in the early stage.

(use gauche.test)
(test-start "memo-table")

(define make-memo-table (with-module gauche.internal make-memo-table))
(define memo-table-putv! (with-module gauche.internal memo-table-putv!))
(define memo-table-put!  (with-module gauche.internal memo-table-put!))
(define memo-table-getv2 (with-module gauche.internal memo-table-getv2))
(define memo-table-get2  (with-module gauche.internal memo-table-get2))

(define (memo-table-getv1 tab x) (values-ref (memo-table-getv2 tab x) 0))
(define (memo-table-get1 tab x)  (values-ref (memo-table-get2 tab x) 0))
(define (memo-table-exists? tab x) (values-ref (memo-table-getv2 tab x) 1))

(define (test-get-put msg args data)
  (define tab)                          ; will be set
  (define (expand-data vec)
    (if (> (cadr args) 0)
      (vector->list vec)                 ; no 'rest' keys
      (apply list* (vector->list vec)))) ; 'rest' keys

  ;; passing keys in vector
  (test* #"build ~|msg|~args" #t
         (begin (set! tab (apply make-memo-table args)) #t))
  (test* #"empty ~|msg|~args" #f
         (any (cut memo-table-exists? tab <>) data))
  (test* #"put 1 ~|msg|~args" 0
         (begin
           (memo-table-putv! tab (car data) 0)
           (memo-table-getv1 tab (car data))))
  (test* #"put 1 (rest) ~|msg|~args" #f
         (any (cut memo-table-exists? tab <>) (cdr data)))
  (test* #"put 2 ~|msg|~args" '(0 1)
         (begin
           (memo-table-putv! tab (cadr data) 1)
           (list (memo-table-getv1 tab (car data))
                 (memo-table-getv1 tab (cadr data)))))
  (test* #"put 2 (rest) ~|msg|~args" #f
         (any (cut memo-table-exists? tab <>) (cddr data)))
  (test* #"put all ~|msg|~args" (iota (length data))
         (begin
           (for-each (^[d v] (memo-table-putv! tab d v))
                     (cddr data) (iota (length (cddr data)) 2))
           (map (cut memo-table-getv1 tab <>) data)))

  ;; passing keys in list
  (test* #"build ~|msg|~args" #t
         (begin (set! tab (apply make-memo-table args)) #t))
  (test* #"put 1 ~|msg|~args" 0
         (begin
           (memo-table-put! tab (expand-data (car data)) 0)
           (memo-table-get1 tab (expand-data (car data)))))
  (test* #"put 1 (rest) ~|msg|~args" #f
         (any (cut memo-table-exists? tab <>) (cdr data)))
  (test* #"put 2 ~|msg|~args" '(0 1)
         (begin
           (memo-table-put! tab (expand-data (cadr data)) 1)
           (list (memo-table-get1 tab (expand-data (car data)))
                 (memo-table-get1 tab (expand-data (cadr data))))))
  (test* #"put 2 (rest) ~|msg|~args" #f
         (any (cut memo-table-exists? tab <>) (cddr data)))
  (test* #"put all ~|msg|~args" (iota (length data))
         (begin
           (for-each (^[d v] (memo-table-put! tab (expand-data d) v))
                     (cddr data) (iota (length (cddr data)) 2))
           (map (^d (memo-table-get1 tab (expand-data d))) data)))
  )

(test-get-put "" '(16 1)
              '(#(a) #(#t) #(4) #("abc") #(4.35)))

(test-get-put "" '(16 2)
              '(#(a b) #(#t #f) #(4 9) #("abc" "def") #(4.35 9.15)))

(test-get-put "" '(16 0)
              '(#((a b)) #((#t)) #((4)) #(("abc" "def")) #((4.35))))

(test-get-put "" '(16 -1)
              '(#(a (b)) #(#t ()) #(4 (8 7)) #("abc" ("def")) #(4.35 (1.0 -i))))


(test-get-put "extend " '(4 1)
              '(#(a) #(b) #(c) #(d) #(e) #(f) #(g) #(h) #(i) #(j) #(k)
                #(l) #(m) #(n) #(o) #(p) #(q) #(r) #(s) #(t) #(u) #(v)))
(test-get-put "extend " '(4 -1)
              '(#(a (A)) #(b (B)) #(c (C)) #(d (D)) #(e (E)) #(f (F))
                #(g (G)) #(h (H)) #(i (I)) #(j (J)) #(k (K)) #(l (L))
                #(m (M)) #(n (N)) #(o (O)) #(p (P)) #(q (Q)) #(r (R))
                #(s (S)) #(t (T)) #(u (U)) #(v (V)) #(w (W)) #(x (X))))

(test-end)
