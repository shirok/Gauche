;; trie
(test-section "data.trie")
(use data.trie)
(test-module 'data.trie)
(use gauche.uvector)
(use gauche.sequence)
(use srfi-1)
(use srfi-13)

(let* ((strs '("kana" "kanaono" "kanawai" "kanawai koa"
               "kanawai mele" "kane" "Kane" "kane make" "kane makua"
               "ku" "kua" "kua`aina" "kua`ana"
               "liliko`i" "lilinoe" "lili`u" "lilo" "maoli" ""))
       (lists (map string->list strs))
       (vecs  (map list->vector lists))
       (uvecs (map string->u8vector strs)))

  ;; string trie tests
  (let1 t1 (make-trie)
    (test* "trie: constructor" '(#t 0)
           (list (trie? t1) (trie-num-entries t1)))
    (test* "trie: exists?" #f (trie-exists? t1 "kane"))

    (test* "trie: put!" 1
           (begin (trie-put! t1 "lilo" 4)
                  (trie-num-entries t1)))
    (test* "trie: get" 4
           (trie-get t1 "lilo"))
    (test* "trie: get (error)" (test-error)
           (trie-get t1 "LILO"))
    (test* "trie: get (fallback)" 'foo
           (trie-get t1 "LILO" 'foo))

    (test* "trie: put! more" (length strs)
           (begin (for-each (lambda (s)
                              (trie-put! t1 s (string-length s)))
                            strs)
                  (trie-num-entries t1)))
    (test* "trie: get more" #t
           (every (lambda (s)
                    (= (trie-get t1 s) (string-length s)))
                  strs))
    (test* "trie: exists? more" #t
           (every (cut trie-exists? t1 <>) strs))
    (test* "trie: exists? on partial key" #f
           (every (cut trie-exists? t1 <>) '("k" "ka" "kan")))
    (test* "trie: partial-key?"
           '(("k" #t) ("kana" #t) ("kanawai koa" #f) ("" #t) ("zzz" #f))
           (map (^k (list k (trie-partial-key? t1 k)))
                '("k" "kana" "kanawai koa" "" "zzz")))
    (test* "trie: longest match" '(("kana" . 4)
                                   ("kana" . 4)
                                   ("kanawai" . 7)
                                   ("" . 0))
           (map (^k (trie-longest-match t1 k #f))
                '("kana" "kanaoka" "kanawai pele" "mahalo")))
    (test* "trie: common-prefix" '(19 12 8 4 4 3)
           (map (^p (length (trie-common-prefix t1 p)))
                '("" "k" "ka" "ku" "li" "lili")))
    (test* "trie: common-prefix" '(("kua" . 3)
                                   ("kua`aina" . 8)
                                   ("kua`ana" . 7))
           (trie-common-prefix t1 "kua")
           (cut lset= equal? <> <>))
    (test* "trie: common-prefix-keys" '("kua" "kua`aina" "kua`ana")
           (trie-common-prefix-keys t1 "kua")
           (cut lset= equal? <> <>))
    (test* "trie: common-prefix-values" '(3 8 7)
           (trie-common-prefix-values t1 "kua")
           (cut lset= = <> <>))
    (test* "trie: common-prefix-fold" 18
           (trie-common-prefix-fold t1 "kua"
                                    (lambda (k v s) (+ v s))
                                    0))
    (test* "trie: common-prefix-map" '("KUA" "KUA`AINA" "KUA`ANA")
           (trie-common-prefix-map t1 "kua"
                                   (lambda (k v) (string-upcase k)))
           (cut lset= equal? <> <>))
    (test* "trie: common-prefix-for-each" '("KUA" "KUA`AINA" "KUA`ANA")
           (let1 p '()
             (trie-common-prefix-for-each t1 "kua"
                                          (lambda (k v)
                                            (push! p (string-upcase k))))
             p)
           (cut lset= equal? <> <>))
    (test* "trie: trie-fold" (fold (lambda (k s) (+ (string-length k) s))
                                      0 strs)
           (trie-fold t1 (lambda (k v s) (+ v s)) 0))
    (test* "trie: trie-map" (fold (lambda (k s) (+ (string-length k) s))
                                     0 strs)
           (apply + (trie-map t1 (lambda (k v) v))))
    (test* "trie: trie-for-each"
           (fold (lambda (k s) (+ (string-length k) s))
                 0 strs)
           (let1 c 0 (trie-for-each t1 (lambda (k v) (inc! c v))) c))
    (test* "trie: trie->list"
           (map (^s (cons s (string-length s))) strs)
           (trie->list t1)
           (cut lset= equal? <> <>))
    (test* "trie: trie-keys"
           strs
           (trie-keys t1)
           (cut lset= equal? <> <>))
    (test* "trie: trie-values"
           (map string-length strs)
           (trie-values t1)
           (cut lset= equal? <> <>))
    (test* "trie: trie-update!" 16
           (begin (trie-update! t1 "liliko`i" (cut + <> 8))
                  (trie-get t1 "liliko`i")))
    (test* "trie: trie-update! (nonexistent)" (test-error)
           (trie-update! t1 "humuhumu" (cut + <> 8)))
    (test* "trie: trie-update! (nonexistent)" 16
           (begin (trie-update! t1 "humuhumu" (cut + <> 8) 8)
                  (trie-get t1 "humuhumu")))
    (test* "trie: delete!" '(19 #f)
           (begin (trie-delete! t1 "humuhumu")
                  (list (trie-num-entries t1)
                        (trie-get t1 "humuhumu" #f))))
    (test* "trie: delete! (nonexistent)" '(19 #f)
           (begin (trie-delete! t1 "HUMUHUMU")
                  (list (trie-num-entries t1)
                        (trie-get t1 "HUMUHUMU" #f))))
    (test* "trie: delete! (everything)" 0
           (begin (for-each (cut trie-delete! t1 <>) strs)
                  (trie-num-entries t1)))
    )
  ;; trie and trie-with-keys
  (let1 t2 (trie '() '("foo" . 0) '("foof" . 1) '("far" . 2))
    (test* "trie: trie" '(("foo" . 0) ("foof" . 1) ("far" . 2))
           (trie->list t2)
           (cut lset= equal? <> <>)))
  (let1 t3 (trie-with-keys '() "foo" "foof" "far")
    (test* "trie: trie-with-keys"
           '(("foo" . "foo") ("foof" . "foof") ("far" . "far"))
           (trie->list t3)
           (cut lset= equal? <> <>)))

  ;; heterogeneous tries
  (let1 t4 (make-trie)
    (for-each (cut for-each
                   (lambda (seq)
                     (trie-put! t4 seq (class-of seq)))
                   <>)
              (list strs lists vecs uvecs))
    (test* "trie(hetero): put!" (* 4 (length strs))
           (trie-num-entries t4))
    (test* "trie(hetero): get" <vector> (trie-get t4 '#()))
    (test* "trie(hetero): get" <u8vector> (trie-get t4 '#u8()))
    (test* "trie(hetero): get" <pair> (trie-get t4 '(#\k #\u)))

    (test* "trie(hetero): delete!" <string>
           (begin (trie-delete! t4 '()) (trie-get t4 "")))
    (test* "trie(hetero): delete!" (* 3 (length strs))
           (begin (for-each (cut trie-delete! t4 <>) lists)
                  (trie-num-entries t4)))
    )

  ;; customizing tables
  (let1 t5 (make-trie list
                      (cut assoc-ref <> <> #f char-ci=?)
                      (lambda (t k v)
                        (if v
                          (assoc-set! t k v char-ci=?)
                          (alist-delete! k t char-ci=?)))
                      (lambda (t f s) (fold f s t)))
    (test* "trie(custom): put!" (- (length strs) 1)
           (begin
             (for-each (^s (trie-put! t5 s (string-length s))) strs)
             (trie-num-entries t5)))
    (test* "trie(custom): get" 99
           (begin
             (trie-put! t5 "LILIKO`I" 99)
             (trie-get t5 "liliko`i")))
    )

  ;; collection api
  (let1 t6 #f
    (test* "trie(collection): builder" (length strs)
           (begin
             (set! t6 (coerce-to <trie> (map (cut cons <> #t) strs)))
             (and (trie? t6) (size-of t6))))
    (test* "trie(collection): iterator" strs
           (let1 p '()
             (call-with-iterator t6
                                 (lambda (end next)
                                   (until (end)
                                     (push! p (car (next))))))
             p)
           (cut lset= equal? <> <>))
    (test* "trie(collection): coerce to list" (map (cut cons <> #t) strs)
           (coerce-to <list> t6)
           (cut lset= equal? <> <>))
    (test* "trie(collection): coerce to vector"
           (map (cut cons <> #t) strs)
           (vector->list (coerce-to <vector> t6))
           (cut lset= equal? <> <>))
    (test* "trie(collection): coerce to hashtable" #t
           (let1 h (coerce-to <hash-table> t6)
             (every (cut hash-table-get h <>) strs)))
    )
  )
