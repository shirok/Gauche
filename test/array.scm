;;
;; Testing array
;;

;; The test cases are based on the test code by Jussi Pittulainen
;; The original code can be obtained from SRFI site
;; Modified by Shiro Kawai to fit in the Gauche test framework

(use gauche.test)
(use gauche.array)

;;; Simple tests

(test-section "simple tests")

(test "shape" #t
      (lambda ()
        (and (shape)
             (shape -1 -1)
             (shape -1 0)
             (shape -1 1)
             (shape 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
             #t)))

(test "shape" 'error
      (lambda ()
        (with-error-handler (lambda (e) 'error)
          (lambda () (shape 1)))))
(test "shape" 'error
      (lambda ()
        (with-error-handler (lambda (e) 'error)
          (lambda () (shape 1 2 3)))))
(test "shape" 'error
      (lambda ()
        (with-error-handler (lambda (e) 'error)
          (lambda () (shape 3 1)))))

(test "make-array" #t
      (lambda ()
        (and (make-array (shape))
             (make-array (shape) *)
             (make-array (shape -1 -1))
             (make-array (shape -1 -1) *)
             (make-array (shape -1 1))
             (make-array (shape 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4) *)
             #t)))

(test "array" #t
      (lambda ()
        (and (array (shape) *)
             (array (shape -1 -1))
             (array (shape -1 1) * *)
             (array (shape 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8) *)
             #t)))

(test-section "array-rank")
(test "array-rank (shape)" 2
      (lambda () (array-rank (shape))))
(test "array-rank (shape)" 2
      (lambda () (array-rank (shape -1 -1))))
(test "array-rank (shape)" 2
      (lambda () (array-rank (shape 1 2 3 4 5 6 7 8))))

(test "array-rank (make-array)" 0
      (lambda () (array-rank (make-array (shape)))))
(test "array-rank (make-array)" 1
      (lambda () (array-rank (make-array (shape -1 -1)))))
(test "array-rank (make-array)" 1
      (lambda () (array-rank (make-array (shape -1 1)))))
(test "array-rank (make-array)" 4
      (lambda () (array-rank (make-array (shape 1 2 3 4 5 6 7 8)))))

(test "array-rank (array)" 0
      (lambda () (array-rank (array (shape) *))))
(test "array-rank (array)" 1
      (lambda () (array-rank (array (shape -1 -1)))))
(test "array-rank (array)" 1
      (lambda () (array-rank (array (shape -1 1) * *))))
(test "array-rank (array)" 4
      (lambda () (array-rank (array (shape 1 2 3 4 5 6 7 8) *))))

(test-section "array-start and array-end")
(test "array-start (shape)" 0
      (lambda () (array-start (shape -1 -1) 0)))
(test "array-start (shape)" 0
      (lambda () (array-start (shape -1 -1) 1)))
(test "array-start (shape)" 0
      (lambda () (array-start (shape -1 1) 0)))
(test "array-start (shape)" 0
      (lambda () (array-start (shape -1 1) 1)))
(test "array-start (shape)" 0
      (lambda () (array-start (shape 1 2 3 4 5 6 7 8) 0)))
(test "array-start (shape)" 0
      (lambda () (array-start (shape 1 2 3 4 5 6 7 8) 1)))

(test "array-end (shape)" 1
      (lambda () (array-end (shape -1 -1) 0)))
(test "array-end (shape)" 2
      (lambda () (array-end (shape -1 -1) 1)))
(test "array-end (shape)" 1
      (lambda () (array-end (shape -1 1) 0)))
(test "array-end (shape)" 2
      (lambda () (array-end (shape -1 1) 1)))
(test "array-end (shape)" 4
      (lambda () (array-end (shape 1 2 3 4 5 6 7 8) 0)))
(test "array-end (shape)" 2
      (lambda () (array-end (shape 1 2 3 4 5 6 7 8) 1)))

(test "array-start (make-array)" -1
      (lambda () (array-start (make-array (shape -1 -1)) 0)))
(test "array-start (make-array)" -1
      (lambda () (array-start (make-array (shape -1 1)) 0)))
(test "array-start (make-array)" '(1 3 5 7)
      (lambda () (map (pa$ array-start (make-array (shape 1 2 3 4 5 6 7 8)))
                      '(0 1 2 3))))

(test "array-end (make-array)" -1
      (lambda () (array-end (make-array (shape -1 -1)) 0)))
(test "array-end (make-array)" 1
      (lambda () (array-end (make-array (shape -1 1)) 0)))
(test "array-end (make-array)" '(2 4 6 8)
      (lambda () (map (pa$ array-end (make-array (shape 1 2 3 4 5 6 7 8)))
                      '(0 1 2 3))))

(test "array-start (array)" -1
      (lambda () (array-start (array (shape -1 -1)) 0)))
(test "array-start (array)" -1
      (lambda () (array-start (array (shape -1 1) * *) 0)))
(test "array-start (array)" '(1 3 5 7)
      (lambda () (map (pa$ array-start (array (shape 1 2 3 4 5 6 7 8) *))
                      '(0 1 2 3))))
                                                          
(test "array-end (array)" -1
      (lambda () (array-end (array (shape -1 -1)) 0)))
(test "array-end (array)" 1
      (lambda () (array-end (array (shape -1 1) * *) 0)))
(test "array-end (array)" '(2 4 6 8)
      (lambda () (map (pa$ array-end (array (shape 1 2 3 4 5 6 7 8) *))
                      '(0 1 2 3))))
                                                          
(test-section "array-ref")
(test "array-ref (list)" 'a
      (lambda () (array-ref (make-array (shape) 'a))))
(test "array-ref (list)" 'b
      (lambda () (array-ref (make-array (shape -1 1) 'b) -1)))
(test "array-ref (list)" 'c
      (lambda () (array-ref (make-array (shape -1 1) 'c) 0)))
(test "array-ref (list)" 'd
      (lambda () (array-ref (make-array (shape 1 2 3 4 5 6 7 8) 'd) 1 3 5 7)))

(test "array-ref (vector)" 'a
      (lambda () (array-ref (make-array (shape) 'a) '#())))
(test "array-ref (vector)" 'b
      (lambda () (array-ref (make-array (shape -1 1) 'b) '#(-1))))
(test "array-ref (vector)" 'c
      (lambda () (array-ref (make-array (shape -1 1) 'c) '#(0))))
(test "array-ref (vector)" 'd
      (lambda () (array-ref (make-array (shape 1 2 3 4 5 6 7 8) 'd)
                            '#(1 3 5 7))))

(test "array-ref (array)" 'a
      (lambda () (array-ref (make-array (shape) 'a)
                            (array (shape 0 0)))))
(test "array-ref (array)" 'b
      (lambda () (array-ref (make-array (shape -1 1) 'b)
                            (array (shape 0 1) -1))))
(test "array-ref (array)" 'c
      (lambda () (array-ref (make-array (shape -1 1) 'c)
                            (array (shape 0 1) 0))))
(test "array-ref (array)" 'd
      (lambda () (array-ref (make-array (shape 1 2 3 4 5 6 7 8) 'd)
                            (array (shape 0 4) 1 3 5 7))))

(test-section "array-set!")
(test "array-set! (list)" 'a
      (lambda ()
        (let ((arr (make-array (shape) 'o)))
          (array-set! arr 'a)
          (array-ref arr))))
(test "array-set! (list)" '(b c)
      (lambda ()
        (let ((arr (make-array (shape -1 1) 'o)))
          (array-set! arr -1 'b)
          (array-set! arr 0 'c)
          (list (array-ref arr -1) (array-ref arr 0)))))
(test "array-set! (list)" 'd
      (lambda ()
        (let ((arr (make-array (shape 1 2 3 4 5 6 7 8) 'o)))
          (array-set! arr 1 3 5 7 'd)
          (array-ref arr 1 3 5 7))))

(test "array-set! (vector)" 'a
      (lambda ()
        (let ((arr (make-array (shape) 'o)))
          (array-set! arr '#() 'a)
          (array-ref arr))))
(test "array-set! (vector)" '(b c)
      (lambda ()
        (let ((arr (make-array (shape -1 1) 'o)))
          (array-set! arr '#(-1) 'b)
          (array-set! arr '#(0) 'c)
          (list (array-ref arr -1) (array-ref arr 0)))))
(test "array-set! (vector)" 'd
      (lambda ()
        (let ((arr (make-array (shape 1 2 3 4 5 6 7 8) 'o)))
          (array-set! arr '#(1 3 5 7) 'd)
          (array-ref arr 1 3 5 7))))

(test "array-set! (array)" 'a
      (lambda ()
        (let ((arr (make-array (shape) 'o)))
          (array-set! arr 'a)
          (array-ref arr))))
(test "array-set! (array)" '(b c)
      (lambda ()
        (let ((arr (make-array (shape -1 1) 'o)))
          (array-set! arr (array (shape 0 1) -1) 'b)
          (array-set! arr (array (shape 0 1) 0) 'c)
          (list (array-ref arr -1)  (array-ref arr 0)))))
(test "array-set! (array)" 'd
      (lambda ()
        (let ((arr (make-array (shape 1 2 3 4 5 6 7 8) 'o)))
          (array-set! arr (array (shape 0 4) 1 3 5 7) 'd)
          (array-ref arr 1 3 5 7))))

;;; Share and change:
;;;
;;;  org     brk     swp            box
;;;
;;;   0 1     1 2     5 6
;;; 6 a b   2 a b   3 d c   0 2 4 6 8: e
;;; 7 c d   3 e f   4 f e
;;; 8 e f

(test-section "shared change")

(let* ((org (array (shape 6 9 0 2) 'a 'b 'c 'd 'e 'f))
       (brk (share-array
             org
             (shape 2 4 1 3)
             (lambda (r k)
               (values
                (+ 6 (* 2 (- r 2)))
                (- k 1)))))
       (swp (share-array
             org
             (shape 3 5 5 7)
             (lambda (r k)
               (values
                (+ 7 (- r 3))
                (- 1 (- k 5))))))
       (box (share-array
             swp
             (shape 0 1 2 3 4 5 6 7 8 9)
             (lambda _ (values 4 6))))
       (org-contents (lambda ()
                       (list (array-ref org 6 0) (array-ref org 6 1)
                             (array-ref org 7 0) (array-ref org 7 1)
                             (array-ref org 8 0) (array-ref org 8 1))))
       (brk-contents (lambda ()
                       (list (array-ref brk 2 1) (array-ref brk 2 2)
                             (array-ref brk 3 1) (array-ref brk 3 2))))
       (swp-contents (lambda ()
                       (list (array-ref swp 3 5) (array-ref swp 3 6)
                             (array-ref swp 4 5) (array-ref swp 4 6))))
       (box-contents (lambda ()
                       (list (array-ref box 0 2 4 6 8)))))
  (test "org-contents" '(a b c d e f) org-contents)
  (test "brk-contents" '(a b e f) brk-contents)
  (test "swp-contents" '(d c f e) swp-contents)
  (test "box-contents" '(e) box-contents)
  (begin (array-set! org 6 0 'x) #t)
  (test "org-contents" '(x b c d e f) org-contents)
  (test "brk-contents" '(x b e f) brk-contents)
  (test "swp-contents" '(d c f e) swp-contents)
  (test "box-contents" '(e) box-contents)
  (begin (array-set! brk 3 1 'y) #t)
  (test "org-contents" '(x b c d y f) org-contents)
  (test "brk-contents" '(x b y f) brk-contents)
  (test "swk-contents" '(d c f y) swp-contents)
  (test "box-contents" '(y) box-contents)
  (begin (array-set! swp 4 5 'z) #t)
  (test "org-contents" '(x b c d y z) org-contents)
  (test "brk-contents" '(x b y z) brk-contents)
  (test "swp-contents" '(d c z y) swp-contents)
  (test "box-contents" '(y) box-contents)
  (begin (array-set! box 0 2 4 6 8 'e) #t)
  (test "org-contents" '(x b c d e z) org-contents)
  (test "brk-contents" '(x b e z) brk-contents)
  (test "swp-contents" '(d c z e) swp-contents)
  (test "box-contents" '(e) box-contents)
  )

;;; Check that arrays copy the shape specification

(test-section "array-set! of shape")

(let ((shp (shape 10 12)))
  (let ((arr (make-array shp))
        (ars (array shp * *))
        (art (share-array (make-array shp) shp (lambda (k) k))))
    (array-set! shp 0 0 '?)
    (array-set! shp 0 1 '!)
    (test "modifying array shape"
          '(2 0 1 0 2 ? ! 1 10 12 1 10 12 1 10 12)
          (lambda ()
            (list (array-rank shp)
                  (array-start shp 0)
                  (array-end shp 0)
                  (array-start shp 1)
                  (array-end shp 1)
                  (array-ref shp 0 0)
                  (array-ref shp 0 1)
                  (array-rank arr)
                  (array-start arr 0)
                  (array-end arr 0)
                  (array-rank ars)
                  (array-start ars 0)
                  (array-end ars 0)
                  (array-rank art)
                  (array-start art 0)
                  (array-end art 0))))))

;;; Check that index arrays work even when they share
;;;
;;; arr       ixn
;;;   5  6      0 1
;;; 4 nw ne   0 4 6
;;; 5 sw se   1 5 4

(test-section "array access with sharing index array")
(let ((arr (array (shape 4 6 5 7) 'nw 'ne 'sw 'se))
      (ixn (array (shape 0 2 0 2) 4 6 5 4)))
  (let ((col0 (share-array
               ixn
               (shape 0 2)
               (lambda (k)
                 (values k 0))))
        (row0 (share-array
               ixn
               (shape 0 2)
               (lambda (k)
                 (values 0 k))))
        (wor1 (share-array
               ixn
               (shape 0 2)
               (lambda (k)
                 (values 1 (- 1 k)))))
        (cod (share-array
              ixn
              (shape 0 2)
              (lambda (k)
                (case k
                  ((0) (values 1 0))
                  ((1) (values 0 1))))))
        (box (share-array
              ixn
              (shape 0 2)
              (lambda (k)
                (values 1 0)))))
    (test "array-ref before change"
          '(nw ne nw se sw)
          (lambda ()
            (list (array-ref arr col0)
                  (array-ref arr row0)
                  (array-ref arr wor1)
                  (array-ref arr cod)
                  (array-ref arr box))))
    (array-set! arr col0 'ul)
    (array-set! arr row0 'ur)
    (array-set! arr cod 'lr)
    (array-set! arr box 'll)
    (test "array-ref after change"
          '(ul ur ll lr)
          (lambda ()
            (list (array-ref arr 4 5)
                  (array-ref arr 4 6)
                  (array-ref arr 5 5)
                  (array-ref arr 5 6))))
    (array-set! arr wor1 'xx)
    (test "array-ref after change" 'xx
          (lambda () (array-ref arr 4 5)))))

;;; Check that shape arrays work even when they share
;;;
;;; arr             shp       shq       shr       shs
;;;    1  2  3  4      0  1      0  1      0  1      0  1 
;;; 1 10 12 16 20   0 10 12   0 12 20   0 10 10   0 12 12
;;; 2 10 11 12 13   1 10 11   1 11 13   1 11 12   1 12 12
;;;                                     2 12 16
;;;                                     3 13 20

(test-section "sharing shape array")
(let ((arr (array (shape 1 3 1 5) 10 12 16 20 10 11 12 13)))
  (let ((shp (share-array
              arr
              (shape 0 2 0 2)
              (lambda (r k)
                (values (+ r 1) (+ k 1)))))
        (shq (share-array
              arr
              (shape 0 2 0 2)
              (lambda (r k)
                (values (+ r 1) (* 2 (+ 1 k))))))
        (shr (share-array
              arr
              (shape 0 4 0 2)
              (lambda (r k)
                (values (- 2 k) (+ r 1)))))
        (shs (share-array
              arr
              (shape 0 2 0 2)
              (lambda (r k)
                (values 2 3)))))
    (test "using make-array shp"
          '(2 10 12 10 11)
          (lambda ()
            (let ((arr-p (make-array shp)))
              (list (array-rank arr-p)
                    (array-start arr-p 0)
                    (array-end arr-p 0)
                    (array-start arr-p 1)
                    (array-end arr-p 1)))))
    (test "using array shq"
          '(2 12 20 11 13)
          (lambda ()
            (let ((arr-q (array shq * * * *  * * * *  * * * *  * * * *)))
              (list (array-rank arr-q)
                    (array-start arr-q 0)
                    (array-end arr-q 0)
                    (array-start arr-q 1)
                    (array-end arr-q 1)))))
    (test "using share-array"
          '(4 10 10 11 12 12 16 13 20)
          (lambda ()
            (let ((arr-r (share-array
                          (array (shape) *)
                          shr
                          (lambda _ (values)))))
              (list (array-rank arr-r)
                    (array-start arr-r 0)
                    (array-end arr-r 0)
                    (array-start arr-r 1)
                    (array-end arr-r 1) 
                    (array-start arr-r 2)
                    (array-end arr-r 2)
                    (array-start arr-r 3)
                    (array-end arr-r 3)))))
    (test "using make-array shs"
          '(2 12 12 12 12)
          (lambda ()
            (let ((arr-s (make-array shs)))
              (list (array-rank arr-s)
                    (array-start arr-s 0)
                    (array-end arr-s 0)
                    (array-start arr-s 1)
                    (array-end arr-s 1)))))))

(test-section "sharing with sharing subshape")
(let ((super (array (shape 4 7 4 7)
                    1 * *
                    * 2 *
                    * * 3))
      (subshape (share-array
                 (array (shape 0 2 0 3)
                        * 4 *
                        * 7 *)
                 (shape 0 1 0 2)
                 (lambda (r k)
                   (values k 1)))))
  (let ((sub (share-array super subshape (lambda (k) (values k k)))))
    ;(array-equal? subshape (shape 4 7))
    (test "sharing subshape" '(2 0 1 0 2 4 7)
          (lambda ()
            (list (array-rank subshape)
                  (array-start subshape 0)
                  (array-end subshape 0)
                  (array-start subshape 1)
                  (array-end subshape 1)
                  (array-ref subshape 0 0)
                  (array-ref subshape 0 1))))
    ;(array-equal? sub (array (shape 4 7) 1 2 3))
    (test "sharing with sharing subshape" '(1 4 7 1 2 3)
          (lambda ()
            (list (array-rank sub)
                  (array-start sub 0)
                  (array-end sub 0)
                  (array-ref sub 4)
                  (array-ref sub 5)
                  (array-ref sub 6))))
    ))

(test-end)
