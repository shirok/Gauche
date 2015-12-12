;;
;; test for parameter & fluids
;;

(use gauche.test)
(test-start "parameters")

(use gauche.parameter)
(test-module 'gauche.parameter)

;;-------------------------------------------------------------------
(test-section "basics")

(define a #f)
(define b #f)

(test* "make-parameter" 3
       (begin
         (set! a (make-parameter 3))
         (a)))
(test* "make-parameter" 88
       (begin
         (set! b (make-parameter 88 x->integer))
         (b)))
(test* "parameter" "abc"
       (begin (a "abc") (a)))
(test* "parameter" 34
       (begin (b "34") (b)))

(test* "parameterize" '(("84" 0) (0 84) ("84" 0) ("abc" 34))
       (let* ([z (parameterize ([a "84"]
                                [b (a)])
                   (let* ([z1 (list (a) (b))]
                          [z2 (parameterize ([a (b)]
                                             [b (a)])
                                (list (a) (b)))]
                          [z3 (list (a) (b))])
                     (list z1 z2 z3)))]
              [zz (list (a) (b))])
         (append z (list zz))))

(test* "parameterize & dynamic-wind" '(("93" 112)
                                       (34 0)
                                       ("93" 112)
                                       (34 0))
       (let* ([z '()]
              [k (parameterize ([a "93"] [b 112])
                   (rlet1 k (call/cc identity)
                     (push! z (list (a) (b)))))])
         (parameterize ([a (b)] [b (a)])
           (push! z (list (a) (b)))
           (if k (k #f)))
         (reverse z)))

(test* "generalized set! for parameters" "foo"
       (begin (set! (a) "foo") (a)))
(test* "generalized set! for parameters" 99
       (begin (set! (b) "99") (b)))

;;-------------------------------------------------------------------
(test-section "observers")

(test* "observers" '((pre1 3 4 3) (pre2 3 4 3) (post2 3 4 4) (post1 3 4 4))
       (let ([p (make-parameter 3)]
             [r '()])
         (parameter-observer-add! p (^[o v] (push! r `(pre1 ,o ,v ,(p))))
                                  'before)
         (parameter-observer-add! p (^[o v] (push! r `(post1 ,o ,v ,(p))))
                                  'after)
         (parameter-observer-add! p (^[o v] (push! r `(pre2 ,o ,v ,(p))))
                                  'before 'append)
         (parameter-observer-add! p (^[o v] (push! r `(post2 ,o ,v ,(p))))
                                  'after 'prepend)
         (p 4)
         (reverse r)))

(test* "observers" '((pre1 4 3) (post1 4 4))
       (let ([p (make-parameter 3)]
             [r '()]
             [pre2 (^[o v] (push! r `(pre2 ,v ,(p))))]
             [post2  (^[o v] (push! r `(post2 ,v ,(p))))])
         (parameter-observer-add! p (^[o v] (push! r `(pre1 ,v ,(p))))
                                  'before)
         (parameter-observer-add! p pre2 'before 'append)
         (parameter-observer-add! p (^[o v] (push! r `(post1 ,v ,(p))))
                                  'after)
         (parameter-observer-add! p post2 'after 'prepend)
         (parameter-observer-delete! p pre2)
         (parameter-observer-delete! p post2 'after)
         (p 4)
         (reverse r)))

;;-------------------------------------------------------------------
(test-section "correct restoration semantics")

;; Make sure restoring values bypasses filter procedure
(let ()
  (define (dotest param)
    (let* ([a1 (param)]
           [a2 (parameterize ([param 4]) (param))]
           [a3 (param)])
      (list a1 a2 a3)))

  (test* "check filter proc isn't called on restoration" '("2" "4" "2")
         (dotest (make-parameter 2 number->string)))

  (test* "parameter-like procedure" '(0 4 0)
         (dotest (let1 v 0
                   (^[:optional new]
                     (if (undefined? new) v (begin0 v (set! v new)))))))

  (test* "filter and observer"
         '((post "4" "2") (pre "4" "2") (post "2" "4") (pre "2" "4"))
         (let ([a (make-parameter 2 number->string)]
               [r '()])
           (parameter-observer-add! a (^[o v] (push! r `(pre ,o ,v))) 'before)
           (parameter-observer-add! a (^[o v] (push! r `(post ,o ,v))) 'after)
           (dotest a)
           r))
  )

;; The dynamic environment needs to work on locations, not the values.
;; In the following code, when the continuation is invoked, the value of
;; f should be restored to c, not b.
;; Test code by Joo ChurlSoo.

(test* "call/cc and side effect" '(a b c d c c d)
       (let ([f (make-parameter 'a)]
             [path '()]
             [c #f])
         (let1 add (^[] (set! path (cons (f) path)))
           (add)
           (parameterize ([f 'b])
             (call/cc (^[c0] (set! c c0)))
             (add) (f 'c) (add))
           (f 'd)
           (add)
           (if (< (length path) 5)
             (c 'end)
             (reverse path)))))

;; Another bug reported by Joo ChurlSoo.

(test* "reassignment of global variable holding a parameter"
       '((10 20) (10 200) (1000 2000) (1 2000))
       (let* ([init '()]
              [add-init (^x (set! init (cons x init)))]
              [a (make-parameter 1)]
              [b (make-parameter 2)])
         (parameterize ([a 10] [b 20])
           (add-init (list (a) (b)))
           (set! b (make-parameter 200))
           (add-init (list (a) (b)))
           (a 1000) (b 2000)
           (add-init (list (a) (b))))
         (add-init (list (a) (b)))
         (reverse init)))

;; A bug reported by Joo ChurlSoo.
;; Up to 0.9.4, this returns '("10" "2") - the value of 'a' wasn't restored.
(test* "Error in filter proc and rewinding"
       '("1" "2")
       (let ([a (make-parameter 1 number->string)]
             [b (make-parameter 2 number->string)])
         (guard [e (else (list (a) (b)))]
           (parameterize ([a 10] [b 'bad])
             'notreached))))

;; Note: ext/threads has extra tests for parameter/thread cooperation.

(test-end)
