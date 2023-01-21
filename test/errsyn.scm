;;;
;;;  erroneous syntax test
;;;

;; We test if invalid syntax is correctly detected.  This is run after
;; error.scm and macro.scm.


(use gauche.test)

(test-start "erroneous syntax test")

(define-syntax test-syn
  (syntax-rules ()
    ([_ msg expect form]
     (test* msg expect (eval 'form (current-module))))))

(test-section "Internal defines")

(test-syn "duplicate internal definitions (var-var)"
          (test-error <error> #/Duplicate internal definitions of a/)
          (let ()
            (define a 3)
            (define b 5)
            (define a 4)
            (list a b)))

(test-syn "duplicate internal definitions (syn-syn)"
          (test-error <error> #/Duplicate internal definitions of p/)
          (let ()
            (define-syntax p (syntax-rules () [(_) 1]))
            (define-syntax q (syntax-rules () [(_) 2]))
            (define-syntax p (syntax-rules () [(_) 3]))
            (list (p) (q))))

(test-syn "duplicate internal definitions (var-syn)"
          (test-error <error> #/Duplicate internal definitions of v/)
          (let ()
            (define v 3)
            (define-syntax w (syntax-rules () [(_) 2]))
            (define-syntax v (syntax-rules () [(_) 3]))
            (list (v) (w))))

(test-end)
