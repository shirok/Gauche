;; -*- coding: euc-jp -*-
;;  Nqueen
;;  n×n の互いに効き線上にないクイーンの配列をみつける

;;  SHINYAMA Yusuke (euske@cl.cs.titech.ac.jp)
;;  This software is public domain.

(define (decr x) (- x 1))
(define (incr x) (+ x 1))

; rotate: ベクタ x を pos要素 だけ回転させる
(define (rotate x pos)
  (do ((last (decr (vector-length x)))
       (x0 (vector-ref x pos))
       (i pos (incr i)))
      ((= i last) (vector-set! x last x0))
    (vector-set! x i (vector-ref x (incr i)))))

; rotrec: ベクタ x の要素の、あらゆる配置の組み合わせに func を適用する
(define (rotrec func x pos)
  (let ((last (decr (vector-length x))))
    (if (= pos last)
        (func x)
        (do ((i pos (incr i)))
            ((< last i) #f)
          (rotrec func x (incr pos))
          (rotate x pos)))))

; genpat: n×n の盤の初期パターンを作る
(define (genpat n)
  (let ((x (make-vector n)))
    (do ((i 0 (incr i)))
        ((= n i) x)
      (vector-set! x i i))))

; checkqueen: パターン p がすべて互いに効き線上にないクイーンなら #t
(define (checkqueen p)
  (define (loop i diag0 diag1) ; i はカウンタ, diag0, diag1 はリスト
    (or (zero? i)
        (let* ((x (decr i))
               (y (vector-ref p x))
               (d0 (+ x y))
               (d1 (- x y)))
;         (format #t "check: ~a (~a,~a) in ~a,~a\n" p x y diag0 diag1)
          (and (not (or (memv d0 diag0)
                        (memv d1 diag1)))
               (loop (decr i) (cons d0 diag0) (cons d1 diag1))))))
  (loop (vector-length p) '() '()))

; nqueen: メインルーチン
(define (nqueen n)
  (let ((result '()))
    (rotrec (lambda (p) 
              (if (checkqueen p)
                  (set! result
                        (cons (vector->list p) result))))
            (genpat n) 0)
    result))

; sample
;(display (nqueen 8))(newline)(exit)

(define (main args)
  (display (nqueen 8))
  (newline)
  0)
