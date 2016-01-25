;;; -*- coding: utf-8 -*-
;;; This is just a joke.

(define-syntax λ
  (syntax-rules () ((_ args body ...) (lambda args body ...))))

(define-syntax 定義
  (syntax-rules (は)
    ((_ (f . args) body ...)
     (define (f . args) body ...))
    ((_ var val)
     (define var val))
    ((_ var は val)
     (define var val))))

(define-syntax もし
  (syntax-rules (ならば でなければ)
    ((_ test ならば then)
     (if test then))
    ((_ test ならば then でなければ else)
     (if test then else))
    ((_ test でなければ else)
     (unless test else))
    ((_ test then)
     (if test then))
    ((_ test then else)
     (if test then else))))

(define-syntax 代入
  (syntax-rules (へ)
    ((_ var へ val)
     (set! var val))
    ((_ var val)
     (set! var val))))

(define-syntax 局所定義
  (syntax-rules (は)
    ((_ ((var は val) ...) body ...)
     (let ((var val) ...) body ...))
    ((_ ((var val) ...) body ...)
     (let ((var val) ...) body ...))
    ))

(define-syntax 順次局所定義
  (syntax-rules (は)
    ((_ ((var は val) ...) body ...)
     (let* ((var val) ...) body ...))
    ((_ ((var val) ...) body ...)
     (let* ((var val) ...) body ...))
    ))

(define-syntax 再帰局所定義
  (syntax-rules (は)
    ((_ ((var は val) ...) body ...)
     (letrec ((var val) ...) body ...))
    ((_ ((var val) ...) body ...)
     (letrec ((var val) ...) body ...))
    ))


(define ＜ <)
(define ≦ <=)
(define ＝ =)
(define ＞ >)
(define ≧ >=)

(define ＋ +)
(define − -)
(define × *)
(define ÷ /)

(define 文字列→リスト string->list)
(define 逆リスト reverse)

;;-----------------------------------------------
;; examples

(定義 階乗 は
   (λ (n) (もし (≦ n 2) ならば n でなければ (× n (階乗 (− n 1))))))

(定義 回文か? は
   (λ (文字列)
     (順次局所定義 ((文字リスト は (文字列→リスト 文字列))
                   (逆文字リスト は (逆リスト 文字リスト)))
       (equal? 逆文字リスト 文字リスト))))

