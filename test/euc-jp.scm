;; this test only works when the core system is compiled with euc-jp.

;; $Id: euc-jp.scm,v 1.2 2001-04-26 20:07:42 shirok Exp $

(use gauche.test)

(test-start "EUC-JP")

;; string-pointer
(define sp #f)
(test "make-string-pointer" #t
      (lambda ()
        (set! sp (make-string-pointer "いろはにhoへと"))
        (string-pointer? sp)))
(test "string-pointer-next" #\い
      (lambda () (string-pointer-next sp)))
(test "string-pointer-next" #\ろ
      (lambda () (string-pointer-next sp)))
(test "string-pointer-prev" #\ろ
      (lambda () (string-pointer-prev sp)))
(test "string-pointer-prev" #\い
      (lambda () (string-pointer-prev sp)))
(test "string-pointer-prev" #t
      (lambda () (eof-object? (string-pointer-prev sp))))
(test "string-pointer-index" 0
      (lambda () (string-pointer-index sp)))
(test "string-pointer-index" 8
      (lambda () (do ((x (string-pointer-next sp) (string-pointer-next sp)))
                     ((eof-object? x) (string-pointer-index sp)))))
(test "string-pointer-substring" '("いろはにhoへと" "")
      (lambda () (list (string-pointer-substring sp)
                       (string-pointer-substring sp :after #t))))
(test "string-pointer-substring" '("いろはにh" "oへと")
      (lambda ()
        (string-pointer-set sp 5)
        (list (string-pointer-substring sp)
              (string-pointer-substring sp :after #t))))
(test "string-pointer-substring" '("" "いろはにhoへと")
      (lambda ()
        (string-pointer-set sp 0)
        (list (string-pointer-substring sp)
              (string-pointer-substring sp :after #t))))

;; char-set

(use srfi-14)

(test "char-set" #t
      (lambda () (char-set= (char-set #\あ #\い #\う #\え #\お)
                            (string->char-set "おうえいあ"))))
(test "char-set" #t
      (lambda () (char-set= (list->char-set '(#\あ #\い #\う #\ん))
                            (string->char-set "んんいいいああう"))))
(test "char-set" #t
      (lambda () (char-set<= (list->char-set '(#\ほ #\げ))
                             char-set:full)))
(test "char-set" #t
      (lambda ()
        (char-set= (->char-set "ぁぃぅぇぉあいうえ")
                   (integer-range->char-set (char->integer #\ぁ)
                                            (char->integer #\お)))))


(test-end)
