;; this test only works when the core system is compiled with euc-jp.

;; $Id: euc-jp.scm,v 1.6 2001-05-03 19:05:39 shirok Exp $

(use gauche.test)

(test-start "EUC-JP")

;;-------------------------------------------------------------------
(test-section "string builtins")

(test "string" "いろhにほへt"
      (lambda () (string #\い #\ろ #\h #\に #\ほ #\へ #\t)))
(test "list->string" "いろhにほへt"
      (lambda () (list->string '(#\い #\ろ #\h #\に #\ほ #\へ #\t))))
(test "make-string" "へへへへへ" (lambda () (make-string 5 #\へ)))
(test "make-string" "" (lambda () (make-string 0 #\へ)))

(test "string->list" '(#\い #\ろ #\h #\に #\ほ #\へ #\t)
      (lambda () (string->list "いろhにほへt")))
(test "string->list" '(#\ろ #\h #\に #\ほ #\へ #\t)
      (lambda () (string->list "いろhにほへt" 1)))
(test "string->list" '(#\ろ #\h #\に)
      (lambda () (string->list "いろhにほへt" 1 4)))

(test "string-copy" '("ぁゃνぃ" #f)
      (lambda () (let* ((x "ぁゃνぃ") (y (string-copy x)))
                   (list y (eq? x y)))))
(test "string-copy" "ゃνぃ" (lambda () (string-copy "ぁゃνぃ" 1)))
(test "string-copy" "ゃν"  (lambda () (string-copy "ぁゃνぃ" 1 3)))

(test "string-ref" #\ろ (lambda () (string-ref "いろは" 1)))
(define x (string-copy "いろはにほ"))
(test "string-set!" "いろZにほ" (lambda () (string-set! x 2 #\Z) x))

(test "string-fill!" "のののののの"
      (lambda () (string-fill! (string-copy "000000") #\の)))
(test "string-fill!" "000ののの"
      (lambda () (string-fill! (string-copy "000000") #\の 3)))
(test "string-fill!" "000のの0"
      (lambda () (string-fill! (string-copy "000000") #\の 3 5)))

(test "string-join" "ふぅ ばぁ ばず"
      (lambda () (string-join '("ふぅ" "ばぁ" "ばず"))))
(test "string-join" "ふぅ！ばぁ！ばず"
      (lambda () (string-join '("ふぅ" "ばぁ" "ばず") "！")))
(test "string-join" "ふぅ→←ばぁ→←ばず"
      (lambda () (string-join '("ふぅ" "ばぁ" "ばず") "→←" 'infix)))
(test "string-join" ""
      (lambda () (string-join '() "→←")))
(test "string-join" "ふぅ！ばぁ！ばず！"
      (lambda () (string-join '("ふぅ" "ばぁ" "ばず") "！" 'suffix)))
(test "string-join" "！ふぅ！ばぁ！ばず"
      (lambda () (string-join '("ふぅ" "ばぁ" "ばず") "！" 'prefix)))
(test "string-join" "ふぅ！ばぁ！ばず"
      (lambda () (string-join '("ふぅ" "ばぁ" "ばず") "！" 'strict-infix)))

(test "string-substitute!" "うえおdefghi"
      (lambda ()
        (let ((s (string-copy "abcdefghi")))
          (string-substitute! s 0 "うえお")
          s)))
(test "string-substitute!" "abcうえおghi"
      (lambda ()
        (let ((s (string-copy "abcdefghi")))
          (string-substitute! s 3 "うえお")
          s)))

;;-------------------------------------------------------------------
(test-section "string-pointer")
(define sp #f)
(test "make-string-pointer" #t
      (lambda ()
        (set! sp (make-string-pointer "いろはにhoへと"))
        (string-pointer? sp)))
(test "string-pointer-next!" #\い
      (lambda () (string-pointer-next! sp)))
(test "string-pointer-next!" #\ろ
      (lambda () (string-pointer-next! sp)))
(test "string-pointer-prev!" #\ろ
      (lambda () (string-pointer-prev! sp)))
(test "string-pointer-prev!" #\い
      (lambda () (string-pointer-prev! sp)))
(test "string-pointer-prev!" #t
      (lambda () (eof-object? (string-pointer-prev! sp))))
(test "string-pointer-index" 0
      (lambda () (string-pointer-index sp)))
(test "string-pointer-index" 8
      (lambda () (do ((x (string-pointer-next! sp) (string-pointer-next! sp)))
                     ((eof-object? x) (string-pointer-index sp)))))
(test "string-pointer-substring" '("いろはにhoへと" "")
      (lambda () (list (string-pointer-substring sp)
                       (string-pointer-substring sp :after #t))))
(test "string-pointer-substring" '("いろはにh" "oへと")
      (lambda ()
        (string-pointer-set! sp 5)
        (list (string-pointer-substring sp)
              (string-pointer-substring sp :after #t))))
(test "string-pointer-substring" '("" "いろはにhoへと")
      (lambda ()
        (string-pointer-set! sp 0)
        (list (string-pointer-substring sp)
              (string-pointer-substring sp :after #t))))

;;-------------------------------------------------------------------
(test-section "string-library")
(use srfi-13)

(test "string-every" #t (lambda () (string-every #\あ "")))
(test "string-every" #t (lambda () (string-every #\あ "ああああ")))
(test "string-every" #f (lambda () (string-every #\あ "あああa")))
(test "string-every" #t (lambda () (string-every #[あ-ん] "ああいあ")))
(test "string-every" #f (lambda () (string-every #[あ-ん] "ああaあ")))
(test "string-every" #t (lambda () (string-every #[あ-ん] "")))
(test "string-every" #t (lambda () (string-every (lambda (x) (char-ci=? x #\あ)) "ああああ")))
(test "string-every" #f (lambda () (string-every (lambda (x) (char-ci=? x #\あ)) "あいあい")))

(test "string-any" #t (lambda () (string-any #\あ "ああああ")))
(test "string-any" #f (lambda () (string-any #\あ "いうえお")))
(test "string-any" #f (lambda () (string-any #\あ "")))
(test "string-any" #t (lambda () (string-any #[あ-ん] "すきーむ")))
(test "string-any" #f (lambda () (string-any #[あ-ん] "スキーム")))
(test "string-any" #f (lambda () (string-any #[あ-ん] "")))
(test "string-any" #t (lambda () (string-any (lambda (x) (char-ci=? x #\あ)) "らららあ")))
(test "string-any" #f (lambda () (string-any (lambda (x) (char-ci=? x #\あ)) "ラララア")))
(test "string-tabulate" "アィイゥウ"
      (lambda ()
        (string-tabulate (lambda (code)
                           (integer->char (+ code
                                             (char->integer #\ア))))
                         5)))
(test "reverse-list->string" "んをわ"
      (lambda () (reverse-list->string '(#\わ #\を #\ん))))
(test "string-copy!" "abうえおfg"
      (lambda () (let ((x (string-copy "abcdefg")))
                   (string-copy! x 2 "あいうえおか" 2 5)
                   x)))
(test "string-take" "あいうえ"  (lambda () (string-take "あいうえおか" 4)))
(test "string-drop" "おか"  (lambda () (string-drop "あいうえおか" 4)))
(test "string-take-right" "うえおか"  (lambda () (string-take-right "あいうえおか" 4)))
(test "string-drop-right" "あい"  (lambda () (string-drop-right "あいうえおか" 4)))
(test "string-pad" "■■パッド" (lambda () (string-pad "パッド" 5 #\■)))
(test "string-pad" "パディング" (lambda () (string-pad "パディング" 5 #\■)))
(test "string-pad" "ディングス" (lambda () (string-pad "パディングス" 5 #\■)))
(test "string-pad-right" "パッド■■" (lambda () (string-pad-right "パッド" 5 #\■)))
(test "string-pad" "パディング" (lambda () (string-pad-right "パディングス" 5 #\■)))

;;-------------------------------------------------------------------
(test-section "char set")

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
