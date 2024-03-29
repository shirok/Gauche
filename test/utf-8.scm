;; -*- coding: utf-8 -*-

(use gauche.test)

(test-start "multibyte (utf-8)")
(use scheme.list)

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

(let ()
  (define (test-string-scan out s1 s2 . opt)
    (if (pair? out)
      (begin
        (test* "string-scan" (car out) (apply string-scan s1 s2 opt))
        (test* "string-scan-right" (cadr out)
               (apply string-scan-right s1 s2 opt)))
      (apply test-string-scan (list out out) s1 s2 opt)))
  (define (test-string-scan2 out1 out2 s1 s2 . opt)
    (if (pair? out1)
      (begin
        (test* "string-scan" (list (car out1) (car out2))
               (receive r (apply string-scan s1 s2 opt) r))
        (test* "string-scan-right" (list (cadr out1) (cadr out2))
               (receive r (apply string-scan-right s1 s2 opt) r)))
      (apply test-string-scan2 (list out1 out1) (list out2 out2) s1 s2 opt)))

  (test-string-scan '(7 20)
                    "あえいうえおあおあいうえおあえいうえおあおあいうえお" "おあい")
  (test-string-scan '("あえいうえおあ" "あえいうえおあおあいうえおあえいうえおあ")
                      "あえいうえおあおあいうえおあえいうえおあおあいうえお" "おあい" 'before)
  (test-string-scan '("うえおあえいうえおあおあいうえお" "うえお")
                    "あえいうえおあおあいうえおあえいうえおあおあいうえお" "おあい" 'after)
  (test-string-scan2 '("あえいうえおあ" "あえいうえおあおあいうえおあえいうえおあ")
                     '("おあいうえおあえいうえおあおあいうえお" "おあいうえお")
                     "あえいうえおあおあいうえおあえいうえおあおあいうえお" "おあい" 'before*)
  (test-string-scan2 '("あえいうえおあおあい" "あえいうえおあおあいうえおあえいうえおあおあい")
                     '("うえおあえいうえおあおあいうえお" "うえお")
                     "あえいうえおあおあいうえおあえいうえおあおあいうえお" "おあい" 'after*)
  (test-string-scan2 '("あえいうえおあ" "あえいうえおあおあいうえおあえいうえおあ")
                     '("うえおあえいうえおあおあいうえお" "うえお")
                     "あえいうえおあおあいうえおあえいうえおあおあいうえお" "おあい" 'both)
  (test-string-scan #f "あえいうえおあおあいうえお" "おい")
  )

;;-------------------------------------------------------------------
(test-section "string-cursor")

(define str "いろはにhoへと")
(define ss #f)
(define se #f)
(test* "string-cursor-start" #t
       (begin
         (set! ss (string-cursor-start str))
         (string-cursor? ss)))
(test* "string-cursor-start" #t
       (begin
         (set! se (string-cursor-end str))
         (string-cursor? se)))
(test* "string-cursor-next" #\ろ
       (string-ref str (string-cursor-next str ss)))
(test* "string-cursor-forward" #\に
       (string-ref str (string-cursor-forward str ss 3)))
(test* "string-cursor-prev" #\と
       (string-ref str (string-cursor-prev str se)))
(test* "string-cursor-back" #\に
       (string-ref str (string-cursor-back str se 5)))
(test* "string-index->cursor" #\へ
       (string-ref str (string-index->cursor str 6)))
(test* "string-cursor->index" 0
       (string-cursor->index str ss))
(test* "string-cursor->index" 8
       (string-cursor->index str se))
(test* "substring with cursors" "はにho"
       (substring str
                  (string-cursor-forward str ss 2)
                  (string-cursor-back str se 2)))
(test* "substring with cursors" "はにho"
       (substring str
                  2
                  (string-cursor-back str se 2)))
(test* "substring with cursors" "はにh"
       (substring str
                  (string-cursor-forward str ss 2)
                  5))
(test* "string->list" '(#\ろ #\は #\に #\h #\o #\へ)
       (string->list str
                     (string-cursor-next str ss)
                     (string-cursor-prev str se)))
(test* "string->list" '(#\ろ #\は #\に #\h #\o #\へ)
       (string->list str
                     1
                     (string-cursor-prev str se)))
(test* "string->list" '(#\ろ #\は #\に #\h #\o #\へ)
       (string->list str
                     (string-cursor-next str ss)
                     7))
(test* "string-cursor=?" #t
       (string-cursor=? ss (string-cursor-back str se 8)))
(test* "string-cursor=?" #t
       (string-cursor=? 0 0))
(test* "string-cursor<?" #t
       (string-cursor<? ss se))
(test* "string-cursor<=?" #t
       (string-cursor<=? ss se))
(test* "string-cursor<=?" #t
       (string-cursor<=? ss ss))
(test* "string-cursor>?" #t
       (string-cursor>? se ss))
(test* "string-cursor>=?" #t
       (string-cursor>=? se se))
(test* "string-cursor-diff" 8
       (string-cursor-diff str ss se))
(test* "string-cursor-diff" -8
       (string-cursor-diff str se ss))

;;-------------------------------------------------------------------
(test-section "string index building")

(let* ([make-cycle (^[start-char count]
                     ($ circular-list
                        $* map ucs->char (iota count (char->ucs start-char))))]
       [cycle1 (make-cycle #\a 26)]
       [cycle2 (make-cycle #\α 25)]
       [cycle3 (make-cycle #\あ 85)])
  (define (make-str max-size)
    (let loop ([size 0] [r '()]
               [srcs (circular-list cycle1 cycle2 cycle3)]
               [counts (circular-list 1 2 3)])
      (let ([next-char (caar srcs)]
            [next-count (car counts)])
        (if (> (+ size next-count) max-size)
          (list->string (reverse r))
          (loop (+ size next-count)
                (cons next-char r)
                (circular-list (cadr srcs) (caddr srcs) (cdar srcs))
                (cdr counts))))))
  (define (test-string-index max-size)
    (let1 s (string-build-index! (make-str max-size))
      ;;((with-module gauche.internal %string-index-dump) s)
      (test* #"string indexing (size=~max-size)"
             #f
             (any (^p (and (not (eqv? (car p) (string-ref s (cdr p))))
                           (list 'pos (cdr p)
                                 'expect (car p)
                                 'actual (string-ref s (cdr p)))))
                  (map cons (string->list s) (liota))))))
  (test-string-index 0)
  (test-string-index 15)
  (test-string-index 33)
  (test-string-index 34)
  (test-string-index 49)
  (test-string-index 63)
  (test-string-index 64)
  (test-string-index 65)
  (test-string-index 255)
  (test-string-index 256)
  (test-string-index 257)
  (test-string-index 8191)
  (test-string-index 8193)
  (test-string-index 65535)
  (test-string-index 65537)
  (test-string-index 131072)
  (test-string-index 131073)
  )

;;-------------------------------------------------------------------
(test-section "incomplete strings")

(test "string-length" 9 (lambda () (string-length #*"あいう")))
(test "string-complete->incomplete" #*"あいう"
      (lambda () (string-complete->incomplete "あいう")))
(test "string-complete->incomplete" #*"あいう"
      (lambda () (string-complete->incomplete #*"あいう")))
(test "string-incomplete->complete" "あいう"
      (lambda () (string-incomplete->complete #*"あいう")))
(test "string-incomplete->complete" "あいう"
      (lambda () (string-incomplete->complete "あいう")))

(test "string-incomplete->complete (reject)" #f
      (lambda () (string-incomplete->complete #*"あい\x80う" #f)))
(test "string-incomplete->complete (omit)" "あいう"
      (lambda () (string-incomplete->complete #*"あい\x80う" :omit)))
(test "string-incomplete->complete (omit)" "あいう"
      (lambda () (string-incomplete->complete #*"\x80あいう" :omit)))
(test "string-incomplete->complete (omit)" "あいう"
      (lambda () (string-incomplete->complete #*"\x80\xe3あいう" :omit)))
(test "string-incomplete->complete (omit)" "あいう"
      (lambda () (string-incomplete->complete #*"あいう\xe3" :omit)))
(test "string-incomplete->complete (omit)" "あいう"
      (lambda () (string-incomplete->complete #*"あいう\xe3\xe3" :omit)))
(test "string-incomplete->complete (omit)" "あいう"
      (lambda () (string-incomplete->complete #*"あいう\xe3\x80" :omit)))
(test "string-incomplete->complete (replace)" "あい?う"
      (lambda () (string-incomplete->complete #*"あい\x80う" :replace)))
(test "string-incomplete->complete (replace)" "ふあいう"
      (lambda () (string-incomplete->complete #*"\x80あいう" :replace #\ふ)))
(test "string-incomplete->complete (replace)" "ふふあいう"
      (lambda () (string-incomplete->complete #*"\x80\xe3あいう" :replace #\ふ)))
(test "string-incomplete->complete (replace)" "あいうふ"
      (lambda () (string-incomplete->complete #*"あいう\xe3" :replace #\ふ)))
(test "string-incomplete->complete (replace)" "あいうふふ"
      (lambda () (string-incomplete->complete #*"あいう\xe3\xe3" :replace #\ふ)))
(test "string-incomplete->complete (replace)" "あいうふふ"
      (lambda () (string-incomplete->complete #*"あいう\xe3\x80" :replace #\ふ)))
(test "string-incomplete->complete (escape)" "あ??い?91う?80え"
      (lambda () (string-incomplete->complete #*"あ?い\x91う\x80え" :escape)))
(test "string-incomplete->complete (escape)" "あ__い_91う_80え"
      (lambda () (string-incomplete->complete #*"あ_い\x91う\x80え" :escape #\_)))

(test "string=?" #t (lambda () (string=? #*"あいう" #*"あいう")))

(test "string-byte-ref" #x81 (lambda () (string-byte-ref #*"あいう" 1)))

(test "string-append" #*"あいうえお"
      (lambda () (string-append "あいう" #*"えお")))
(test "string-append" #*"あいうえお"
      (lambda () (string-append #*"あいう" "えお")))
(test "string-append" #*"あいうえお"
      (lambda () (string-append #*"あいう" #*"えお")))
(test "string-append" 15
      (lambda () (string-length (string-append "あいう" "えお" #*""))))

(test "string-incomplete->complete" "あ"
      (lambda () (string-incomplete->complete
                  (string-append #*"\xe3" #*"\x81" #*"\x82"))))

;;-------------------------------------------------------------------
(test-section "format")

(test "format" "あぶら"
      (lambda () (format #f "~,,,,3a" "あぶらかだぶら")))
(test "format" "abら"
      (lambda () (format #f "~,,,,3a" "abらかだぶら")))
(test "format" "あぶらかだぶら"
      (lambda () (format #f "~,,,,7:a" "あぶらかだぶら")))
(test "format" "あぶらか"
      (lambda () (format #f "~,,,,7:a" "あぶらか")))
(test "format" "あぶら ..."
      (lambda () (format #f "~,,,,7:a" "あぶらかだぶらぶらぶら")))

;;-------------------------------------------------------------------
(test-section "string-library")
(use srfi.13)

(test "string-every" #t (lambda () (string-every #\あ "")))
(test "string-every" #t (lambda () (string-every #\あ "ああああ")))
(test "string-every" #f (lambda () (string-every #\あ "あああa")))
(test "string-every" #t (lambda () (string-every #[あ-ん] "ああいあ")))
(test "string-every" #f (lambda () (string-every #[あ-ん] "ああaあ")))
(test "string-every" #t (lambda () (string-every #[あ-ん] "")))
(test "string-every" #t (lambda () (string-every (^x (char-ci=? x #\あ)) "ああああ")))
(test "string-every" #f (lambda () (string-every (^x (char-ci=? x #\あ)) "あいあい")))

(test "string-any" #t (lambda () (string-any #\あ "ああああ")))
(test "string-any" #f (lambda () (string-any #\あ "いうえお")))
(test "string-any" #f (lambda () (string-any #\あ "")))
(test "string-any" #t (lambda () (string-any #[あ-ん] "すきーむ")))
(test "string-any" #f (lambda () (string-any #[あ-ん] "スキーム")))
(test "string-any" #f (lambda () (string-any #[あ-ん] "")))
(test "string-any" #t (lambda () (string-any (^x (char-ci=? x #\あ)) "らららあ")))
(test "string-any" #f (lambda () (string-any (^x (char-ci=? x #\あ)) "ラララア")))
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

(use scheme.charset)

(let ((data '(#[ぁ-ん他-唖ァ-ン]
              #[aあ唖ア])))
  (define (t x)
    (test* "char-set mutable/immutable roundtrip" #t
           (char-set= (char-set-copy x)
                      (char-set-copy (char-set-freeze! (char-set-copy x))))))
  (for-each t data))

(test-char-set-1 "char-set-contains?" #t (cut char-set-contains? <> #\あ)
                 (char-set #\あ #\い #\う #\え #\お))
(test-char-set-1 "char-set-contains?" #f (cut char-set-contains? <> #\ぁ)
                 (char-set #\あ #\い #\う #\え #\お))
(test-char-set-1 "char-set-contains?" #t (cut char-set-contains? <> #\ほ)
                 (char-set-union
                  (integer-range->char-set (char->integer #\ぁ)
                                           (char->integer #\ん))
                  (integer-range->char-set (char->integer #\ァ)
                                           (char->integer #\ン))))
(test-char-set-1 "char-set-contains?" #t (cut char-set-contains? <> #\ケ)
                 (char-set-union
                  (integer-range->char-set (char->integer #\ぁ)
                                           (char->integer #\ん))
                  (integer-range->char-set (char->integer #\ァ)
                                           (char->integer #\ン))))
(test-char-set-1 "char-set-contains?" #f (cut char-set-contains? <> #\ん)
                 (char-set-union
                  (integer-range->char-set (char->integer #\ぁ)
                                           (char->integer #\ん))
                  (integer-range->char-set (char->integer #\ァ)
                                           (char->integer #\ン))))
(test-char-set-1 "char-set-contains?" #f (cut char-set-contains? <> #\ヴ)
                 (char-set-union
                  (integer-range->char-set (char->integer #\ぁ)
                                           (char->integer #\ん))
                  (integer-range->char-set (char->integer #\ァ)
                                           (char->integer #\ン))))

(test-char-set-2 "char-set=" #t char-set=
                 (char-set #\あ #\い #\う #\え #\お)
                 (string->char-set "おうえいあ"))
(test-char-set-2 "char-set=" #t char-set=
                 (list->char-set '(#\あ #\い #\う #\ん))
                 (string->char-set "んんいいいああう"))
(test-char-set-2 "char-set<=" #t char-set<=
                 (list->char-set '(#\ほ #\げ))
                 char-set:full)
(test-char-set-2 "char-set<=" #t char-set<= #[あい] #[あい])
(test-char-set-2 "char-set<=" #t char-set<= #[あい] #[あ-い])
(test-char-set-2 "char-set<=" #f char-set<= #[あ-い] #[あい])
(test-char-set-2 "char-set<=" #t char-set<= #[あ-いう-え] #[あ-え])
(test-char-set-2 "char-set<=" #f char-set<= #[あ-え] #[あ-いう-え])
(test-char-set-2 "char-set<=" #f char-set<= #[あ-いう-ぉ] #[あ-え])
(test-char-set-2 "char-set<=" #f char-set<= #[ぁ-いう-え] #[あ-え])
(test-char-set-2 "char-set<=" #t char-set<= #[あ-いか-き] #[あ-うお-け])
(test-char-set-2 "char-set<=" #t char-set<= #[あ-いか-き] #[あ-うお-き])
(test-char-set-2 "char-set<=" #t char-set<= #[あ-いか-き] #[あ-うか-こ])
(test-char-set-2 "char-set<=" #t char-set<= #[か-き] #[あ-うか-こ])
(test-char-set-2 "char-set<=" #t char-set<= #[か-きく-け] #[あ-うか-こ])
(test-char-set-2 "char-set<=" #f char-set<= #[う-く] #[あ-えか-こ])
(test-char-set-2 "->char-set" #t char-set=
                 (->char-set "ぁぃぅぇぉあいうえ")
                 (integer-range->char-set (char->integer #\ぁ)
                                          (char->integer #\お)))

;;-------------------------------------------------------------------
(test-section "ports")

;; イ     ロ     ハ     ニ      ホ     ヘ     ト
;; e382a4.e383ad.e3838f.e3838b.e3839b.e38398.e38388
(define istr (open-input-string "イロハニホヘト"))
(test* "read-char" #\イ (read-char istr))
(test* "read-byte" #xe3 (read-byte istr))
(test* "read-byte (using scratch)" #xad
       (begin (read-byte istr) (read-byte istr)))
(test* "read-char (using scratch)" #\ハ
       (begin (peek-byte istr) (read-char istr)))
(test* "read-block (using scratch)" #*"ニ"
       (begin (peek-char istr) (read-block 3 istr)))
(test* "read-block (using scratch)" #*"\xe3"
       (begin (peek-char istr) (read-block 1 istr)))
(test* "read-block (using scratch)" #*"\x83\x9bヘト"
       (begin (read-block 10 istr)))

;; start over
(set! istr (open-input-string "イロハニホヘト"))
(test* "peek-byte" #xe3 (peek-byte istr))
(test* "peek-char" #\イ (peek-char istr))
(test* "read-byte" #xe3 (read-byte istr))
(test* "peek-byte" #x82 (peek-byte istr))
(test* "peek-char" #\ロ
       (begin (read-byte istr) (read-byte istr) (peek-char istr)))
(test* "read-byte" #\ロ (begin (peek-byte istr) (read-char istr)))
(test* "peek-byte" #x83
       (begin (peek-char istr) (read-byte istr) (peek-byte istr)))
(test* "read-block" #*"\x83\x8fニホヘ\xe3\x83" (read-block 13 istr))
(test* "peek-byte" #x88 (peek-byte istr))
(test* "peek-byte" #t (begin (read-byte istr) (eof-object? (peek-byte istr))))

(test* "read-line (LF)" "なむ"
       (read-line (open-input-string "なむ\n")))
(test* "read-line (CR)" "なむ"
       (read-line (open-input-string "なむ\r")))
(test* "read-line (CRLF)" "なむ"
       (read-line (open-input-string "なむ\r\n")))
(test* "read-line (using ungotten)" "なむ"
       (let1 s (open-input-string "なむ\n")
         (peek-char s) (read-line s)))
(test* "read-line (using ungotten)" "なむ"
       (let1 s (open-input-string "なむ\n")
         (peek-byte s) (read-line s)))

;;-------------------------------------------------------------------
(test-section "buffered ports")

(define (make-filler)
  (let* ((str #*"あいうえおかきくけこ")  ;incomplete string
         (len (string-size str))
         (ind 0))
    (lambda (siz)
      (cond ((>= ind len) #f)
            ((>= (+ ind siz) len)
             (let ((r (substring str ind len)))
               (set! ind len)
               r))
            (else
             (let ((r (substring str ind (+ ind siz))))
               (set! ind (+ ind siz))
               r))))))

(define (port->char-list p)
  (let loop ((c (read-char p)) (r '()))
    (if (eof-object? c) (reverse r) (loop (read-char p) (cons c r)))))

(define (port->byte-list p)
  (let loop ((b (read-byte p)) (r '()))
    (if (eof-object? b) (reverse r) (loop (read-byte p) (cons b r)))))

(define (port->chunk-list p siz)
  (let loop ((b (read-block siz p)) (r '()))
    (if (eof-object? b) (reverse r) (loop (read-block siz p) (cons b r)))))

(test "buffered port (getc, bufsiz=256)"
      '(#\あ #\い #\う #\え #\お #\か #\き #\く #\け #\こ)
      (lambda ()
        (port->char-list (open-input-buffered-port (make-filler) 256))))

(test "buffered port (getc, bufsiz=7)"
      '(#\あ #\い #\う #\え #\お #\か #\き #\く #\け #\こ)
      (lambda ()
        (port->char-list (open-input-buffered-port (make-filler) 7))))

(test "buffered port (getc, bufsiz=3)"
      '(#\あ #\い #\う #\え #\お #\か #\き #\く #\け #\こ)
      (lambda ()
        (port->char-list (open-input-buffered-port (make-filler) 3))))

(test "buffered port (getc, bufsiz=2)"
      '(#\あ #\い #\う #\え #\お #\か #\き #\く #\け #\こ)
      (lambda ()
        (port->char-list (open-input-buffered-port (make-filler) 2))))

(test "buffered port (getc, bufsiz=1)"
      '(#\あ #\い #\う #\え #\お #\か #\き #\く #\け #\こ)
      (lambda ()
        (port->char-list (open-input-buffered-port (make-filler) 1))))

(test "buffered port (getb, bufsiz=256)"
      '(#xe3 #x81 #x82 #xe3 #x81 #x84 #xe3 #x81 #x86 #xe3 #x81 #x88
        #xe3 #x81 #x8a #xe3 #x81 #x8b #xe3 #x81 #x8d #xe3 #x81 #x8f
        #xe3 #x81 #x91 #xe3 #x81 #x93)
      (lambda ()
        (port->byte-list (open-input-buffered-port (make-filler) 256))))

(test "buffered port (getb, bufsiz=20)"
      '(#xe3 #x81 #x82 #xe3 #x81 #x84 #xe3 #x81 #x86 #xe3 #x81 #x88
        #xe3 #x81 #x8a #xe3 #x81 #x8b #xe3 #x81 #x8d #xe3 #x81 #x8f
        #xe3 #x81 #x91 #xe3 #x81 #x93)
      (lambda ()
        (port->byte-list (open-input-buffered-port (make-filler) 20))))

(test "buffered port (getb, bufsiz=19)"
      '(#xe3 #x81 #x82 #xe3 #x81 #x84 #xe3 #x81 #x86 #xe3 #x81 #x88
        #xe3 #x81 #x8a #xe3 #x81 #x8b #xe3 #x81 #x8d #xe3 #x81 #x8f
        #xe3 #x81 #x91 #xe3 #x81 #x93)
      (lambda ()
        (port->byte-list (open-input-buffered-port (make-filler) 19))))

(test "buffered port (getb, bufsiz=2)"
      '(#xe3 #x81 #x82 #xe3 #x81 #x84 #xe3 #x81 #x86 #xe3 #x81 #x88
        #xe3 #x81 #x8a #xe3 #x81 #x8b #xe3 #x81 #x8d #xe3 #x81 #x8f
        #xe3 #x81 #x91 #xe3 #x81 #x93)
      (lambda ()
        (port->byte-list (open-input-buffered-port (make-filler) 2))))

(test "buffered port (getb, bufsiz=1)"
      '(#xe3 #x81 #x82 #xe3 #x81 #x84 #xe3 #x81 #x86 #xe3 #x81 #x88
        #xe3 #x81 #x8a #xe3 #x81 #x8b #xe3 #x81 #x8d #xe3 #x81 #x8f
        #xe3 #x81 #x91 #xe3 #x81 #x93)
      (lambda ()
        (port->byte-list (open-input-buffered-port (make-filler) 1))))

(test "buffered port (getz, siz=20,5)"
      '(#*"\xe3\x81\x82\xe3\x81" #*"\x84\xe3\x81\x86\xe3"
        #*"\x81\x88\xe3\x81\x8a" #*"\xe3\x81\x8b\xe3\x81"
        #*"\x8d\xe3\x81\x8f\xe3" #*"\x81\x91\xe3\x81\x93")
      (lambda ()
        (port->chunk-list (open-input-buffered-port (make-filler) 20) 5)))

(test "buffered port (getz, siz=20,20)"
      '(#*"\xe3\x81\x82\xe3\x81\x84\xe3\x81\x86\xe3\x81\x88\xe3\x81\x8a\xe3\x81\x8b\xe3\x81"
        #*"\x8d\xe3\x81\x8f\xe3\x81\x91\xe3\x81\x93")
      (lambda ()
        (port->chunk-list (open-input-buffered-port (make-filler) 20) 20)))

(test "buffered port (getz, siz=9,20)"
      '(#*"\xe3\x81\x82\xe3\x81\x84\xe3\x81\x86\xe3\x81\x88\xe3\x81\x8a\xe3\x81\x8b\xe3\x81"
        #*"\x8d\xe3\x81\x8f\xe3\x81\x91\xe3\x81\x93")
      (lambda ()
        (port->chunk-list (open-input-buffered-port (make-filler) 9) 20)))

(test "buffered port (getz, siz=9,7)"
      '(#*"\xe3\x81\x82\xe3\x81\x84\xe3"
        #*"\x81\x86\xe3\x81\x88\xe3\x81"
        #*"\x8a\xe3\x81\x8b\xe3\x81\x8d"
        #*"\xe3\x81\x8f\xe3\x81\x91\xe3"
        #*"\x81\x93")
      (lambda ()
        (port->chunk-list (open-input-buffered-port (make-filler) 9) 7)))

(test "buffered port (getz, siz=3,50)"
      '(#*"\xe3\x81\x82\xe3\x81\x84\xe3\x81\x86\xe3\x81\x88\xe3\x81\x8a\xe3\x81\x8b\xe3\x81\x8d\xe3\x81\x8f\xe3\x81\x91\xe3\x81\x93")
      (lambda ()
        (port->chunk-list (open-input-buffered-port (make-filler) 3) 50)))

(test "buffered port (getz, siz=2,7)"
      '(#*"\xe3\x81\x82\xe3\x81\x84\xe3"
        #*"\x81\x86\xe3\x81\x88\xe3\x81"
        #*"\x8a\xe3\x81\x8b\xe3\x81\x8d"
        #*"\xe3\x81\x8f\xe3\x81\x91\xe3"
        #*"\x81\x93")
      (lambda ()
        (port->chunk-list (open-input-buffered-port (make-filler) 2) 7)))

(test "buffered port (getz, siz=1,7)"
      '(#*"\xe3\x81\x82\xe3\x81\x84\xe3"
        #*"\x81\x86\xe3\x81\x88\xe3\x81"
        #*"\x8a\xe3\x81\x8b\xe3\x81\x8d"
        #*"\xe3\x81\x8f\xe3\x81\x91\xe3"
        #*"\x81\x93")
      (lambda ()
        (port->chunk-list (open-input-buffered-port (make-filler) 1) 7)))

(define *flusher-out* '())

(define (flusher str)
  (if str
      (set! *flusher-out* (cons str *flusher-out*))
      (set! *flusher-out* (string-concatenate-reverse *flusher-out*))))

(define (byte-list->port p bytes)
  (set! *flusher-out* '())
  (for-each (^b (write-byte b p)) bytes)
  (close-output-port p)
  *flusher-out*)

(define (char-list->port p chars)
  (set! *flusher-out* '())
  (for-each (^c (write-char c p)) chars)
  (close-output-port p)
  *flusher-out*)

(define (string-list->port p strs)
  (set! *flusher-out* '())
  (for-each (^s (display s p)) strs)
  (close-output-port p)
  *flusher-out*)

(test "buffered port (putb, bufsiz=7)"
      #*"@ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      (lambda ()
        (byte-list->port (open-output-buffered-port flusher 7)
                         (iota 27 #x40))))

(test "buffered port (putb, bufsiz=30)"
      #*"@ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      (lambda ()
        (byte-list->port (open-output-buffered-port flusher 30)
                         (iota 27 #x40))))

(test "buffered port (putc, bufsiz=7)"
      #*"あいうえおかきくけこさしすせそ"
      (lambda ()
        (char-list->port (open-output-buffered-port flusher 7)
                         '(#\あ #\い #\う #\え #\お #\か #\き #\く #\け #\こ
                           #\さ #\し #\す #\せ #\そ))))

(test "buffered port (putc, bufsiz=30)"
      #*"あいうえおかきくけこさしすせそ"
      (lambda ()
        (char-list->port (open-output-buffered-port flusher 30)
                         '(#\あ #\い #\う #\え #\お #\か #\き #\く #\け #\こ
                           #\さ #\し #\す #\せ #\そ))))

(test "buffered port (puts, bufsiz=6)"
      #*"あいうえおかきくけこさしすせそ"
      (lambda ()
        (string-list->port (open-output-buffered-port flusher 6)
                           '("あいう" "えおか" "きくけ" "こさし" "すせそ"))))

(test "buffered port (puts, bufsiz=7)"
      #*"あいうえおかきくけこさしすせそ"
      (lambda ()
        (string-list->port (open-output-buffered-port flusher 7)
                           '("あいう" "えおか" "きくけ" "こさし" "すせそ"))))

(test "buffered port (puts, bufsiz=7)"
      #*"あいうえおかきくけこさしすせそ"
      (lambda ()
        (string-list->port (open-output-buffered-port flusher 7)
                           '("あいうえお" "かきくけこ" "さしすせ" "そ"))))

(test "buffered port (puts, bufsiz=3)"
      #*"あいうえおかきくけこさしすせそ"
      (lambda ()
        (string-list->port (open-output-buffered-port flusher 3)
                           '("あいうえお" "かきくけこ" "さしすせ" "そ"))))

;;-------------------------------------------------------------------
(test-section "regexp")

(test "regexp" "いaろbはc"
      (lambda ()
        (cond ((rxmatch #/([ぁ-ん][a-z])+/ "xyいaろbはcdに")
               => rxmatch-substring)
              (else #f))))
(test "regexp" "いaろBはC"
      (lambda ()
        (cond ((rxmatch #/([ぁ-ん][a-z])+/i "XYいaろBはCdに")
               => rxmatch-substring)
              (else #f))))

(test* "regexp" #f
       (cond ((rxmatch #/(.*)a/ "あいう")
              => rxmatch-substring)
             (else #f)))

(test* "regexp" "あいa"
       (cond ((rxmatch #/(.*)a/ "あいaう")
              => rxmatch-substring)
             (else #f)))

(test* "regexp" #f
       (cond ((rxmatch #/([^a]*)a/ "あいう")
              => rxmatch-substring)
             (else #f)))

(test* "regexp" "あいう"
       (cond ((rxmatch #/([^a]*)う/ "あいう")
              => rxmatch-substring)
             (else #f)))

(test* "regexp" "あいa"
       (cond ((rxmatch #/([^a]+)a/ "aあいaう")
              => rxmatch-substring)
             (else #f)))

(test* "regexp" #f
       (cond ((rxmatch #/([^a]+)う/ "aあいaう")
              => rxmatch-substring)
             (else #f)))

(test* "regexp" "あい"
       (cond ((rxmatch #/([^a]+)い/ "aあいaう")
              => rxmatch-substring)
             (else #f)))

(test* "regexp" "う"
       (cond ((rxmatch #/(?<=[あい]+)う/ "あいう")
              => rxmatch-substring)
             (else #f)))

(test* "regexp" #f
       (cond ((rxmatch #/(?<=[あい]+)う/ "aあいaう")
              => rxmatch-substring)
             (else #f)))

(test* "regexp" "う"
       (cond ((rxmatch #/(?<=[^あいう]+)う/ "えおう")
              => rxmatch-substring)
             (else #f)))

(test* "regexp" "う"
       (cond ((rxmatch #/(?<=.あ.)う/ "aあaう")
              => rxmatch-substring)
             (else #f)))

(test* "regexp" #f
       (cond ((rxmatch #/(?<=.あ.)う/ "あaaう")
              => rxmatch-substring)
             (else #f)))

;; The following tests are tailored to cover all paths
;; introduced at regexp.c,v 1.63 to minimize counting
;; string length.

(test* "regexp (full coverage - start)" '("あa" "あaいiうu")
       (list
        ;; count start directly
        (rxmatch-before (rxmatch #/いiう/ "あaいiうuえeおo"))
        ;; count the rest
        (rxmatch-before (rxmatch #/えeお/ "あaいiうuえeおo"))))

(test* "regexp (full coverage - length)" '("いiう" "aいiうuえeお")
       (list
        ;; count length directly
        (rxmatch-substring (rxmatch #/いiう/ "あaいiうuえeおo"))
        ;; count the rest
        (rxmatch-substring (rxmatch #/aいiうuえeお/ "あaいiうuえeおo"))))

(test* "regexp (full coverage - after)" '("おo" "iうuえeおo")
       (list
        ;; count after directly
        (rxmatch-after (rxmatch #/iうuえe/ "あaいiうuえeおo"))
        ;; count the rest
        (rxmatch-after (rxmatch #/あaい/ "あaいiうuえeおo"))))

(test* "regexp (full coverage - memoized length/after)" '("えeお" "o")
       (let1 m (rxmatch #/えeお/ "あaいiうuえeおo")
         (rxmatch-start m) ;; memoizes length and after
         (list (rxmatch-substring m) (rxmatch-after m))))

(test* "regexp (full coverage - memoized start/after)" '("あ" "おo")
       (let1 m (rxmatch #/aいiうuえe/ "あaいiうuえeおo")
         (rxmatch-substring m) ;; memoizes start and after
         (list (rxmatch-before m) (rxmatch-after m))))

(test* "regexp (full coverage - memoized start/length)" '("あa" "いi")
       (let1 m (rxmatch #/いi/ "あaいiうuえeおo")
         (rxmatch-after m) ;; memoizes start and length
         (list (rxmatch-before m) (rxmatch-substring m))))

;;-------------------------------------------------------------------
(test* "regexp/unicode-ci (aa)" "λ"
       (cond ((rxmatch #/λ/i "λ") => rxmatch-substring)
             (else #f)))
(test* "regexp/unicode-ci (aA)" "Λ"
       (cond ((rxmatch #/λ/i "Λ") => rxmatch-substring)
             (else #f)))
(test* "regexp/unicode-ci (Aa)" "λ"
       (cond ((rxmatch #/Λ/i "λ") => rxmatch-substring)
             (else #f)))
(test* "regexp/unicode-ci (AA)" "Λ"
       (cond ((rxmatch #/Λ/i "Λ") => rxmatch-substring)
             (else #f)))

(test* "regexp/unicode-ci (uncase + backref, aa)" "λλ"
       (cond ((rxmatch #/(λ)(?i:\1)/ "λλ") => rxmatch-substring)
             (else #f)))
(test* "regexp/unicode-ci (uncase + backref, aA)" "λΛ"
       (cond ((rxmatch #/(λ)(?i:\1)/ "λΛ") => rxmatch-substring)
             (else #f)))
(test* "regexp/unicode-ci (uncase + backref, Aa)" "Λλ"
       (cond ((rxmatch #/(Λ)(?i:\1)/ "Λλ") => rxmatch-substring)
             (else #f)))
(test* "regexp/unicode-ci (uncase + backref, AA)" "ΛΛ"
       (cond ((rxmatch #/(Λ)(?i:\1)/ "ΛΛ") => rxmatch-substring)
             (else #f)))

(test* "regexp/unicode-ci (charset, aa)" "λ"
       (cond ((rxmatch #/[λ]/i "λ") => rxmatch-substring)
             (else #f)))
(test* "regexp/unicode-ci (charset, aA)" "Λ"
       (cond ((rxmatch #/[λ]/i "Λ") => rxmatch-substring)
             (else #f)))
(test* "regexp/unicode-ci (charset, Aa)" "λ"
       (cond ((rxmatch #/[Λ]/i "λ") => rxmatch-substring)
             (else #f)))
(test* "regexp/unicode-ci (charset, AA)" "Λ"
       (cond ((rxmatch #/[Λ]/i "Λ") => rxmatch-substring)
             (else #f)))

(test* "regexp/unicode-ci" "ΒΓ"
       (cond ((rxmatch #/βγ/i "ΑΒΓΔ") => rxmatch-substring)
             (else #f)))
(test* "regexp/unicode-ci" "βΓ"
       (cond ((rxmatch #/Βγ/i "ΑβΓΔ") => rxmatch-substring)
             (else #f)))
(test* "regexp/unicode-ci" "βγ"
       (cond ((rxmatch #/ΒΓ/i "ΑβγΔ") => rxmatch-substring)
             (else #f)))
(test* "regexp/unicode-ci" #f
       (cond ((rxmatch #/Βγ/ "ΑβΓΔ") => rxmatch-substring)
             (else #f)))
(test* "regexp/unicode-ci" #f
       (cond ((rxmatch #/ΒΓ/ "ΑΒγΔ") => rxmatch-substring)
             (else #f)))

(test* "regexp/unicode-ci" "ОНА"
       (cond ((rxmatch #/о[а-я]а/i "ОНА") => rxmatch-substring)
             (else #f)))
(test* "regexp/unicode-ci" "она"
       (cond ((rxmatch #/О[А-Я]А/i "она") => rxmatch-substring)
             (else #f)))
(test* "regexp/unicode-ci" "они"
       (cond ((rxmatch #/[а-пР-Я][А-Пр-я]./i "они") => rxmatch-substring)
             (else #f)))
(test* "regexp/unicode-ci" #f
       (cond ((rxmatch #/о[а-я]а/ "ОНА") => rxmatch-substring)
             (else #f)))
(test* "regexp/unicode-ci" #f
       (cond ((rxmatch #/О[А-Я]А/ "она") => rxmatch-substring)
             (else #f)))
(test* "regexp/unicode-ci" #f
       (cond ((rxmatch #/[а-пР-Я][А-Пр-я]./ "они") => rxmatch-substring)
             (else #f)))
(test* "regexp/unicode-ci" "она"
       (cond ((rxmatch (string->regexp "о[А-Я]а" :case-fold #t) "она")
              => rxmatch-substring)
             (else #f)))
(test* "regexp/unicode-ci" #f
       (cond ((rxmatch (string->regexp "о[А-Я]а") "она")
              => rxmatch-substring)
             (else #f)))
(test* "regexp/unicode-ci" "σζ\x01C5;"
       (cond ((rxmatch (string->regexp "[[:LOWER:]]*" :case-fold #t) "σζ\x01C5;")
              => rxmatch-substring)
             (else #f)))

(test-end)
