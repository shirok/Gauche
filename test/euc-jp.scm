;; this test only works when the core system is compiled with euc-jp.

;; $Id: euc-jp.scm,v 1.9 2001-05-31 19:43:30 shirok Exp $

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
(test-section "incomplete strings")

(test "string-length" 6 (lambda () (string-length #"あいう")))
(test "string-complete->incomplete" #"あいう" 
      (lambda () (string-complete->incomplete "あいう")))
(test "string-complete->incomplete" #"あいう"
      (lambda () (string-complete->incomplete #"あいう")))
(test "string-incomplete->complete" "あいう"
      (lambda () (string-incomplete->complete #"あいう")))
(test "string-incomplete->complete" "あいう"
      (lambda () (string-incomplete->complete "あいう")))

(test "string=?" #t (lambda () (string=? #"あいう" #"あいう")))

(test "string-byte-ref" #xa2 (lambda () (string-byte-ref #"あいう" 1)))

(test "string-append" #"あいうえお"
      (lambda () (string-append "あいう" #"えお")))
(test "string-append" #"あいうえお"
      (lambda () (string-append #"あいう" "えお")))
(test "string-append" #"あいうえお"
      (lambda () (string-append #"あいう" #"えお")))
(test "string-append" 10
      (lambda () (string-length (string-append "あいう" "えお" #""))))

(test "string-substitute!" #"\xa4bc\xa4"
      (lambda () (string-substitute! (string-copy #"あい") 1 #"bc")))

(test "string-incompltet->incomplete" "あ"
      (lambda () (string-incomplete->complete
                  (string-append #"\xa4" #"\xa2"))))

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


;;-------------------------------------------------------------------
(test-section "buffered ports")

(define (make-filler)
  (let* ((str #"あいうえおかきくけこ")  ;incomplete string
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
      '(#xa4 #xa2 #xa4 #xa4 #xa4 #xa6 #xa4 #xa8 #xa4 #xaa
        #xa4 #xab #xa4 #xad #xa4 #xaf #xa4 #xb1 #xa4 #xb3)
      (lambda ()
        (port->byte-list (open-input-buffered-port (make-filler) 256))))

(test "buffered port (getb, bufsiz=20)"
      '(#xa4 #xa2 #xa4 #xa4 #xa4 #xa6 #xa4 #xa8 #xa4 #xaa
        #xa4 #xab #xa4 #xad #xa4 #xaf #xa4 #xb1 #xa4 #xb3)
      (lambda ()
        (port->byte-list (open-input-buffered-port (make-filler) 20))))

(test "buffered port (getb, bufsiz=19)"
      '(#xa4 #xa2 #xa4 #xa4 #xa4 #xa6 #xa4 #xa8 #xa4 #xaa
        #xa4 #xab #xa4 #xad #xa4 #xaf #xa4 #xb1 #xa4 #xb3)
      (lambda ()
        (port->byte-list (open-input-buffered-port (make-filler) 19))))

(test "buffered port (getb, bufsiz=2)"
      '(#xa4 #xa2 #xa4 #xa4 #xa4 #xa6 #xa4 #xa8 #xa4 #xaa
        #xa4 #xab #xa4 #xad #xa4 #xaf #xa4 #xb1 #xa4 #xb3)
      (lambda ()
        (port->byte-list (open-input-buffered-port (make-filler) 2))))

(test "buffered port (getb, bufsiz=1)"
      '(#xa4 #xa2 #xa4 #xa4 #xa4 #xa6 #xa4 #xa8 #xa4 #xaa
        #xa4 #xab #xa4 #xad #xa4 #xaf #xa4 #xb1 #xa4 #xb3)
      (lambda ()
        (port->byte-list (open-input-buffered-port (make-filler) 1))))

(test "buffered port (getz, siz=20,5)"
      '(#"\xa4\xa2\xa4\xa4\xa4" #"\xa6\xa4\xa8\xa4\xaa"
        #"\xa4\xab\xa4\xad\xa4" #"\xaf\xa4\xb1\xa4\xb3")
      (lambda ()
        (port->chunk-list (open-input-buffered-port (make-filler) 20) 5)))

(test "buffered port (getz, siz=20,20)"
      '(#"\xa4\xa2\xa4\xa4\xa4\xa6\xa4\xa8\xa4\xaa\xa4\xab\xa4\xad\xa4\xaf\xa4\xb1\xa4\xb3")
      (lambda ()
        (port->chunk-list (open-input-buffered-port (make-filler) 20) 20)))

(test "buffered port (getz, siz=9,20)"
      '(#"\xa4\xa2\xa4\xa4\xa4\xa6\xa4\xa8\xa4\xaa\xa4\xab\xa4\xad\xa4\xaf\xa4\xb1\xa4\xb3")
      (lambda ()
        (port->chunk-list (open-input-buffered-port (make-filler) 9) 20)))

(test "buffered port (getz, siz=9,7)"
      '(#"\xa4\xa2\xa4\xa4\xa4\xa6\xa4" #"\xa8\xa4\xaa\xa4\xab\xa4\xad"
        #"\xa4\xaf\xa4\xb1\xa4\xb3")
      (lambda ()
        (port->chunk-list (open-input-buffered-port (make-filler) 9) 7)))

(test "buffered port (getz, siz=3,50)"
      '(#"\xa4\xa2\xa4\xa4\xa4\xa6\xa4\xa8\xa4\xaa\xa4\xab\xa4\xad\xa4\xaf\xa4\xb1\xa4\xb3")
      (lambda ()
        (port->chunk-list (open-input-buffered-port (make-filler) 3) 50)))

(test "buffered port (getz, siz=2,7)"
      '(#"\xa4\xa2\xa4\xa4\xa4\xa6\xa4" #"\xa8\xa4\xaa\xa4\xab\xa4\xad"
        #"\xa4\xaf\xa4\xb1\xa4\xb3")
      (lambda ()
        (port->chunk-list (open-input-buffered-port (make-filler) 2) 7)))

(test "buffered port (getz, siz=1,7)"
      '(#"\xa4\xa2\xa4\xa4\xa4\xa6\xa4" #"\xa8\xa4\xaa\xa4\xab\xa4\xad"
        #"\xa4\xaf\xa4\xb1\xa4\xb3")
      (lambda ()
        (port->chunk-list (open-input-buffered-port (make-filler) 1) 7)))

(test-end)
