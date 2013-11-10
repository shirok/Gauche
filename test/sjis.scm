;; -*- coding: shift_jis -*-

(use gauche.test)

(test-start "multibyte (sjis)")
(use srfi-1)

;;-------------------------------------------------------------------
(test-section "string builtins")

(test* "string" "いろhにほへt"
       (string #\い #\ろ #\h #\に #\ほ #\へ #\t))
(test* "list->string" "いろhにほへt"
       (list->string '(#\い #\ろ #\h #\に #\ほ #\へ #\t)))
(test* "make-string" "へへへへへ" (make-string 5 #\へ))
(test* "make-string" "" (make-string 0 #\へ))

(test* "string->list" '(#\い #\ろ #\h #\に #\ほ #\へ #\t)
       (string->list "いろhにほへt"))
(test* "string->list" '(#\ろ #\h #\に #\ほ #\へ #\t)
       (string->list "いろhにほへt" 1))
(test* "string->list" '(#\ろ #\h #\に)
       (string->list "いろhにほへt" 1 4))

(test* "string-copy" '("ぁゃνぃ" #f)
       (let* ((x "ぁゃνぃ") (y (string-copy x)))
         (list y (eq? x y))))
(test* "string-copy" "ゃνぃ" (string-copy "ぁゃνぃ" 1))
(test* "string-copy" "ゃν"  (string-copy "ぁゃνぃ" 1 3))

(test* "string-ref" #\ろ (string-ref "いろは" 1))
(define x (string-copy "いろはにほ"))
(test* "string-set!" "いろZにほ" (begin (string-set! x 2 #\Z) x))

(test* "string-fill!" "のののののの"
       (string-fill! (string-copy "000000") #\の))
(test* "string-fill!" "000ののの"
       (string-fill! (string-copy "000000") #\の 3))
(test* "string-fill!" "000のの0"
       (string-fill! (string-copy "000000") #\の 3 5))

(test* "string-join" "ふぅ ばぁ ばず"
       (string-join '("ふぅ" "ばぁ" "ばず")))
(test* "string-join" "ふぅ！ばぁ！ばず"
       (string-join '("ふぅ" "ばぁ" "ばず") "！"))
(test* "string-join" "ふぅ→←ばぁ→←ばず"
       (string-join '("ふぅ" "ばぁ" "ばず") "→←" 'infix))
(test* "string-join" ""
       (string-join '() "→←"))
(test* "string-join" "ふぅ！ばぁ！ばず！"
       (string-join '("ふぅ" "ばぁ" "ばず") "！" 'suffix))
(test* "string-join" "！ふぅ！ばぁ！ばず"
       (string-join '("ふぅ" "ばぁ" "ばず") "！" 'prefix))
(test* "string-join" "ふぅ！ばぁ！ばず"
       (string-join '("ふぅ" "ばぁ" "ばず") "！" 'strict-infix))

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
(test-section "string-pointer")
(define sp #f)
(test* "make-string-pointer" #t
       (begin
         (set! sp (make-string-pointer "いろはにhoへと"))
         (string-pointer? sp)))
(test* "string-pointer-next!" #\い
       (string-pointer-next! sp))
(test* "string-pointer-next!" #\ろ
       (string-pointer-next! sp))
(test* "string-pointer-next!" #\は
       (string-pointer-next! sp))
(test* "string-pointer-next!" #\に
       (string-pointer-next! sp))
(test* "string-pointer-next!" #\h
       (string-pointer-next! sp))
(test* "string-pointer-next!" #\o
       (string-pointer-next! sp))
(test* "string-pointer-next!" #\へ
       (string-pointer-next! sp))
(test* "string-pointer-prev!" #\へ
       (string-pointer-prev! sp))
(test* "string-pointer-prev!" #\o
       (string-pointer-prev! sp))
(test* "string-pointer-prev!" #\h
       (string-pointer-prev! sp))
(test* "string-pointer-prev!" #\に
       (string-pointer-prev! sp))
(test* "string-pointer-prev!" #\は
       (string-pointer-prev! sp))
(test* "string-pointer-prev!" #\ろ
       (string-pointer-prev! sp))
(test* "string-pointer-prev!" #\い
       (string-pointer-prev! sp))
(test* "string-pointer-prev!" #t
       (eof-object? (string-pointer-prev! sp)))
(test* "string-pointer-index" 0
       (string-pointer-index sp))
(test* "string-pointer-index" 8
       (do ((x (string-pointer-next! sp) (string-pointer-next! sp)))
           ((eof-object? x) (string-pointer-index sp))))
(test* "string-pointer-substring" '("いろはにhoへと" "")
       (list (string-pointer-substring sp)
             (string-pointer-substring sp :after #t)))
(test* "string-pointer-substring" '("いろはにh" "oへと")
       (begin
         (string-pointer-set! sp 5)
         (list (string-pointer-substring sp)
               (string-pointer-substring sp :after #t))))
(test* "string-pointer-substring" '("" "いろはにhoへと")
       (begin
         (string-pointer-set! sp 0)
         (list (string-pointer-substring sp)
               (string-pointer-substring sp :after #t))))

;;-------------------------------------------------------------------
(test-section "incomplete strings")

(test* "string-length" 6 (string-length #*"あいう"))
(test* "string-complete->incomplete" #*"あいう" 
       (string-complete->incomplete "あいう"))
(test* "string-complete->incomplete" #*"あいう"
       (string-complete->incomplete #*"あいう"))
(test* "string-incomplete->complete" "あいう"
       (string-incomplete->complete #*"あいう"))
(test* "string-incomplete->complete" "あいう"
       (string-incomplete->complete "あいう"))

(test* "string=?" #t (string=? #*"あいう" #*"あいう"))

(test* "string-byte-ref" #xa0 (string-byte-ref #*"あいう" 1))

(test* "string-append" #*"あいうえお"
       (string-append "あいう" #*"えお"))
(test* "string-append" #*"あいうえお"
       (string-append #*"あいう" "えお"))
(test* "string-append" #*"あいうえお"
       (string-append #*"あいう" #*"えお"))
(test* "string-append" 10
       (string-length (string-append "あいう" "えお" #*"")))

(test* "string-incomplete->complete" "あ"
       (string-incomplete->complete (string-append #*"\x82" #*"\xa0")))

;;-------------------------------------------------------------------
(test-section "format")

(test* "format" "あぶら"
       (format #f "~,,,,3a" "あぶらかだぶら"))
(test* "format" "abら"
       (format #f "~,,,,3a" "abらかだぶら"))
(test* "format" "あぶらかだぶら"
       (format #f "~,,,,7:a" "あぶらかだぶら"))
(test* "format" "あぶらか"
       (format #f "~,,,,7:a" "あぶらか"))
(test* "format" "あぶら ..."
       (format #f "~,,,,7:a" "あぶらかだぶらぶらぶら"))

;;-------------------------------------------------------------------
(test-section "string-library")
(use srfi-13)

(test* "string-every" #t (string-every #\あ ""))
(test* "string-every" #t (string-every #\あ "ああああ"))
(test* "string-every" #f (string-every #\あ "あああa"))
(test* "string-every" #t (string-every #[あ-ん] "ああいあ"))
(test* "string-every" #f (string-every #[あ-ん] "ああaあ"))
(test* "string-every" #t (string-every #[あ-ん] ""))
(test* "string-every" #t (string-every (^x (char-ci=? x #\あ)) "ああああ"))
(test* "string-every" #f (string-every (^x (char-ci=? x #\あ)) "あいあい"))

(test* "string-any" #t (string-any #\あ "ああああ"))
(test* "string-any" #f (string-any #\あ "いうえお"))
(test* "string-any" #f (string-any #\あ ""))
(test* "string-any" #t (string-any #[あ-ん] "すきーむ"))
(test* "string-any" #f (string-any #[あ-ん] "スキーム"))
(test* "string-any" #f (string-any #[あ-ん] ""))
(test* "string-any" #t (string-any (^x (char-ci=? x #\あ)) "らららあ"))
(test* "string-any" #f (string-any (^x (char-ci=? x #\あ)) "ラララア"))
(test* "string-tabulate" "アィイゥウ"
       (string-tabulate (lambda (code)
                          (integer->char (+ code
                                            (char->integer #\ア))))
                        5))
(test* "reverse-list->string" "んをわ"
       (reverse-list->string '(#\わ #\を #\ん)))
(test* "string-copy!" "abうえおfg"
       (let ((x (string-copy "abcdefg")))
         (string-copy! x 2 "あいうえおか" 2 5)
         x))
(test* "string-take" "あいうえ"  (string-take "あいうえおか" 4))
(test* "string-drop" "おか"  (string-drop "あいうえおか" 4))
(test* "string-take-right" "うえおか"  (string-take-right "あいうえおか" 4))
(test* "string-drop-right" "あい"  (string-drop-right "あいうえおか" 4))
(test* "string-pad" "■■パッド" (string-pad "パッド" 5 #\■))
(test* "string-pad" "パディング" (string-pad "パディング" 5 #\■))
(test* "string-pad" "ディングス" (string-pad "パディングス" 5 #\■))
(test* "string-pad-right" "パッド■■" (string-pad-right "パッド" 5 #\■))
(test* "string-pad" "パディング" (string-pad-right "パディングス" 5 #\■))

;;-------------------------------------------------------------------
(test-section "char set")

(use srfi-14)

(test* "char-set=" #t
       (char-set= (char-set #\あ #\い #\う #\え #\お)
                  (string->char-set "おうえいあ")))
(test* "char-set=" #t
       (char-set= (list->char-set '(#\あ #\い #\う #\ん))
                  (string->char-set "んんいいいああう")))
(test* "char-set=" #t
       (char-set= (->char-set "ぁぃぅぇぉあいうえ")
                  (integer-range->char-set (char->integer #\ぁ)
                                           (char->integer #\お))))

(test* "char-set" #t
       (char-set<= (list->char-set '(#\ほ #\げ))
                   char-set:full))
(test* "char-set<=" #t (char-set<= #[あい] #[あい]))
(test* "char-set<=" #t (char-set<= #[あい] #[あ-い]))
(test* "char-set<=" #f (char-set<= #[あ-い] #[あい]))
(test* "char-set<=" #t (char-set<= #[あ-いう-え] #[あ-え]))
(test* "char-set<=" #f (char-set<= #[あ-え] #[あ-いう-え]))
(test* "char-set<=" #f (char-set<= #[あ-いう-ぉ] #[あ-え]))
(test* "char-set<=" #f (char-set<= #[ぁ-いう-え] #[あ-え]))
(test* "char-set<=" #t (char-set<= #[あ-いか-き] #[あ-うお-け]))
(test* "char-set<=" #t (char-set<= #[あ-いか-き] #[あ-うお-き]))
(test* "char-set<=" #t (char-set<= #[あ-いか-き] #[あ-うか-こ]))
(test* "char-set<=" #t (char-set<= #[か-き] #[あ-うか-こ]))
(test* "char-set<=" #t (char-set<= #[か-きく-け] #[あ-うか-こ]))
(test* "char-set<=" #f (char-set<= #[う-く] #[あ-えか-こ]))

;;-------------------------------------------------------------------
(test-section "ports")

;; イロハニホヘト : 8343.838d.836e.836a.837a.8377.8367
(define istr (open-input-string "イロハニホヘト"))
(test* "read-char" #\イ (read-char istr))
(test* "read-byte" #x83 (read-byte istr))
(test* "read-byte (using scratch)" #x8d
       (begin (peek-char istr) (read-byte istr)))
(test* "read-char (using scratch)" #\ハ
       (read-char istr))
(test* "read-block (using scratch)" #*"ニ"
       (begin (peek-char istr) (read-block 2 istr)))
(test* "read-block (using scratch)" #*"\x83"
       (begin (peek-char istr) (read-block 1 istr)))
(test* "read-block (using scratch)" #*"\x7aヘト"
       (begin (peek-char istr) (read-block 10 istr)))

;; start over
(set! istr (open-input-string "イロハニホヘト"))
(test* "peek-byte" #x83 (peek-byte istr))
(test* "peek-char" #\イ (peek-char istr))
(test* "read-byte" #x83 (read-byte istr))
(test* "peek-byte" #x43 (peek-byte istr))
(test* "peek-char" #\ロ (begin (read-byte istr) (peek-char istr)))
(test* "read-char" #\ロ (begin (peek-byte istr) (read-char istr)))
(test* "peek-byte" #x6e
       (begin (peek-char istr) (read-byte istr) (peek-byte istr)))
(test* "read-block" #*"\x6eニホヘ\x83" (read-block 8 istr))
(test* "peek-byte" #x67 (peek-byte istr))
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

(test* "buffered port (getc, bufsiz=256)"
       '(#\あ #\い #\う #\え #\お #\か #\き #\く #\け #\こ)
       (port->char-list (open-input-buffered-port (make-filler) 256)))

(test* "buffered port (getc, bufsiz=7)"
       '(#\あ #\い #\う #\え #\お #\か #\き #\く #\け #\こ)
       (port->char-list (open-input-buffered-port (make-filler) 7)))

(test* "buffered port (getc, bufsiz=3)"
       '(#\あ #\い #\う #\え #\お #\か #\き #\く #\け #\こ)
       (port->char-list (open-input-buffered-port (make-filler) 3)))

(test* "buffered port (getc, bufsiz=2)"
       '(#\あ #\い #\う #\え #\お #\か #\き #\く #\け #\こ)
       (port->char-list (open-input-buffered-port (make-filler) 2)))

(test* "buffered port (getc, bufsiz=1)"
       '(#\あ #\い #\う #\え #\お #\か #\き #\く #\け #\こ)
       (port->char-list (open-input-buffered-port (make-filler) 1)))

(test* "buffered port (getb, bufsiz=256)"
       '(#x82 #xa0 #x82 #xa2 #x82 #xa4 #x82 #xa6 #x82 #xa8
              #x82 #xa9 #x82 #xab #x82 #xad #x82 #xaf #x82 #xb1)
       (port->byte-list (open-input-buffered-port (make-filler) 256)))

(test* "buffered port (getb, bufsiz=20)"
       '(#x82 #xa0 #x82 #xa2 #x82 #xa4 #x82 #xa6 #x82 #xa8
              #x82 #xa9 #x82 #xab #x82 #xad #x82 #xaf #x82 #xb1)
       (port->byte-list (open-input-buffered-port (make-filler) 20)))

(test* "buffered port (getb, bufsiz=19)"
       '(#x82 #xa0 #x82 #xa2 #x82 #xa4 #x82 #xa6 #x82 #xa8
              #x82 #xa9 #x82 #xab #x82 #xad #x82 #xaf #x82 #xb1)
       (port->byte-list (open-input-buffered-port (make-filler) 19)))

(test* "buffered port (getb, bufsiz=2)"
       '(#x82 #xa0 #x82 #xa2 #x82 #xa4 #x82 #xa6 #x82 #xa8
              #x82 #xa9 #x82 #xab #x82 #xad #x82 #xaf #x82 #xb1)
       (port->byte-list (open-input-buffered-port (make-filler) 2)))

(test* "buffered port (getb, bufsiz=1)"
       '(#x82 #xa0 #x82 #xa2 #x82 #xa4 #x82 #xa6 #x82 #xa8
              #x82 #xa9 #x82 #xab #x82 #xad #x82 #xaf #x82 #xb1)
       (port->byte-list (open-input-buffered-port (make-filler) 1)))

(test* "buffered port (getz, siz=20,5)"
       '(#*"\x82\xa0\x82\xa2\x82" #*"\xa4\x82\xa6\x82\xa8"
           #*"\x82\xa9\x82\xab\x82" #*"\xad\x82\xaf\x82\xb1")
       (port->chunk-list (open-input-buffered-port (make-filler) 20) 5))

(test* "buffered port (getz, siz=20,20)"
       '(#*"\x82\xa0\x82\xa2\x82\xa4\x82\xa6\x82\xa8\x82\xa9\x82\xab\x82\xad\x82\xaf\x82\xb1")
       (port->chunk-list (open-input-buffered-port (make-filler) 20) 20))

(test* "buffered port (getz, siz=9,20)"
       '(#*"\x82\xa0\x82\xa2\x82\xa4\x82\xa6\x82\xa8\x82\xa9\x82\xab\x82\xad\x82\xaf\x82\xb1")
       (port->chunk-list (open-input-buffered-port (make-filler) 9) 20))

(test* "buffered port (getz, siz=9,7)"
       '(#*"\x82\xa0\x82\xa2\x82\xa4\x82" #*"\xa6\x82\xa8\x82\xa9\x82\xab"
           #*"\x82\xad\x82\xaf\x82\xb1")
       (port->chunk-list (open-input-buffered-port (make-filler) 9) 7))

(test* "buffered port (getz, siz=3,50)"
       '(#*"\x82\xa0\x82\xa2\x82\xa4\x82\xa6\x82\xa8\x82\xa9\x82\xab\x82\xad\x82\xaf\x82\xb1")
       (port->chunk-list (open-input-buffered-port (make-filler) 3) 50))

(test* "buffered port (getz, siz=2,7)"
       '(#*"\x82\xa0\x82\xa2\x82\xa4\x82" #*"\xa6\x82\xa8\x82\xa9\x82\xab"
           #*"\x82\xad\x82\xaf\x82\xb1")
       (port->chunk-list (open-input-buffered-port (make-filler) 2) 7))

(test* "buffered port (getz, siz=1,7)"
       '(#*"\x82\xa0\x82\xa2\x82\xa4\x82" #*"\xa6\x82\xa8\x82\xa9\x82\xab"
           #*"\x82\xad\x82\xaf\x82\xb1")
       (port->chunk-list (open-input-buffered-port (make-filler) 1) 7))

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

(test* "buffered port (putb, bufsiz=7)"
       #*"@ABCDEFGHIJKLMNOPQRSTUVWXYZ"
       (byte-list->port (open-output-buffered-port flusher 7)
                        (iota 27 #x40)))

(test* "buffered port (putb, bufsiz=30)"
       #*"@ABCDEFGHIJKLMNOPQRSTUVWXYZ"
       (byte-list->port (open-output-buffered-port flusher 30)
                        (iota 27 #x40)))

(test* "buffered port (putc, bufsiz=7)"
       #*"あいうえおかきくけこさしすせそ"
       (char-list->port (open-output-buffered-port flusher 7)
                        '(#\あ #\い #\う #\え #\お #\か #\き #\く #\け #\こ
                          #\さ #\し #\す #\せ #\そ)))

(test* "buffered port (putc, bufsiz=30)"
       #*"あいうえおかきくけこさしすせそ"
       (char-list->port (open-output-buffered-port flusher 30)
                        '(#\あ #\い #\う #\え #\お #\か #\き #\く #\け #\こ
                          #\さ #\し #\す #\せ #\そ)))

(test* "buffered port (puts, bufsiz=6)"
       #*"あいうえおかきくけこさしすせそ"
       (string-list->port (open-output-buffered-port flusher 6)
                          '("あいう" "えおか" "きくけ" "こさし" "すせそ")))

(test* "buffered port (puts, bufsiz=7)"
       #*"あいうえおかきくけこさしすせそ"
       (string-list->port (open-output-buffered-port flusher 7)
                          '("あいう" "えおか" "きくけ" "こさし" "すせそ")))

(test* "buffered port (puts, bufsiz=7)"
       #*"あいうえおかきくけこさしすせそ"
       (string-list->port (open-output-buffered-port flusher 7)
                          '("あいうえお" "かきくけこ" "さしすせ" "そ")))

(test* "buffered port (puts, bufsiz=3)"
       #*"あいうえおかきくけこさしすせそ"
       (string-list->port (open-output-buffered-port flusher 3)
                          '("あいうえお" "かきくけこ" "さしすせ" "そ")))

;;-------------------------------------------------------------------
(test-section "regexp")

(test* "regexp" "いaろbはc"
       (cond ((rxmatch #/([ぁ-ん][a-z])+/ "xyいaろbはcdに")
              => rxmatch-substring)
             (else #f)))
(test* "regexp" "いaろBはC"
       (cond ((rxmatch #/([ぁ-ん][a-z])+/i "XYいaろBはCdに")
              => rxmatch-substring)
             (else #f)))

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

(test-end)
