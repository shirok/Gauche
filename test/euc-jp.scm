;; -*- coding: euc-jp -*-

(use gauche.test)

(test-start "multibyte (euc-jp)")
(use srfi-1)

;;-------------------------------------------------------------------
(test-section "string builtins")

(test* "string" "����h�ˤۤ�t"
       (string #\�� #\�� #\h #\�� #\�� #\�� #\t))
(test* "list->string" "����h�ˤۤ�t"
       (list->string '(#\�� #\�� #\h #\�� #\�� #\�� #\t)))
(test* "make-string" "�ؤؤؤؤ�" (make-string 5 #\��))
(test* "make-string" "" (make-string 0 #\��))

(test* "string->list" '(#\�� #\�� #\h #\�� #\�� #\�� #\t)
       (string->list "����h�ˤۤ�t"))
(test* "string->list" '(#\�� #\h #\�� #\�� #\�� #\t)
       (string->list "����h�ˤۤ�t" 1))
(test* "string->list" '(#\�� #\h #\��)
       (string->list "����h�ˤۤ�t" 1 4))

(test* "string-copy" '("����ͤ�" #f)
       (let* ((x "����ͤ�") (y (string-copy x)))
         (list y (eq? x y))))
(test* "string-copy" "��ͤ�" (string-copy "����ͤ�" 1))
(test* "string-copy" "���"  (string-copy "����ͤ�" 1 3))

(test* "string-ref" #\�� (string-ref "�����" 1))
(define x (string-copy "����Ϥˤ�"))
(test* "string-set!" "����Z�ˤ�" (begin (string-set! x 2 #\Z) x))

(test* "string-fill!" "�ΤΤΤΤΤ�"
       (string-fill! (string-copy "000000") #\��))
(test* "string-fill!" "000�ΤΤ�"
       (string-fill! (string-copy "000000") #\�� 3))
(test* "string-fill!" "000�Τ�0"
       (string-fill! (string-copy "000000") #\�� 3 5))

(test* "string-join" "�դ� �Ф� �Ф�"
       (string-join '("�դ�" "�Ф�" "�Ф�")))
(test* "string-join" "�դ����Ф����Ф�"
       (string-join '("�դ�" "�Ф�" "�Ф�") "��"))
(test* "string-join" "�դ������Ф������Ф�"
       (string-join '("�դ�" "�Ф�" "�Ф�") "����" 'infix))
(test* "string-join" ""
       (string-join '() "����"))
(test* "string-join" "�դ����Ф����Ф���"
       (string-join '("�դ�" "�Ф�" "�Ф�") "��" 'suffix))
(test* "string-join" "���դ����Ф����Ф�"
       (string-join '("�դ�" "�Ф�" "�Ф�") "��" 'prefix))
(test* "string-join" "�դ����Ф����Ф�"
       (string-join '("�դ�" "�Ф�" "�Ф�") "��" 'strict-infix))

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
                    "����������������������������������������������������" "������")
  (test-string-scan '("��������������" "����������������������������������������")
                      "����������������������������������������������������" "������" 'before)
  (test-string-scan '("��������������������������������" "������")
                    "����������������������������������������������������" "������" 'after)
  (test-string-scan2 '("��������������" "����������������������������������������")
                     '("��������������������������������������" "������������")
                     "����������������������������������������������������" "������" 'before*)
  (test-string-scan2 '("��������������������" "����������������������������������������������")
                     '("��������������������������������" "������")
                     "����������������������������������������������������" "������" 'after*)
  (test-string-scan2 '("��������������" "����������������������������������������")
                     '("��������������������������������" "������")
                     "����������������������������������������������������" "������" 'both)
  (test-string-scan #f "��������������������������" "����")
  )

;;-------------------------------------------------------------------
(test-section "string-pointer")
(define sp #f)
(test* "make-string-pointer" #t
       (begin (set! sp (make-string-pointer "����Ϥ�ho�ؤ�"))
              (string-pointer? sp)))
(test* "string-pointer-next!" #\�� (string-pointer-next! sp))
(test* "string-pointer-next!" #\�� (string-pointer-next! sp))
(test* "string-pointer-prev!" #\�� (string-pointer-prev! sp))
(test* "string-pointer-prev!" #\�� (string-pointer-prev! sp))
(test* "string-pointer-prev!" #t (eof-object? (string-pointer-prev! sp)))
(test* "string-pointer-index" 0 (string-pointer-index sp))
(test* "string-pointer-index" 8
       (do ((x (string-pointer-next! sp) (string-pointer-next! sp)))
           ((eof-object? x) (string-pointer-index sp))))
(test* "string-pointer-substring" '("����Ϥ�ho�ؤ�" "")
       (list (string-pointer-substring sp)
             (string-pointer-substring sp :after #t)))
(test* "string-pointer-substring" '("����Ϥ�h" "o�ؤ�")
       (begin
         (string-pointer-set! sp 5)
         (list (string-pointer-substring sp)
               (string-pointer-substring sp :after #t))))
(test* "string-pointer-substring" '("" "����Ϥ�ho�ؤ�")
       (begin
         (string-pointer-set! sp 0)
         (list (string-pointer-substring sp)
               (string-pointer-substring sp :after #t))))

;; torturing backward pointer movement
(define sp (make-string-pointer "��a" -1))
(test* "string-pointer-prev!" #\a (string-pointer-prev! sp))
(test* "string-pointer-prev!" #\�� (string-pointer-prev! sp))
(test* "string-pointer-prev!" #t (eof-object? (string-pointer-prev! sp)))

(define sp (make-string-pointer "�����ޤ��a" -1)) ;;dreaded jisx0201 kana
(test* "string-pointer-prev!" #\a (string-pointer-prev! sp))
(test* "string-pointer-prev!" #\�� (string-pointer-prev! sp))
(test* "string-pointer-prev!" #\�� (string-pointer-prev! sp))
(test* "string-pointer-prev!" #\�� (string-pointer-prev! sp))

;;-------------------------------------------------------------------
(test-section "incomplete strings")

(test* "string-length" 6 (string-length #*"������"))
(test* "string-complete->incomplete" #*"������" 
       (string-complete->incomplete "������"))
(test* "string-complete->incomplete" #*"������"
       (string-complete->incomplete #*"������"))
(test* "string-incomplete->complete" "������"
       (string-incomplete->complete #*"������"))
(test* "string-incomplete->complete" "������"
       (string-incomplete->complete "������"))

(test* "string=?" #t (string=? #*"������" #*"������"))

(test* "string-byte-ref" #xa2 (string-byte-ref #*"������" 1))

(test* "string-append" #*"����������"
       (string-append "������" #*"����"))
(test* "string-append" #*"����������"
       (string-append #*"������" "����"))
(test* "string-append" #*"����������"
       (string-append #*"������" #*"����"))
(test* "string-append" 10
       (string-length (string-append "������" "����" #*"")))

(test* "string-incompltet->incomplete" "��"
       (string-incomplete->complete
        (string-append #*"\xa4" #*"\xa2")))

;;-------------------------------------------------------------------
(test-section "format")

(test* "format" "���֤�"
       (format #f "~,,,,3a" "���֤餫���֤�"))
(test* "format" "ab��"
       (format #f "~,,,,3a" "ab�餫���֤�"))
(test* "format" "���֤餫���֤�"
       (format #f "~,,,,7:a" "���֤餫���֤�"))
(test* "format" "���֤餫"
       (format #f "~,,,,7:a" "���֤餫"))
(test* "format" "���֤� ..."
       (format #f "~,,,,7:a" "���֤餫���֤�֤�֤�"))

;;-------------------------------------------------------------------
(test-section "string-library")
(use srfi-13)

(test* "string-every" #t (string-every #\�� ""))
(test* "string-every" #t (string-every #\�� "��������"))
(test* "string-every" #f (string-every #\�� "������a"))
(test* "string-every" #t (string-every #[��-��] "��������"))
(test* "string-every" #f (string-every #[��-��] "����a��"))
(test* "string-every" #t (string-every #[��-��] ""))
(test* "string-every" #t (string-every (^x (char-ci=? x #\��)) "��������"))
(test* "string-every" #f (string-every (^x (char-ci=? x #\��)) "��������"))

(test* "string-any" #t (string-any #\�� "��������"))
(test* "string-any" #f (string-any #\�� "��������"))
(test* "string-any" #f (string-any #\�� ""))
(test* "string-any" #t (string-any #[��-��] "��������"))
(test* "string-any" #f (string-any #[��-��] "��������"))
(test* "string-any" #f (string-any #[��-��] ""))
(test* "string-any" #t (string-any (^x (char-ci=? x #\��)) "���餢"))
(test* "string-any" #f (string-any (^x (char-ci=? x #\��)) "���饢"))
(test* "string-tabulate" "����������"
       (string-tabulate (lambda (code)
                          (integer->char (+ code
                                            (char->integer #\��))))
                        5))
(test* "reverse-list->string" "����"
       (reverse-list->string '(#\�� #\�� #\��)))
(test* "string-copy!" "ab������fg"
       (let ((x (string-copy "abcdefg")))
         (string-copy! x 2 "������������" 2 5)
         x))
(test* "string-take" "��������"  (string-take "������������" 4))
(test* "string-drop" "����"  (string-drop "������������" 4))
(test* "string-take-right" "��������"  (string-take-right "������������" 4))
(test* "string-drop-right" "����"  (string-drop-right "������������" 4))
(test* "string-pad" "�����ѥå�" (string-pad "�ѥå�" 5 #\��))
(test* "string-pad" "�ѥǥ���" (string-pad "�ѥǥ���" 5 #\��))
(test* "string-pad" "�ǥ��󥰥�" (string-pad "�ѥǥ��󥰥�" 5 #\��))
(test* "string-pad-right" "�ѥåɢ���" (string-pad-right "�ѥå�" 5 #\��))
(test* "string-pad" "�ѥǥ���" (string-pad-right "�ѥǥ��󥰥�" 5 #\��))

;;-------------------------------------------------------------------
(test-section "char set")

(use srfi-14)

(let ((data '(#[��-��¾-����-��]
              #[a������])))
  (define (t x)
    (test* "mutable/immutable roundtrip" #t
           (char-set= (char-set-copy x)
                      (char-set-copy (char-set-freeze! (char-set-copy x))))))
  (for-each t data))

(test-char-set-1 "char-set-contains?" #t (cut char-set-contains? <> #\��)
                 (char-set #\�� #\�� #\�� #\�� #\��))
(test-char-set-1 "char-set-contains?" #f (cut char-set-contains? <> #\��)
                 (char-set #\�� #\�� #\�� #\�� #\��))
(test-char-set-1 "char-set-contains?" #t (cut char-set-contains? <> #\��)
                 (char-set-union
                  (integer-range->char-set (char->integer #\��)
                                           (char->integer #\��))
                  (integer-range->char-set (char->integer #\��)
                                           (char->integer #\��))))
(test-char-set-1 "char-set-contains?" #t (cut char-set-contains? <> #\��)
                 (char-set-union
                  (integer-range->char-set (char->integer #\��)
                                           (char->integer #\��))
                  (integer-range->char-set (char->integer #\��)
                                           (char->integer #\��))))
(test-char-set-1 "char-set-contains?" #f (cut char-set-contains? <> #\��)
                 (char-set-union
                  (integer-range->char-set (char->integer #\��)
                                           (char->integer #\��))
                  (integer-range->char-set (char->integer #\��)
                                           (char->integer #\��))))
(test-char-set-1 "char-set-contains?" #f (cut char-set-contains? <> #\��)
                 (char-set-union
                  (integer-range->char-set (char->integer #\��)
                                           (char->integer #\��))
                  (integer-range->char-set (char->integer #\��)
                                           (char->integer #\��))))

(test-char-set-2 "char-set=" #t char-set=
                 (char-set #\�� #\�� #\�� #\�� #\��)
                 (string->char-set "����������"))
(test-char-set-2 "char-set=" #t char-set=
                 (list->char-set '(#\�� #\�� #\�� #\��))
                 (string->char-set "��󤤤���������"))
(test-char-set-2 "char-set=" #t char-set=
                 (->char-set "������������������")
                 (integer-range->char-set (char->integer #\��)
                                          (char->integer #\��)))
(test-char-set-2 "char-set<=" #t char-set<=
                 (list->char-set '(#\�� #\��))
                 char-set:full)
(test-char-set-2 "char-set<=" #t char-set<= #[����] #[����])
(test-char-set-2 "char-set<=" #t char-set<= #[����] #[��-��])
(test-char-set-2 "char-set<=" #f char-set<= #[��-��] #[����])
(test-char-set-2 "char-set<=" #t char-set<= #[��-����-��] #[��-��])
(test-char-set-2 "char-set<=" #f char-set<= #[��-��] #[��-����-��])
(test-char-set-2 "char-set<=" #f char-set<= #[��-����-��] #[��-��])
(test-char-set-2 "char-set<=" #f char-set<= #[��-����-��] #[��-��])
(test-char-set-2 "char-set<=" #t char-set<= #[��-����-��] #[��-����-��])
(test-char-set-2 "char-set<=" #t char-set<= #[��-����-��] #[��-����-��])
(test-char-set-2 "char-set<=" #t char-set<= #[��-����-��] #[��-����-��])
(test-char-set-2 "char-set<=" #t char-set<= #[��-��] #[��-����-��])
(test-char-set-2 "char-set<=" #t char-set<= #[��-����-��] #[��-����-��])
(test-char-set-2 "char-set<=" #f char-set<= #[��-��] #[��-����-��])

;;-------------------------------------------------------------------
(test-section "ports")

(define istr (open-input-string "����ϥ˥ۥإ�"))
(test* "read-char" #\�� (read-char istr))
(test* "read-byte" #xa5 (read-byte istr))
(test* "read-byte (using scratch)" #xed
       (begin (peek-char istr) (read-byte istr)))
(test* "read-char (using scratch)" #\��
       (read-char istr))
(test* "read-block (using scratch)" #*"��"
       (begin (peek-char istr) (read-block 2 istr)))
(test* "read-block (using scratch)" #*"\xa5"
       (begin (peek-char istr) (read-block 1 istr)))
(test* "read-block (using scratch)" #*"\xdb�إ�"
       (begin (peek-char istr) (read-block 10 istr)))

;; start over
(set! istr (open-input-string "����ϥ˥ۥإ�"))
(test* "peek-byte" #xa5 (peek-byte istr))
(test* "peek-char" #\�� (peek-char istr))
(test* "read-byte" #xa5 (read-byte istr))
(test* "peek-byte" #xa4 (peek-byte istr))
(test* "peek-char" #\�� (begin (read-byte istr) (peek-char istr)))
(test* "read-byte" #\�� (begin (peek-byte istr) (read-char istr)))
(test* "peek-byte" #xcf
       (begin (peek-char istr) (read-byte istr) (peek-byte istr)))
(test* "read-block" #*"\xcf�˥ۥ�\xa5" (read-block 8 istr))
(test* "peek-byte" #xc8 (peek-byte istr))
(test* "peek-byte" #t (begin (read-byte istr) (eof-object? (peek-byte istr))))

(test* "read-line (LF)" "�ʤ�"
       (read-line (open-input-string "�ʤ�\n")))
(test* "read-line (CR)" "�ʤ�"
       (read-line (open-input-string "�ʤ�\r")))
(test* "read-line (CRLF)" "�ʤ�"
       (read-line (open-input-string "�ʤ�\r\n")))
(test* "read-line (using ungotten)" "�ʤ�"
       (let1 s (open-input-string "�ʤ�\n")
         (peek-char s) (read-line s)))
(test* "read-line (using ungotten)" "�ʤ�"
       (let1 s (open-input-string "�ʤ�\n")
         (peek-byte s) (read-line s)))

;(test "read-line (using scratch)" "�ʤ�"
;      (lambda ()
;        (let ((i (open-input-string "�ʤ�\n"))
;              (o (open-output-string)))
;          (peek-char i)
;          (write-byte (read-byte i) o)
;          (display (read-line i) o)
;          (get-output-string o))))

;;-------------------------------------------------------------------
(test-section "buffered ports")

(define (make-filler)
  (let* ((str #*"��������������������")  ;incomplete string
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
       '(#\�� #\�� #\�� #\�� #\�� #\�� #\�� #\�� #\�� #\��)
       (port->char-list (open-input-buffered-port (make-filler) 256)))

(test* "buffered port (getc, bufsiz=7)"
       '(#\�� #\�� #\�� #\�� #\�� #\�� #\�� #\�� #\�� #\��)
       (port->char-list (open-input-buffered-port (make-filler) 7)))

(test* "buffered port (getc, bufsiz=3)"
       '(#\�� #\�� #\�� #\�� #\�� #\�� #\�� #\�� #\�� #\��)
       (port->char-list (open-input-buffered-port (make-filler) 3)))

(test* "buffered port (getc, bufsiz=2)"
       '(#\�� #\�� #\�� #\�� #\�� #\�� #\�� #\�� #\�� #\��)
       (port->char-list (open-input-buffered-port (make-filler) 2)))

(test* "buffered port (getc, bufsiz=1)"
       '(#\�� #\�� #\�� #\�� #\�� #\�� #\�� #\�� #\�� #\��)
       (port->char-list (open-input-buffered-port (make-filler) 1)))

(test* "buffered port (getb, bufsiz=256)"
       '(#xa4 #xa2 #xa4 #xa4 #xa4 #xa6 #xa4 #xa8 #xa4 #xaa
              #xa4 #xab #xa4 #xad #xa4 #xaf #xa4 #xb1 #xa4 #xb3)
       (port->byte-list (open-input-buffered-port (make-filler) 256)))

(test* "buffered port (getb, bufsiz=20)"
       '(#xa4 #xa2 #xa4 #xa4 #xa4 #xa6 #xa4 #xa8 #xa4 #xaa
              #xa4 #xab #xa4 #xad #xa4 #xaf #xa4 #xb1 #xa4 #xb3)
       (port->byte-list (open-input-buffered-port (make-filler) 20)))

(test* "buffered port (getb, bufsiz=19)"
       '(#xa4 #xa2 #xa4 #xa4 #xa4 #xa6 #xa4 #xa8 #xa4 #xaa
              #xa4 #xab #xa4 #xad #xa4 #xaf #xa4 #xb1 #xa4 #xb3)
       (port->byte-list (open-input-buffered-port (make-filler) 19)))

(test* "buffered port (getb, bufsiz=2)"
       '(#xa4 #xa2 #xa4 #xa4 #xa4 #xa6 #xa4 #xa8 #xa4 #xaa
              #xa4 #xab #xa4 #xad #xa4 #xaf #xa4 #xb1 #xa4 #xb3)
       (port->byte-list (open-input-buffered-port (make-filler) 2)))

(test* "buffered port (getb, bufsiz=1)"
       '(#xa4 #xa2 #xa4 #xa4 #xa4 #xa6 #xa4 #xa8 #xa4 #xaa
              #xa4 #xab #xa4 #xad #xa4 #xaf #xa4 #xb1 #xa4 #xb3)
       (port->byte-list (open-input-buffered-port (make-filler) 1)))

(test* "buffered port (getz, siz=20,5)"
       '(#*"\xa4\xa2\xa4\xa4\xa4" #*"\xa6\xa4\xa8\xa4\xaa"
           #*"\xa4\xab\xa4\xad\xa4" #*"\xaf\xa4\xb1\xa4\xb3")
       (port->chunk-list (open-input-buffered-port (make-filler) 20) 5))

(test* "buffered port (getz, siz=20,20)"
       '(#*"\xa4\xa2\xa4\xa4\xa4\xa6\xa4\xa8\xa4\xaa\xa4\xab\xa4\xad\xa4\xaf\xa4\xb1\xa4\xb3")
       (port->chunk-list (open-input-buffered-port (make-filler) 20) 20))

(test* "buffered port (getz, siz=9,20)"
       '(#*"\xa4\xa2\xa4\xa4\xa4\xa6\xa4\xa8\xa4\xaa\xa4\xab\xa4\xad\xa4\xaf\xa4\xb1\xa4\xb3")
       (port->chunk-list (open-input-buffered-port (make-filler) 9) 20))

(test* "buffered port (getz, siz=9,7)"
       '(#*"\xa4\xa2\xa4\xa4\xa4\xa6\xa4" #*"\xa8\xa4\xaa\xa4\xab\xa4\xad"
           #*"\xa4\xaf\xa4\xb1\xa4\xb3")
       (port->chunk-list (open-input-buffered-port (make-filler) 9) 7))

(test* "buffered port (getz, siz=3,50)"
       '(#*"\xa4\xa2\xa4\xa4\xa4\xa6\xa4\xa8\xa4\xaa\xa4\xab\xa4\xad\xa4\xaf\xa4\xb1\xa4\xb3")
       (port->chunk-list (open-input-buffered-port (make-filler) 3) 50))

(test* "buffered port (getz, siz=2,7)"
       '(#*"\xa4\xa2\xa4\xa4\xa4\xa6\xa4" #*"\xa8\xa4\xaa\xa4\xab\xa4\xad"
           #*"\xa4\xaf\xa4\xb1\xa4\xb3")
       (port->chunk-list (open-input-buffered-port (make-filler) 2) 7))

(test* "buffered port (getz, siz=1,7)"
       '(#*"\xa4\xa2\xa4\xa4\xa4\xa6\xa4" #*"\xa8\xa4\xaa\xa4\xab\xa4\xad"
           #*"\xa4\xaf\xa4\xb1\xa4\xb3")
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
       #*"������������������������������"
       (char-list->port (open-output-buffered-port flusher 7)
                        '(#\�� #\�� #\�� #\�� #\�� #\�� #\�� #\�� #\�� #\��
                          #\�� #\�� #\�� #\�� #\��)))

(test* "buffered port (putc, bufsiz=30)"
       #*"������������������������������"
       (char-list->port (open-output-buffered-port flusher 30)
                        '(#\�� #\�� #\�� #\�� #\�� #\�� #\�� #\�� #\�� #\��
                          #\�� #\�� #\�� #\�� #\��)))

(test* "buffered port (puts, bufsiz=6)"
       #*"������������������������������"
       (string-list->port (open-output-buffered-port flusher 6)
                          '("������" "������" "������" "������" "������")))

(test* "buffered port (puts, bufsiz=7)"
       #*"������������������������������"
       (string-list->port (open-output-buffered-port flusher 7)
                          '("������" "������" "������" "������" "������")))

(test* "buffered port (puts, bufsiz=7)"
       #*"������������������������������"
       (string-list->port (open-output-buffered-port flusher 7)
                          '("����������" "����������" "��������" "��")))

(test* "buffered port (puts, bufsiz=3)"
       #*"������������������������������"
       (string-list->port (open-output-buffered-port flusher 3)
                          '("����������" "����������" "��������" "��")))

;;-------------------------------------------------------------------
(test-section "regexp")

(test* "regexp" "��a��b��c"
       (cond ((rxmatch #/([��-��][a-z])+/ "xy��a��b��cd��")
              => rxmatch-substring)
             (else #f)))
(test* "regexp" "��a��B��C"
       (cond ((rxmatch #/([��-��][a-z])+/i "XY��a��B��Cd��")
              => rxmatch-substring)
             (else #f)))

(test* "regexp" #f
       (cond ((rxmatch #/(.*)a/ "������")
              => rxmatch-substring)
             (else #f)))

(test* "regexp" "����a"
       (cond ((rxmatch #/(.*)a/ "����a��")
              => rxmatch-substring)
             (else #f)))

(test* "regexp" #f
       (cond ((rxmatch #/([^a]*)a/ "������")
              => rxmatch-substring)
             (else #f)))

(test* "regexp" "������"
       (cond ((rxmatch #/([^a]*)��/ "������")
              => rxmatch-substring)
             (else #f)))

(test* "regexp" "����a"
       (cond ((rxmatch #/([^a]+)a/ "a����a��")
              => rxmatch-substring)
             (else #f)))

(test* "regexp" #f
       (cond ((rxmatch #/([^a]+)��/ "a����a��")
              => rxmatch-substring)
             (else #f)))

(test* "regexp" "����"
       (cond ((rxmatch #/([^a]+)��/ "a����a��")
              => rxmatch-substring)
             (else #f)))

(test-end)
