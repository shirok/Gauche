;; -*- coding: shift_jis -*-

(use gauche.test)

(test-start "multibyte (sjis)")
(use srfi-1)

;;-------------------------------------------------------------------
(test-section "string builtins")

(test* "string" "����h�ɂق�t"
       (string #\�� #\�� #\h #\�� #\�� #\�� #\t))
(test* "list->string" "����h�ɂق�t"
       (list->string '(#\�� #\�� #\h #\�� #\�� #\�� #\t)))
(test* "make-string" "�ււււ�" (make-string 5 #\��))
(test* "make-string" "" (make-string 0 #\��))

(test* "string->list" '(#\�� #\�� #\h #\�� #\�� #\�� #\t)
       (string->list "����h�ɂق�t"))
(test* "string->list" '(#\�� #\h #\�� #\�� #\�� #\t)
       (string->list "����h�ɂق�t" 1))
(test* "string->list" '(#\�� #\h #\��)
       (string->list "����h�ɂق�t" 1 4))

(test* "string-copy" '("����˂�" #f)
       (let* ((x "����˂�") (y (string-copy x)))
         (list y (eq? x y))))
(test* "string-copy" "��˂�" (string-copy "����˂�" 1))
(test* "string-copy" "���"  (string-copy "����˂�" 1 3))

(test* "string-ref" #\�� (string-ref "�����" 1))
(define x (string-copy "����͂ɂ�"))
(test* "string-set!" "����Z�ɂ�" (begin (string-set! x 2 #\Z) x))

(test* "string-fill!" "�̂̂̂̂̂�"
       (string-fill! (string-copy "000000") #\��))
(test* "string-fill!" "000�̂̂�"
       (string-fill! (string-copy "000000") #\�� 3))
(test* "string-fill!" "000�̂�0"
       (string-fill! (string-copy "000000") #\�� 3 5))

(test* "string-join" "�ӂ� �΂� �΂�"
       (string-join '("�ӂ�" "�΂�" "�΂�")))
(test* "string-join" "�ӂ��I�΂��I�΂�"
       (string-join '("�ӂ�" "�΂�" "�΂�") "�I"))
(test* "string-join" "�ӂ������΂������΂�"
       (string-join '("�ӂ�" "�΂�" "�΂�") "����" 'infix))
(test* "string-join" ""
       (string-join '() "����"))
(test* "string-join" "�ӂ��I�΂��I�΂��I"
       (string-join '("�ӂ�" "�΂�" "�΂�") "�I" 'suffix))
(test* "string-join" "�I�ӂ��I�΂��I�΂�"
       (string-join '("�ӂ�" "�΂�" "�΂�") "�I" 'prefix))
(test* "string-join" "�ӂ��I�΂��I�΂�"
       (string-join '("�ӂ�" "�΂�" "�΂�") "�I" 'strict-infix))

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
       (begin
         (set! sp (make-string-pointer "����͂�ho�ւ�"))
         (string-pointer? sp)))
(test* "string-pointer-next!" #\��
       (string-pointer-next! sp))
(test* "string-pointer-next!" #\��
       (string-pointer-next! sp))
(test* "string-pointer-next!" #\��
       (string-pointer-next! sp))
(test* "string-pointer-next!" #\��
       (string-pointer-next! sp))
(test* "string-pointer-next!" #\h
       (string-pointer-next! sp))
(test* "string-pointer-next!" #\o
       (string-pointer-next! sp))
(test* "string-pointer-next!" #\��
       (string-pointer-next! sp))
(test* "string-pointer-prev!" #\��
       (string-pointer-prev! sp))
(test* "string-pointer-prev!" #\o
       (string-pointer-prev! sp))
(test* "string-pointer-prev!" #\h
       (string-pointer-prev! sp))
(test* "string-pointer-prev!" #\��
       (string-pointer-prev! sp))
(test* "string-pointer-prev!" #\��
       (string-pointer-prev! sp))
(test* "string-pointer-prev!" #\��
       (string-pointer-prev! sp))
(test* "string-pointer-prev!" #\��
       (string-pointer-prev! sp))
(test* "string-pointer-prev!" #t
       (eof-object? (string-pointer-prev! sp)))
(test* "string-pointer-index" 0
       (string-pointer-index sp))
(test* "string-pointer-index" 8
       (do ((x (string-pointer-next! sp) (string-pointer-next! sp)))
           ((eof-object? x) (string-pointer-index sp))))
(test* "string-pointer-substring" '("����͂�ho�ւ�" "")
       (list (string-pointer-substring sp)
             (string-pointer-substring sp :after #t)))
(test* "string-pointer-substring" '("����͂�h" "o�ւ�")
       (begin
         (string-pointer-set! sp 5)
         (list (string-pointer-substring sp)
               (string-pointer-substring sp :after #t))))
(test* "string-pointer-substring" '("" "����͂�ho�ւ�")
       (begin
         (string-pointer-set! sp 0)
         (list (string-pointer-substring sp)
               (string-pointer-substring sp :after #t))))

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

(test* "string-byte-ref" #xa0 (string-byte-ref #*"������" 1))

(test* "string-append" #*"����������"
       (string-append "������" #*"����"))
(test* "string-append" #*"����������"
       (string-append #*"������" "����"))
(test* "string-append" #*"����������"
       (string-append #*"������" #*"����"))
(test* "string-append" 10
       (string-length (string-append "������" "����" #*"")))

(test* "string-incomplete->complete" "��"
       (string-incomplete->complete (string-append #*"\x82" #*"\xa0")))

;;-------------------------------------------------------------------
(test-section "format")

(test* "format" "���Ԃ�"
       (format #f "~,,,,3a" "���Ԃ炩���Ԃ�"))
(test* "format" "ab��"
       (format #f "~,,,,3a" "ab�炩���Ԃ�"))
(test* "format" "���Ԃ炩���Ԃ�"
       (format #f "~,,,,7:a" "���Ԃ炩���Ԃ�"))
(test* "format" "���Ԃ炩"
       (format #f "~,,,,7:a" "���Ԃ炩"))
(test* "format" "���Ԃ� ..."
       (format #f "~,,,,7:a" "���Ԃ炩���Ԃ�Ԃ�Ԃ�"))

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
(test* "string-any" #t (string-any #[��-��] "�����[��"))
(test* "string-any" #f (string-any #[��-��] "�X�L�[��"))
(test* "string-any" #f (string-any #[��-��] ""))
(test* "string-any" #t (string-any (^x (char-ci=? x #\��)) "���炠"))
(test* "string-any" #f (string-any (^x (char-ci=? x #\��)) "�������A"))
(test* "string-tabulate" "�A�B�C�D�E"
       (string-tabulate (lambda (code)
                          (integer->char (+ code
                                            (char->integer #\�A))))
                        5))
(test* "reverse-list->string" "�����"
       (reverse-list->string '(#\�� #\�� #\��)))
(test* "string-copy!" "ab������fg"
       (let ((x (string-copy "abcdefg")))
         (string-copy! x 2 "������������" 2 5)
         x))
(test* "string-take" "��������"  (string-take "������������" 4))
(test* "string-drop" "����"  (string-drop "������������" 4))
(test* "string-take-right" "��������"  (string-take-right "������������" 4))
(test* "string-drop-right" "����"  (string-drop-right "������������" 4))
(test* "string-pad" "�����p�b�h" (string-pad "�p�b�h" 5 #\��))
(test* "string-pad" "�p�f�B���O" (string-pad "�p�f�B���O" 5 #\��))
(test* "string-pad" "�f�B���O�X" (string-pad "�p�f�B���O�X" 5 #\��))
(test* "string-pad-right" "�p�b�h����" (string-pad-right "�p�b�h" 5 #\��))
(test* "string-pad" "�p�f�B���O" (string-pad-right "�p�f�B���O�X" 5 #\��))

;;-------------------------------------------------------------------
(test-section "char set")

(use srfi-14)

(let ((data '(#[��-��-���@-��]
              #[a�����A])))
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
                  (integer-range->char-set (char->integer #\�@)
                                           (char->integer #\��))))
(test-char-set-1 "char-set-contains?" #t (cut char-set-contains? <> #\�P)
                 (char-set-union
                  (integer-range->char-set (char->integer #\��)
                                           (char->integer #\��))
                  (integer-range->char-set (char->integer #\�@)
                                           (char->integer #\��))))
(test-char-set-1 "char-set-contains?" #f (cut char-set-contains? <> #\��)
                 (char-set-union
                  (integer-range->char-set (char->integer #\��)
                                           (char->integer #\��))
                  (integer-range->char-set (char->integer #\�@)
                                           (char->integer #\��))))
(test-char-set-1 "char-set-contains?" #f (cut char-set-contains? <> #\��)
                 (char-set-union
                  (integer-range->char-set (char->integer #\��)
                                           (char->integer #\��))
                  (integer-range->char-set (char->integer #\�@)
                                           (char->integer #\��))))
(test-char-set-2 "char-set=" #t char-set=
                 (char-set #\�� #\�� #\�� #\�� #\��)
                 (string->char-set "����������"))
(test-char-set-2 "char-set=" #t char-set=
                 (list->char-set '(#\�� #\�� #\�� #\��))
                 (string->char-set "��񂢂���������"))
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

;; �C���n�j�z�w�g : 8343.838d.836e.836a.837a.8377.8367
(define istr (open-input-string "�C���n�j�z�w�g"))
(test* "read-char" #\�C (read-char istr))
(test* "read-byte" #x83 (read-byte istr))
(test* "read-byte (using scratch)" #x8d
       (begin (peek-char istr) (read-byte istr)))
(test* "read-char (using scratch)" #\�n
       (read-char istr))
(test* "read-block (using scratch)" #*"�j"
       (begin (peek-char istr) (read-block 2 istr)))
(test* "read-block (using scratch)" #*"\x83"
       (begin (peek-char istr) (read-block 1 istr)))
(test* "read-block (using scratch)" #*"\x7a�w�g"
       (begin (peek-char istr) (read-block 10 istr)))

;; start over
(set! istr (open-input-string "�C���n�j�z�w�g"))
(test* "peek-byte" #x83 (peek-byte istr))
(test* "peek-char" #\�C (peek-char istr))
(test* "read-byte" #x83 (read-byte istr))
(test* "peek-byte" #x43 (peek-byte istr))
(test* "peek-char" #\�� (begin (read-byte istr) (peek-char istr)))
(test* "read-char" #\�� (begin (peek-byte istr) (read-char istr)))
(test* "peek-byte" #x6e
       (begin (peek-char istr) (read-byte istr) (peek-byte istr)))
(test* "read-block" #*"\x6e�j�z�w\x83" (read-block 8 istr))
(test* "peek-byte" #x67 (peek-byte istr))
(test* "peek-byte" #t (begin (read-byte istr) (eof-object? (peek-byte istr))))

(test* "read-line (LF)" "�Ȃ�"
       (read-line (open-input-string "�Ȃ�\n")))
(test* "read-line (CR)" "�Ȃ�"
       (read-line (open-input-string "�Ȃ�\r")))
(test* "read-line (CRLF)" "�Ȃ�"
       (read-line (open-input-string "�Ȃ�\r\n")))
(test* "read-line (using ungotten)" "�Ȃ�"
       (let1 s (open-input-string "�Ȃ�\n")
         (peek-char s) (read-line s)))

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
