;;
;; test for string related functions
;;

(use gauche.test)

(test-start "string")

;;-------------------------------------------------------------------
(test-section "string-pointer")

(define sp #f)
(test "make-string-pointer" #t
      (lambda ()
        (set! sp (make-string-pointer "abcdefg"))
        (string-pointer? sp)))
(test "string-pointer-next" #\a
      (lambda () (string-pointer-next sp)))
(test "string-pointer-next" #\b
      (lambda () (string-pointer-next sp)))
(test "string-pointer-prev" #\b
      (lambda () (string-pointer-prev sp)))
(test "string-pointer-prev" #\a
      (lambda () (string-pointer-prev sp)))
(test "string-pointer-prev" #t
      (lambda () (eof-object? (string-pointer-prev sp))))
(test "string-pointer-index" 0
      (lambda () (string-pointer-index sp)))
(test "string-pointer-index" 7
      (lambda () (do ((x (string-pointer-next sp) (string-pointer-next sp)))
                     ((eof-object? x) (string-pointer-index sp)))))
(test "string-pointer-substring" '("abcdefg" "")
      (lambda () (list (string-pointer-substring sp)
                       (string-pointer-substring sp :after #t))))
(test "string-pointer-substring" '("abcd" "efg")
      (lambda ()
        (string-pointer-set sp 4)
        (list (string-pointer-substring sp)
              (string-pointer-substring sp :after #t))))
(test "string-pointer-substring" '("" "abcdefg")
      (lambda ()
        (string-pointer-set sp 0)
        (list (string-pointer-substring sp)
              (string-pointer-substring sp :after #t))))
(test "string-pointer-substring" '("" "")
      (lambda ()
        (let ((sp (make-string-pointer "")))
          (list (string-pointer-substring sp)
                (string-pointer-substring sp :after #t)))))




              