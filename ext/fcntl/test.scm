;;
;; testing fcntl
;;

(use gauche.test)

(load "fcntl")
(import gauche.fcntl)

(test-start "fcntl")

;; It is difficult to compose a full test that works on every situation.
;; Here I provide tests for some common features.

(sys-unlink "test.o")

(test "F_GETFL" |O_WRONLY|
      (lambda ()
        (call-with-output-file "test.o"
          (lambda (p) (logand (sys-fcntl p |F_GETFL|) |O_ACCMODE|)))))

(test "F_GETFL" |O_RDONLY|
      (lambda ()
        (call-with-input-file "test.o"
          (lambda (p) (logand (sys-fcntl p |F_GETFL|) |O_ACCMODE|)))))

(test "F_GETFL" #t
      (lambda ()
        (call-with-input-file "test.o"
          (lambda (p)
            (zero? (logand (sys-fcntl p |F_GETFL|) |O_APPEND|))))))

(test "F_GETFL" #f
      (lambda ()
        (call-with-output-file "test.o"
          (lambda (p) (zero? (logand (sys-fcntl p |F_GETFL|) |O_APPEND|)))
          :if-exists :append)))

(sys-unlink "test.o")

;; TODO: test lock

(test-end)
