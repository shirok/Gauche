;;
;; testing fcntl
;;

#!no-fold-case

(use gauche.test)
(test-start "fcntl")

(use gauche.fcntl)
(test-module 'gauche.fcntl)

(cond-expand
 (gauche.sys.fcntl
  ;; It is difficult to compose a full test that works on every situation.
  ;; Here I provide tests for some common features.

  (test-section "F_GETFL")

  (sys-unlink "test.o")

  (test* "F_GETFL" O_WRONLY
         (call-with-output-file "test.o"
           (^p (logand (sys-fcntl p F_GETFL) O_ACCMODE))))

  (test* "F_GETFL" O_RDONLY
         (call-with-input-file "test.o"
           (^p (logand (sys-fcntl p F_GETFL) O_ACCMODE))))

  (test* "F_GETFL" #t
         (call-with-input-file "test.o"
           (^p (zero? (logand (sys-fcntl p F_GETFL) O_APPEND)))))

  (test* "F_GETFL" #f
         (call-with-output-file "test.o"
           (^p (zero? (logand (sys-fcntl p F_GETFL) O_APPEND)))
           :if-exists :append))

  (sys-unlink "test.o")

  ;; TODO: test lock
  )
 (else #f))

(test-end)
