;; test script for (add-load-path <path> :relative)
;; on success, it prints "ok!"

(add-load-path "test-lib" :relative)
(use my.module)

(define (main args)
  (my-module)
  0)

