;;;
;;; (srfi 226 thread-local)
;;;

(define-module srfi.226.thread-local
  (use gauche.threads)
  (export make-thread-local
          thread-local?
          tlref
          tlset!))
