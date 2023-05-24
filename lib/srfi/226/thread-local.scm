;;;
;;; (srfi 226 thread-local)
;;;

(define-module srfi.226.thread-local
  (use gauche.thread)
  (export make-thread-local
          thread-local?
          tlref
          tlset!))
