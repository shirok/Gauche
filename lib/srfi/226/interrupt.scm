;;;
;;; (srfi 226 interrupt)
;;;

(define-module srfi.226.interrupt
  (use gauche.thread)
  (export ;; current-interrupt-level
          ;; disable-interrupts!
          ;; enable-interrupts!
          ;; with-interrupts-disabled
          ;; with-interrupts-enabled
          ;; thread-interrupt!
          ))
