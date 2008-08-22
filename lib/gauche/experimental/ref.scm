;;;
;;; gauche.experimental.ref - shorthand notation of ref
;;;  

(define-module gauche.experimental.ref
  (export ~))
(select-module gauche.experimental.ref)

;; A small experiment to see how I feel this...
;;  [~ a b c d] => (ref (ref (ref a b) c) d)
;; Ideally this should be a compiler-macro (we can't make it a macro,
;; for we want to say (set! [~ x'y] val).
(define ~
  (getter-with-setter
   (case-lambda
     [(obj selector) (ref obj selector)]
     [(obj selector . more) (apply ~ (ref obj selector) more)])
   (case-lambda
     [(obj selector val) ((setter ref) obj selector val)]
     [(obj selector selector2 . rest)
      (apply (setter ref) (ref obj selector) selector2 rest)])))

(provide "gauche/experimental/ref")
