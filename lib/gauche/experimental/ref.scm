;;;
;;; gauche.experimental.ref - shorthand notation of ref
;;;  

(define-module gauche.experimental.ref
  (use util.match)
  (export ~))
(select-module gauche.experimental.ref)

;; A small experiment to see how I feel this...
;;  (~ a b c d) => (ref (ref (ref a b) c) d)

(define ~
  (getter-with-setter
   (case-lambda
     [(obj selector) (ref obj selector)]
     [(obj selector . more) (apply ~ (ref obj selector) more)])
   (case-lambda
     [(obj selector val) ((setter ref) obj selector val)]
     [(obj selector selector2 . rest)
      (apply (setter ref) (ref obj selector) selector2 rest)])))

;; EXPERIMENTAL EXPERIMENTAL
;; ((with-module gauche.internal attach-inline-transformer)
;;  ~
;;  (lambda (x r c)
;;    (define (nested form)
;;      (match form
;;        [(obj sel) `(ref ,obj ,sel)]
;;        [(obj sel . sels) (nested `((ref ,obj ,sel) ,@sels))]))
;;    (match x
;;      [(_ obj sel . sels) (nested (cdr x))]
;;      [_ x])))

