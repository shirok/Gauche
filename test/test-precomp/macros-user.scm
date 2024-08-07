(import macros)

(define (er-aif-test item)
  ;; test hygiene
  (let ([if list]
        [let list]
        [it 'boo])
    (er-aif (memq item '(apple banana cherry))
            (list (car it))
            'bonk)))

(define (im-state-test)
  (let* ([v0 im-state]
         [_ (set! im-state 1)]
         [v1 im-state]
         [_ (set! im-state 2)]
         [v2 im-state])
    (list v0 v1 v2)))
