;; This is to check test-module issue descried in
;; https://github.com/shirok/Gauche/issues/1157
;; https://github.com/shirok/Gauche/issues/1237

(define-module auto.loadee
  (export auto-loadee1 auto-loadee2))
(select-module auto.loadee)

(use gauche.cond-expand-rt)

(define (auto-loadee1)
  (cond-expand/runtime
   [nonexistent-feature *nonexistent-variable*]
   [else 'ok]))

(define auto-loadee2 'ho)
