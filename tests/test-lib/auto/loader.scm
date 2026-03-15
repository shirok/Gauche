;; This is to check test-module issue descried in
;; https://github.com/shirok/Gauche/issues/1157
;; https://github.com/shirok/Gauche/issues/1237

(define-module auto.loader
  (export auto-loader auto-loader2-ref auto-loader2-set!))
(select-module auto.loader)

(autoload auto.loadee auto-loadee1 auto-loadee2)

(define (auto-loader) (auto-loadee1))

(define (auto-loader2-ref) auto-loadee2)
(define (auto-loader2-set! v) (set! auto-loadee2 v))
