;; This is loaded from io.scm
;; Cf. https://github.com/shirok/Gauche/issues/521

(define load-read-test-uvector-1 (read-from-string "#f64(0)"))

(define (load-read-test-uvectors)
  (list #f64(0)
        load-read-test-uvector-1
        (read-from-string "#f64(0)")))
