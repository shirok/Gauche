;;
;; This file is included from continuation.scm
;;
;; We split this so that we can also run it with srfi-226 reference
;; implementation to compare behavior.  Be careful not to use Gauche's
;; extended syntax.

;; The comment before each test records results with different implementations.
;;
;; native : Gauche native partial continuation
;; meta   : gauche.partcont-meta, implementation using full continuation
;; srfi226: Srfi-226 reference implementation run on ChezScheme
;; racket : Racket r7rs + racket/control


;; native : 1000
;; meta   : 1000
;; srfi226: 1000
;; racket : 1000
(test* "reset/shift combination 1"
       1000
       (begin
         (define k1 #f)
         (define k2 #f)
         (define k3 #f)
         (reset
          (shift k (set! k1 k)
                 (shift k (set! k2 k)
                        (shift k (set! k3 k))))
          1000)
         (k1)
         ;(k2)
         ;(k3)
         ))

;; native : (1 2 3)
;; meta   : (1 2 3)
;; srfi226: (1 2 3)
;; racket : (1 2 3)
(test* "reset/shift + values 1"
       '(1 2 3)
       (values->list (reset (values 1 2 3))))

;; native : (1 2 3)
;; meta   : (1 2 3)
;; srfi226: (1 2 3)
;; racket : (1 2 3)
(test* "reset/shift + values 2"
       '(1 2 3)
       (begin
         (define k1 #f)
         (reset
          (shift k (set! k1 k))
          (values 1 2 3))
         (values->list (k1))))

;; native : 010
;; meta   : 010
;; srfi226: 010
;; racket : 010
(test* "reset/shift + parameterize 1"
       "010"
       (with-output-to-string
         (lambda ()
           (define p (make-parameter 0))
           (display (p))
           (reset
            (parameterize ((p 1))
              (display (p))
              ;; expr of 'shift' is executed on the outside of 'reset'
              (shift k (display (p))))))))

;; native : [r01][r02][r02][r03]
;; meta   : [r01][r02][r02][r03]
;; srfi226: [r01][r02][r02][r03]
;; racket : [r01][r02][r02][r03]
(test* "reset/shift + call/cc 1"
       "[r01][r02][r02][r03]"
       (with-output-to-string
         (lambda ()
           (define k1 #f)
           (define done #f)
           (call/cc
            (lambda (k0)
              (reset
               (display "[r01]")
               (shift k (set! k1 k))
               (display "[r02]")
               (unless done
                 (set! done #t)
                 (k0))
               (display "[r03]"))))
           (k1))))

;; native : [r01][s01][s02][s02]
;; meta   : [r01][s01][s02][s02]
;; srfi226: [r01][s01][s02][s02]
;; racket : [r01][s01][s02][s02]
(test* "reset/shift + call/cc 2"
       "[r01][s01][s02][s02]"
       (with-output-to-string
         (lambda ()
           (define k1 #f)
           (define k2 #f)
           (reset
            (display "[r01]")
            (shift k (set! k1 k))
            (display "[s01]")
            (call/cc (lambda (k) (set! k2 k)))
            (display "[s02]"))
           (k1)
           (reset (reset (k2))))))

;; native : [r01][s01]
;; meta   : [r01][s01]
;; srfi226: [r01][s01]
;; racket : [r01][s01]
(test* "reset/shift + call/cc 2-B"
       "[r01][s01]"
       (with-output-to-string
         (lambda ()
           (define k1 #f)
           (define k2 #f)
           (reset
            (display "[r01]")
            (shift k (set! k1 k))
            (display "[s01]")
            (call/cc (lambda (k) (set! k2 k)))
            ;; empty after call/cc
                                        ;(display "[s02]")
            )
           (k1)
           (reset (reset (k2))))))

;; native : [d01][d02][d03][d01][s01][s02][d03][d01][s02][d03]
;; meta   : [d01][d02][d03][d01][s01][s02][d03][d01][s02][d03]
;; srfi226: [d01][d02][d03][d01][s01][s02][d03][d01][s02][d03]
;; racket : [d01][d02][d03][d01][s01][s02][d03][d01][s02][d03]
(test* "reset/shift + call/cc 2-C"
       "[d01][d02][d03][d01][s01][s02][d03][d01][s02][d03]"
       (with-output-to-string
         (lambda ()
           (define k1 #f)
           (define k2 #f)
           (reset
            (dynamic-wind
              (lambda () (display "[d01]"))
              (lambda ()
                (display "[d02]")
                (shift k (set! k1 k))
                (display "[s01]")
                (call/cc (lambda (k) (set! k2 k)))
                (display "[s02]"))
              (lambda () (display "[d03]"))))
           (k1)
           (reset (reset (k2))))))

;; native : [r01][s01][s02][d01][d02][d03][s02][d01]12345[d03]
;; meta   : [r01][s01][s02][d01][d02][d03][s02][d01]12345[d03]
;; srfi226: [r01][s01][s02][d01][d02][d03][s02][d01]12345[d03]
;; racket : [r01][s01][s02][d01][d02][s02]12345[d03]
(test* "reset/shift + call/cc 2-D (from Kahua nqueen broken)"
       "[r01][s01][s02][d01][d02][d03][s02][d01]12345[d03]"
       (with-output-to-string
         (lambda ()
           (define k1 #f)
           (define k2 #f)
           (reset
            (display "[r01]")
            (shift k (set! k1 k))
            (display "[s01]")
            (call/cc (lambda (k) (set! k2 k)))
            (display "[s02]")
            12345)
           (k1)
           (dynamic-wind
             (lambda () (display "[d01]"))
             (lambda () (display "[d02]")
                     (display (reset (reset (k2)))))
             (lambda () (display "[d03]"))))))

;; native : [r01][s01][s01]
;; meta   : [r01][s01][s01]
;; srfi226: [r01][s01][s01]
;; racket : [r01][s01][s01]
(test* "reset/shift + call/cc 3"
       "[r01][s01][s01]"
       (with-output-to-string
         (lambda ()
           (define k1 #f)
           (define k2 #f)
           (reset
            (display "[r01]")
            (call/cc (lambda (k)
                       (set! k1 k)
                       (shift k (set! k2 k))))
            (display "[s01]"))
           (k2)
           (reset (k1)))))

;; native : error
;; meta   : ""
;; srfi226: -
;; racket : -
(gauche-only
 (test* "reset/shift + call/cc error 1"
        (test-error)
        (with-output-to-string
          (lambda ()
            (define k1 #f)
            (define k2 #f)
            (define (f1) (call/cc (lambda (k) (set! k1 k)))
              (shift k (set! k2 k))
              (display "[f01]"))
            (define (f2) (display "[f02]"))
            (reset (f1) (f2))
            (reset (k1))))))

;; native : error
;; meta   : ""
;; srfi226: -
;; racket : -
(gauche-only
 (test* "reset/shift + call/cc error 2"
        (test-error)
        (with-output-to-string
          (lambda ()
            (define k1 #f)
            (define k2 #f)
            (define k3 #f)
            (define (f1) (call/cc (lambda (k) (set! k1 k)))
              (shift k (set! k2 k))
              (display "[f01]"))
            (define (f2) (display "[f02]"))
            (reset (f1) (f2))
            (reset (shift k (set! k3 k)) (k1))
            (k3)))))

;; native : error
;; meta   : -
;; srfi226: -
;; racket : -
(gauche-only
 ;; Avoid running with partcont-meta, for it hangs.
 (unless (provided? "gauche/partcont-meta")
   (test* "reset/shift + call/cc error 3"
          (test-error)
          (with-output-to-string
            (lambda ()
              (define k1 #f)
              (define k2 #f)
              (reset
               (call/cc (lambda (k) (set! k1 k)))
               (shift k (set! k2 k)))
              (k2)
              (k1))))))

(gauche-only
 ;; Avoid running with partcont-meta, for something weird happnes.
 (unless (provided? "gauche/partcont-meta")
   (let ((p (make-parameter 0))
         (c #f))
     (define (foo)
       (reset
        (display (p))
        (parameterize ((p 1))
          (call/cc
           (lambda (cont)
             (display (p))
             (shift k (display (p)) (cont k))
             (display (p)))))))
     ;; native : 010
     ;; meta   :
     ;; srfi226:
     ;; racket : -
     (test* "reset/shift + call/cc + parameterize" "010"
            (with-output-to-string
              (lambda () (set! c (foo)))))
     ;; native : 1
     ;; meta   :
     ;; srfi226:
     ;; racket : -
     (test* "reset/shift + call/cc + parameterize" "1"
            (with-output-to-string c)))))


(let ((p (make-parameter 1))
      (c #f))
  (define (foo)
    (parameterize ((p 2))
      (reset
       (display (p))
       (temporarily ((p 3))
         (display (p))
         (shift k (display (p)) (set! c k))
         (display (p)))
       (display (p)))))
  ;; native : 232
  ;; meta   : 232
  ;; srfi226: 232
  ;; racket : 23#<void>
  (test* "reset/shift + temporarily + parameterize" "232"
         (with-output-to-string foo))
  ;; native : 32
  ;; meta   : ""
  ;; srfi226: ""
  ;; racket : #<void>#<void>
  (test* "reset/shift + temporarily + parameterize (cont)" "32"
         (with-output-to-string c)))

;; native : [E01][E02]
;; meta   : [E01][E02]
;; srfi226: -
;; racket : -
(gauche-only
 (test* "reset/shift + with-error-handler 1"
        "[E01][E02]"
        (with-output-to-string
          (lambda ()
            (with-error-handler
                (lambda (e) (display (~ e 'message)))
              (lambda ()
                (display "[E01]")
                (reset (error "[E02]"))
                (display "[E03]")))))))

;; native : [W01][D01][D02][W01][D01][D01][E01][D02][D02]
;; meta   : [W01][D01][D02][W01][D01][D02][D01][E01][D02][D01][D02]
;; srfi226: [W01][D01][D02][W01][D01][D02][D01][E01][D02][D01][D02]
;; racket : [W01][D01][D02][W01][D01][D01][E01][D02][D02]
(test* "reset/shift + guard 1"
       "[W01][D01][D02][W01][D01][D01][E01][D02][D02]"
       (with-output-to-string
         (lambda ()
           (define queue '())
           (define (yield) (shift k (push! queue k)))
           (push! queue (lambda ()
                          (guard (e (else (display (condition-message e))))
                            (yield)
                            (error "[E01]"))))
           (while (and (pair? queue) (pop! queue))
             => next
             (display "[W01]")
             (reset
              (dynamic-wind
                (lambda () (display "[D01]"))
                next
                (lambda () (display "[D02]"))))))))

;; native : [d01][d02][d03][d04]
;; meta   : [d01][d02][d04][d01][d03][d04]
;; srfi226: [d01][d02][d04][d01][d03][d04]
;; racket : [d01][d02][d03][d04]
(test* "dynamic-wind + reset/shift 1"
       "[d01][d02][d03][d04]"
       (with-output-to-string
         (lambda ()
           (reset
            (shift
             k
             (dynamic-wind
              (lambda () (display "[d01]"))
              (lambda () (display "[d02]")
                   (k)
                   (display "[d03]"))
              (lambda () (display "[d04]"))))))))

;; native : [d01][d02][d04][d01][d03][d04]
;; meta   : [d01][d02][d04][d01][d03][d04]
;; srfi226: [d01][d02][d04][d01][d03][d04]
;; racket : [d01][d02][d04][d01][d03][d04]
(test* "dynamic-wind + reset/shift 2"
       "[d01][d02][d04][d01][d03][d04]"
       (with-output-to-string
         (lambda ()
           (define k1 #f)
           (reset
            (dynamic-wind
             (lambda () (display "[d01]"))
             (lambda ()
               (display "[d02]")
               (shift k (set! k1 k))
               (display "[d03]"))
             (lambda () (display "[d04]"))))
           (k1))))

;; native : [d01][d02][d01][d02][d01][d02][d01][d02]
;; meta   : [d01][d02][d01][d02][d01][d02][d01][d02]
;; srfi226: [d01][d02][d01][d02][d01][d02][d01][d02]
;; racket : [d01][d02][d01][d02][d01][d02][d01][d02]
(test* "dynamic-wind + reset/shift 3"
       "[d01][d02][d01][d02][d01][d02][d01][d02]"
       (with-output-to-string
         (lambda ()
           (reset
            (define k1 #f)
            (define k2 #f)
            (reset
             (dynamic-wind
               (lambda () (display "[d01]"))
               (lambda ()
                 (shift k (set! k1 k))
                 (shift k (set! k2 k)))
               (lambda () (display "[d02]"))))
            (k1)
            (k2)
            (k2)))))

;; native : [d01][d02][d01][d11][d12][d02][d01][d11][d12][d02][d01][d11][d12][d02]
;; meta   : [d01][d02][d01][d11][d12][d02][d01][d11][d12][d02][d01][d11][d12][d02]
;; srfi226: [d01][d02][d01][d11][d12][d02][d01][d11][d12][d02][d01][d11][d12][d02]
;; racket : [d01][d02][d01][d11][d12][d02][d01][d11][d12][d02][d01][d11][d12][d02]
(test* "dynamic-wind + reset/shift 3-B"
       "[d01][d02][d01][d11][d12][d02][d01][d11][d12][d02][d01][d11][d12][d02]"
       (with-output-to-string
         (lambda ()
           (define k1 #f)
           (define k2 #f)
           (reset
            (reset
             (dynamic-wind
               (lambda () (display "[d01]"))
               (lambda ()
                 (shift k (set! k1 k))
                 (dynamic-wind
                   (lambda () (display "[d11]"))
                   (lambda () (shift k (set! k2 k)))
                   (lambda () (display "[d12]"))))
               (lambda () (display "[d02]"))))
            (k1)
            (k2)
            (k2)))))

;; native : [d01][d02][d21][d01][d11][d12][d02][d01][d11][d12][d02][d01][d11][d12][d02][d22]
;; meta   : [d01][d02][d01][d11][d12][d02][d01][d11][d12][d02][d01][d11][d12][d02]
;; srfi226: [d01][d02][d21][d22][d01][d11][d12][d02][d21][d22][d21][d22][d01][d11][d12][d02][d21][d22][d01][d11][d12][d02][d21][d22]
;; racket : [d01][d02][d21][d01][d11][d12][d02][d01][d11][d12][d02][d01][d11][d12][d02][d22]
(test* "dynamic-wind + reset/shift 3-C"
       "[d01][d02][d21][d01][d11][d12][d02][d01][d11][d12][d02][d01][d11][d12][d02][d22]"
       (with-output-to-string
         (lambda ()
           (define k1 #f)
           (define k2 #f)
           (reset
            (reset
             (dynamic-wind
               (lambda () (display "[d01]"))
               (lambda ()
                 (shift k (set! k1 k))
                 (dynamic-wind
                   (lambda () (display "[d11]"))
                   (lambda () (shift k (set! k2 k)))
                   (lambda () (display "[d12]"))))
               (lambda () (display "[d02]"))))
            (dynamic-wind
              (lambda () (display "[d21]"))
              (lambda () (k1) (k2) (k2))
              (lambda () (display "[d22]")))))))

;; native : [d01][d11][d12][d02][d11][d12]
;; meta   : [d01][d11][d12][d02][d01][d11][d12][d02]
;; srfi226: [d01][d02][d11][d12][d01][d02][d01][d02][d11][d12]
;; racket:  [d01][d11][d12][d02][d11][d12]
(test* "dynamic-wind + reset/shift 4"
       "[d01][d11][d12][d02][d11][d12]"
       (with-output-to-string
         (lambda ()
           (define k1 #f)
           (reset
            (dynamic-wind
              (lambda () (display "[d01]"))
              (lambda ()
                (reset
                 (dynamic-wind
                   (lambda () (display "[d11]"))
                   (lambda () (shift k (set! k1 k)))
                   (lambda () (display "[d12]")))))
              (lambda () (display "[d02]"))))
           (k1))))

;; native : [d01][d02][d01][d11][d12][d02][d11][d12][d11][d12]
;; meta   : [d01][d02][d01][d11][d12][d02][d01][d11][d12][d02][d01][d11][d12][d02]
;; srfi226: [d01][d02][d01][d02][d11][d12][d01][d02][d01][d02][d11][d12][d11][d12]
;; racket : [d01][d02][d01][d11][d12][d02][d11][d12][d11][d12]
(test* "dynamic-wind + reset/shift 5"
       "[d01][d02][d01][d11][d12][d02][d11][d12][d11][d12]"
       (with-output-to-string
         (lambda ()
           (define k1 #f)
           (define k2 #f)
           (define k3 #f)
           (reset
            (reset
             (dynamic-wind
               (lambda () (display "[d01]"))
               (lambda ()
                 (shift k (set! k1 k))
                 (reset
                  (dynamic-wind
                    (lambda () (display "[d11]"))
                    (lambda ()
                      (shift k (set! k2 k))
                      (shift k (set! k3 k)))
                    (lambda () (display "[d12]")))))
               (lambda () (display "[d02]"))))
            (k1)
            (k2)
            (k3)))))

;; native : [d01][d02][d11][d12][d13][d14][d03][d04]
;; meta   : [d01][d02][d11][d12][d14][d04][d01][d11][d13][d14][d03][d04]
;; srfi226: [d01][d02][d11][d12][d14][d04][d01][d11][d13][d14][d03][d04]
;; racket : [d01][d02][d11][d12][d13][d14][d03][d04]
(test* "dynamic-wind + reset/shift 6"
       "[d01][d02][d11][d12][d13][d14][d03][d04]"
       (with-output-to-string
         (lambda ()
           (reset
            (shift
             k
             (dynamic-wind
              (lambda () (display "[d01]"))
              (lambda ()
                (display "[d02]")
                (dynamic-wind
                  (lambda () (display "[d11]"))
                  (lambda ()
                    (display "[d12]")
                    (k)
                    (display "[d13]"))
                  (lambda () (display "[d14]")))
                (display "[d03]"))
              (lambda () (display "[d04]"))))))))

;; native : [d01][d02][d11][d12][d14][d04][d01][d11][d13][d14][d03][d04]
;; meta   : [d01][d02][d11][d12][d14][d04][d01][d11][d13][d14][d03][d04]
;; srfi226: [d01][d02][d11][d12][d14][d04][d01][d11][d13][d14][d03][d04]
;; racket : [d01][d02][d11][d12][d14][d04][d01][d11][d13][d14][d03][d04]
(test* "dynamic-wind + reset/shift 7"
       "[d01][d02][d11][d12][d14][d04][d01][d11][d13][d14][d03][d04]"
       (with-output-to-string
         (lambda ()
           (define k1 #f)
           (reset
            (dynamic-wind
             (lambda () (display "[d01]"))
             (lambda ()
               (display "[d02]")
               (dynamic-wind
                 (lambda () (display "[d11]"))
                 (lambda ()
                   (display "[d12]")
                   (shift k (set! k1 k))
                   (display "[d13]"))
                 (lambda () (display "[d14]")))
               (display "[d03]"))
             (lambda () (display "[d04]"))))
           (k1))))

;; native : [d01][d02][d04][d11][d12][d01][d03][d04][d13][d14]
;; meta   : [d01][d02][d04][d11][d12][d14][d01][d03][d04][d11][d13][d14]
;; srfi226: [d01][d02][d04][d11][d12][d14][d01][d03][d04][d11][d13][d14]
;; racket : [d01][d02][d04][d11][d12][d01][d03][d04][d13][d14]
(test* "dynamic-wind + reset/shift 8"
       "[d01][d02][d04][d11][d12][d01][d03][d04][d13][d14]"
       (with-output-to-string
         (lambda ()
           (define k1 #f)
           (reset
            (dynamic-wind
             (lambda () (display "[d01]"))
             (lambda ()
               (display "[d02]")
               (shift k (set! k1 k))
               (display "[d03]"))
             (lambda () (display "[d04]"))))
           (dynamic-wind
            (lambda () (display "[d11]"))
            (lambda ()
              (display "[d12]")
              (k1)
              (display "[d13]"))
            (lambda () (display "[d14]"))))))
