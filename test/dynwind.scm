;;
;; Test dynamic-wind, call/cc and related stuff
;;

;; $Id: dynwind.scm,v 1.5 2001-03-10 07:51:49 shiro Exp $

(add-load-path "../lib")
(use tester.tester)

(test-start "dynamic-wind and call/cc")

;;------------------------------------------------------------------------

(define c #f)

;; An example in R5RS
(define (dynwind-test1)
  (let ((path '()))
    (let ((add (lambda (s) (set! path (cons s path)))))
      (dynamic-wind
       (lambda () (add 'connect))
       (lambda ()
         (add (call-with-current-continuation
               (lambda (c0) (set! c c0) 'talk1))))
       (lambda () (add 'disconnect)))
      (if (< (length path) 4)
          (c 'talk2)
          (reverse path)))))

(test "dynamic-wind"
      '(connect talk1 disconnect connect talk2 disconnect)
      dynwind-test1)

;; Test for handler stack.
(define (dynwind-test2)
  (let ((path '()))
    (dynamic-wind
     (lambda () (set! path (cons 1 path)))
     (lambda () (set! path (append (dynwind-test1) path)))
     (lambda () (set! path (cons 3 path))))
    path))

(test "dynamic-wind"
      '(3 connect talk1 disconnect connect talk2 disconnect 1)
      dynwind-test2)

;;-----------------------------------------------------------------------
;; Test for continuation with the saved environment is altered.

(define (callcc-test1)
  (let ((r '()))
    (let ((w (let ((v 1))
               (set! v (+ (call-with-current-continuation
                           (lambda (c0) (set! c c0) v))
                          v))
               (set! r (cons v r))
               v)))
      (if (<= w 1024) (c w) r))))

(test "call/cc" '(2048 1024 512 256 128 64 32 16 8 4 2)
      callcc-test1)

;;-----------------------------------------------------------------------
;; See if port stuff is cleaned up properly

(test "call-with-output-file -> port-closed?"
      #t
      (lambda ()
        (let ((p #f))
          (call-with-output-file
              "tmp1.o"
              (lambda (port)
                (write '(a b c d e) port)
                (set! p port)))
          (port-closed? p))))

(test "call-with-input-file -> port-closed?"
      '(#t a b c d e)
      (lambda ()
        (let* ((p #f)
               (r (call-with-input-file "tmp1.o"
                    (lambda (port)
                      (set! p port)
                      (read port)))))
          (cons (port-closed? p) r))))

(test "with-output-to-file -> port-closed?"
      '(#t #f)
      (lambda ()
        (let ((p #f))
          (with-output-to-file "tmp1.o"
            (lambda ()
              (set! p (current-output-port))
              (write '(a b c d e))))
          (list (port-closed? p)
                (eq? p (current-output-port))))))

(test "with-input-from-file -> port-closed?"
      '(#t #f)
      (lambda ()
        (let* ((p #f)
               (r (with-input-from-file "tmp1.o"
                    (lambda ()
                      (set! p (current-input-port))
                      (read)))))
          (list (port-closed? p)
                (eq? p (current-input-port))))))

(test-end)
