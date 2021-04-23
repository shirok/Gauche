(import (scheme base)
        (timer)
        (srfi 18)
        (srfi 19))

(cond-expand
 (gauche
  (import (rename (only (gauche test) test-start test-section test* test-end)
                  (test-start test-begin)
                  (test-section test-group)))
  (begin
    (define-syntax test-equal
      (syntax-rules ()
        ((_ name expect expr)
         (test* name expect expr))
        ((_ expect expr)
         (test-equal 'expr expect expr))))
    (define-syntax test-assert
      (syntax-rules ()
        ((_ name expr)
         (test* name #t expr (lambda (e r) (and e r))))
        ((_ expect expr)
         (test-assert 'expr expr))))
    (define-syntax test-error
      (syntax-rules ()
        ((_ name condition? expr)
         (test-assert name
          (guard (e (else (condition? e)))
            expr
            #f)))))))
 (else (import (srfi 64))))

(test-begin "Timer")

(test-assert "timer?" (timer? (make-timer)))

(let ((timer (make-timer)))
  ;; (test-assert "timer-start!" (timer? (timer-start! timer)))

  (test-assert "timer-schedule! (1)"
               (integer? (timer-schedule! timer (lambda () 1) 0)))
  (test-assert "timer-schedule! (2)"
               (integer? (timer-schedule! timer (lambda () 2)
                                          (make-timer-delta 0 'm))))

  (let* ((ls '())
         (id (timer-schedule! timer
                              (lambda ()
                                (thread-sleep! 0.5) ;; remove timing issue
                                (set! ls (cons 'a ls)))
                              0 500)))
    ;; run at least 3 times
    (test-assert "timer-task-exists? (1)" (timer-task-exists? timer id))
    (thread-sleep! 1)
    (test-assert "timer-task-remove!" (timer-task-remove! timer id))
    ;; (print (timer-task-exists? timer id))
    (test-assert "timer-task-exists? (2)" (not (timer-task-exists? timer id)))
    ;; this depends on timing thing.
    ;; (test-assert "result" (or (equal? ls '(a a a)) (equal? ls '(a a a a))))
    (test-assert "timer-cancel!" (timer-cancel! timer)))
  )

(let* ((handled #f)
       (timer (make-timer (lambda (e) (set! handled e)))))
  ;; (test-assert "timer-start!" (timer? (timer-start! timer)))

  (test-assert "timer-schedule! (3)"
               (integer? (timer-schedule! timer (lambda ()  (raise 'dummy))
                                          (make-timer-delta 0 'm))))
  (thread-sleep! 0.1) ;; wait a bit

  (test-equal "error-handling" 'dummy handled)

  (test-assert "timer-cancel!" (timer-cancel! timer))
  )

;; error case
(let ((timer (make-timer)))
  (test-error "timer-schedule! (negative first)" error-object?
              (timer-schedule! timer (lambda () 1) -1))
  (test-error "timer-schedule! (negative period)" error-object?
              (timer-schedule! timer (lambda () 1) 0 -1)))

;; reschedule

(let ((a '()))
  (define timer (make-timer))
  (define id (timer-schedule! timer (lambda () (set! a (cons 1 a))) 600))

  (timer-schedule! timer (lambda () (set! a (cons 2 a))) 400)
  ;; reschedule
  (timer-reschedule! timer id 300 0)
  (thread-sleep! 0.5) ;; wait 500ms
  ;; first one must be executed first so 2 1
  (test-equal "reschedule" '(2 1) a)
  )

;; timer-delta
(let ((timer (make-timer)))
  ;; (timer-start! timer)
  (let* ((ls '())
         (id (timer-schedule! timer
                              (lambda () (set! ls (cons 'a ls)))
                              0
                              ;; 500 ms (in nsec)
                              (make-timer-delta 500000000 'ns))))
    ;; run at least 2 times
    (thread-sleep! 1)
    (timer-reschedule! timer id 300 (make-timer-delta 0 'ns))
    (test-assert "result" (>= (length ls) 2))
    (thread-sleep! 0.5)
    (test-assert "removed" (not (timer-task-exists? timer id)))
    (test-assert "timer-cancel!" (timer-cancel! timer))))

(test-end)
