;; test gauche.selector

(cond-expand
 [(or (not gauche.sys.select) gauche.os.windows)
  ;; gauche.selector doesn't work unless sys-select is supported.
  ;; Besides, on windows, sys-select only works for socket fds, so
  ;; the tests here won't work.  We skip in those cases.
  (exit 0)]
 [else])

(use gauche.test)

(test-start "selector")
(use gauche.selector)
(test-module 'gauche.selector)

(define *sel* #f)
(define-values (*p0* *p1*) (sys-pipe))
(define-values (*q0* *q1*) (sys-pipe))

(define *x* #f)
(define *y* #f)

(define (set-x port flags)
  (case flags
    ((r) (set! *x* (read port)))
    ((w) (write '(xxx) port) (flush port))))

  
(define (set-y port flags)
  (case flags
    ((r) (set! *y* (read port)))
    ((w) (write '(yyy) port) (flush port))))

(test* "make" #t
       (begin (set! *sel* (make <selector>))
              (is-a? *sel* <selector>)))

(test* "selector-add!" #f
       (begin
         (selector-add! *sel* *p0* set-x '(r))
         *x*))

(test* "selector-select" '(foo)
       (begin
         (write '(foo) *p1*)
         (flush *p1*)
         (selector-select *sel*)
         *x*))

(test* "selector-add!" #f
       (begin
         (selector-add! *sel* *q0* set-y '(r))
         *y*))

(test* "selector-select" '(bar baz)
       (begin
         (write '(bar baz) *q1*)
         (flush *q1*)
         (selector-select *sel* '(1 0))
         *y*))

(test* "selector-delete! (by port)" '(foo)
       (begin
         (selector-delete! *sel* *p0* #f #f)
         (write '(zzz) *p1*)
         (flush *p1*)
         (selector-select *sel* 0)
         *x*))

(test* "selector-delete! (by proc)" '(bar baz)
       (begin
         (selector-delete! *sel* #f set-y #f)
         (write '(yyy) *q1*)
         (flush *q1*)
         (selector-select *sel* 0)
         *y*))

(test* "selector-select (flags)" '(((zzz) (yyy))
                                   ((xxx) (yyy)))
       (begin
         (selector-add! *sel* *p0* set-x '(r))
         (selector-add! *sel* *q0* set-y '(r))
         (selector-add! *sel* *p1* set-x '(w))
         (selector-add! *sel* *q1* set-y '(w))
         (selector-select *sel*)
         (let ((a (list *x* *y*)))
           (selector-select *sel*)
           (selector-select *sel* 0)
           (list a (list *x* *y*)))))

(test* "selector-delete! (flags)" '((xxx) (yyy))
       (begin
         (write '(aaa) *p1*) (flush *p1*)
         (write '(bbb) *q1*) (flush *q1*)
         (selector-delete! *sel* #f #f '(r))
         (selector-select *sel* 0)
         (list *x* *y*)))

(test-end)
