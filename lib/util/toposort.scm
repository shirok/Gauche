;;;
;;; toposort.scm - topological sorting
;;;
;;;  Written by Shiro Kawai (shiro@acm.org)  2001
;;;  Public Domain..  I guess lots of Scheme programmers have already
;;;  written similar code.
;;;

(define-module util.toposort
  (use srfi-1)
  (export topological-sort)
  )
(select-module util.toposort)

;; (topological-sort nodes &optional =)
;;
;;  nodes : a list of (<from> <to0> <to1> ...)

(define (topological-sort nodes :optional (eq eqv?))
  (define table (map (^n (cons (car n) 0)) nodes))
  (define queue '())
  (define result '())

  ;; set up - compute number of nodes that each node depends on.
  (define (set-up)
    (dolist [node nodes]
      (dolist [to (cdr node)]
        (if-let1 p (assoc to table eq)
          (inc! (cdr p))
          (push! table (cons to 1))))))

  ;; traverse
  (define (traverse)
    (unless (null? queue)
      (let1 n0 (assoc (pop! queue) nodes eq)
        (when n0
          (dolist [to (cdr n0)]
            (if-let1 p (assoc to table eq)
              (let1 cnt (- (cdr p) 1)
                (when (= cnt 0)
                  (push! result to)
                  (push! queue to))
                (set! (cdr p) cnt)))))
        (traverse))))

  (set-up)
  (set! queue (append-map (^p (if (= (cdr p) 0) (list (car p)) '())) table))
  (set! result queue)
  (traverse)
  (let1 rest (filter (^e (not (zero? (cdr e)))) table)
    (unless (null? rest)
      (error "graph has circular dependency" (map car rest))))
  (reverse result))

