;;;
;;; toposort.scm - topological sorting
;;;
;;;  The original code was enhanced and became srfi-234.  We keep this
;;;  as an adapter to srfi-234 for the backward compatibility.
;;;  Since the srfi-234 reference implementation refers to this file,
;;;  we keep the original code below, within the block comment.
;;;

(define-module util.toposort
  (use srfi.234 :rename ((topological-sort srfi-234:topological-sort)))
  (export topological-sort))
(select-module util.toposort)

(define (topological-sort nodes :optional (eq eqv?))
  (or (srfi-234:topological-sort nodes eq)
      (error "graph has circular dependency")))

#|

;; The algorithm is loosely based on Algorithm T in TAOCP section 2.2.3.

(define-module util.toposort
  (use scheme.list)
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
            (and-let1 p (assoc to table eq)
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

|#
