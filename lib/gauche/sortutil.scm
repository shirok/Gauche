;;;
;;; Public Domain.
;;;
;;; sorted?, merge, merge!, sort, sort!, stable-sort and stable-sort!
;;; are written by Richard A. O'Keefe (based on Prolog code by D.H.D.Warren).
;;; See sort.orig.scm for the original public domain code, with long
;;; explanatory comments.
;;;
;;; sort-by family is addition by SK.
;;;

;; To be autoloaded
(define-module gauche.sortutil
  (export sorted? merge merge! sort sort!
          stable-sort stable-sort!
          sort-by sort-by! stable-sort-by stable-sort-by!
          ))
(select-module gauche.sortutil)

(define (default-less? x y)
  (< (compare x y) 0))

;;; (sorted? sequence less?)
;;; is true when sequence is a list (x0 x1 ... xm) or a vector #(x0 ... xm)
;;; such that for all 1 <= i <= m,
;;;	(not (less? (list-ref list i) (list-ref list (- i 1)))).

(define (sorted? seq less?)
  (cond
   ((null? seq) #t)
   ((vector? seq)
    (let ((n (vector-length seq)))
      (if (<= n 1)
        #t
        (do ((i 1 (+ i 1)))
            ((or (= i n)
                 (less? (vector-ref seq (- i 1))
                        (vector-ref seq i)))
             (= i n)) )) ))
   (else
    (let loop ((last (car seq)) (next (cdr seq)))
      (or (null? next)
          (and (not (less? (car next) last))
               (loop (car next) (cdr next)) )) )) ))

;;; (merge a b less?)
;;; takes two lists a and b such that (sorted? a less?) and (sorted? b less?)
;;; and returns a new list in which the elements of a and b have been stably
;;; interleaved so that (sorted? (merge a b less?) less?).
;;; Note:  this does _not_ accept vectors.  See below.

(define (merge a b less?)
  (cond
   ((null? a) b)
   ((null? b) a)
   (else (let loop ((x (car a)) (a (cdr a)) (y (car b)) (b (cdr b)))
           ;; The loop handles the merging of non-empty lists.  It has
           ;; been written this way to save testing and car/cdring.
           (if (less? y x)
             (if (null? b)
               (cons y (cons x a))
               (cons y (loop x a (car b) (cdr b)) ))
             ;; x <= y
             (if (null? a)
               (cons x (cons y b))
               (cons x (loop (car a) (cdr a) y b)) )) )) ))

;;; (merge! a b less?)
;;; takes two sorted lists a and b and smashes their cdr fields to form a
;;; single sorted list including the elements of both.
;;; Note:  this does _not_ accept vectors.

(define (merge! a b less?)
  (define (loop r a b)
    (if (less? (car b) (car a))
      (begin
        (set-cdr! r b)
        (if (null? (cdr b))
          (set-cdr! b a)
          (loop b a (cdr b)) ))
      ;; (car a) <= (car b)
      (begin
        (set-cdr! r a)
        (if (null? (cdr a))
          (set-cdr! a b)
          (loop a (cdr a) b)) )) )
  (cond
   ((null? a) b)
   ((null? b) a)
   ((less? (car b) (car a))
    (if (null? (cdr b))
      (set-cdr! b a)
      (loop b a (cdr b)))
    b)
   (else ; (car a) <= (car b)
    (if (null? (cdr a))
      (set-cdr! a b)
      (loop a (cdr a) b))
    a)))

;;; (sort! sequence less?)
;;; sorts the list or vector sequence destructively.  It uses a version
;;; of merge-sort invented, to the best of my knowledge, by David H. D.
;;; Warren, and first used in the DEC-10 Prolog system.  R. A. O'Keefe
;;; adapted it to work destructively in Scheme.

(define (sort! seq . maybe-less?)
  (if (null? maybe-less?)
    (%sort! seq) ;; use internal version
    (stable-sort! seq (car maybe-less?))))

(define (stable-sort! seq :optional (less? default-less?))
  (define (step n)
    (cond
     [(> n 2) (let* ((j (ash n -1))
                     (a (step j))
                     (k (- n j))
                     (b (step k)))
                (merge! a b less?))]
     [(= n 2) (let ((x (car seq))
                    (y (cadr seq))
                    (p seq))
                (set! seq (cddr seq))
                (when (less? y x)
                  (set-car! p y)
                  (set-car! (cdr p) x))
                (set-cdr! (cdr p) '())
                p)]
     [(= n 1) (let ((p seq))
                (set! seq (cdr seq))
                (set-cdr! p '())
                p)]
     [else '()]))
  (if (vector? seq)
    (let ((n (vector-length seq))
          (vector seq))
      (set! seq (vector->list seq))
      (do ((p (step n) (cdr p))
           (i 0 (+ i 1)))
          ((null? p) vector)
        (vector-set! vector i (car p)) ))
    ;; otherwise, assume it is a list
    (step (length seq)) ))

;;; (sort sequence less?)
;;; sorts a vector or list non-destructively.  It does this by sorting a
;;; copy of the sequence.

(define (sort seq . maybe-less?)
  (if (null? maybe-less?)
    (%sort seq)  ;; use internal version
    (stable-sort seq (car maybe-less?))))

(define (stable-sort seq :optional (less? default-less?))
  (if (vector? seq)
    (list->vector (sort! (vector->list seq) less?))
    (sort! (list-copy seq) less?)))

;;;
;;; (sort-by seq key :optional less?)
;;;

(define (%make-cmp less?)
  (if less?
    (lambda (a b) (less? (cdr a) (cdr b)))
    (lambda (a b) (< (compare (cdr a) (cdr b)) 0))))

(define (sort-by seq key :optional (less? #f))
  (if (vector? seq)
    (list->vector (sort-by (vector->list seq) key less?))
    (map car (stable-sort (map (lambda (e) (cons e (key e))) seq)
                          (%make-cmp less?)))))

(define stable-sort-by sort-by)

(define (sort-by! seq key :optional (less? #f))
  (cond [(vector? seq)
         (let1 len (vector-length seq)
           (do ([i 0 (+ i 1)])
               [(= i len)]
             (vector-set! seq i (cons (vector-ref seq i)
                                      (key (vector-ref seq i)))))
           (do ([seq (stable-sort! seq (%make-cmp less?))]
                [i 0 (+ i 1)])
               [(= i len)]
             (vector-set! seq i (car (vector-ref seq i))))
           seq)]
        [else
         (do ([spine seq (cdr spine)])
             [(null? spine)]
           (set-car! spine (cons (car spine) (key (car spine)))))
         (let1 spine (stable-sort! seq (%make-cmp less?))
           (do ([lis spine (cdr lis)])
               [(null? lis)]
             (set-car! lis (caar lis)))
           spine)]))

(define stable-sort-by! sort-by!)

(provide "gauche/sortutil")

