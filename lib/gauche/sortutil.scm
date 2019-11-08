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
;;; modified to adopt srfi-95, and handle <sequence> objects.

;; To be autoloaded
(define-module gauche.sortutil
  (export sorted? merge merge! sort sort!
          stable-sort stable-sort!
          sort-by sort-by! stable-sort-by stable-sort-by!
          ))
(select-module gauche.sortutil)

;; we factor out generic versions to a separate file, to avoid
;; inadvertent circular dependency with gauche.seqeunce.
(autoload gauche.generic-sortutil
          %generic-sorted? %generic-sort %generic-sort!)

(define %sort  (with-module gauche.internal %sort))
(define %sort! (with-module gauche.internal %sort!))

(define-syntax define-less?
  (syntax-rules ()
    [(_ less? cmp this)
     (define less?
       (cond [(comparator? cmp) (^[a b] (< (comparator-compare cmp a b) 0))]
             [(not cmp) (^[a b] (< (compare a b) 0))]
             [(applicable? cmp <bottom> <bottom>) cmp]
             [else (errorf "~a requires a comparator or a procedure that \
                            takes two-arguments, but got: ~s" this cmp)]))]))

;;; (sorted? sequence :optional less? key)

(define (sorted? seq :optional (cmp #f) (key identity))
  (define-less? less? cmp 'sorted?)
  (cond
   [(null? seq) #t]
   [(vector? seq)
    (let1 n (vector-length seq)
      (if (<= n 1)
        #t
        (let loop ([i 1]
                   [last (key (vector-ref seq 0))])
          (or (= i n)
              (let1 next (key (vector-ref seq i))
                (and (not (less? next last))
                     (loop (+ i 1) next)))))))]
   [(pair? seq)  ; avoid list? for scanning the entire list
    (let loop ([last (key (car seq))] [rest (cdr seq)])
      (or (null? rest)
          (let1 next (key (car rest))
            (and (not (less? next last))
                 (loop next (cdr rest))))))]
   [(string? seq)
    (if (<= (string-length seq) 1)
      #t
      (let* ([p (open-input-string seq)]
             [last (key (read-char p))])
        (let loop ([last last] [next (read-char p)])
          (or (eof-object? next)
              (let1 knext (key next)
                (and (not (less? knext last))
                     (loop knext (read-char p))))))))]
   [(is-a? seq <sequence>) (%generic-sorted? seq less? key)]
   [else (error "seqeuence required, but got:" seq)]))

;;; (merge a b less?)
;;; takes two lists a and b such that (sorted? a less?) and (sorted? b less?)
;;; and returns a new list in which the elements of a and b have been stably
;;; interleaved so that (sorted? (merge a b less?) less?).
;;; Note:  this does _not_ accept vectors.  See below.

(define (merge a b :optional (cmp #f) (key identity))
  (define-less? less? cmp 'merge)
  (cond
   [(null? a) b]
   [(null? b) a]
   [else (let loop ([x (car a)] [kx (key (car a))] [a (cdr a)]
                    [y (car b)] [ky (key (car b))] [b (cdr b)])
           ;; The loop handles the merging of non-empty lists.  It has
           ;; been written this way to save testing and car/cdring.
           (if (less? ky kx)
             (if (null? b)
               (cons y (cons x a))
               (cons y (loop x kx a (car b) (key (car b)) (cdr b))))
             ;; x <= y
             (if (null? a)
               (cons x (cons y b))
               (cons x (loop (car a) (key (car a)) (cdr a) y ky b)))))]))

;;; (merge! a b less?)
;;; takes two sorted lists a and b and smashes their cdr fields to form a
;;; single sorted list including the elements of both.
;;; Note:  this does _not_ accept vectors.

(define (merge! a b :optional (cmp #f) (key identity))
  (define-less? less? cmp 'merge!)
  (define (loop r a kx b ky)
    (if (less? ky kx)
      (begin
        (set-cdr! r b)
        (if (null? (cdr b))
          (set-cdr! b a)
          (loop b a kx (cdr b) (key (cadr b)))))
      (begin
        (set-cdr! r a)
        (if (null? (cdr a))
          (set-cdr! a b)
          (loop a (cdr a) (key (cadr a)) b ky)))))
  (cond
   [(null? a) b]
   [(null? b) a]
   [else (let ([kx (key (car a))]
               [ky (key (car b))])
           (if (less? ky kx)
             (begin
               (if (null? (cdr b))
                 (set-cdr! b a)
                 (loop b a kx (cdr b) (key (cadr b))))
               b)
             (begin
               (if (null? (cdr a))
                 (set-cdr! a b)
                 (loop a (cdr a) (key (cadr a)) b ky))
               a)))]))

;;; (sort! sequence :optional less? key)
;;; sorts the list or vector sequence destructively.  It uses a version
;;; of merge-sort invented, to the best of my knowledge, by David H. D.
;;; Warren, and first used in the DEC-10 Prolog system.  R. A. O'Keefe
;;; adapted it to work destructively in Scheme.

(define (sort! seq . args)
  (if (and (or (pair? seq) (vector? seq)) (null? args))
    (%sort! seq)                  ; use internal version
    (apply stable-sort! seq args)))

(define (stable-sort! seq :optional (cmp #f) (key identity))
  (let1 sorted (%stable-sort! seq cmp key)
    (if (and (pair? sorted) (not (eq? sorted seq)))
      ;; %stable-sort! on a list may return a cell that's not the same
      ;; cell as the head of input.  We have to ensure we preserve the
      ;; identity.
      (let loop ([p sorted])
        (if (eq? (cdr p) seq)
          (let ([sorted-car (car sorted)]
                [sorted-cdr (cdr sorted)]
                [seq-car (car seq)]
                [seq-cdr (cdr seq)])
            (set! (car sorted) seq-car)
            (set! (cdr sorted) seq-cdr)
            (set! (car seq) sorted-car)
            (if (eq? p sorted)
              (set! (cdr seq) sorted)
              (begin
                (set! (cdr seq) sorted-cdr)
                (set! (cdr p) sorted)))
            seq)
          (loop (cdr p))))
      sorted)))

;; Internal stable sorter.  If key is identity we use merge sort
;; straightforwardly.  Otherwise, we extract keys first, sort
;; by the key, and strip the keys.
;; Note that if SEQ is a list, the returned cell doesn't need to
;; be the same cell as the head of SEQ.
(define (%stable-sort! seq :optional (cmp #f) (key identity))
  (define-less? less? cmp 'sort!)
  (if (memq key `(,identity ,values))
    ;; shortcut
    (letrec ([step (^n (cond [(> n 2) (let* ([j (ash n -1)]
                                             [a (step j)]
                                             [k (- n j)]
                                             [b (step k)])
                                        (merge! a b less?))]
                             [(= n 2) (let ([x (car seq)]
                                            [y (cadr seq)]
                                            [p seq])
                                        (set! seq (cddr seq))
                                        (when (less? y x)
                                          (set-car! p y)
                                          (set-car! (cdr p) x))
                                        (set-cdr! (cdr p) '())
                                        p)]
                             [(= n 1) (let ([p seq])
                                        (set! seq (cdr seq))
                                        (set-cdr! p '())
                                        p)]
                             [else '()]))])
      (cond [(null? seq) seq]
            [(pair? seq) (step (length seq))]
            [(vector? seq)
             (let ([n (vector-length seq)]
                   [vector seq])
               (set! seq (vector->list seq))
               (do ([p (step n) (cdr p)]
                    [i 0 (+ i 1)])
                   [(null? p) vector]
                 (vector-set! vector i (car p))))]
            [(is-a? seq <sequence>) (%generic-sort! seq less?)]
            [else (error "sequence required, but got:" seq)]))
    ;; Avoid making intermediate structure, for the point of stable-sort!
    ;; is to avoid allocation.
    (letrec ([kless? (^[a b] (less? (cdr a) (cdr b)))])
      (cond [(null? seq) seq]
            [(pair? seq)
             (do ([spine seq (cdr spine)])
                 [(null? spine)]
               (set-car! spine (cons (car spine) (key (car spine)))))
             (let1 spine (%stable-sort! seq kless?)
               (do ([lis spine (cdr lis)])
                   [(null? lis)]
                 (set-car! lis (caar lis)))
               spine)]
            [(vector? seq)
             (let1 len (vector-length seq)
               (do ([i 0 (+ i 1)])
                   [(= i len)]
                 (vector-set! seq i (cons (vector-ref seq i)
                                          (key (vector-ref seq i)))))
               (do ([seq (%stable-sort! seq kless?)]
                    [i 0 (+ i 1)])
                   [(= i len)]
                 (vector-set! seq i (car (vector-ref seq i))))
               seq)]
            [(is-a? seq <sequence>) (%generic-sort! seq less? key)]
            [else (error "sequence required, but got:" seq)]))))

;;; (sort sequence less?)
;;; sorts a vector or list non-destructively.  It does this by sorting a
;;; copy of the sequence.

(define (sort seq . args)
  (if (and (or (pair? seq) (vector? seq)) (null? args))
    (%sort seq)  ;; use internal version
    (apply stable-sort seq args)))

(define (stable-sort seq :optional (cmp #f) (key identity))
  (define-less? less? cmp 'sort)
  (if (memq key `(,identity ,values))
    (cond [(null? seq) seq]
          [(pair? seq) (%stable-sort! (list-copy seq) less?)]
          [(vector? seq) (list->vector (sort! (vector->list seq) less?))]
          [(is-a? seq <sequence>) (%generic-sort seq less?)]
          [else (error "sequence required, but got:" seq)])
    (cond [(null? seq) seq]
          [(pair? seq) (%stable-sort! (list-copy seq) less? key)]
          [(vector? seq) (%stable-sort! (vector-copy seq) less? key)]
          [(is-a? seq <sequence>) (%generic-sort seq less? key)]
          [else (error "sequence required, but got:" seq)])))

;; For the backward compatibility
(define (sort-by seq key :optional (cmp #f)) (sort seq cmp key))
(define stable-sort-by sort-by)
(define (sort-by! seq key :optional (cmp #f)) (sort! seq cmp key))
(define stable-sort-by! sort-by!)

