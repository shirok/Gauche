;;;
;;; generic version of sort utilities
;;;

;; This is a part of gauche.sortutil (autoloaded module)
;; Splitted to avoid circular dependency.  Users shouldn't use
;; this module directly.

(define-module gauche.generic-sortutil
  (use srfi-42)
  (use gauche.sequence)
  (export %generic-sorted? %generic-sort %generic-sort!))
(select-module gauche.generic-sortutil)

(define (%generic-sorted? seq less? key)
  ($ call-with-iterator seq
     (^[end? next]
       (or (end?)
           (let loop ([last (key (next))])
             (or (end?)
                 (let1 current (key (next))
                   (and (not (less? current last))
                        (loop current)))))))))

(define (%generic-sort seq less? :optional (key #f))
  (if key
    ($ map-to (class-of seq) car
       $ stable-sort (map (^e (cons e (key e))) seq)
       (^[a b] (less? (cdr a) (cdr b))))
    (coerce-to (class-of seq) (stable-sort (coerce-to <list> seq) less?))))

(define (%generic-sort! seq less? :optional (key #f))
  (if key
    (do-ec (:parallel
            (: p (stable-sort! (map (^e (cons e (key e))) seq)
                               (^[a b] (less? (cdr a) (cdr b)))))
            (:integers i))
           (set! (ref seq i) (car p)))    
    (do-ec (:parallel
            (: elt (stable-sort! (coerce-to <list> seq) less?))
            (:integers i))
           (set! (ref seq i) elt)))
  seq)
