;;;
;;; srfi-196 - Range objects
;;;

;; data.range is a superset of srfi-196.
;; We simply re-export srfi-196 bindings.

(define-module srfi-196
  (use data.range)
  (export
   range numeric-range iota-range vector-range string-range
   range-append
   range? range=?
   range-length range-ref range-first range-last
   range-split-at subrange range-segment
   range-take range-take-right
   range-drop range-drop-right
   range-count range-any
   range-every range-map range-map->list range-map->vector
   range-for-each range-filter-map range-filter-map->list
   range-filter range-filter->list range-remove range-remove->list
   range-fold range-fold-right
   range-reverse
   range-index range-index-right
   range-take-while range-take-while-right
   range-drop-while range-drop-while-right
   range->list range->vector range->string
   vector->range range->generator))
