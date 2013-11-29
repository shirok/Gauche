;;;; diff.scm -- LCS for Scheme

;;; Created:    <2002-06-21 15:36:46 foof>
;;; Time-stamp: <2003-02-15 00:09:55 foof>
;;; Author:     Alex Shinn <foof@synthcode.com>

(define-module text.diff
  (use util.lcs)
  (export diff diff-report)
  )
(select-module text.diff)

;; aux. fun to convert arg to a list
(define (source->list src reader)
  (port->list reader
              (cond [(port? src) src]
                    [(string? src) (open-input-string src)]
                    [else (error "don't know how to diff from:" src)])))

;; lcs on text.  Returns edit-list (as defined in lcs-edit-list).
(define (diff a b :key (reader read-line) (equal equal?))
  (lcs-edit-list (source->list a reader)
                 (source->list b reader)
                 equal))

(define (write-line-diff line type)
  (case type
    [(+)  (format #t "+ ~A\n" line)]
    [(-)  (format #t "- ~A\n" line)]
    [else (format #t "  ~A\n" line)]))

(define (diff-report a b :key (writer write-line-diff)
                              (reader read-line) (equal equal?))
  (lcs-fold (^[line _] [writer line '-])
            (^[line _] (writer line '+))
            (^[line _] (writer line #f))
            #f
            (source->list a reader)
            (source->list b reader)
            equal))

