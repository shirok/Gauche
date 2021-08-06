;;;; diff.scm -- LCS for Scheme

;;; Created:    <2002-06-21 15:36:46 foof>
;;; Time-stamp: <2003-02-15 00:09:55 foof>
;;; Author:     Alex Shinn <foof@synthcode.com>

(define-module text.diff
  (use util.lcs)
  (use util.match)
  (export diff diff-report diff-report/context diff-report/unified)
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

(define (diff-report/context a b :key (writer write-line-diff)
                                      (reader read-line)
                                      (equal equal?)
                                      (context-size (undefined)))
  (define (show-lines half-hunk header-lead header-tail)
    (match-let1 (start end . lines) half-hunk
      ;; Traditionally, line number is 1-origin, and both ends are inclusive,
      ;; so we need adjustment.  As a special case, if (= start end),
      ;; we only show a single index (it only occurs when that side is empty).
      (if (= start end)
        (format #t "~a 0 ~a\n" header-lead header-tail)
        (format #t "~a ~d,~d ~a\n" header-lead (+ 1 start) end header-tail))
      ;; If the lines are all unchanged, we don't emit it, following
      ;; the context-diff format.
      (unless (every (^e (eq? (car e) '=)) lines)
        (dolist [line lines]
          (match line
            [('= x) (format #t "  ~a\n" x)]
            [('- x) (format #t "- ~a\n" x)]
            [('+ x) (format #t "+ ~a\n" x)]
            [('! x) (format #t "! ~a\n" x)])))))

  (dolist [hunk (lcs-edit-list/context (source->list a reader)
                                       (source->list b reader)
                                       equal
                                       :context-size context-size)]
    (print "***************")
    (show-lines (~ hunk 0) "***" "****")
    (show-lines (~ hunk 1) "---" "----")))

(define (diff-report/unified a b :key (writer write-line-diff)
                                      (reader read-line)
                                      (equal equal?)
                                      (context-size (undefined)))
  (define (show-line start len)
    (if (= len 1)
      (format #t "~d" (+ start 1))
      (format #t "~d,~d" (if (= len 0) 0 (+ start 1)) len)))

  (define (show-hunk hunk)
    (match-let1 #(as alen bs blen lines) hunk
      (display "@@ -")
      (show-line as alen)
      (display " +")
      (show-line bs blen)
      (display " @@\n")
      (dolist [line lines]
        (match line
            [('= x) (format #t " ~a\n" x)]
            [('- x) (format #t "-~a\n" x)]
            [('+ x) (format #t "+~a\n" x)]))))
  (for-each show-hunk
            (lcs-edit-list/unified (source->list a reader)
                                   (source->list b reader)
                                   equal
                                   :context-size context-size)))
