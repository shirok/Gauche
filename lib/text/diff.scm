;;;; diff.scm -- LCS for Scheme

;;; Created:    <2002-06-21 15:36:46 foof>
;;; Time-stamp: <2003-02-15 00:09:55 foof>
;;; Author:     Alex Shinn <foof@synthcode.com>

(define-module text.diff
  (use util.lcs)
  (export diff diff-report)
  )
(select-module text.diff)

;; lcs on text, generates lists from ports (or strings)
(define (diff a b . options)
  (define (source->list x lx)
    (port->list
     lx (cond ((port? x) x)
              ((string? x) (open-input-string x))
              (else (error "don't know how to diff from:" x)))))
  (let-keywords* options ((reader read-line)
                          (equal equal?))
    (let ((a-ls (source->list a reader))
          (b-ls (source->list b reader)))
      ;;(debug "a: ~S\nb: ~S\n" a-ls b-ls)
      (list a-ls b-ls (lcs-with-positions a-ls b-ls)))))

(define (write-line-diff line type)
  (case type
    ((add)
     (format #t "+~A\n" line))
    ((remove)
     (format #t "-~A\n" line))
    ((same)
     (format #t " ~A\n" line))
    (else (error "unknown diff type:" type))))

(define (diff-report a b . options)
  (let-keywords* options ((writer write-line-diff))
    (let* ((r (apply diff (append (list a b) options)))
           (a-ls (car r))
           (b-ls (cadr r))
           (diff-res (caddr r))
           (d-ls (cadr diff-res)))
      ;; context diff
      ;;(debug "diff: ~S" r)
      (let loop ((d d-ls) (a a-ls) (a-pos 0) (b b-ls) (b-pos 0))
        (unless (null? d)
          (let* ((d1 (car d))
                 (a-off (cadr d1))
                 (a-skip (- a-off a-pos))
                 (b-off (caddr d1))
                 (b-skip (- b-off b-pos)))
            ;;(debug "a-off: ~S a-pos: ~S a-skip: ~S" a-off a-pos a-skip)
            (let-values (((a-head a-tail) (split-at a a-skip))
                         ((b-head b-tail) (split-at b b-skip)))
              ;;(debug "a-head: ~S a-tail: ~S" a-head a-tail)
              ;; lines only in a have been removed
              (if (pair? a-head)
                (for-each (cut writer <> 'remove) (cdr a-head)))
              ;; lines only in b have been added
              (if (pair? b-head)
                (for-each (cut writer <> 'add) (cdr b-head)))
              ;; reprint this common line
              (writer (car d1) 'same)
              ;; recurse
              (loop (cdr d) a-tail a-off b-tail b-off))))))))

(provide "text/diff")
