;;;
;;; info.scm - parse info file
;;;
;;;  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: info.scm,v 1.1 2002-07-10 21:55:58 shirok Exp $
;;;

(define-module text.info
  (use srfi-13)
  (use srfi-14)
  (use text.parse)
  (use gauche.process)
  (use file.util)
  (export <info-file> <info-node>
          open-info-file info-get-node info-parse-menu
          )
  )
(select-module text.info)

;; NB: node-table maps node name (string) => <info-node> or
;; subinfo-file (string).
(define-class <info-file> ()
  ((path           :init-keyword :path)
   (directory      :init-keyword :directory)
   (node-table     :init-form (make-hash-table 'string=?))
   ))

(define-class <info-node> ()
  ((name    :init-keyword :name)
   (next    :init-keyword :next)
   (prev    :init-keyword :prev)
   (up      :init-keyowrd :up)
   (file    :init-keyword :file)
   (content :init-keyword :content)
   ))

;; Read an info file FILE, and returns a list of strings splitted by
;; ^_ (#\x1f)

(define (read-info-file-split file opts)
  (define (with-input-from-info thunk)
    (if (string-suffix? ".gz" file)
        (with-input-from-process #`"gunzip -c ,file" thunk)
        (with-input-from-file file thunk)))
  
  (with-input-from-info
   (lambda ()
     (let loop ((c (skip-while (char-set-complement #[\x1f])))
                (r '()))
       (if (eof-object? c)
           (reverse! r)
           (let* ((head (next-token #[\x1f\n] '(#[\x1f\n] *eof*)))
                  (body (next-token #[\n] '(#[\x1f] *eof*))))
             (loop (read-char) (acons head body r)))))))
  )

(define (read-master-info-file file opts)
  (let1 parts (read-info-file-split file opts)
    (when (null? parts)
      (error "file is not an info file" file))
    (let1 info (make <info-file> :path file :directory (sys-dirname file))
      (if (string=? (caar parts) "Indirect:")
          (let1 indirect-table (parse-indirect-table (cdar parts))
            (parse-tag-table info indirect-table (cdr (cadr parts))))
          (parse-nodes info parts))
      info)))

(define (parse-indirect-table indirects)
  (with-input-from-string indirects
    (lambda ()
      (port-map (lambda (line)
                  (rxmatch-case line
                    (#/^([^:]+):\s+(\d+)/ (#f file count)
                        (cons (x->integer count) file))
                    (else '())))
                read-line))))

(define (parse-tag-table info indirect tags)
  (define (find-file count)
    (let loop ((indirect indirect)
               (prev #f))
      (cond ((null? indirect) prev)
            ((< count (caar indirect)) prev)
            (else (loop (cdr indirect) (cdar indirect))))))
  (with-input-from-string tags
    (lambda ()
      (port-for-each (lambda (line)
                       (rxmatch-case line
                         (#/^Node: ([^\x7f]+)\x7f(\d+)/ (#f node count)
                           (hash-table-put! (ref info 'node-table)
                                            node
                                            (find-file (x->integer count))))
                         (else line #f)))
                     read-line)))
  )

(define (read-sub-info-file info file opts)
  (let1 parts (read-info-file-split file opts)
    (when (null? parts)
      (error "file is not an info file" file))
    (parse-nodes info parts)))

(define (parse-nodes info parts)
  (for-each (lambda (p)
              (unless (string=? (car p) "Tag Table:")
                (parse-node info p)))
            parts))

(define (parse-node info part)
  (rxmatch-case (car part)
    (#/File: [^,]+,  Node: ([^,]+)(,  Next: ([^,]+))?,  Prev: ([^,]+),  Up: ([^,]+)/
     (#f node #f next prev up)
     (let1 info-node (make <info-node>
                       :node node :next next :prev prev :up up :file info
                       :content (cdr part))
       (hash-table-put! (ref info 'node-table) node info-node)
       info-node))
    (else #f)
    ))

;;; External API

(define (open-info-file file)
  (read-master-info-file file '()))

(define-method info-get-node ((info <info-file>) nodename)
  (let1 node (hash-table-get (ref info 'node-table) nodename #f)
    (and node
         (if (is-a? node <info-node>)
             node
             (begin
               (read-sub-info-file info
                                   (build-path (ref info 'directory) node)
                                   '())
               (hash-table-get (ref info 'node-table) nodename #f))))))

(define-method info-parse-menu ((info <info-node>))
  (with-input-from-string (ref info 'content)
    (lambda ()
      (define (skip line)
        (cond ((eof-object? line) '())
              ((string=? line "* Menu:") (menu (read-line) '()))
              (else (skip (read-line)))))
      (define (menu line r)
        (rxmatch-case line
          (test eof-object? (reverse! r))
          (#/^\* (.+)::/ (#f node)
           (menu (read-line) (acons node node r)))
          (#/^\* (.+):\s+(.+)\./ (#f index node)
           (menu (read-line) (acons index node r)))
          (else (menu (read-line) r))))
      (skip (read-line)))))

(provide "text/info")
