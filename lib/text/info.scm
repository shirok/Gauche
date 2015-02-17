;;;
;;; info.scm - parse info file
;;;
;;;   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(define-module text.info
  (use srfi-13)
  (use srfi-14)
  (use text.parse)
  (use gauche.process)
  (use file.util)
  (use rfc.zlib)
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

;; Find bzip2 location
(define bzip2  (find-file-in-paths "bzip2"))

;; Read an info file FILE, and returns a list of strings splitted by ^_ (#\u001f)
;; If FILE is not found, look for compressed one.
(define (read-info-file-split file opts)
  (define (with-input-from-info thunk)
    (cond [(file-exists? file)
           (with-input-from-file file thunk)]
          [(file-exists? #`",|file|.gz")
           (call-with-input-file #`",|file|.gz"
             (^p (let1 zp (open-inflating-port p :window-bits 31) ;force gzip format
                   (unwind-protect (with-input-from-port zp thunk)
                     (close-input-port zp)))))]
          [(and bzip2 (file-exists? #`",|file|.bz2"))
           (with-input-from-process #`",bzip2 -c -d ,|file|.bz2" thunk)]
          [else (error "can't find info file" file)]))
  (with-input-from-info
   (lambda ()
     (let loop ([c (skip-while (char-set-complement #[\u001f]))]
                [r '()])
       (if (eof-object? c)
         (reverse! r)
         (let* ([head (next-token #[\u001f\n] '(#[\u001f\n] *eof*))]
                [body (next-token #[\n] '(#[\u001f] *eof*))])
           (loop (read-char) (acons head body r)))))))
  )

(define (read-master-info-file file opts)
  (let1 parts (read-info-file-split file opts)
    (when (null? parts)
      (error "file is not an info file" file))
    (rlet1 info (make <info-file> :path file :directory (sys-dirname file))
      (if (string=? (caar parts) "Indirect:")
        (let1 indirect-table (parse-indirect-table (cdar parts))
          (parse-tag-table info indirect-table (cdr (cadr parts))))
        (parse-nodes info parts)))))

(define (parse-indirect-table indirects)
  (with-input-from-string indirects
    (cut generator-map
         (^[line] (rxmatch-case line
                    [#/^([^:]+):\s+(\d+)/ (#f file count)
                     (cons (x->integer count) file)]
                    [else '()]))
                     read-line)))

(define (parse-tag-table info indirect tags)
  (define (find-file count)
    (let loop ([indirect indirect]
               [prev #f])
      (cond [(null? indirect) prev]
            [(< count (caar indirect)) prev]
            [else (loop (cdr indirect) (cdar indirect))])))
  (with-input-from-string tags
    (cut generator-for-each
         (^[line]
           (rxmatch-case line
             [#/^Node: ([^\u007f]+)\u007f(\d+)/ (#f node count)
              (hash-table-put! (ref info 'node-table)
                               node
                               (find-file (x->integer count)))]
             [else line #f]))
         read-line)))

(define (read-sub-info-file info file opts)
  (let1 parts (read-info-file-split file opts)
    (when (null? parts)
      (error "file is not an info file" file))
    (parse-nodes info parts)))

(define (parse-nodes info parts)
  (dolist [p parts]
    (unless (string=? (car p) "Tag Table:")
      (parse-node info p))))

(define (parse-node info part)
  (rxmatch-case (car part)
    [#/File: [^,]+,  Node: ([^,]+)(,  Next: ([^,]+))?,  Prev: ([^,]+),  Up: ([^,]+)/
     (#f node #f next prev up)
     (rlet1 info-node (make <info-node>
                       :name node :next next :prev prev :up up :file info
                       :content (cdr part))
       (hash-table-put! (ref info 'node-table) node info-node))]
    [else #f]))

;;; External API

(define (open-info-file file)
  (read-master-info-file file '()))

(define-method info-get-node ((info <info-file>) nodename)
  (if-let1 node (hash-table-get (ref info 'node-table) nodename #f)
    (cond [(is-a? node <info-node>) node]
          [else
           (read-sub-info-file info
                               (build-path (ref info 'directory) node)
                               '())
           (hash-table-get (ref info 'node-table) nodename #f)])
    #f))

(define-method info-parse-menu ((info <info-node>))
  (with-input-from-string (ref info 'content)
    (^[]
      (define (skip line)
        (cond [(eof-object? line) '()]
              [(string=? line "* Menu:") (menu (read-line) '())]
              [else (skip (read-line))]))
      (define (menu line r)
        (rxmatch-case line
          [test eof-object? (reverse! r)]
          [#/^\* (.+)::/ (#f node)
                 (menu (read-line) (acons node node r))]
          [#/^\* (.+):\s+(.+)\./ (#f index node)
                 (menu (read-line) (acons index node r))]
          [else (menu (read-line) r)]))
      (skip (read-line)))))

