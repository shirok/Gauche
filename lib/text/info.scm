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
          info-extract-definition
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
          [(file-exists? #"~|file|.gz")
           (call-with-input-file #"~|file|.gz"
             (^p (let1 zp (open-inflating-port p :window-bits 31) ;force gzip format
                   (unwind-protect (with-input-from-port zp thunk)
                     (close-input-port zp)))))]
          [(and bzip2 (file-exists? #"~|file|.bz2"))
           (with-input-from-process #"~bzip2 -c -d ~|file|.bz2" thunk)]
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

;; API
;; Returns <info-file>
(define (open-info-file file)
  (read-master-info-file file '()))

;; API
;; Returns <info-node>
(define-method info-get-node ((info <info-file>) nodename)
  (if-let1 node (hash-table-get (ref info 'node-table) nodename #f)
    (cond [(is-a? node <info-node>) node]
          [else
           (read-sub-info-file info
                               (build-path (ref info 'directory) node)
                               '())
           (hash-table-get (ref info 'node-table) nodename #f)])
    #f))

;; API
;; Search menu in the given node, and returns list of menu entries.
;; Menu entry:
;;    (<entry-name> <node-name> [<line-number>])
;; Where <entry-name> is either a node name, function or macro name,
;; module name, 
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
           (menu (read-line) `((,node ,node) ,@r))]
          [#/^\* (.+):\s+(.+)\.(?:\s*\(line\s+(\d+)\))?/ (#f index node line)
           (if line
             (menu (read-line) `((,index ,node ,(x->integer line)) ,@r))
             ;; The '(line \d+)' may be in the next line.
             (let1 line2 (read-line)
               (if-let1 m (and (string? line2)
                               (#/^\s+\(line\s+(\d+)\)/ line2))
                 (menu (read-line) `((,index ,node ,(x->integer (m 1))) ,@r))
                 (menu line2 `((,index ,node) ,@r)))))]
          [else (menu (read-line) r)]))
      (skip (read-line)))))

;; API
;; Extract one definition from the node's content.  Assumes the definition
;; begins from the specified line; then we go forward to find the end of
;; the definition.  The end of definition is when we see the end of content,
;; or we see a line begins with less than or equal to 3 whitespaces.
;; (Except the 'defunx'-type multi entry)
(define (info-extract-definition info-node start-line)

  ;; Skip the lines before the entry.
  ;; START-LINE counts from the beginning of the info doc; the first 3 lines
  ;; are taken by the node header and the node's content doesn't include them.
  ;; Also note that line count starts from 1.
  ;;
  ;; Caveat: If the entry header spans multiple lines because of large
  ;; number of arguments, the texinfo menu's line number somehow points to the
  ;; last line of the entry header.  For example, the entry of http-get
  ;; begins with this:
  ;;
  ;;   -- Function: http-get server request-uri :key sink flusher
  ;;        redirect-handler secure ...
  ;;
  ;; And the texinfo menu's line points to "redirect-handler secure ..." line
  ;; instead of "-- Function: http-get" line.  So we have to check the lines
  ;; to find out the last #/^ --/ line before START-LINE.
  (define (skip-lines)
    (let loop ([n (- start-line 3)]
               [lines '()])
      (if (<= n 0)
        (let1 line (read-line)
          (for-each print (reverse lines))
          line)
        (let1 line (read-line)
          (cond [(eof-object? line) line]  ;something's wrong, but tolerate.
                [(#/^ --/ line) (loop (- n 1) (cons line lines))]
                [(#/^ {10}/ line) (loop (- n 1) (cons line lines))]
                [else (loop (- n 1) '())])))))
  
  (with-string-io (~ info-node'content)
    (^[]
      (let entry ([line (skip-lines)])
        (unless (eof-object? line)
          (cond [(#/^ --/ line) (print line) (entry (read-line))]
                [(#/^$/ line)] ;; no description
                [(#/^ {6}/ line) ;; folded entry line
                 (print line) (entry (read-line))]
                [(#/^ {5}\S/ line) ;; start description
                 (print line)
                 (let desc ([line (read-line)])
                   (unless (eof-object? line)
                     (cond [(#/^$/ line) (print) (desc (read-line))]
                           [(#/^ {4}/ line) (print line) (desc (read-line))]
                           [else])))]))))))
      
  
