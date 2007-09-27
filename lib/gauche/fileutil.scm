;;;
;;; file related utility functions.  to be autoloaded.
;;;  
;;;   Copyright (c) 2000-2007  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: fileutil.scm,v 1.4 2007-09-27 22:32:25 shirok Exp $
;;;

(define-module gauche.fileutil
  (export file-exists? file-is-regular? file-is-directory?
          glob glob-fold
          sys-stat->file-type sys-stat->mode sys-stat->ino
          sys-stat->dev sys-stat->rdev sys-stat->nlink
          sys-stat->size sys-stat->uid sys-stat->gid
          sys-stat->atime sys-stat->mtime sys-stat->ctime sys-stat->type
          sys-tm->alist)
  )
(select-module gauche.fileutil)

(define (file-exists? path)
  (sys-access path |F_OK|))
(define (file-is-regular? path)
  (and (sys-access path |F_OK|)
       (eq? (slot-ref (sys-stat path) 'type) 'regular)))
(define (file-is-directory? path)
  (and (sys-access path |F_OK|)
       (eq? (slot-ref (sys-stat path) 'type) 'directory)))


;; system object accessors (for backward compatibility)
(define (sys-stat->file-type s)  (slot-ref s 'type))
(define (sys-stat->mode s)  (slot-ref s 'mode))
(define (sys-stat->ino s)   (slot-ref s 'ino))
(define (sys-stat->dev s)   (slot-ref s 'dev))
(define (sys-stat->rdev s)  (slot-ref s 'rdev))
(define (sys-stat->nlink s) (slot-ref s 'nlink))
(define (sys-stat->size s)  (slot-ref s 'size))
(define (sys-stat->uid s)   (slot-ref s 'uid))
(define (sys-stat->gid s)   (slot-ref s 'gid))
(define (sys-stat->atime s) (slot-ref s 'atime))
(define (sys-stat->mtime s) (slot-ref s 'mtime))
(define (sys-stat->ctime s) (slot-ref s 'ctime))
(define (sys-stat->type s)  (slot-ref s 'type))

(define (sys-tm->alist tm)
  (map (lambda (n s) (cons n (slot-ref tm s)))
       '(tm_sec tm_min tm_hour tm_mday tm_mon tm_year tm_wday tm_yday tm_isdst)
       '(sec min hour mday mon year wday yday isdst)))

;;;
;;; globbing.
;;;

;; glob-fold provides the fundamental logic of glob.  It does not
;; depend on filesystems---any tree structure that has "pathname"
;; will do (though some unix-fs specific knowledge is inherently embedded
;; in 
;;
;; <glob-pattern> : [<separator>] (<selector> <separator>)* [<separator>]
;; <selector>     : '**' | <outer>*
;; <outer>        : <inner> | ','
;; <inner>        : <ordinary> | '*' | '?' | <char-range> | <choice>
;; <choice>       : '{' <inner> (',' <inner>)* '}'
;; <char-range>   : '[' <char-set-spec> ']'
;; <ordinary>     : characters except #[,*?\{\}\[\]\\] and <selector>
;;                  | '\\' <character>
;;
;; <separator> splits the components in the path.
;;
;;
;; glob-fold begins matching from either root of the tree or the "current"
;; node.  For every component in the pattern, it calls LISTER to query
;; the matching component.
;; LISTER is called with the following argument.
;;   node - An object representing the node to search.  As special cases,
;;          this can be #t for the root node and #f for the current node.
;;          Otherwise, the type of object depends on what the lister returns.
;;   regexp - Lister should pick the elements within the node whose name
;;          matches this regexp.   As a special case, if this is a symbol dir?,
;;          LISTER should return NODE itself, but it may indicate NODE
;;          "as a directory"---e.g. if NODE is represented as a pathname,
;;          LISTER returns a pathname with trailing directory mark.
;;   non-leaf? - If true, lister should omit leaf nodes from the results.
;; LISTER should return a list of the nodes matching regexp.

(define (glob patterns . opts)
  (apply glob-fold patterns cons '() opts))

(define sys-glob glob) ;; backward compatibility

(define (glob-fold patterns proc seed . opts)
  (if (list? patterns)
    (fold (lambda (pat seed) (glob-fold-1 pat proc seed opts))
          seed patterns)
    (glob-fold-1 patterns proc seed opts)))

(define (glob-fold-1 pattern proc seed opts)
  (let-keywords opts ((separator #[/])
                      (folder glob-fs-folder))
    (define (rec node matcher seed)
      (cond [(null? matcher) (proc seed)]
            [(null? (cdr matcher))
             (folder proc seed node (car matcher) #f)]
            [else
             (folder (lambda (node seed) (rec node (cdr matcher) seed))
                     seed node (car matcher) #t)]))
    (let1 p (glob-prepare-pattern pattern separator)
      (rec (car p) (cdr p) seed))))

(define (glob-prepare-pattern pattern separator)
  (define (f comp)
    (cond [(equal? comp "") 'dir?]    ; pattern ends with '/'
          ;((equal? comp "**") '**)
          [else (glob-component->regexp comp)]))
  (let1 comps (string-split pattern separator)
    (if (equal? (car comps) "")
      (cons #t (map f (cdr comps)))
      (cons #f (map f comps)))))

(define (glob-component->regexp pattern) ; "**" is already excluded
  (regexp-compile
   (regexp-optimize
    (with-input-from-string pattern
      (lambda ()
        (define next read-char)
        (define (outer0 ch)
          (case ch
            [(#\*) (outer0* (next))]
            [(#\?) `((comp . #[.]) ,@(outer1 (next)))]
            [else (outer1 ch)]))
        (define (outer0* ch)
          (case ch
            [(#\*) (outer0* (read-char))]
            [(#\?) `((comp . #[.]) (rep 0 #f any) ,@(outer1 (next)))]
            [(#\.) `((comp . #[.]) (rep 0 #f any) #\. ,@(outer1 (next)))]
            [else `((rep 0 1 (seq (comp . #[.]) (rep 0 #f any)))
                    ,@(outer1 ch))]))
        (define (outer1 ch)
          (cond [(eof-object? ch) '(eol)]
                [(eqv? ch #\*) `((rep 0 #f any) ,@(outer1* (next)))]
                [(eqv? ch #\?) `(any ,@(outer1 (next)))]
                [(eqv? ch #\[)
                 (case (peek-char)
                   ;; we have to treat [!...] as [^...]
                   [(#\!) (next)
                    (let1 cs (read-char-set (current-input-port))
                      (cons (%char-set-complement! cs) (outer1 (next))))]
                   [else
                    (let1 cs (read-char-set (current-input-port))
                      (cons cs (outer1 (next))))])]
                [else (cons ch (outer1 (next)))]))
        (define (outer1* ch)
          (case ch
            [(#\*) (outer1* (next))]
            [else  (outer1 ch)]))
        `(seq bol ,@(outer0 (next))))))))

(define (glob-fs-folder proc seed node regexp non-leaf?)
  (let* ((separ (cond-expand
                 (gauche.os.windows "\\")
                 (else "/")))
         (prefix
          (case node ((#t) separ) ((#f) "") (else (string-append node separ))))
         )
    ;; NB: we can't use filter, for it is not built-in.
    ;; also we can't use build-path, from the same reason.
    (if (eq? regexp 'dir?)
      (proc prefix seed)
      (fold (lambda (child seed)
              (or (and-let* ([ (regexp child) ]
                             [full (string-append prefix child)]
                             [ (or (not non-leaf?)
                                   (file-is-directory? full)) ])
                    (proc full seed))
                  seed))
            seed
            (sys-readdir (case node ((#t) "/") ((#f) ".") (else node)))))))

(provide "gauche/fileutil")
