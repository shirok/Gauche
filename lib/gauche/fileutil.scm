;;;
;;; file related utility functions.  to be autoloaded.
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
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

#!no-fold-case

(define-module gauche.fileutil
  (export glob glob-fold glob-component->regexp make-glob-fs-fold
          sys-stat->file-type sys-stat->mode sys-stat->ino
          sys-stat->dev sys-stat->rdev sys-stat->nlink
          sys-stat->size sys-stat->uid sys-stat->gid
          sys-stat->atime sys-stat->mtime sys-stat->ctime sys-stat->type
          sys-tm->alist)
  )
(select-module gauche.fileutil)

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
  (map (^[n s] (cons n (slot-ref tm s)))
       '(tm_sec tm_min tm_hour tm_mday tm_mon tm_year tm_wday tm_yday tm_isdst)
       '(sec min hour mday mon year wday yday isdst)))

;;;
;;; globbing.
;;;

;; glob-fold provides the fundamental logic of glob.  It does not
;; depend on filesystems---any tree structure that has "pathname"
;; will do.
;;
;; <glob-pattern> : [<separator>] (<selector> <separator>)* [<separator>]
;; <selector>     : '**' | <element>*
;; <element>      : <ordinary> | '*' | '?' | <char-range>
;; <char-range>   : '[' <char-set-spec> ']'
;; <ordinary>     : characters except #[,*?\{\}\[\]\\] and <separator>
;;                  | '\\' <character>
;;
;; <separator> splits the components in the path.

(define (glob patterns . opts)
  (apply glob-fold patterns cons '() opts))

(define sys-glob glob) ;; backward compatibility

(define (glob-fold patterns proc seed :key (separator #[/])
                                           (folder glob-fs-folder)
                                           (sorter sort))
  (let1 r (fold (cut glob-fold-1 <> proc <> separator folder) seed
                (fold glob-expand-braces '()
                      (if (list? patterns) patterns (list patterns))))
    (if sorter (sorter r) r)))

;; NB: we avoid util.match due to the hairy dependency problem.
(define (glob-fold-1 pattern proc seed separator folder)
  (define (rec node matcher seed)
    (cond [(null? matcher) seed]
          [(eq? (car matcher) '**) (rec* node (cdr matcher) seed)]
          [(null? (cdr matcher)) (folder proc seed node (car matcher) #f)]
          [else (folder (^[node seed] (rec node (cdr matcher) seed))
                        seed node (car matcher) #t)]))
  (define (rec* node matcher seed)
    (fold (cut rec* <> matcher <>)
          (rec node matcher seed)
          (folder cons '() node #/^[^.].*$/ #t)))
  (let1 p (glob-prepare-pattern pattern separator)
    (rec (car p) (cdr p) seed)))

(define (glob-prepare-pattern pattern separator)
  (define (f comp)
    (cond [(equal? comp "") 'dir?]    ; pattern ends with '/'
          [(equal? comp "**") '**]
          [else (glob-component->regexp comp)]))
  (let1 comps (string-split pattern separator)
    (if (equal? (car comps) "")
      (cons #t (map f (cdr comps)))
      (cons #f (map f comps)))))

;; */*.{c,scm} -> '(*/*.c */*.scm)
;;
;; NB: we first expand the braces to separate patterns.  This is how
;; zsh and tcsh handles {...}.  However, it is not good in terms of
;; performance, since the common prefix are searched mulitple times.
;; Hopefully we'll put some optimization here, making single traversal
;; for the common prefix.
;;
;; The treatment of backslashes is tricky.
;;
(define (glob-expand-braces pattern seed)
  (define (parse str pres level)
    (let loop ([str str]
               [segs pres])
      (cond
       [(rxmatch #/[{}]/ str) =>
        (^m
          (cond [(equal? (m 0) "{")
                 (receive (ins post) (parse (m'after) '("") (+ level 1))
                   (loop post
                         (fold (^[seg seed]
                                 (fold (^[in seed]
                                         (cons (string-append seg in) seed))
                                       seed ins))
                               '()
                               (map (cute string-append <> (m'before)) segs))))]
                [(= level 0)
                 (error "extra closing curly-brace in glob pattern:" pattern)]
                [else         ; closing curly-brace
                 (values (fold expand '()
                               (map (cute string-append <> (m'before)) segs))
                         (m'after))]))]
       [(= level 0) (values (map (cute string-append <> str) segs) #f)]
       [else (error "unclosed curly-brace in glob pattern:" pattern)])))
  (define (expand pat seed)
    (let1 segs (string-split pat #\,)
      (if (null? seed) segs (append segs seed))))
  (if (string-scan pattern #\{)
    (append (values-ref (parse pattern '("") 0) 0) seed)
    (cons pattern seed)))

(define (glob-component->regexp pattern) ; "**" is already excluded
  (define n read-char)
  (define nd '(comp . #[.]))
  (define ra '(rep 0 #f any))
  (regexp-compile
   (regexp-optimize
    (with-input-from-string pattern
      (^[]
        (define (element0 ch ct)
          (case ch
            [(#\*) (element0* (n) ct)]
            [(#\?) `(,nd ,@(element1 (n) ct))]
            [else (element1 ch ct)]))
        (define (element0* ch ct)
          (case ch
            [(#\*) (element0* (n) ct)]
            [(#\?) `(,nd ,ra ,@(element1 (n) ct))]
            [(#\.) `(,nd ,ra #\. ,@(element1 (n) ct))]
            [else `((rep 0 1 (seq ,nd ,ra))
                    ,@(element1 ch ct))]))
        (define (element1 ch ct)
          (cond [(eof-object? ch) '(eol)]
                [(eqv? ch #\*) `(,ra ,@(element1* (n) ct))]
                [(eqv? ch #\?) `(any ,@(element1 (n) ct))]
                [(eqv? ch #\[)
                 (case (peek-char)
                   ;; we have to treat [!...] as [^...]
                   [(#\!) (n)
                    (let1 cs (read-char-set (current-input-port))
                      (cons (char-set-complement! cs) (element1 (n) ct)))]
                   [else
                    (let1 cs (read-char-set (current-input-port))
                      (cons cs (element1 (n) ct)))])]
                [else (cons ch (element1 (n) ct))]))
        (define (element1* ch ct)
          (case ch
            [(#\*) (element1* (n) ct)]
            [else  (element1 ch ct)]))
        `(0 #f bol ,@(element0 (n) '())))))))

;; if rx is just test perfect match, e.g. #/^string$/, returns
;; string portion.
(define (fixed-regexp? rx)
  (let1 ast (regexp-ast rx)
    (and (> (length ast) 4)
         (eq? (caddr ast) 'bol)
         (let loop ([cs (cdddr ast)] [r '()])
           (cond [(null? (cdr cs))
                  (and (eq? (car cs) 'eol) (list->string (reverse r)))]
                 [(char? (car cs)) (loop (cdr cs) (cons (car cs) r))]
                 [else #f])))))

(define (make-glob-fs-fold :key (root-path #f) (current-path #f))
  (let1 separ (cond-expand [gauche.os.windows "\\"]
                           [else "/"])
    (define (ensure-dirname s)
      (and s
           (or (and-let* ([len (string-length s)]
                          [ (> len 0) ]
                          [ (not (eqv? (string-ref s (- len 1))
                                       (string-ref separ 0))) ])
                 (string-append s separ))
               s)))
    (define root-path/    (ensure-dirname root-path))
    (define current-path/ (ensure-dirname current-path))
    (^[proc seed node regexp non-leaf?]
      (let1 prefix (case node
                     [(#t) (or root-path/ separ)]
                     [(#f) (or current-path/ "")]
                     [else (string-append node separ)])
        ;; NB: we can't use filter, for it is not built-in.
        ;; also we can't use build-path from the same reason.
        ;; We treat fixed-regexp specially, since it allows
        ;; us not to search the directory---sometimes the directory
        ;; has 'x' permission but not 'r' permission, and it would be
        ;; unreasonable if we fail to go down the path even if we know
        ;; the exact name.
        (cond [(eq? regexp 'dir?) (proc prefix seed)]
              [(fixed-regexp? regexp)
               => (^s (let1 full (string-append prefix s)
                        (if (and (file-exists? full)
                                 (or (not non-leaf?)
                                     (file-is-directory? full)))
                          (proc full seed)
                          seed)))]
              [else
               (fold (^[child seed]
                       (or (and-let* ([ (regexp child) ]
                                      [full (string-append prefix child)]
                                      [ (or (not non-leaf?)
                                            (file-is-directory? full)) ])
                             (proc full seed))
                           seed))
                     seed
                     (sys-readdir (case node
                                    [(#t) (or root-path/ "/")]
                                    [(#f) (or current-path/ ".")]
                                    [else node])))])))
    ))

(define glob-fs-folder (make-glob-fs-fold))

