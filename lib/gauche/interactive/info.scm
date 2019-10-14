;;;
;;; interactive/info.scm - online helper
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

(define-module gauche.interactive.info
  (use srfi-1)
  (use srfi-13)
  (use text.info)
  (use file.util)
  (use util.match)
  (use util.levenshtein)
  (use gauche.process)
  (use gauche.config)
  (use gauche.sequence)
  (export info info-page info-search)
  )
(select-module gauche.interactive.info)

(define *info-file* "gauche-refe.info")

(define-class <repl-info> () ;; a singleton
  ((info  :init-keyword :info)   ;; <info>
   (index :init-keyword :index)  ;; hashtable name -> [(node-name line-no)]
   ))

(define get-info
  (let1 repl-info
      (delay
        (let ([info  (open-info-file (find-info-file))]
              [index (make-hash-table 'string=?)])
          (dolist [index-page '("Function and Syntax Index"
                                "Module Index"
                                "Variable Index")]
            (dolist [p ($ info-parse-menu $ info-get-node info index-page)]
              (hash-table-push! index (entry-name (car p)) (cdr p))))
          ;; class index doesn't have surrounding '<>', but we want to search
          ;; with them.
          (dolist [p ($ info-parse-menu $ info-get-node info "Class Index")]
            (hash-table-push! index #"<~(car p)>" (cdr p)))
          ($ hash-table-for-each index
             ;; reverse v here so that earlier entry listed first
             (^[k v] (hash-table-put! index k (squash-entries (reverse v)))))
          (make <repl-info>
            :info info :index index)))
    (^[] (force repl-info))))

;; We sometimes list API variations using @defunx.  We want to pick only
;; the first one of such entries.
(define (squash-entries node&lines)
  (if (length=? node&lines 1)
    node&lines
    (append-map (^[node&lines]
                  ($ map (^l `(,(caar node&lines) ,(car l)))
                     $ group-contiguous-sequence $ sort $ map cadr node&lines))
         (group-collection node&lines :key car :test equal?))))

;; When there are more than one entry with the same name, texinfo appends
;; " <n>" in the index entry.  This strips that.
(define (entry-name e)
  (if-let1 m (#/ <\d+>$/ e) (rxmatch-before m) e))
  
(define *pager* (or (and-let1 s (sys-getenv "PAGER")
                      (shell-tokenize-string s))
                    (and-let1 cmd (or (find-file-in-paths "less")
                                      (find-file-in-paths "more"))
                      (list cmd))))

(define (viewer-pager s)
  (let1 p (run-process *pager* :input :pipe)
    (guard (e (else #f))
      (display s (process-input p)))
    (close-output-port (process-input p))
    (process-wait p)))

(define (viewer-dumb s) (display s))

(define (viewer-nounicode s)
  (display ($ regexp-replace-all* s
              #/\u21d2/ "==>"      ; @result{}
              #/\u2026/ "..."      ; @dots{}
              #/\u2018/ "`"        ; @code{}
              #/\u2019/ "'"        ; @code{}
              #/\u201C/ "``"       ; ``         (e.g. ,i do)
              #/\u201D/ "''"       ; ''         (e.g. ,i do)
              #/\u2261/ "=="       ; @equiv{}   (e.g. ,i cut)
              #/\u2212/ "-"        ; @minus     (e.g. ,i modulo)
              #/\u2022/ "*"        ; @bullet    (e.g. ,i lambda)
              #/\u2013/ "--"       ; --         (e.g. ,i utf8-length)
              #/\u2014/ "---"      ; ---        (e.g. ,i lambda)
              #/\u00df/ "[Eszett]" ; eszett     (e.g. ,i char-upcase)
              )))

(define viewer
  (cond [(cond-expand
          [(and gauche.os.windows
                (not gauche.ces.none))
           (use os.windows)
           (guard (e [else #f])
             (and (sys-isatty 1)
                  (sys-get-console-mode 
                   (sys-get-std-handle STD_OUTPUT_HANDLE))
                  (not (= (sys-get-console-output-cp) 65001))))] ;unicode cp
          [else #f])
         viewer-nounicode]
        [(or (equal? (sys-getenv "TERM") "emacs")
             (equal? (sys-getenv "TERM") "dumb")
             (not (sys-isatty (current-output-port)))
             (not *pager*))
         viewer-dumb]
        [else 
         viewer-pager]))

(define (get-info-paths)
  (let* ([syspath (cond [(sys-getenv "INFOPATH") => (cut string-split <> #\:)]
                        [else '()])]
         [instpath (list (gauche-config "--infodir"))]
         [in-place (cond-expand
                    [gauche.in-place (if (member "../../lib" *load-path*)
                                       '("../../doc")
                                       '("../doc"))]
                    [else '()])])
    (append in-place syspath instpath)))

(define (find-info-file)
  (let1 paths (get-info-paths)
    (or (find-file-in-paths *info-file*
                            :paths paths
                            :pred (^p (or (file-is-readable? p)
                                          (file-is-readable? #"~|p|.gz")
                                          (file-is-readable? #"~|p|.bz2"))))
        (errorf "couldn't find info file ~s in paths: ~s" *info-file* paths))
    ))

(define (handle-ambiguous-name entry-name)
  (let* ([keys (map x->string (hash-table-keys (~ (get-info)'index)))]
         [c    (min 2 (- (string-length (x->string entry-name)) 1))]
         [candidates ($ filter-map (^[word dist] (and dist word))
                        keys
                        (re-distances (x->string entry-name) keys :cutoff c))])
    (if (null? candidates)
      (print "No info document for " entry-name)
      (begin
        (print "There is no entry for " entry-name ".")
        (print "Did you mean:")
        (dolist [w candidates]
          (print "  " w))))
    '()))

(define (get-node&line entry-name)
  (or (hash-table-get (~ (get-info)'index) (x->string entry-name) #f)
      (handle-ambiguous-name entry-name)))

(define (lookup&show key show)
  (match (get-node&line key)
    [()  #f]  ; no candidates; message already shown
    [(e) (show e)]
    [(es ...)
     (print "There are multiple entries for " key ":")
     (for-each-with-index (^[i e] (format #t "~2d. ~s\n" (+ i 1) (car e))) es)
     (let loop ()
       (format #t "Select number, or q to cancel [1]: ") (flush)
       (rxmatch-case (read-line)
         [test eof-object? #f]
         [#/^\s*$/ (_) (show (car es))]  ; the first entry by default
         [#/^\s*(\d+)\s*$/ (_ N)
          (let1 n (- (x->integer N) 1)
            (if (and (<= 0 n) (< n (length es)))
              (show (list-ref es n))
              (loop)))]
         [#/^\s*q\s*$/ (_) #f]
         [else (loop)]))])
  (values))

;; API
(define (info key)
  ($ lookup&show key
     (^[node&line]
       (let1 node (info-get-node (~ (get-info)'info) (car node&line))
         (viewer (if (null? (cdr node&line))
                   (~ node'content)
                   (info-extract-definition node (cadr node&line))))))))

;; API
(define (info-page key)
  ($ lookup&show key
     (^[node&line]
       (viewer (~ (info-get-node (~ (get-info)'info) (car node&line))
                  'content)))))

;;
;; Search info entries by regexp
;;

(define (search-entries rx)
  (sort (filter (^e (rx (car e))) (hash-table->alist (~ (get-info)'index)))
        string<? car))

(define *search-entry-indent* 25)

(define (format-search-result-entry entry) ; (key (node line) ...)
  (define indent (make-string *search-entry-indent* #\space))
  (define (subsequent-lines node&lines)
    (dolist [l node&lines]
      (format #t "~va~a:~d\n" *search-entry-indent* " " (car l) (cadr l))))
  (match-let1 (key node&lines ...) entry
    (if (> (string-length key) (- *search-entry-indent* 1))
      (begin (print key) (subsequent-lines node&lines))
      (begin (format #t "~va ~a:~d\n" (- *search-entry-indent* 1) key
                     (caar node&lines) (cadar node&lines))
             (subsequent-lines (cdr node&lines))))))

;; API
(define (info-search rx)
  (assume-type rx <regexp>)
  (let1 entries (search-entries rx)
    (if (null? entries)
      (print #"No entry matching ~|rx|")
      (viewer (with-output-to-string
                (^[] (for-each format-search-result-entry entries))))))
  (values))

               
