;;;
;;; interactive/info.scm - online helper
;;;
;;;   Copyright (c) 2000-2024  Shiro Kawai  <shiro@acm.org>
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
  (use scheme.list)
  (use srfi.13)
  (use text.info)
  (use text.pager)
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

(define get-info
  (let1 info-doc
      (delay
        (if-let1 info-doc (and-let1 path (find-info-file)
                            (open-info-document path))
          (begin
            (info-index-add! info-doc "Function and Syntax Index")
            (info-index-add! info-doc "Module Index")
            (info-index-add! info-doc "Variable Index")
            (info-index-add! info-doc "Class Index" (^e #"<~|e|>"))
            info-doc)
          (begin
            (warn "Couldn't find info document in ~s. \
                   Maybe it has't been installed. \
                   Check your Gauche installation.\n"
                  (get-info-paths))
            #f)))
    (^[] (force info-doc))))

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
  (find-file-in-paths *info-file*
                      :paths (get-info-paths)
                      :pred (^p (or (file-is-readable? p)
                                    (file-is-readable? #"~|p|.gz")
                                    (file-is-readable? #"~|p|.bz2")))))

(define (handle-ambiguous-name entry-name)
  (let* ([keys (info-index-keys (get-info))]
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
  (let1 es (info-index-ref (get-info) (x->string entry-name))
    (if (null? es)
      (handle-ambiguous-name entry-name)
      es)))

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
;; NB: The info slot may be #f, but in that case key never hits, so
;; the proc won't be called.
(define (info key)
  ($ lookup&show key
     (^[node&line]
       (let1 node (info-get-node (get-info) (car node&line))
         (display/pager
          (if (null? (cdr node&line))
            (~ node'content)
            (info-extract-definition node (cadr node&line))))))))

;; API
;; NB: The info slot may be #f, but in that case key never hits, so
;; the proc won't be called.
(define (info-page key)
  ($ lookup&show key
     (^[node&line]
       (display/pager
        (~ (info-get-node (get-info) (car node&line)) 'content)))))

;; API
;; Experimental; invoke


;;;
;;; Search info entries by regexp
;;;

(define (search-entries rx)
  (sort (filter (^e (rx (car e))) (info-index->alist (get-info)))
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
      (display/pager
       (with-output-to-string
         (^[] (for-each format-search-result-entry entries))))))
  (values))
