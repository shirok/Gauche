;;;
;;; interactive/info.scm - online helper
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

(define-module gauche.interactive.info
  (use srfi-1)
  (use srfi-13)
  (use text.info)
  (use file.util)
  (use gauche.process)
  (use gauche.config)
  (export info info-page)
  )
(select-module gauche.interactive.info)

(define *info-file* "gauche-refe.info")

(define-class <repl-info> () ;; a signleton
  ((info  :init-keyword :info)   ;; <info>
   (index :init-keyword :index)  ;; hashtable name -> (node-name line-no)
   ))

(define get-info
  (let1 repl-info
      (delay
        (let ([info  (open-info-file (find-info-file))]
              [index (make-hash-table 'string=?)])
          (dolist [index-page '("Function and Syntax Index" "Module Index")]
            (dolist [p ($ info-parse-menu $ info-get-node info index-page)]
              (hash-table-put! index (car p) (cdr p))))
          ;; class index doesn't have surrounding '<>', but we want to search
          ;; with them.
          (dolist [p ($ info-parse-menu $ info-get-node info "Class Index")]
            (hash-table-put! index #"<~(car p)>" (cdr p)))
          (make <repl-info>
            :info info :index index)))
    (^[] (force repl-info))))
  
(define *pager* (or (sys-getenv "PAGER")
                    (find-file-in-paths "less")
                    (find-file-in-paths "more")))

(define viewer
  (if (or (equal? (sys-getenv "TERM") "emacs")
          (equal? (sys-getenv "TERM") "dumb")
          (not (sys-isatty (current-output-port)))
          (not *pager*))
    display
    (^s
     (let1 p (run-process *pager* :input :pipe)
       (guard (e (else #f))
         (display s (process-input p)))
       (close-output-port (process-input p))
       (process-wait p)))))

(define (get-info-paths)
  (let* ([syspath (cond [(sys-getenv "INFOPATH") => (cut string-split <> #\:)]
                        [else '()])]
         [instpath (list (gauche-config "--infodir"))]
         [in-place (list "../doc")])
    (append syspath instpath in-place)))

(define (find-info-file)
  (let1 paths (get-info-paths)
    (or (find-file-in-paths *info-file*
                            :paths paths
                            :pred (^p (or (file-is-readable? p)
                                          (file-is-readable? #"~|p|.gz")
                                          (file-is-readable? #"~|p|.bz2"))))
        (errorf "couldn't find info file ~s in paths: ~s" *info-file* paths))
    ))

(define (get-node&line entry-name)
  (or (hash-table-get (~ (get-info)'index) (x->string entry-name) #f)
      (errorf "no info document for ~a" entry-name)))

;; API
(define (info fn)
  (let* ([node&line (get-node&line fn)]
         [node (info-get-node (~ (get-info)'info) (car node&line))])
    (viewer (if (null? (cdr node&line))
              (~ node'content)
              (info-extract-definition node (cadr node&line)))))
  (values))

;; API
(define (info-page fn)
  (let* ([node&line (get-node&line fn)]
         [nodename (car node&line)])
    (viewer (ref (info-get-node (~ (get-info)'info) nodename) 'content)))
  (values))
