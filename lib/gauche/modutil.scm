;;;
;;; module utilities - to be autoloaded.
;;;  
;;;   Copyright (c) 2003 Shiro Kawai, All rights reserved.
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
;;;  $Id: modutil.scm,v 1.2 2003-09-12 21:12:46 shirok Exp $
;;;

(define-module gauche.modutil
  (use srfi-1)
  (use srfi-13)
  (export library-exists? library-fold library-has-module?))
(select-module gauche.modutil)

;; library-fold - iterate over the modules or libraries whose name matches
;;  the given pattern.
;;  proc takes the matched module/library name, full pathname, and seed value.
;;  This can be more involved once customized module mapping system
;;  is introduced; for now, we simply apply the default mapping rule.

(define (library-fold pattern proc seed . opts)
  (let-keywords* opts ((paths *load-path*)
                       (allow-duplicates? #f)
                       (strict? #f))
    (define pats
      (cond ((string? pattern)
             (string-split pattern #\/))
            ((symbol? pattern)
             (string-split (x->string pattern) #\.))
            (else
             (error "string or symbol required for pattern, but got"
                    pattern))))

    (define seen '())

    (define (match? pat component) ;; for now...
      (cond ((equal? pat "*") #t)
            (else (string=? pat component))))

    ;; pats - list of pattern components splitted by '.' or '/'
    ;; prefix - one of load paths, e.g. /usr/share/gauche/site/lib/
    ;; file - path components after prefix, e.g. gauche/mop
    ;; base - the last component of file, e.g. mop
    (define (search pats prefix file base seed)
      (let* ((path (topath prefix file)))
        (cond ((and (not (null? (cdr pats)))
                    (match? (car pats) base)
                    (file-is-directory? path))
               (fold (lambda (subfile seed)
                       (search (cdr pats) prefix #`",|file|/,|subfile|"
                               subfile seed))
                     seed (readdir path)))
              ((and (null? (cdr pats))
                    (string-suffix? ".scm" base)
                    (match? (car pats) (string-drop-right base 4))
                    (file-exists? path))
               (proc file path seed))
              (else seed))))

    ;; main body
    (fold (lambda (prefix seed)
            (fold (lambda (file seed)
                    (search pats prefix file file seed))
                  seed (readdir prefix)))
          seed paths)
    ))

(define (library-exists? mod/path . opts)
  (let-optionals* opts ((force-search? #f)
                        (strict? #f)
                        (paths *load-path*))
    (define (for-each-paths proc name)
      (let loop ((paths paths))
        (if (null? paths)
          #f
          (let1 p (string-append (car paths) "/" name ".scm")
            (or (and (file-exists? p) (proc p))
                (loop (cdr paths)))))))
    
    (or (and (not force-search?)
             ;; see if specified mod/path is already loaded
             (or (and (string? mod/path) (provided? mod/path))
                 (and (symbol? mod/path) (find-module mod/path))))
        ;; scan the filesystem
        (cond ((string? mod/path)
               (for-each-paths identity mod/path))
              ((symbol? mod/path)
               (let1 name (module-name->path mod/path)
                 (for-each-paths (lambda (p)
                                   (if strict?
                                     (library-has-module? p mod/path)
                                     #t))
                                 name)))
              (else
               (error "string or symbol required, bot got" mod/path))))
    ))

;; Try to determine the file is a module source file
(define (module-file? file name)
  (let1 exp (with-error-handler (lambda (e) #f)
              (lambda ()
                (with-input-from-file file read :if-does-not-exist #f)))
    (and (pair? exp)
         (eq? (car exp) 'define-module)
         (pair? (cdr exp))
         (eq? (cadr exp) name)
         file)))
  
;; Auxiliary procedures
;;  we don't want to depend file.util here, so some simple utils.

;; readdir with removing "." and "..".
(define (readdir dir)
  (if (file-is-directory? dir)
    (let loop ((p (sys-readdir dir)) (r '()))
      (cond ((null? p) (reverse! r))
            ((member (car p) '("." "..")) (loop (cdr p) r))
            (else (loop (cdr p) (cons (car p) r)))))
    '()))

(define (topath prefix file)
  (sys-normalize-pathname (string-append prefix "/" file) :canonicalize #t))

(provide "gauche/modutil")
