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
;;;  $Id: modutil.scm,v 1.1 2003-08-08 20:54:39 shirok Exp $
;;;

(define-module gauche.modutil
  (export-all))
(select-module gauche.modutil)

;; module-available? tries to determine if the named module can be
;; 'use'-ed, using heuristics depending on the module file convention.
(define (module-available? name)

  (define (module-file? file)
    (let1 exp (with-error-handler
                  (lambda (e) #f)
                (lambda ()
                  (with-input-from-file file read :if-does-not-exist #f)))
      (and (pair? exp)
           (eq? (car exp) 'define-module)
           (pair? (cdr exp))
           (eq? (cadr exp) name)
           file)))
  
  (or (not (not (find-module name))) ;; if it's already loaded, no problemo.
      (let ((path (%module-name->path name)))
        (let loop ((paths *load-path*))
          (cond ((null? paths) #f)
                ;; NB: avoid build-path, for file.util depends on whole
                ;; bunch of other modules.
                ((module-file? (string-append (car paths) "/" path ".scm")))
                (else (loop (cdr paths)))))))
  )

(provide "gauche/modutil")
