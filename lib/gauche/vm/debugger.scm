;;;
;;; Debugger - terminal base debugger
;;;  
;;;   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
;;;  $Id: debugger.scm,v 1.18 2005-04-22 04:50:53 shirok Exp $
;;;

;; NB: this is still a working version.  

(define-module gauche.vm.debugger
  (use srfi-1)
  (use srfi-2)
  (use srfi-13)
  (use text.parse)
  (extend gauche.internal)
  (export debug-print)
  )
(select-module gauche.vm.debugger)

(define *stack-show-depth* 20)
(define *expr-show-length* 65)

;; Debug print stub ------------------------------------------
;; (this is temporary implementation)
(define-syntax debug-print
  (syntax-rules ()
    ((_ ?form)
     (let1 f '?form
       (or (and-let* ((info (and (pair? f)
                                 (pair-attribute-get f 'source-info #f)))
                      ((pair? info))
                      ((pair? (cdr info))))
             (format/ss (current-error-port) "#?=~s:~a:~,,,,65:s\n"
                        (car info) (cadr info) f)
             #t)
           (format (current-error-port) "#?=~,,,,65:s\n" f))
       (receive vals ?form
         (if (null? vals)
             (format (current-error-port) "#?-<void>\n")
             (begin
               (format/ss (current-error-port) "#?-    ~,,,,65:s\n" (car vals))
               (for-each (lambda (elt)
                           (format/ss (current-error-port)
                                      "#?+    ~,,,,65:s\n" elt))
                         (cdr vals))))
         (apply values vals))))))

(provide "gauche/vm/debugger")

