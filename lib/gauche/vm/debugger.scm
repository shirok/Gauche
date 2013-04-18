;;;
;;; Debugging aids
;;;
;;;   Copyright (c) 2000-2013  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.vm.debugger
  (use srfi-1)
  (use srfi-13)
  (use gauche.parameter)
  (export debug-print debug-print-width debug-source-info
          debug-print-pre debug-print-post))
(select-module gauche.vm.debugger)

(define (debug-source-info obj)
  (and-let* ([ (pair? obj) ]
             [info ((with-module gauche.internal pair-attribute-get)
                    obj 'source-info #f)]
             [ (pair? info) ]
             [ (pair? (cdr info)) ])
    info))

(define debug-print-width (make-parameter 65))

;; Debug print stub ------------------------------------------
;; (this is temporary implementation)
(define-syntax debug-print
  (syntax-rules ()
    [(_ ?form)
     (begin
       (debug-print-pre '?form)
       (receive vals ?form
         (debug-print-post vals)))]))

;; These are internal APIs, but we need to export them in order to
;; autoload gauche.vm.debug from precompiled code works.
(define (debug-print-pre form)
  (cond [(debug-source-info form)
         => (^[info]
              (format/ss (current-error-port) "#?=~s:~a:~,,,,v:s\n"
                         (car info) (cadr info) (debug-print-width) form))]
        [else
         (format/ss (current-error-port) "#?=~,,,,v:s\n"
                    (debug-print-width) form)]))

(define (debug-print-post vals)
  (if (null? vals)
    (format (current-error-port) "#?-<void>\n")
    (begin
      (format/ss (current-error-port) "#?-    ~,,,,v:s\n"
                 (debug-print-width) (car vals))
      (for-each (^[elt]
                  (format/ss (current-error-port)
                             "#?+    ~,,,,v:s\n"
                             (debug-print-width) elt))
                (cdr vals))))
  (apply values vals))


