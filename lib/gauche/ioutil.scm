;;;
;;; i/o utilities - to be autoloaded.
;;;  
;;;   Copyright (c) 2004 Shiro Kawai, All rights reserved.
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
;;;  $Id: ioutil.scm,v 1.1 2004-02-03 13:12:27 shirok Exp $
;;;

(define-module gauche.ioutil
  (export print
          write-with-shared-structure write/ss
          read-with-shared-structure read/ss
          format format/ss)
  )
(select-module gauche.ioutil)

;; print (from SCM, Chicken)
(define (print . args)
  (for-each display args) (newline))

;; srfi-38
(define read-with-shared-structure read)
(define read/ss read)

(define (write-with-shared-structure obj . args)
  (write* obj (if (pair? args) (car args) (current-output-port))))
(define write/ss write-with-shared-structure)

(define (format-int port fmt args shared?)
  (cond ((eqv? port #f)
         (let ((out (open-output-string :private? #t)))
           (%format out fmt args shared?)
           (get-output-string out)))
        ((eqv? port #t) (%format (current-output-port) fmt args shared?))
        (else (%format port fmt args shared?))))

(define (format fmt . args)
  (if (string? fmt)
    (format-int #f fmt args #f) ;; srfi-28 compatible behavior
    (format-int fmt (car args) (cdr args) #f)))

(define (format/ss fmt . args)
  (if (string? fmt)
    (format-int #f fmt args #t) ;; srfi-28 compatible behavior
    (format-int fmt (car args) (cdr args) #t)))

(provide "gauche/ioutil")

          