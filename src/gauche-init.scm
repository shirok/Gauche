;;;
;;; gauche-init.scm - initialize standard environment
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
;;;  $Id: gauche-init.scm,v 1.117.2.1 2005-01-06 08:43:41 shirok Exp $
;;;

(select-module gauche)

;;
;; Loading, require and provide
;;

;; Load path needs to be dealt with at the compile time.  this is a
;; hack to do so.   Don't modify *load-path* directly, since it causes
;; weird compiler-evaluator problem.
;; I don't like the current name "add-load-path", though---looks like
;; more a procedure than a compiler syntax---any ideas?
(define-macro (add-load-path path . args)
  `',(apply %add-load-path path args))

;; Same as above.
(define-macro (require feature)
  `',(%require feature))

(define-macro (export-all)
  `',(%export-all))

(define-macro (autoload file . vars)
  `(%autoload (current-module) ',file ',vars))

;; Preferred way
;;  (use x.y.z) ==> (require "x/y/z") (import x.y.z)

(define-macro (use module)
  `(begin
     (with-module gauche
       (require ,(module-name->path module)))
     (import ,module)))

;; create built-in modules, so that (use srfi-6) won't complain, for example.
(define-module srfi-6 )
(define-module srfi-8 )
(define-module srfi-10 )
(define-module srfi-17 )

;; for backqard compatibility
(define-module gauche.vm.debugger )

;;
;; Auxiliary definitions
;;

(define-in-module scheme call/cc call-with-current-continuation)

(define-in-module scheme (call-with-values producer consumer)
  (receive vals (producer) (apply consumer vals)))

(define <exception> <condition>) ;; backward compatibility

(define-reader-ctor 'string-interpolate
  (lambda (s) (string-interpolate s))) ;;lambda is required to delay loading

;; srfi-17
(define (getter-with-setter get set)
  (let ((proc (lambda x (apply get x))))
    (set! (setter proc) set)
    proc))

;; print (as in SCM, Chicken)
(define (print . args) (for-each display args) (newline))

;; srfi-38
(define read-with-shared-structure read)
(define read/ss read)

(define (write-with-shared-structure obj . args)
  (write* obj (if (pair? args) (car args) (current-output-port))))
(define write/ss write-with-shared-structure)

;; format
(define format (undefined))
(define format/ss (undefined))
(let ((format-int
       (lambda (port fmt args shared?)
         (cond ((eqv? port #f)
                (let ((out (open-output-string :private? #t)))
                  (%format out fmt args shared?)
                  (get-output-string out)))
               ((eqv? port #t)
                (%format (current-output-port) fmt args shared?))
               (else (%format port fmt args shared?))))))
  (set! format
        (lambda (fmt . args)
          (if (string? fmt)
            (format-int #f fmt args #f) ;; srfi-28 compatible behavior
            (format-int fmt (car args) (cdr args) #f))))
  (set! format/ss
        (lambda (fmt . args)
          (if (string? fmt)
            (format-int #f fmt args #t) ;; srfi-28 compatible behavior
            (format-int fmt (car args) (cdr args) #t))))
  )

;;
;; Load object system
;;

(require "gauche/object")

