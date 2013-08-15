;;; args-fold.scm - a program argument processor
;;;
;;; Copyright (c) 2002 Anthony Carrico
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the authors may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Modified by Shiro Kawai to adapt to Gauche
;;;  * Module stuff added.
;;;  * Replaced a srfi-9 record type for a class.
;;;  * Replaced some expressions to utilize Gauche's native features.

(define-module srfi-37
  (use srfi-1)
  (use srfi-11)
  (export option option-names option-required-arg? option-optional-arg?
          option-processor option? args-fold)
  )
(select-module srfi-37)

(define-class <option-type> ()
  ((names :init-keyword :names :getter option-names)
   (required-arg? :init-keyword :required-arg? :getter option-required-arg?)
   (optional-arg? :init-keyword :optional-arg? :getter option-optional-arg?)
   (processor :init-keyword :processor :getter option-processor)
   ))

(define (option names required-arg? optional-arg? processor)
  (make <option-type>
    :names names :required-arg? required-arg?
    :optional-arg? optional-arg? :processor processor))

(define (option? obj) (is-a? obj <option-type>))

(define (args-fold args options unrecognized-option-proc operand-proc . seeds)
  (define (find-option name)
    (find (^[option] (find (cut equal? name <>) (option-names option))) options))
  (define (scan-short-options index shorts args seeds)
    (if (= index (string-length shorts))
      (scan-args args seeds)
      (let* ([name (string-ref shorts index)]
             [option (or (find-option name)
                         (option (list name) #f #f
                                 unrecognized-option-proc))])
        (cond [(and (< (+ index 1) (string-length shorts))
                    (or (option-required-arg? option)
                        (option-optional-arg? option)))
               (receive seeds
                   (apply (option-processor option) option name
                          (substring shorts (+ index 1)
                                     (string-length shorts))
                          seeds)
                 (scan-args args seeds))]
              [(and (option-required-arg? option)
                    (pair? args))
               (receive seeds
                   (apply (option-processor option) option name
                          (car args) seeds)
                 (scan-args (cdr args) seeds))]
              [else
               (receive seeds
                   (apply (option-processor option) option name #f seeds)
                 (scan-short-options (+ index 1) shorts args seeds))]
              ))))
  (define (scan-operands operands seeds)
    (if (null? operands)
      (apply values seeds)
      (receive seeds (apply operand-proc (car operands) seeds)
        (scan-operands (cdr operands) seeds))))
  (define (scan-args args seeds)
    (if (null? args)
      (apply values seeds)
      (let ([arg (car args)]
            [args (cdr args)])
        (cond
         [(string=? "--" arg)
          ;; End option scanning:
          (scan-operands args seeds)]
         [(#/^--([^=]+)=(.*)$/ arg)
          ;; Found long option with arg:
          => (^m (let*-values ([(name) (m 1)]
                               [(option-arg) (m 2)]
                               [(option)
                                (or (find-option name)
                                    (option (list name) #t #f
                                            unrecognized-option-proc))]
                               [seeds
                                (apply (option-processor option) option name
                                       option-arg seeds)])
                   (scan-args args seeds)))]
         [(#/^--(.+)$/ arg)
          ;; Found long option:
          => (^m (let* ([name (m 1)]
                        [option (or (find-option name)
                                    (option (list name) #f #f
                                            unrecognized-option-proc))])
                   [if (and (option-required-arg? option)
                            (pair? args))
                     (receive seeds
                         (apply (option-processor option) option name
                                (car args) seeds)
                       (scan-args (cdr args) seeds))
                     (receive seeds
                         (apply (option-processor option) option name
                                #f seeds)
                       (scan-args args seeds))]))]
         [(#/^-(.+)$/ arg)
          ;; Found short options
          => (^m (scan-short-options 0 (m 1) args seeds))]
         [else
          (receive seeds (apply operand-proc arg seeds)
            (scan-args args seeds))]
         )
        )))
  (scan-args args seeds))

