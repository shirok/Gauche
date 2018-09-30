;; -*- coding:utf-8 -*-
;;;
;;; srfi-154
;;;

;; Based on the reference implementation, modified to fit Gauche style.
;; NB: This is too slow for actual use because of the heavy use of call/cc.
;; We'd probably expose internal dynamic extent chain.

;; Copyright notice of the reference implementation:
;;
;; Copyright (C) Marc Nieper-Wißkirchen (2017).  All Rights Reserved.
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-module srfi-154
  (export dynamic-extent?
          current-dynamic-extent
          with-dynamic-extent
          dynamic-lambda))
(select-module srfi-154)

(define-class <dynamic-extent> ()
  ((run :init-keyword :run)))

(define (dynamic-extent? obj) (is-a? obj <dynamic-extent>))

(define (current-dynamic-extent)
  (let/cc return
    (receive (k thunk) (let/cc c
                         (return
                          (make <dynamic-extent>
                            :run (^[thunk]
                                   (let/cc k (c k thunk))))))
      (call-with-values thunk k))))

(define (with-dynamic-extent dynamic-extent thunk)
  ((slot-ref dynamic-extent 'run) thunk))

(define-syntax dynamic-lambda
  (syntax-rules ()
    [(_ formals body)
     (let1 dynamic-extent (current-dynamic-extent)
       (^ formals
	 (with-dynamic-extent dynamic-extent (^[] body))))]))


