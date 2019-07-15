;;;
;;; serializer.scm - generic serializer framework
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
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

;; THIS MODULE IS UNDER DEVELOPMENT.
;;  This is a straightforward port from the module I wrote for STk before.
;;  I need to rethink API to make it reasonably extendable.

(define-module gauche.serializer
  (export <serializer>
          write-to-serializer
          read-from-serializer
          port-of
          direction-of
          serializer?
          input-serializer?
          output-serializer?
          write-to-string-with-serializer
          write-to-file-with-serializer
          read-from-string-with-serializer
          read-from-file-with-serializer
          get-serializable-slots)
  )
(select-module gauche.serializer)

;;;
;;; Class <SERIALIZER>
;;;
;;;   Defines a common interface of the serializer
;;;

(define-class <serializer> ()
  ((port       :init-keyword :port      :getter port-of)
   (direction  :init-keyword :direction :getter direction-of)
   ))

(define-method initialize ((self <serializer>) initargs)
  (next-method)
  (if (or (not (slot-bound? self 'port))
          (not (or (input-port? (port-of self))
                   (output-port? (port-of self)))))
      (error "<serializer> class requires :port argument in initialization"))
  (if (not (slot-bound? self 'direction))
      (if (input-port? (port-of self))
          (slot-set! self 'direction :in)
          (slot-set! self 'direction :out))
      (let ((dir (direction-of self)))
        (unless (or (and (eq? dir :in) (input-port? (port-of self)))
                    (and (eq? dir :out) (output-port? (port-of self))))
          (error "While initializing <serializer>, port type and direction don't match"))))
  )

(define-method write-to-serializer ((self <serializer>) object)
  ;; need to be implemented by a derived class
  '()
  )

(define-method read-from-serializer ((self <serializer>))
  ;; need to be implemented by a derived class
  '()
  )

(define (serializer? obj) (is-a? obj <serializer>))
(define (input-serializer? obj)
  (and (serializer? obj) (eq? (direction-of obj) :in)))
(define (output-serializer? obj)
  (and (serializer? obj) (eq? (direction-of obj) :out)))

;; Utility method

(define-method write-to-string-with-serializer ((class <class>) obj . options)
  (let1 out (open-output-string)
    (write-to-serializer
     (apply make class :port out :direction :out options)
     obj)
    (get-output-string out)))

(define-method read-from-string-with-serializer ((class <class>) str . options)
  (let1 in (open-input-string str)
    (read-from-serializer
     (apply make class :port in :direction :in options))))

(define-method write-to-file-with-serializer ((class <class>) obj file . options)
  (call-with-output-file file
   (^[port]
     (write-to-serializer
      (apply make class :port port :direction :out options)
      obj))))

(define-method read-from-file-with-serializer ((class <class>) file . options)
  (call-with-input-file file
   (^[port]
     (read-from-serializer
      (apply make class :port port :direction :in options)))))

;; For generic object serializer
(define-method get-serializable-slots ((obj <object>))
  (let loop ([slots (class-slots (class-of obj))]
             [result '()])
    (cond [(null? slots) (reverse result)]
          [(not (eqv? (slot-definition-allocation (car slots)) :virtual))
           (loop (cdr slots) (cons (slot-definition-name (car slots)) result))]
          [else (loop (cdr slots) result)])))


