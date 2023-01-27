;;;
;;; control.plumbing - Ports as communication channel
;;;
;;;   Copyright (c) 2023  Shiro Kawai  <shiro@acm.org>
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

;; EXPERIMENTAL

;; This module provides utilities that uses ports for communication
;; between threads and/or processes.

;; TODO: Flow control.  Hold inlets if internal buffer reaches a certain limit.

(define-module control.plumbing
  (use data.queue)
  (use gauche.record)
  (use gauche.threads)
  (use gauche.uvector)
  (use gauche.vport)
  (use util.match)

  (export make-plumbing
          open-inlet-output-port add-inlet-input-port!
          open-outlet-input-port add-outlet-output-port!

          open-broadcast-output-port
          make-pump
          )
  )
(select-module control.plumbing)

;; A plumbing is a device that takes data from one or more ports (inlets)
;; and feed them to one or more ports (outlets).  Producers and consumers
;; can be different threads, and synchronization is handled within the
;; device.

(define-class <plumbing> ()
  ;; All internals are private
  ((impl :init-keyword :impl)         ;(atom <plumbing-impl>)
   ;; These two slots are for information only.  They are used for external
   ;; representation.  We don't want to lock the object just to display it,
   ;; so they're outside of atom.
   (num-inlets :init-value 0)
   (num-outlets :init-value 0)))

(define-method write-object ((p <plumbing>) port)
  (format port "#<plumbing ~a inlet~:p, ~a outlet~:p>"
          (~ p'num-inlets)
          (~ p'num-outlets)))

;; Private classes
(define-class <plumbing-impl> ()
  ((outlets :init-value '())
   (inlets :init-value '())))

(define-class <plumbing-outlet> ()
  ((put :init-keyword :put)
   (close :init-keyword :close)))

(define-class <plumbing-inlet> ()
  ((port :init-keyword :port)
   (thread :init-keyword :thread)))

;; The direction of the port (input/output) and whether it is used
;; as inlet or outlet for a plumbing is independent.  We have the
;; following combinations:
;;
;;  inlet oport - A producer pushes the data to a plumbing.
;;
;;  inlet iport - A producer make data available to read.  The plumbing
;;                uses a dedicated thread to pull the data from it.
;;
;;  outlet oport - Data flows through the plubming is pushed to the oport.
;;
;;  outlet iport - Data flows through the plumbing is buffered and
;;                 made available to the consumer.
;;
;; Inlet oports and outlet iports are created by the plumbing.  Inlet iports
;; and outlet oports should be provided by the user.

;; API
(define (make-plumbing)
  (make <plumbing> :impl (atom (make <plumbing-impl>))))


(define-syntax %with-locked-plumbing
  (syntax-rules ()
    [(_ plumbing proc)
     (atomic (~ plumbing'impl) proc)]))

;; impl is assumed to be locked in the following procedures
(define (%add-inlet! plumbing impl inlet)
  (push! (~ impl'inlets) inlet)
  (set! (~ plumbing'num-inlets) (length (~ impl'inlets))))

(define (%delete-inlet! plumbing impl inlet)
  (update! (~ impl'inlets) (cut delete inlet <>))
  (set! (~ plumbing'num-inlets) (length (~ impl'inlets))))

(define (%add-outlet! plumbing impl outlet)
  (push! (~ impl'outlets) outlet)
  (set! (~ plumbing'num-outlets) (length (~ impl'outlets))))

(define (%delete-outlet! plumbing impl outlet)
  (update! (~ impl'outlets) (cut delete outlet <>))
  (set! (~ plumbing'num-outlets) (length (~ impl'outlets))))

;;----------------------------------------------------------
;; inlets
;;

;; API
(define (open-inlet-output-port plumbing)
  (define inlet (make <plumbing-inlet> :thread #f))
  (define (flusher buffer flag)
    (let1 data (u8vector-copy buffer)
      ($ %with-locked-plumbing plumbing
         (^[impl] (dolist [o (~ impl'outlets)]
                    ((~ o'put) impl data))))
      (u8vector-length data)))
  (define (closer)
    ($ %with-locked-plumbing plumbing
       (^[impl]
         (%delete-inlet! plumbing impl inlet)
         (when (null? (~ impl'inlets))
           (dolist [o (~ impl'outlets)]
             ((~ o'close) impl))))))
  (define port
    (make <buffered-output-port> :flush flusher :close closer))
  (port-attribute-set! port 'plumbing plumbing)
  (set! (~ inlet'port) port)
  (%with-locked-plumbing plumbing (cut %add-inlet! plumbing <> inlet))
  port)

;; API
(define (add-inlet-input-port! plumbing iport)
  (define inlet (make <plumbing-inlet> :port iport))
  (define (pump)
    (let1 data (read-uvector <u8vector> 4096 iport)
      (if (eof-object? data)
        ($ %with-locked-plumbing plumbing
           (^[impl]
             (%delete-inlet! plumbing impl inlet)
             (when (null? (~ impl'inlets))
               (dolist [o (~ impl'outlets)]
                 ((~ o'close) impl)))))
        (begin
          ($ %with-locked-plumbing plumbing
             (^[impl]
               (dolist [o (~ impl'outlets)]
                 ((~ o'put) impl data))))
          (pump)))))
  (define thread (make-thread pump))
  (set! (~ inlet'thread) thread)
  (%with-locked-plumbing plumbing (cut %add-inlet! plumbing <> inlet))
  plumbing)

;;----------------------------------------------------------
;; outlets
;;

;; API
(define (add-outlet-output-port! plumbing oport :key (close-on-eof #f))
  (define outlet (make <plumbing-outlet>
                   :put (^[impl data] (write-uvector data oport))
                   :close (^[impl]
                            (when close-on-eof
                              (close-output-port oport))
                            (%delete-outlet! plumbing impl outlet))))
  (%with-locked-plumbing plumbing (cut %add-outlet! plumbing <> outlet))
  plumbing)

;; API
(define (open-outlet-input-port plumbing)
  (define mtq (make-mtqueue))
  (define outlet
    (make <plumbing-outlet>
      :put (^[impl data] (enqueue/wait! mtq data))
      :close (^[impl] (enqueue/wait! mtq (eof-object)))))
  (define (filler buf)
    (let ([len (u8vector-length buf)]
          [data (dequeue/wait! mtq)])     ;this may block
      (cond [(eof-object? data)
             ($ %with-locked-plumbing plumbing
                (cut %delete-outlet! plumbing <> outlet))
             data]
            [(<= (u8vector-length data) len)
             (u8vector-copy! buf 0 data 0)
             (u8vector-length data)]
            [else
             (u8vector-copy! buf 0 data 0 len)
             (queue-push/wait! mtq (uvector-alias <u8vector> data len))
             len])))
  (define port (make <buffered-input-port> :fill filler))
  (%with-locked-plumbing plumbing (cut %add-outlet! plumbing <> outlet))
  port)

;;;
;;; Convenience utilities
;;;

;; CL's make-broadcast-stream
;; Each arg can be an output port, or (<oport> <close-on-eof?>)
(define (open-broadcast-output-port . destinations)
  (define port&flags
    (map (^d (match d
               [(? output-port? oport) `(,oport #f)]
               [((? output-port? oport) flag) `(,oport ,(boolean flag))]
               [_ (error "An output port, or (<output-port> <flag>) is
                          expected, but got" d)]))
         destinations))
  (define plumbing (make-plumbing))
  (for-each (^p (add-outlet-output-port! plumbing (car p)
                                         :close-on-eof (cadr p)))
            port&flags)
  (open-inlet-output-port plumbing))

;; Create a 'pump' - a device that reads from inlet-iport and
;; writes out to outlet-oport, run in an independent thread.
;; Returns a plumbing.
(define (make-pump inlet-iport outlet-oport :key (close-on-eof #f))
  (assume (input-port? inlet-iport))
  (assume (output-port? outlet-oport))
  (rlet1 plumbing (make-plumbing)
    (add-inlet-input-port! plumbing inlet-iport)
    (add-outlet-output-port! plumbing outlet-oport :close-on-eof close-on-eof)))
