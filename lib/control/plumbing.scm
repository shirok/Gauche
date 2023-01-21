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

(define-module control.plumbing
  (use data.queue)
  (use gauche.record)
  (use gauche.threads)
  (use gauche.uvector)
  (use gauche.vport)

  (export make-pipe pipe-inlet? pipe-outlet?
          duplicate-pipe-inlet duplicate-pipe-outlet
          make-pump open-tapping-pump-port)
  )
(select-module control.plumbing)

;; A common structure to access and control particular configuration of
;; a plubming system.
;; The internal data access is hidden within the 'accessor' closure.

(define-class <plumbing-device> ()
  ;; closure to hide internals
  ((accessor :init-keyword :accessor)))

(define-generic plumbing-device-queue-size)
(define-generic plumbing-device-thread)

(define-method plumbing-device-queue-size ((c <plumbing-device>))
  ((~ c'accessor) plumbing-device-queue-size))

(define-method plumbing-device-thread ((c <plumbing-device>))
  ((~ c'accessor) plumbing-device-thread))

;;=======================================================
;; Pipes
;;

;; A pipe is a passive device that has one or more inlets (output ports)
;; and one or more outlets (input ports).  Data pushed into the inlets
;; will be available to read from outlets.
;; It is passive, that is, no active thread is monitoring inputs.
;; If all inlets are closed, the outlets read EOF.
;;
;; Unlike sys-pipe, each outlets have individual buffers, so each reader
;; gets the entire contents independently.  In general, you want to use
;; sys-pipe when you do inter-process communication, while this pipe is
;; handy for intra-thread communication.

(define-class <pipe-impl> ()
  ([qs :init-keyword :qs]
   [inlets :init-value '()]
   [outlets :init-value '()]))

(define-class <pipe> ()
  (;; All slots are private
   [impl :init-keyword :impl]))         ;(atom <pipe-impl>)

(define (%open-pipe-inlet pipe)
  (define (flusher buffer flag)
    (let1 data (u8vector-copy buffer)
      (atomic (~ pipe'impl)
              (^d (dolist [q (~ d'qs)]
                    (enqueue/wait! q data))))
      (u8vector-length data)))
  (define (closer)
    (atomic (~ pipe'impl)
            (^d (update! (~ d'inlets) (cut delete port <>))
                (when (null? (~ d'inlets))
                  (dolist [q (~ d'qs)] (enqueue/wait! q (eof-object)))))))
  (define port
    (make <buffered-output-port> :flush flusher :close closer))
  (port-attribute-set! port 'plumbing-device pipe)
  (atomic (~ pipe'impl) (^d (push! (~ d'inlets) port)))
  port)

(define (%open-pipe-outlet pipe)
  ($ atomic (~ pipe'impl)
     (^d (let1 mtq (if (null? (~ d'outlets))
                     (car (~ d'qs))
                     (rlet1 q (make-mtqueue)
                       (push! (~ d'qs) q)))
           (define (filler buf)
             (let ([len (u8vector-length buf)]
                   [data (dequeue/wait! mtq)])     ;this may block
               (cond [(eof-object? data) data]
                     [(<= (u8vector-length data) len)
                      (u8vector-copy! buf 0 data 0)
                      (u8vector-length data)]
                     [else
                      (u8vector-copy! buf 0 data 0 len)
                      (queue-push/wait! mtq (uvector-alias <u8vector> data len))
                      len])))
           (define port (make <buffered-input-port> :fill filler))
           (port-attribute-set! port 'plumbing-device pipe)
           (push! (~ d'outlets) port)
           port))))

(define (%port-pipe port)
  (port-attribute-ref port 'plumbing-device #f))

;; Returns two values, an iport and an oport.  The iport is the pipe's outlet,
;; and the oport is the pipe's inlet.
(define (make-pipe)
  (let1 device (make <pipe>
                 :impl (atom (make <pipe-impl> :qs (list (make-mtqueue)))))
    (values (%open-pipe-outlet device)
            (%open-pipe-inlet device))))

(define (pipe-inlet? oport)
  (and (output-port? oport)
       (is-a? (%port-pipe oport) <pipe>)))

(define (pipe-outlet? iport)
  (and (input-port? iport)
       (is-a? (%port-pipe iport) <pipe>)))

;; Create another pipe inlet from the existing pipe inlet
(define (duplicate-pipe-inlet oport)
  (assume (pipe-inlet? oport) "Argument is not a pipe inlet:" oport)
  (%open-pipe-inlet (%port-pipe oport)))

;; Create another pipe outlet from the existing pipe outlet
(define (duplicate-pipe-outlet iport)
  (assume (pipe-outlet? iport) "Argument is not a pipe outlet:" iport)
  (%open-pipe-outlet (%port-pipe iport)))

;;=======================================================
;; Pumps
;;

;; A pump is an active device to pull the data from the given iport(s) and
;; feed it to the given oport(s).   It spawns thread(s) to handle the
;; data, so that the data feed is handled autonomously.

;; Common routine to handle 'unit' keyword argument.  Returns a reader
;; and a writer.
(define (%unit-reader&writer unit)
  (case unit
    [(:byte) (values read-byte write-byte)]
    [(:char) (values read-char write-char)]
    [else (assume (and (exact-integer? unit) (>= unit 0))
                  "A nonnegative exact integer, :byte, or :char expected \
                   for unit argument, but got: " unit)
          (let1 chunk-size (if (zero? unit) 4096 unit)
            (values (^[port] (read-uvector <u8vector> chunk-size port))
                    (^[obj port] (write-uvector obj port))))]))

;; internal routine to connect iport and oport, returns the handler thread
;; without starting.
(define (%make-pump-1 iport oport oport-owned?
                      reader writer
                      input-callback)
  (define (handler)
    (let loop ()
      ;; We ignore errors; it prevents the thread to exit inadvertently.
      ;; However, it has a risk to go into busy loop if error condition
      ;; persists.  We'll revisit this later.
      (unless (guard [e (else #f)]
                (let1 data (reader iport)
                  (unless (port-closed? oport)
                    (if (eof-object? data)
                      (when oport-owned? (close-output-port oport))
                      (writer data oport)))
                  (if (eof-object? data)
                    (begin (close-input-port iport) #t)
                    (begin (when input-callback (input-callback data)) #f))))
        (loop))))
  (make-thread handler))

(define (make-pump iport oport
                   :key (own-output #f)
                        (unit 0))
  (define-values (reader writer) (%unit-reader&writer unit))
  (define thread (%make-pump-1 iport oport own-output
                               reader writer #f))
  (define (accessor key . args)
    (cond
     [(eq? key plumbing-device-thread) thread]
     [else (error "Unsupported method:" key)]))
  (make <plumbing-device> :accessor accessor))

;;=======================================================
;; Tapping ports
;;

;; EXPERIMENTAL - This depends on gauche.threads and data.queue, so we
;; might want to split this to a separate module (either util.* or control.*)
;; but not sure which.

;; open-tapping-port iport oport :key close-output
;;   Run a thread that copies data from iport and oport.  Returns an
;;   input port, from which you can read the data read from iport;
;;   that is, you can 'tap' the data stream flowing from iport to oport.
;;
;;   Note that the tapped data is accumulated until it si read from
;;   the returned port.  If you're done with tapping, you can close the
;;   returned port to prevent further accumulation of the data.
;;   The dataflow from iport to oport keeps running until iport reaches EOF.
;;
;;   Once iport reaches EOF, the thread terminates.  Additionally, oport is
;;   closed if :close-output argument is #t.
;;
;;   :unit is similar to the :unit argument of copy-port.

(define (open-tapping-pump-port iport oport
                                :key (close-output #f)
                                     (unit 0))
  (define mtq (make-mtqueue))
  (define tee-closed #f)
  (define straight-closed #f)
  (define queue-size 0)
  (define-values (reader writer) (%unit-reader&writer unit))
  (define (filler buf)
    (let ([len (u8vector-length buf)]
          [data (dequeue/wait! mtq)])     ;this may block
      (cond [(eof-object? data) data]
            [(<= (u8vector-length data) len)
             (u8vector-copy! buf 0 data 0)
             (u8vector-length data)]
            [else
             (u8vector-copy! buf 0 data 0 len)
             (queue-push/wait! mtq (uvector-alias <u8vector> data len))
             len])))
  (define (closer)
    (set! tee-closed #t))
  (define (input-callback data)
    (unless tee-closed
      (enqueue/wait! mtq data)))
  (define thread (%make-pump-1 iport oport close-output reader writer
                               input-callback))
  (define (accessor key . args)
    (cond
     [(eq? key plumbing-device-thread) thread]
     [(eq? key plumbing-device-queue-size) queue-size]
     [else (error "Unsupported method:" key)]))
  (thread-start! thread)
  (rlet1 p (make <buffered-input-port> :fill filler :close closer)
    (port-attribute-set! p 'plumbing-device
                         (make <plumbing-device> :accessor accessor))))
