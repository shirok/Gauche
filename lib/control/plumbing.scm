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

;; This module provides utilities that uses ports for communication
;; between threads and/or processes.

;; TODO: Flow control.  Hold inlets if internal buffer reaches a certain limit.

(define-module control.plumbing
  (use data.queue)
  (use gauche.record)
  (use gauche.threads)
  (use gauche.uvector)
  (use gauche.vport)
  (use scheme.list)                     ;list-tabulate
  (use util.match)

  (export make-plumbing plumbing? plumbing-inlet-ports plumbing-outlet-ports
          plumbing-get-port port-plumbing
          open-inlet-output-port add-inlet-input-port!
          open-outlet-input-port add-outlet-output-port!

          open-broadcast-output-port open-tapping-input-port
          make-pipe make-pump
          plumbing
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

;; Private classes - they're not exposed to outside.
(define-class <plumbing-impl> ()
  ((outlets :init-value '())
   (inlets :init-value '())))

(define-class <plumbing-outlet> ()
  ((name :init-keyword :name)
   (port  :init-keyword :port)
   (put   :init-keyword :put)     ; (^ impl u8vector) -> void
   (close :init-keyword :close)   ; (^ impl) -> void
   (thread :init-value #f)))

(define-class <plumbing-inlet> ()
  ((name :init-keyword :name)
   (port :init-keyword :port)
   (thread :init-keyword :thread)))

;; Key for port attributes.  Ports created by this module
;; holds the originating <plumbing> object in this attribute.
(define *plumbing-key* '#:plumbing)

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

;; API
(define (plumbing? obj) (is-a? obj <plumbing>))

(define-syntax %with-locked-plumbing
  (syntax-rules ()
    [(_ plumbing proc)
     (atomic (~ plumbing'impl) proc)]))

;; API
(define (plumbing-inlet-ports plumbing)
  (assume-type plumbing <plumbing>)
  ($ %with-locked-plumbing plumbing
     (^[impl] (map (cut ~ <> 'port) (~ impl'inlets)))))

;; API
(define (plumbing-outlet-ports plumbing)
  (assume-type plumbing <plumbing>)
  ($ %with-locked-plumbing plumbing
     (^[impl] (map (cut ~ <> 'port) (~ impl'outlets)))))

;; API
(define (plumbing-get-port plumbing name)
  (assume-type plumbing <plumbing>)
  (assume-type name <symbol>)
  ($ %with-locked-plumbing plumbing
     (^[impl]
       (define (port-with-name x) (and (eq? (~ x'name) name) (~ x'port)))
       (or (any port-with-name (~ impl'outlets))
           (any port-with-name (~ impl'inlets))))))

;; API
(define (port-plumbing port)
  (assume-type port <port>)
  (port-attribute-ref port *plumbing-key* #f))

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
(define (open-inlet-output-port plumbing :optional (name #f))
  (define inlet (make <plumbing-inlet> :thread #f :name name))
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
  (port-attribute-set! port *plumbing-key* plumbing)
  (set! (~ inlet'port) port)
  (%with-locked-plumbing plumbing (cut %add-inlet! plumbing <> inlet))
  port)

;; API
(define (add-inlet-input-port! plumbing iport :optional (name #f))
  (define inlet (make <plumbing-inlet> :port iport :name name))
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
  (thread-start! thread)
  plumbing)

;;----------------------------------------------------------
;; outlets
;;

;; API
(define (add-outlet-output-port! plumbing oport
                                 :optional (name #f)
                                 :key (close-on-eof #f)
                                      (asynchronous #f))
  (define outlet
    (if asynchronous
      (%make-async-output-outlet! plumbing oport name close-on-eof)
      (%make-simple-output-outlet! plumbing oport name close-on-eof)))
  (%with-locked-plumbing plumbing (cut %add-outlet! plumbing <> outlet))
  plumbing)

(define (%make-simple-output-outlet! plumbing oport name close-on-eof)
  (define outlet (make <plumbing-outlet>
                   :name name
                   :port oport
                   :put (^[impl data] (write-uvector data oport))
                   :close (^[impl]
                            (when close-on-eof
                              (close-output-port oport))
                            (%delete-outlet! plumbing impl outlet))))
  outlet)

(define (%make-async-output-outlet! plumbing oport name close-on-eof)
  (define mtq (make-mtqueue))
  (define (feeder)
    (let1 data (dequeue/wait! mtq)
      (if (eof-object? data)
        (begin (when close-on-eof
                 (close-output-port oport))
               ($ %with-locked-plumbing plumbing
                  (cut %delete-outlet! plumbing <> outlet)))
        (begin (write-uvector data oport)
               (feeder)))))
  (define outlet
    (make <plumbing-outlet>
      :name name
      :port oport
      :put (^[impl data] (enqueue/wait! mtq data))
      :close (^[impl] (enqueue/wait! mtq (eof-object)))))
  (set! (~ outlet'thread) (thread-start! (make-thread feeder)))
  outlet)

;; API
(define (open-outlet-input-port plumbing :optional (name #f))
  (define mtq (make-mtqueue))
  (define eof-reached? #f)
  (define outlet
    (make <plumbing-outlet>
      :name name
      :put (^[impl data] (enqueue/wait! mtq data))
      :close (^[impl] (enqueue/wait! mtq (eof-object)))))
  (define (filler buf)
    (if eof-reached?
      (eof-object)
      (let ([len (u8vector-length buf)]
            [data (dequeue/wait! mtq)])     ;this may block
        (cond [(eof-object? data)
               (set! eof-reached? #t)
               ($ %with-locked-plumbing plumbing
                  (cut %delete-outlet! plumbing <> outlet))
               data]
              [(<= (u8vector-length data) len)
               (u8vector-copy! buf 0 data 0)
               (u8vector-length data)]
              [else
               (u8vector-copy! buf 0 data 0 len)
               (queue-push/wait! mtq (uvector-alias <u8vector> data len))
               len]))))
  (define (ready?)
    (or eof-reached? (not (queue-empty? mtq))))
  (define port (make <buffered-input-port> :fill filler :ready ready?))
  (set! (~ outlet'port) port)
  (port-attribute-set! port *plumbing-key* plumbing)
  (set! (port-buffering port) :none)
  (%with-locked-plumbing plumbing (cut %add-outlet! plumbing <> outlet))
  port)

;;;
;;; Convenience utilities
;;;

;; CL's make-broadcast-stream
(define (open-broadcast-output-port . oports)
  (define plumbing (make-plumbing))
  (dolist [oport oports]
    (add-outlet-output-port! plumbing oport #f))
  (open-inlet-output-port plumbing))

;; Pipe owns one or more inlet oports and one or more outlet iports.
;; Returns a list of inlet oports and a list of outlet iports.
(define (make-pipe :key (num-inlets 1) (num-outlets 1))
  (assume (and (exact-integer? num-inlets)
               (>= num-inlets 1))
          "One or more exact integer is expected, but got" num-inlets)
  (assume (and (exact-integer? num-outlets)
               (>= num-outlets 1))
          "One or more exact integer is expected, but got" num-outlets)
  (let1 plumbing (make-plumbing)
    (values (list-tabulate num-inlets (^_ (open-inlet-output-port plumbing)))
            (list-tabulate num-outlets (^_ (open-outlet-input-port plumbing))))))

;; Create a 'pump' - a device that reads from inlet-iport(s) and
;; writes out to outlet-oport(s), run in an independent thread.
;; Returns a plumbing.
(define (make-pump inlet-iports outlet-oports)
  (assume (every input-port? inlet-iports))
  (assume (every output-port? outlet-oports))
  (rlet1 plumbing (make-plumbing)
    (dolist [ip inlet-iports]
      (add-inlet-input-port! plumbing ip))
    (dolist [op outlet-oports]
      (add-outlet-output-port! plumbing op #f))))

;; Similar to CL's make-echo-stream, but we support only reading from
;; the craeted port.  We may make it bidirectional stream later.
(define (open-tapping-input-port inlet-iport outlet-oport :key (close-on-eof #f))
  (assume (input-port? inlet-iport))
  (assume (output-port? outlet-oport))
  (let1 plumbing (make-plumbing)
    (add-inlet-input-port! plumbing inlet-iport)
    (add-outlet-output-port! plumbing outlet-oport #f :close-on-eof close-on-eof)
    (open-outlet-input-port plumbing)))

;; A convenience routine to create a plumbing and attach inlets&outlets.
;;
;; Each argument can be one of the following forms:
;;   (< #<iport> [name])  ; add iport as inlet
;;   (< name)             ; create inlet oport with the name
;;   (> #<oport> [name] [(option ...)]) ; add oport as outlet
;;   (> name)             ; create outlet iport with the name
;;
;; NAME is a symbol.  OPTION can be :coe, :close-on-eof, or :async.
;;
;; Returns a plumbing.  Created ports can be accessed by their name.

(define (plumbing . args)
  (rlet1 p (make-plumbing)
    (define (add-oo! port name opts)
      ($ apply add-outlet-output-port! p port name
         $ fold-right (^[opt r]
                        (case opt
                          [(:coe :close-on-eof) `(:close-on-eof #t ,@r)]
                          [(:async :asynchronous) `(:asynchronous #t ,@r)]
                          [else (error "Unrecognized outlet option:" opt)]))
         '() opts))
    (dolist [arg args]
      (match arg
        [('< (? symbol? name)) (open-inlet-output-port p name)]
        [('< (? input-port? port)) (add-inlet-input-port! p port)]
        [('< (? input-port? port) (? symbol? name))
         (add-inlet-input-port! p port name)]
        [('< . _) (error "Invalid inlet spec:" arg)]
        [('> (? symbol? name)) (open-outlet-input-port p name)]
        [('> (? output-port? port)) (add-oo! port #f '())]
        [('> (? output-port? port) (? symbol? name)) (add-oo! port name '())]
        [('> (? output-port? port) (opts ...)) (add-oo! port #f opts)]
        [('> (? output-port? port) (? symbol? name) (opts ...))
         (add-oo! port name opts)]
        [('> . _) (error "Invalid outlet spec:" arg)]
        [_ (error "Invalid plumbing spec:" arg)]))))
