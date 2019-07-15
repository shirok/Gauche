;;;
;;; selector - simple event loop by select()
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


(define-module gauche.selector
  (use srfi-1)
  (export <selector> selector-add! selector-delete! selector-select)
  )
(select-module gauche.selector)

(define-class <selector> ()
  ((rfds :init-form #f)
   (wfds :init-form #f)
   (xfds :init-form #f)
   (rhandlers :init-form '())  ; list of (port-or-fd . proc)
   (whandlers :init-form '())  ; ditto
   (xhandlers :init-form '())  ; ditto
  ))

(define (canon-flag flag)
  (case flag
    [(r read) 'r]
    [(w write) 'w]
    [(x exception) 'x]
    [else (errorf "invalid flag ~s, must be r, w, or x" flag)]))

(define (flag->fd-slot flag)
  (case flag
    [(r) 'rfds] [(w) 'wfds] [(x) 'xfds]))

(define (flag->handler-slot flag)
  (case flag
    [(r) 'rhandlers] [(w) 'whandlers] [(x) 'xhandlers]))

(define-method selector-add! ((selector <selector>) port-or-fd proc flags)
  (assume-type proc <procedure>)
  (assume-type flags <list>)
  (dolist [flag (map canon-flag flags)]
    (let* ([slot (flag->fd-slot flag)]
           [fds (or (slot-ref selector slot)
                    (rlet1 f (make <sys-fdset>)
                      (slot-set! selector slot f)))])
      (set! (sys-fdset-ref fds port-or-fd) #t))
    (slot-push! selector (flag->handler-slot flag) (cons port-or-fd proc))))

(define-method selector-delete! ((selector <selector>) port-or-fd proc flags)
  (let1 flags (if flags (map canon-flag flags) '(r w x))
    (for-each (^[fds handlers]
                (cond
                 [port-or-fd
                  (if-let1 p (assoc port-or-fd (slot-ref selector handlers))
                    (when (or (not proc) (eq? proc (cdr p)))
                      (slot-set! selector handlers
                                 (delete p (slot-ref selector handlers)))
                      (if-let1 fds (slot-ref selector fds)
                        (sys-fdset-set! fds port-or-fd #f))))]
                 [proc
                  (let loop ([h (slot-ref selector handlers)]
                             [newh '()])
                    (cond [(null? h)
                           (slot-set! selector handlers (reverse newh))]
                          [(eq? proc (cdar h))
                           (if-let1 fds (slot-ref selector fds)
                             (sys-fdset-set! fds (caar h) #f))
                           (loop (cdr h) newh)]
                          [else
                           (loop (cdr h) (cons (car h) newh))]))]
                 [else
                  (slot-set! selector fds #f)
                  (slot-set! selector handlers '())]))
              (map flag->fd-slot flags)
              (map flag->handler-slot flags))))

(define-method selector-select ((selector <selector>) :optional (timeout #f))

  (define (pick-handlers fds handlers flag)
    (fold (^[entry tail]
            (let1 fd (car entry)
              (if (sys-fdset-ref fds fd)
                (cons (list (cdr entry) fd flag) tail)
                tail)))
          '()
          handlers))

  (receive (nfds rfds wfds xfds)
      (sys-select (slot-ref selector 'rfds)
                  (slot-ref selector 'wfds)
                  (slot-ref selector 'xfds)
                  timeout)
    (when (> nfds 0)
      (for-each (^h (apply (car h) (cdr h)))
                (append
                 (pick-handlers rfds (slot-ref selector 'rhandlers) 'r)
                 (pick-handlers wfds (slot-ref selector 'whandlers) 'w)
                 (pick-handlers xfds (slot-ref selector 'xhandlers) 'x))))
    nfds))
