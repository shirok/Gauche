;;;
;;; Debugging aids
;;;
;;;   Copyright (c) 2000-2022  Shiro Kawai  <shiro@acm.org>
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
  (use scheme.list)
  (use srfi-13)
  (export debug-print debug-print-width
          debug-print-pre debug-print-post
          debug-source-info
          debug-thread-log debug-thread-pre debug-thread-post))
(select-module gauche.vm.debugger)

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
;; NB: We create a string by format then display, to avoid messages from
;; different threads won't intermixed.
(define (debug-print-pre form)
  (let1 thr-prefix (case (~ (current-thread)'vmid)
                     [(0) ""]
                     [else => (cut format "[~a]" <>)])
    (display (if-let1 info (debug-source-info form)
               (format "#?=~a~s:~a:~,,,,v:s\n" thr-prefix
                       (car info) (cadr info) (debug-print-width) form)
               (format "#?=~a~,,,,v:s\n" thr-prefix
                       (debug-print-width) form))
             (current-trace-port))
    (flush (current-trace-port))))

(define (debug-print-post vals)
  (let1 thr-prefix (case (~ (current-thread)'vmid)
                     [(0) ""]
                     [else => (cut format "[~a]" <>)])
    (if (null? vals)
      (display (format "#?-~a<void>\n" thr-prefix) (current-trace-port))
      (begin
        (display (format "#?-~a    ~,,,,v:s\n" thr-prefix
                         (debug-print-width) (car vals))
                 (current-trace-port))
        (for-each (^[elt]
                    (display (format/ss "#?+~a    ~,,,,v:s\n" thr-prefix
                                        (debug-print-width) elt)
                             (current-trace-port)))
                  (cdr vals))))
    (flush (current-trace-port))
    (apply values vals)))

;; debug-funcall
;; we need aux syntax definition, since we had to get hold of the original
;; form itself for the source code info, as well as decomposition of it.
(define-syntax debug-funcall
  (syntax-rules ()
    [(_ ?form) (debug-funcall-aux ?form ?form)]))

(define-syntax debug-funcall-aux
  (syntax-rules ()
    [(_ ?form (?proc ?arg ...))
     (debug-funcall-rec ?form ?proc (?arg ...) () ())]
    [(_ ?form ?_) ?form]))  ;; ignore on non-procedure call

(define-syntax debug-funcall-rec
  (syntax-rules ()
    [(_ ?form ?proc () (?tmp ...) (?arg ...))
     (let ((?tmp ?arg) ...)
       (debug-funcall-pre '?form '?proc '(?arg ...) (list ?tmp ...))
       (receive vals (?proc ?tmp ...)
         (debug-print-post vals)))]
    [(_ ?form ?proc (?a0 . ?as) (?tmp ...) (?arg ...))
     (debug-funcall-rec ?form ?proc ?as (?tmp ... tmp) (?arg ... ?a0))]))

;; For now, we don't use argforms, but we pass them in so that in future
;; we may be able to use them.
(define (debug-funcall-pre form procname argforms args)
  (define p (current-trace-port))
  (define thr-prefix (case (~ (current-thread)'vmid)
                       [(0) ""]
                       [else => (cut format "[~a]" <>)]))
  (define w (- (debug-print-width)
               (string-length "calling `' with args:")
               (string-length thr-prefix)))
  (define argvalw (- (debug-print-width)
                     (string-length "#?,- : ")
                     (string-length thr-prefix)))
  (cond [(debug-source-info form)
         => (^[info] (format p "#?,~a~s:~a:calling `~,,,,v:s' with args:\n"
                             thr-prefix (car info) (cadr info) w procname))]
        [else
         (format p "#?,~acalling `~,,,,v:s' with args:\n"
                 thr-prefix w procname)])
  (dolist [arg args]
    (format p "#?,>~a ~,,,,v:s\n" thr-prefix w arg))
  (flush p))

;; Per-thread logging --------------------------------------------

;; Can be (<thread> . <port>)
(define thread-log-sink (make-parameter #f))

(define thread-log-prefix (make-parameter "gauche-debug"))

(define (ensure-thread-log-sink)
  (let1 p (thread-log-sink)
    (if (and p (eq? (car p) (current-thread)))
      (cdr p)
      (rlet1 out (open-output-file (format "~a.~a.~a.log" (thread-log-prefix)
                                           (sys-getpid)
                                           (~ (current-thread)'vmid))
                                   :buffering :none)
        (thread-log-sink (cons (current-thread) out))))))

(define (write-to-thread-log x)
  (let1 p (ensure-thread-log-sink)
    (display x p)
    (flush p)))

(define (debug-thread-pre form)
  (receive (s ns) (sys-clock-gettime-monotonic)
    (let1 prefix (format "~9d.~9,'0d[~2d]" s ns (~ (current-thread)'vmid))
      (write-to-thread-log
       (if-let1 info (debug-source-info form)
         (format "~a#?=~s:~a:~,,,,v:s\n" prefix
                 (car info) (cadr info) (debug-print-width) form)
         (format "~a#?=~,,,,v:s\n" prefix
                 (debug-print-width) form))))))

(define (debug-thread-post vals)
  (receive (s ns) (sys-clock-gettime-monotonic)
    (let1 prefix (format "~9d.~9,'0d[~2d]" s ns (~ (current-thread)'vmid))
      (write-to-thread-log
       (with-output-to-string
         (^[]
           (if (null? vals)
             (display (format "~a#?-<void>\n" prefix))
             (begin
               (display (format "~a#?-    ~,,,,v:s\n" prefix
                                (debug-print-width) (car vals)))
               (for-each (^[elt]
                           (display (format/ss "~a#?+    ~,,,,v:s\n" prefix
                                               (debug-print-width) elt)))
                         (cdr vals)))))))
      (apply values vals))))

(define-syntax debug-thread-log
  (syntax-rules ()
    [(_ ?form)
     (begin
       (debug-thread-pre '?form)
       (receive vals ?form
         (debug-thread-post vals)))]))
