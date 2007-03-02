;;;
;;; scmlib.scm - more Scheme libraries
;;;
;;;   Copyright (c) 2000-2007  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: scmlib.scm,v 1.11 2007-03-02 07:39:14 shirok Exp $
;;;

;; This file contains builtin library functions that are easier to be
;; written in Scheme instead of as stubs.
;;

(select-module gauche)

;;;=======================================================
;;; List utilities
;;;

;; R5RS cxr's

;; NB: we avoid using getter-with-setter here, since
;;   - The current compiler doesn't take advantage of locked setters
;;   - Using getter-with-setter loses the inferred closure name
;; But this may change in future, of course.
(define-syntax %define-cxr
  (syntax-rules ()
    ((_ name a b)
     (begin
       (define-inline (name x) (a (b x)))
       (define-in-module scheme name name)
       (set! (setter name) (lambda (x v) (set! (a (b x)) v)))))))

(%define-cxr caaar  car  caar)
(%define-cxr caadr  car  cadr)
(%define-cxr cadar  car  cdar)
(%define-cxr caddr  car  cddr)
(%define-cxr cdaar  cdr  caar)
(%define-cxr cdadr  cdr  cadr)
(%define-cxr cddar  cdr  cdar)
(%define-cxr cdddr  cdr  cddr)
(%define-cxr caaaar caar caar)
(%define-cxr caaadr caar cadr)
(%define-cxr caadar caar cdar)
(%define-cxr caaddr caar cddr)
(%define-cxr cadaar cadr caar)
(%define-cxr cadadr cadr cadr)
(%define-cxr caddar cadr cdar)
(%define-cxr cadddr cadr cddr)
(%define-cxr cdaaar cdar caar)
(%define-cxr cdaadr cdar cadr)
(%define-cxr cdadar cdar cdar)
(%define-cxr cdaddr cdar cddr)
(%define-cxr cddaar cddr caar)
(%define-cxr cddadr cddr cadr)
(%define-cxr cdddar cddr cdar)
(%define-cxr cddddr cddr cddr)

;; Some srfi-1 functions that are used in the compiler
;; (hence we need to define here)

(define-inline (null-list? l)
  (cond ((null? l))
        ((pair? l) #f)
        (else (error "argument must be a list, but got:" l))))

(with-module gauche.internal
  (define (%zip-nary-args arglists . seed)
    (let loop ((as arglists)
               (cars '())
               (cdrs '()))
      (cond ((null? as)
             (values (reverse! (if (null? seed) cars (cons (car seed) cars)))
                     (reverse! cdrs)))
            ((null? (car as)) (values #f #f)) ;;exhausted
            ((pair? (car as))
             (loop (cdr as) (cons (caar as) cars) (cons (cdar as) cdrs)))
            (else
             (error "argument lists contained an improper list ending with:"
                    (car as))))))
  )

(define (any pred lis . more)
  (if (null? more)
    (and (not (null-list? lis))
         (let loop ((head (car lis)) (tail (cdr lis)))
           (cond ((null-list? tail) (pred head)) ; tail call
                 ((pred head))
                 (else (loop (car tail) (cdr tail))))))
    (let loop ((liss (cons lis more)))
      (receive (cars cdrs)
          ((with-module gauche.internal %zip-nary-args) liss)
        (cond ((not cars) #f)
              ((apply pred cars))
              (else (loop cdrs)))))))

(define (fold kons knil lis . more)
  (if (null? more)
    (let loop ((lis lis) (knil knil))
      (if (null-list? lis) knil (loop (cdr lis) (kons (car lis) knil))))
    (let loop ((liss (cons lis more)) (knil knil))
      (receive (cars cdrs)
          ((with-module gauche.internal %zip-nary-args) liss knil)
        (if cars
          (loop cdrs (apply kons cars))
          knil)))))

(define (fold-right kons knil lis . more)
  (if (null? more)
    (let rec ((lis lis))
      (if (null-list? lis)
        knil
        (kons (car lis) (rec (cdr lis)))))
    (let rec ((liss (cons lis more)))
      (receive (cars cdrs)
          ((with-module gauche.internal %zip-nary-args) liss)
        (if cars
          (apply kons (append! cars (list (rec cdrs))))
          knil)))))

(define (find pred lis)
  (let loop ((lis lis))
    (cond ((not (pair? lis)) #f)
          ((pred (car lis)) (car lis))
          (else (loop (cdr lis))))))

(define (split-at lis i)
  (let loop ((i i) (rest lis) (r '()))
    (cond ((= i 0) (values (reverse! r) rest))
          ((null? rest) (error "given list is too short:" lis))
          (else (loop (- i 1) (cdr rest) (cons (car rest) r))))))

;;;=======================================================
;;; string mutators
;;;

;; They are just for backward compatibility, and they are expensive
;; anyway, so we provide them here instead of natively.

(define-in-module scheme (string-set! str k ch)
  (check-arg string? str)
  (check-arg integer? k)
  (check-arg exact? k)
  (check-arg char? ch)
  (let ((len (string-length str)))
    (when (or (< k 0) (<= len k))
      (error "string index out of range:" k))
    (%string-replace-body! str
                           (string-append (substring str 0 k)
                                          (string ch)
                                          (substring str (+ k 1) len)))))

(set! (setter string-ref) string-set!)

(define (string-byte-set! str k b)
  (check-arg string? str)
  (check-arg integer? k)
  (check-arg exact? k)
  (check-arg integer? b)
  (let ((siz (string-size str))
        (out (open-output-string :private? #t)))
    (when (or (< k 0) (<= siz k))
      (error "string index out of range:" k))
    (display (byte-substring str 0 k) out)
    (write-byte b out)
    (display (byte-substring str (+ k 1) siz) out)
    (%string-replace-body! str (get-output-byte-string out))))

(set! (setter string-byte-ref) string-byte-set!)

(define (string-fill! str c . opts)
  (check-arg string? str)
  (check-arg char? c)
  (let1 len (string-length str)
    (let-optionals* opts ((start 0)
                          (end   len))
      (when (or (< start 0) (< len start))
        (error "start index out of range:" start))
      (when (or (< end 0) (< len end))
        (error "end index out of range:" end))
      (when (< end start)
        (errorf "end index ~s is smaller than start index ~s" end start))
      (if (and (= start 0) (= end len))
        (%string-replace-body! str (make-string len c))
        (%string-replace-body! str
                               (string-append (substring str 0 start)
                                              (make-string (- end start) c)
                                              (substring str end len)))))))

;;;=======================================================
;;; call/cc alias
;;;
(define-in-module scheme call/cc call-with-current-continuation)

;;;=======================================================
;;; error stuff, in terms of the condition system
;;;
(define-values (error errorf)
  (let ()
    (define (compose-error-message msg args) ;; srfi-23 style message
      (let1 p (open-output-string)
        (display msg p)
        (let loop ((args args))
          (if (null? args)
            (get-output-string p)
            (begin (display " " p)
                   (write (car args) p)
                   (loop (cdr args)))))))
    (define (scan-keys args)
      (let loop ((args args)
                 (keys '()))
        (if (and (not (null? args))
                 (keyword? (car args))
                 (not (null? (cdr args))))
          (loop (cddr args) (list* (cadr args) (car args) keys))
          (values (reverse! keys) args))))
    
    (define (error msg . args)
      (raise
       (cond
        ((is-a? msg <condition-meta>)
         (receive (keys msgs) (scan-keys args)
           (if (null? msgs)
             (apply make msg keys)
             (apply make msg
                    :message (compose-error-message (car msgs) (cdr msgs))
                    keys))))
        (else (make <error> :message (compose-error-message msg args))))))

    (define (errorf fmt . args)
      (raise
       (cond
        ((is-a? fmt <condition-meta>)
         (receive (keys msgs) (scan-keys args)
           (if (null? msgs)
             (apply make fmt keys)
             (apply make fmt
                    :message (apply format #f msgs)
                    keys))))
        (else (make <error> :message (apply format #f fmt args))))))

    (values error errorf)))

;;;=======================================================
;;; symbol-bound? (deprecated)
;;;
(define (symbol-bound? name . maybe-module)
  (global-variable-bound? (get-optional maybe-module #f) name))

;;;=======================================================
;;; call-with-values
;;;
(define-in-module scheme (call-with-values producer consumer)
  (receive vals (producer) (apply consumer vals)))

;;;=======================================================
;;; signal utility
;;;
(define (sys-sigset . signals)
  (if (null? signals)
    (make <sys-sigset>)
    (apply sys-sigset-add! (make <sys-sigset>) signals)))

;;;=======================================================
;;; srfi-17
;;;
(define (getter-with-setter get set)
  (let ((proc (lambda x (apply get x))))
    (set! (setter proc) set)
    proc))

;;;=======================================================
;;; srfi-38
;;;

(define read-with-shared-structure read)
(define read/ss read)

(define (write-with-shared-structure obj . args)
  (write* obj (if (pair? args) (car args) (current-output-port))))
(define write/ss write-with-shared-structure)

;;;=======================================================
;;; i/o utility
;;;

(define (print . args) (for-each display args) (newline))

(define-values (format format/ss)
  (letrec ((format-int
            (lambda (port fmt args shared?)
              (cond ((eqv? port #f)
                     (let ((out (open-output-string :private? #t)))
                       (%format out fmt args shared?)
                       (get-output-string out)))
                    ((eqv? port #t)
                     (%format (current-output-port) fmt args shared?))
                    (else (%format port fmt args shared?)))))
           (format
            (lambda (fmt . args)
              (if (string? fmt)
                (format-int #f fmt args #f) ;; srfi-28 compatible behavior
                (format-int fmt (car args) (cdr args) #f))))
           (format/ss
            (lambda (fmt . args)
              (if (string? fmt)
                (format-int #f fmt args #t) ;; srfi-28 compatible behavior
                (format-int fmt (car args) (cdr args) #t))))
           )
    (values format format/ss)))

;;;=======================================================
;;; with-something
;;;

;; R5RS open-{input|output}-file can be hooked by conversion port.
;; %open-{input|output}-file/conv are autoloaded.

(define-in-module scheme (open-input-file filename . args)
  (if (get-keyword :encoding args #f)
    (apply %open-input-file/conv filename args)
    (apply %open-input-file filename args)))

(define-in-module scheme (open-output-file filename . args)
  (if (get-keyword :encoding args #f)
    (apply %open-output-file/conv filename args)
    (apply %open-output-file filename args)))

;; File ports.

(define-in-module scheme (call-with-input-file filename proc . flags)
  (let ((port (apply open-input-file filename flags)))
    (with-error-handler
     (lambda (e)
       (when port (close-input-port port))
       (raise e))
     (lambda ()
       (receive r (proc port)
         (when port (close-input-port port))
         (apply values r))))))

(define-in-module scheme (call-with-output-file filename proc . flags)
  (let ((port (apply open-output-file filename flags)))
    (with-error-handler
     (lambda (e)
       (when port (close-output-port port))
       (raise e))
     (lambda ()
       (receive r (proc port)
         (when port (close-output-port port))
         (apply values r))))))

(define-in-module scheme (with-input-from-file filename thunk . flags)
  (let ((port (apply open-input-file filename flags)))
    (and port
         (with-error-handler
          (lambda (e) (close-input-port port) (raise e))
          (lambda ()
            (receive r (with-input-from-port port thunk)
              (close-input-port port)
              (apply values r)))))))
                  

(define-in-module scheme (with-output-to-file filename thunk . flags)
  (let ((port (apply open-output-file filename flags)))
    (and port
         (with-error-handler
          (lambda (e) (close-output-port port) (raise e))
          (lambda ()
            (receive r (with-output-to-port port thunk)
              (close-output-port port)
              (apply values r)))))))

;; String ports

(define (with-output-to-string thunk)
  (let ((out (open-output-string)))
    (with-output-to-port out thunk)
    (get-output-string out)))

(define (with-input-from-string str thunk)
  (with-input-from-port (open-input-string str) thunk))

(define (call-with-output-string proc)
  (let ((out (open-output-string)))
    (proc out)
    (get-output-string out)))

(define (call-with-input-string str proc)
  (let ((in (open-input-string str)))
    (proc in)))

(define (call-with-string-io str proc)
  (let ((out (open-output-string))
        (in  (open-input-string str)))
    (proc in out)
    (get-output-string out)))

(define (with-string-io str thunk)
  (with-output-to-string
    (lambda ()
      (with-input-from-string str
        thunk))))

(define (write-to-string obj . args)
  (with-output-to-string
    (lambda () ((if (pair? args) (car args) write) obj))))

(define (read-from-string string . args)
  (with-input-from-string
      (if (null? args) string (apply %maybe-substring string args))
    read))

