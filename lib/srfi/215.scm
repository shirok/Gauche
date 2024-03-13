;;;
;;; SRFI-215 - Central Log Exchange
;;;
;;;   Copyright (c) 2020  Goran Weinholt
;;;   Copyright (c) 2024  Antero Mejr  <mail@antr.me>
;;;
;;;   This module is a Gauche port of the SRFI 215 sample implementation,
;;;   and is therefore MIT-licensed.
;;;
;;;   Permission is hereby granted, free of charge, to any person
;;;   obtaining a copy of this software and associated documentation files
;;;   (the "Software"), to deal in the Software without restriction,
;;;   including without limitation the rights to use, copy, modify, merge,
;;;   publish, distribute, sublicense, and/or sell copies of the Software,
;;;   and to permit persons to whom the Software is furnished to do so,
;;;   subject to the following conditions:
;;;
;;;   The above copyright notice and this permission notice (including the
;;;   next paragraph) shall be included in all copies or substantial
;;;   portions of the Software.
;;;
;;;   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;   NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;;   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;;   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;;   SOFTWARE.

(define-module srfi.215
  (use gauche.logger)
  (use gauche.uvector)
  (export send-log
          current-log-fields
          current-log-callback
          EMERGENCY ALERT CRITICAL ERROR WARNING NOTICE INFO DEBUG
          ))
(select-module srfi.215)

;; These severities are from RFC 5424 ("The Syslog Protocol").
(define EMERGENCY 0)                ; system is unusable
(define ALERT 1)                    ; action must be taken immediately
(define CRITICAL 2)                 ; critical conditions
(define ERROR 3)                    ; error conditions
(define WARNING 4)                  ; warning conditions
(define NOTICE 5)                   ; normal but significant condition
(define INFO 6)                     ; informational messages
(define DEBUG 7)                    ; debug-level messages

(define severity-strings
  '((0 . "EMERGENCY")
    (1 . "ALERT")
    (2 . "CRITICAL")
    (3 . "ERROR")
    (4 . "WARNING")
    (5 . "NOTICE")
    (6 . "INFO")
    (7 . "DEBUG")))

(define (field-list->alist plist)
  (let f ((fields plist))
    (cond ((null? fields)
           '())
          ((or (not (pair? fields)) (not (pair? (cdr fields))))
           (error "short field list" plist))
          (else
           (let ((k (car fields)) (v (cadr fields)))
             (if (not v)
                 (f (cddr fields))
                 (let ((k^ (cond ((symbol? k) k)
                                 (else
                                  (error "invalid key" k plist))))
                       (v^ (cond ((string? v) v)
                                 ((and (integer? v) (exact? v)) v)
                                 ((bytevector? v) v)
                                 ((error-object? v) v) ;R7RS
                                 (else
                                  (let ((p (open-output-string)))
                                    (write v p)
                                    (get-output-string p))))))
                   (cons (cons k^ v^)
                         (f (cddr fields))))))))))

(define current-log-fields
  (make-parameter '()
                  (lambda (plist)
                    (field-list->alist plist)
                    plist)))

(define current-log-callback
  (make-parameter (lambda (log-entry)
                    (log-format "~a: ~a ~a"
                                (assoc-ref severity-strings
                                           (assoc-ref log-entry 'SEVERITY))
                                (assoc-ref log-entry 'MESSAGE)
                                (alist-delete
                                 'SEVERITY
                                 (alist-delete 'MESSAGE log-entry eq?) eq?)))
                  (lambda (hook)
                    (unless (procedure? hook)
                      (error "current-log-callback: expected a procedure" hook))
                    hook)))

(define (send-log severity message . plist)
  (unless (and (exact? severity) (integer? severity) (<= 0 severity 7))
    (error "send-log: expected a severity from 0 to 7"
           severity message plist))
  (unless (string? message)
    (error "send-log: expected message to be a string"
           severity message plist))
  (let* ((fields (append plist (current-log-fields)))
         (alist (field-list->alist fields)))
    ((current-log-callback) `((SEVERITY . ,severity)
                              (MESSAGE . ,message)
                              ,@alist))))
