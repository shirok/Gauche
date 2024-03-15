;;;
;;; SRFI-215 - Central Log Exchange
;;;
;;;   Copyright (c) 2020  Goran Weinholt
;;;   Copyright (c) 2024  Antero Mejr  <mail@antr.me>
;;;   Copyright (c) 2024  Shiro Kawai <shiro@acm.org>
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

;; Originally from SRFI-215 reference implementation by Goran Weinholt
;; Antero Mejr adapted it to Gauche's gauche.logger layer.
;; Shiro Kawai modified it to utilize Gauche's style as much.

(define-module srfi.215
  (use gauche.logger)
  (use util.match)
  (export send-log
          current-log-fields
          current-log-callback
          EMERGENCY ALERT CRITICAL ERROR WARNING NOTICE INFO DEBUG
          ))
(select-module srfi.215)

(define-syntax define-severities
  (syntax-rules ()
    [(_ lookup (name val) ...)
     (begin (define-constant name val) ...
            (define (lookup v) (alist-ref '((val . name) ...) v)))]))

;; These severities are from RFC 5424 ("The Syslog Protocol").
(define-severities severity
  (EMERGENCY 0)                ; system is unusable
  (ALERT 1)                    ; action must be taken immediately
  (CRITICAL 2)                 ; critical conditions
  (ERROR 3)                    ; error conditions
  (WARNING 4)                  ; warning conditions
  (NOTICE 5)                   ; normal but significant condition
  (INFO 6)                     ; informational messages
  (DEBUG 7)                    ; debug-level messages
  )

;; (key1 val1 key2 val2 ...) -> ((key1 . val1) (key2 . val2) ...)
;; SRFI only allows certain types in VALs.  We convert evreything else
;; to a string.
;; TAIL should be a already converted alist.
(define (field-list->alist plist :optional (tail '()))
  (let rec ([fields plist])
    (match fields
      [() tail]
      [(k #f . rest) (rec rest)]
      [(k v . rest)
       (let ([kk (if (symbol? k) k (error "invalid key:" k))]
             [vv (if (or (string? v)
                         (exact-integer? v)
                         (u8vector? v)
                         (<error> v))
                   v
                   (write-to-string v))])
         `((,kk . ,vv) ,@(rec rest)))]
      [_ (error "Incomplete field plist:" plist)])))

(define current-log-fields (make-parameter '() field-list->alist))

(define current-log-callback
  (make-parameter (^[log-entry]
                    (log-format "~a: ~a ~s"
                                (severity (alist-ref log-entry 'SEVERITY))
                                (alist-ref log-entry 'MESSAGE)
                                (remove
                                 (^p (memq (car p) '(SEVERITY MESSAGE)))
                                 log-entry)))
                  (^[hook]
                    (assume (procedure? hook))
                    hook)))

(define (send-log severity message . plist)
  (assume (and (exact-integer? severity) (<= 0 severity 7))
          "Expected a severity from 0 to 7, but got:" severity)
  (assume (string? message)
          "Expected message to be a string, but got:" message)
  ((current-log-callback) `((SEVERITY . ,severity)
                            (MESSAGE . ,message)
                            ,@(field-list->alist plist (current-log-fields))))
  (undefined)) ; make the caller not rely on the result of callback
