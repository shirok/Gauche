;;;
;;; rfc.http.tunnel - submodule to use stunnel for secure connection
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

;; This module is autoloaded from rfc.http

(define-module rfc.http.tunnel
  (use gauche.process)
  (use file.util)
  (export probe-stunnel
          make-stunnel-connection))
(select-module rfc.http.tunnel)

(define probe-stunnel
  (let1 result #f
    (define (run-stunnel3 path)
      (lambda (host:port)
        (run-process `(,path "-c" "-r" ,host:port) :input :pipe :output :pipe
                     :error :null :wait #f)))
    (define (run-stunnel4 path)
      (lambda (host:port)
        (receive (in out) (sys-pipe)
          (rlet1 p (run-process `(,path "-fd" 3)
                                :redirects `((< 0 stdin)
                                             (> 1 stdout)
                                             (> 2 :null)
                                             (< 3 ,(port-file-number in)))
                                :wait #f)
            (format out "client = yes\n")
            (format out "connect = ~a" host:port)
            (close-output-port out)))))

    (lambda (:key (force #f))
      (if (and result (not force))
        result
        (and-let* ([path (or (find-file-in-paths "stunnel4") ;ubuntu has this
                             (find-file-in-paths "stunnel"))]
                   [p (run-process `(,path "-version")
                                   :error :pipe :output :pipe)]
                   [vers (read-line (process-error p))])
          (process-wait p)
          (rlet1 proc (cond
                       [(eof-object? vers) (run-stunnel3 path)]
                       [(#/stunnel (\d+)\.\d+/ vers)
                        => (^m (if (>= (x->integer (m 1)) 4)
                                 (run-stunnel4 path)
                                 (run-stunnel3 path)))]
                       [(#/exec failed/i vers) #f]
                       [else (run-stunnel3 path)])
            (set! result proc)))))))

(define (make-stunnel-connection host:port)
  (if-let1 p ((probe-stunnel) host:port)
    (make-process-connection p)
    (error "Couldn't start stunnel subprocess")))
