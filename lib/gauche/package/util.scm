;;;
;;; gauche.package.util - internal utilities used in package manager
;;;  
;;;   Copyright (c) 2004-2009  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: util.scm,v 1.6 2008-05-10 13:36:04 shirok Exp $
;;;

;;; NB: this module is not intended for external use.

(define-module gauche.package.util
  (use gauche.process)
  (use gauche.parameter)
  (use gauche.termios)
  (export run dry-run verbose-run get-password))
(select-module gauche.package.util)

(define dry-run     (make-parameter #f))
(define verbose-run (make-parameter #f))

(define (run cmdline :key (stdin-string #f))
  (when (or (dry-run) (verbose-run))
    (print cmdline))
  (unless (dry-run)
    (let1 p (run-process "/bin/sh" "-c" cmdline
                         :input (if stdin-string :pipe "/dev/null")
                         :wait #f)
      (when stdin-string
        (let1 pi (process-input p)
          (display stdin-string pi)
          (flush pi)
          (close-output-port pi)))
      (process-wait p)
      (unless (zero? (process-exit-status p))
        (errorf "command execution failed: ~a" cmdline)))))

;; Read password from the terminal without echoing
(define (get-password)
  (with-output-to-file "/dev/tty"
    (lambda () (display "Password: ") (flush)))
  (without-echoing #f read-line))

(provide "gauche/package/util")
