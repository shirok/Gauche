;;;
;;; srfi-13/internal - string library (low-level procedures)
;;;  
;;;   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
;;;  $Id: internal.scm,v 1.6 2003-07-05 03:29:12 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

;; Low-level procedures.  These are included for completeness, but
;; I'm not using these in other SRFI-13 routines, since it is more
;; efficient to let %maybe-substring handle argument checking as well.

(define (string-parse-start+end proc s args)
  (check-arg string? s)
  (let ((slen (string-length s)))
    (let-optionals* args (start end)
      (if (undefined? start)
          (values '() 0 slen)
          (if (not (and (integer? start) (exact? start) (<= 0 start slen)))
              (errorf "~s: argument out of range: ~s" proc start)
              (if (undefined? end)
                  (values '() start slen)
                  (if (not (and (integer? end) (exact? end) (<= start end slen)))
                      (errorf "~s: argument out of range: ~s" proc end)
                      (values (cddr args) start end))
                  )
              )
          )
      )))

(define (string-parse-final-start+end proc s args)
  (check-arg string? s)
  (let ((slen (string-length s)))
    (let-optionals* args (start end)
      (if (undefined? start)
          (values '() 0 slen)
          (if (not (and (integer? start) (exact? start) (<= 0 start slen)))
              (errorf "~s: argument out of range: ~s" proc start)
              (if (undefined? end)
                  (values '() start slen)
                  (if (not (and (integer? end) (exact? end) (<= start end slen)))
                      (errorf "~s: argument out of range: ~s" proc end)
                      (if (not (null? (cddr args)))
                          (errorf "~s: too many arguments" proc)
                          (values (cddr args) start end)))
                  )
              )
          )
      )))

(define-syntax let-string-start+end
  (syntax-rules ()
    ((_ (?start ?end ?rest) ?proc ?s ?args . ?body)
     (call-with-values
      (lambda () (string-parse-start+end ?proc ?s ?args))
      (lambda (?rest ?start ?end) . ?body)))
    ((_ (?start ?end) ?proc ?s ?args . ?body)
     (call-with-values
      (lambda () (string-parse-final-start+end ?proc ?s ?args))
      (lambda (?start ?end) . ?body)))
    ))

(define (check-substring-spec proc s start end)
  (unless (substring-spec-ok? s start end)
    (errorf "~s: invalid substring spec: ~s (~s, ~s)" proc s start end)))

(define (substring-spec-ok? s start end)
  (and (string? s)
       (integer? start) (exact? start)
       (integer? end) (exact? end)
       (<= 0 start end (string-length s))))

  
    


  

