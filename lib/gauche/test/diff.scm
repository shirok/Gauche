;;;
;;; gauche.test.diff - test-diff
;;;
;;;   Copyright (c) 2021-2022  Shiro Kawai  <shiro@acm.org>
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

;; This file is autoloaded from gauche.test, to avoid gauche.test from
;; depending on other modules.
(select-module gauche.test)
(use file.util)
(use text.diff)
(use util.match)

;; Usage example:
;;  (test*/diff "foo" '(content-of "sample.out")
;;              <expression-to-generate-output>)
;;
;; This is a handy macro to compare a generated text against a pre-generated
;; text.  EXPECTED argument can be either a string,
;; a list of string, or a form (content-of <filename>).
;; The test expression must yield a string or a list of strings.
;; (NB: The test expression can yield (content-of <filename>), but if you do so,
;; the <filename> must exist to the end of the entire tests, for the
;; discrepancy report may need to access it.)
;; If it is a list of string, each string is regarded as a line (no newline
;; character is required).  If it is (content-of <filename>), the <filename>
;; is read and used.  If <filename> is a relative path, it is relative to
;; the current loading path.

(define-syntax test*/diff
  (syntax-rules ()
    ([_ msg expected expr]
     (test* msg expected expr
            test-check-diff test-report-failure-diff))))

;; internal.  converts src to string.  if can't convert, just return src as is.
(define (%->input src)
  (match src
    [('content-of filename)
     (let1 file (if (relative-path? filename)
                  (build-path (sys-dirname (current-load-path)) filename)
                  filename)
       (if (file-exists? file)
         (file->string file)
         ""))]
    [(x ...) (=> fail)
     (if (every string? x)
       (string-join x "\n" 'suffix)
       (fail))]
    [else src]))                        ;convers string src case

(define (test-check-diff expected actual :optional (fallback equal?))
  (fallback (%->input expected) (%->input actual)))

(define (test-report-failure-diff msg expected actual)
  (let ([e (%->input expected)]
        [a (%->input actual)])
    (if (and (string? e) (string? a))
      (begin
        (print "diffs:")
        (print "--- expected")
        (print "+++ actual")
        (diff-report/unified e a))
      (test-report-failure msg expected actual)))) ;fallback
