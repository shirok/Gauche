;;;
;;; auxiliary string utilities.  to be autoloaded.
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

(define-module gauche.stringutil
  (export string-split)
  )
(select-module gauche.stringutil)

;; trick to delay loading of srfi-13 until needed
(autoload srfi-13 string-tokenize)

(define %string-split-by-char
  (with-module gauche.internal %string-split-by-char))

;; Generic string-split
;;  splitter can be a character, a char-set, a string, or a regexp.
;;  NB: To adapt to srfi-152, we changed the optional argument - now the
;;  'grammar' argument comes before 'limit'.  For the bacward compatibility,
;;  we recognize an integer argument in 'grammar' as 'limit'.
(define (string-split string splitter :optional (grammar 'infix) (limit #f) start end)
  (if (or (not grammar) (integer? grammar))
    (%string-split string splitter 'infix grammar limit start)
    (%string-split string splitter grammar limit start end)))

(define (%string-split string splitter grammar limit start end)
  (unless (memq grammar '(infix strict-infix prefix suffix))
    (error "grammar argument must be one of (infix strict-infix prefix suffix), but got" grammar))
  (unless (or (not limit) (and (integer? limit) (>= limit 0)))
    (error "limit argument must be a nonnegative integer or #f, but got" limit))
  (let1 s ((with-module gauche.internal %maybe-substring) string start end)
    (if (equal? s "")
      (if (eq? grammar 'strict-infix)
        (error "string must not be empty with strict-infix grammar")
        '())
      (let1 r (if (char? splitter)
                (%string-split-by-char s splitter (or limit -1))
                (%string-split-by-scanner s (%string-split-scanner splitter)
                                          (or limit -1)))
        (case grammar
          [(prefix) (if (and (pair? r) (equal? (car r) ""))
                      (cdr r)
                      r)]
          [(suffix) (if (and (pair? r) (equal? (car (last-pair r)) ""))
                      (drop-right r 1)
                      r)]
          [else r])))))

;; aux fns
(define (%string-split-scanner splitter)
  (cond [(string? splitter)
         (if (string=? splitter "")
           (^s (if (<= (string-length s) 1)
                 (values s #f)
                 (values (string-copy s 0 1) (string-copy s 1))))
           (^s (receive (before after) (string-scan s splitter 'both)
                 (if before (values before after) (values s #f)))))]
        [(char-set? splitter)
         (%string-split-scanner-each-char
          (cut char-set-contains? splitter <>))]
        [(regexp? splitter)
         (^s (cond [(rxmatch splitter s)
                    => (^m (let ([before (m 'before)]
                                 [after  (m 'after)])
                             (if (string=? s after)
                               (if (<= (string-length s) 1)
                                 (values s #f)
                                 (values (string-copy s 0 1)
                                         (string-copy s 1)))
                               (values before after))))]
                   [else (values s #f)]))]
        [else ;; assume splitter is a predicate
         (%string-split-scanner-each-char splitter)]))

(define (%string-split-scanner-each-char pred)
  (define (scan-in p)
    (let1 c (string-pointer-ref p)
      (cond [(eof-object? c) (values (string-pointer-substring p) #f)]
            [(pred c) (let1 before (string-pointer-substring p)
                        (string-pointer-next! p)
                        (scan-out p before))]
            [else (string-pointer-next! p) (scan-in p)])))
  (define (scan-out p before)
    (let1 c (string-pointer-ref p)
      (cond [(eof-object? c) (values before "")]
            [(pred c) (string-pointer-next! p) (scan-out p before)]
            [else (values before (string-pointer-substring p :after #t))])))
  (^s (scan-in (make-string-pointer s))))

(define (%string-split-by-scanner string scanner limit)
  (let loop ([s string]
             [r '()]
             [limit limit])
    (if (zero? limit)
      (reverse! (cons s r))
      (receive (before after) (scanner s)
        (if after
          (loop after (cons before r) (- limit 1))
          (reverse! (cons before r)))))))
