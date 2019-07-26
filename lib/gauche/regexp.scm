;;;
;;; regexp.scm - auxiliary macros and procedures for regexp.  autoloaded.
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

;; NB: refrain from using util.match, for it could cause nasty dependency
;; problem.  (Should be resolved once we incorporate match into the core).
(define-module gauche.regexp
  (export rxmatch-let rxmatch-if rxmatch-cond rxmatch-case
          regexp-unparse <regexp-invalid-ast>))
(select-module gauche.regexp)

(define-syntax rxmatch-bind*
  (syntax-rules ()
    [(rxmatch-bind* ?n ?match () ?form ...)
     (begin ?form ...)]
    [(rxmatch-bind* ?n ?match (#f ?vars ...) ?form ...)
     (rxmatch-bind* (+ ?n 1) ?match (?vars ...) ?form ...)]
    [(rxmatch-bind* ?n ?match (?var ?vars ...) ?form ...)
     (let1 ?var (rxmatch-substring ?match ?n)
       (rxmatch-bind* (+ ?n 1) ?match (?vars ...) ?form ...))]))

(define-syntax rxmatch-let
  (syntax-rules ()
    [(rxmatch-let ?expr (?var ...) ?form ...)
     (cond [?expr => (^(match) (rxmatch-bind* 0 match (?var ...) ?form ...))]
           [else (error "rxmatch-let: match failed:" '?expr)])]))

(define-syntax rxmatch-if
  (syntax-rules ()
    [(rxmatch-if ?expr (?var ...) ?then ?else)
     (cond [?expr => (^(match) (rxmatch-bind* 0 match (?var ...) ?then))]
           [else ?else])]))

(define-syntax rxmatch-cond
  (syntax-rules (test else =>)
    [(rxmatch-cond) #f]
    [(rxmatch-cond (else ?form ...))
     (begin ?form ...)]
    [(rxmatch-cond (test ?expr => ?obj) ?clause ...)
     (cond (?expr => ?obj) (else (rxmatch-cond ?clause ...)))]
    [(rxmatch-cond (test ?expr ?form ...) ?clause ...)
     (if ?expr (begin ?form ...) (rxmatch-cond ?clause ...))]
    [(rxmatch-cond (?matchexp ?bind ?form ...) ?clause ...)
     (rxmatch-if ?matchexp ?bind
       (begin ?form ...)
       (rxmatch-cond ?clause ...))]))

(define-syntax rxmatch-case
  (syntax-rules (test else =>)
    [(rxmatch-case #t ?temp ?strp) #f]
    [(rxmatch-case #t ?temp ?strp (else => ?proc))
     (?proc ?temp)]
    [(rxmatch-case #t ?temp ?strp (else ?form ...))
     (begin ?form ...)]
    [(rxmatch-case #t ?temp ?strp (test ?proc => ?obj) ?clause ...)
     (cond [(?proc ?temp) => ?obj]
           [else (rxmatch-case #t ?temp ?strp ?clause ...)])]
    [(rxmatch-case #t ?temp ?strp (test ?proc ?form ...) ?clause ...)
     (if (?proc ?temp)
       (begin ?form ...)
       (rxmatch-case #t ?temp ?strp ?clause ...))]
    [(rxmatch-case #t ?temp ?strp (?re ?bind ?form ...) ?clause ...)
     (rxmatch-if (and ?strp (rxmatch ?re ?temp))
         ?bind
       (begin ?form ...)
       (rxmatch-case #t ?temp ?strp ?clause ...))]
    [(rxmatch-case #t ?temp ?strip ?clause ...)
     (syntax-error "malformed rxmatch-case")]
    ;; main entry
    [(rxmatch-case ?str ?clause ...)
     (let* ([temp ?str]
            [strp (string? temp)])
       (rxmatch-case #t temp strp ?clause ...))]))

;; Extract all matches (including entire match).  Allows match to be #f.
(define (rxmatch-substrings match :optional (start 0) (end #f))
  (if match
    (do ([end (if (and end (>= end 0))
                (min end (rxmatch-num-matches match))
                (rxmatch-num-matches match))]
         [k start (+ k 1)]
         [r '() (cons (rxmatch-substring match k) r)])
        [(>= k end) (reverse r)])
    '()))
(define (rxmatch-positions match :optional (start 0) (end #f))
  (if match
    (do ([end (if (and end (>= end 0))
                (min end (rxmatch-num-matches match))
                (rxmatch-num-matches match))]
         [k start (+ k 1)]
         [r '() (acons (rxmatch-start match k) (rxmatch-end match k) r)])
        [(>= k end) (reverse r)])
    '()))

;; NB: This should eventually be defined in regexp.c, and Scm_RegComp
;; should throw this on invalid AST tree.
(define-condition-type <regexp-invalid-ast> <error> #f
  (root-ast)
  (offending-item))

;; Reconsturct string representation of regexp from the parsed tree.
(define (regexp-unparse ast :key (on-error :error))
  (define (doit) (call-with-output-string (cut regexp-unparse-int ast <>)))
  (case on-error
    [(#f) (guard (e [(<regexp-invalid-ast> e) #f][else (raise e)]) (doit))]
    [(:error) (doit)]
    [else (error "bad value for :on-error argument; \
                  must be #f or :error, but got" on-error)]))

(define (regexp-unparse-int ast p)
  (define (err msg item)
    (error <regexp-invalid-ast> :ast ast :offending-item item msg item))
  (define (disp x) (display x p))
  (define (seq ns) (for-each unparse ns))
  (define (between pre ns post) (disp pre) (seq ns) (disp post))
  (define (intersp ns separ)
    (or (null? ns)
        (begin (disp "(?:")
               (unparse (car ns))
               (for-each (^n (disp separ) (unparse n)) (cdr ns))
               (disp ")"))))

  (define (unparse-rep op M N . ns)
    ;; special optimization - if the content is a simple character,
    ;; or a single group, omit extra grouping
    (if (and (not (null? ns)) (null? (cdr ns))
             (let1 item (car ns)
               (or (char? item)
                   (eq? item 'any)
                   (char-set? item)
                   (and (pair? item)
                        (or (integer? (car item)) ;capturing group
                            (memq (car item)
                                  '(alt comp seq-uncase seq-case cpat once
                                        assert nassert))
                            (and (eq? (car item) 'seq)
                                 (pair? (cdr item))
                                 (null? (cddr item))))))))
      (unparse (car ns))
      (between "(?:" ns ")"))
    (disp (cond [(not N) (case M
                           [(0 #f) "*"]
                           [(1) "+"]
                           [else (format "{~a,}" M)])]
                [(= M N) (case M
                           [(0) "{0}"] ;weird, but tolerate
                           [(1) ""]
                           [else (format "{~a}" M)])]
                [(and (= M 0) (= N 1)) "?"]
                [else (format "{~a,~a}" M N)]))
    (or (not (eq? op 'rep-min))
        (disp "?"))) ;non-greedy match marker

  (define (unparse-cpat test yes no . x)
    (unless (null? x)
      (err "invalid cpat node in AST:" `(cpat ,test ,yes ,no ,@x)))
    (disp "(?")
    (if (integer? test)
      (disp (format "(~a)" test))
      (case (car test)
        [(assert nassert)  (apply unparse-assert-like test)]
        [else (err "invalid AST in the test part of cpat" test)]))
    (seq yes)
    (unless (null? no) (disp "|") (seq no))
    (disp ")"))

  (define (unparse-assert-like op . asst)
    (let ((ch (if (eq? op 'assert) "=" "!")))
      (if (and (pair? asst) (pair? (car asst)) (eq? (caar asst) 'lookbehind))
        (if (null? (cdr asst))
          (between (format "(?<~a" ch) (cdar asst) ")")
          (err "invalid AST within assert or nassert" asst))
        (between (format "(?~a" ch) asst ")"))))

  ;; TODO: Count groups that has seen so far so that the integer NUM
  ;; is correct.
  (define (unparse-capture num name . ast)
    (cond [(= num 0)                    ;toplevel group.  we don't show it.
           (when name (err "toplevel group can't have name" name))
           (seq ast)]
          [name (between (format "(?<~a>" name) ast ")")]
          [else (between "(" ast ")")]))

  (define (unparse n)
    (cond [(char? n)     (cond [(memv n '(#\. #\^ #\$ #\( #\) #\{ #\} #\[ #\]
                                          #\\ #\* #\+ #\? #\|))
                                (disp "\\") (disp n)]
                               [(eq? (char-general-category n) 'Cc)
                                (disp (format "\\u~4,'0x" (char->ucs n)))]
                               [else (disp n)])]
          [(char-set? n) (disp (substring (write-to-string n) 1 -1))]
          [(eq? n 'any)  (disp #\.)]
          [(eq? n 'bos)  (disp "^^")]   ; made up syntax for reporting only
          [(eq? n 'eos)  (disp "$$")]   ; made up syntax for reporting only
          [(eq? n 'bol)  (disp #\^)]
          [(eq? n 'eol)  (disp #\$)]
          [(eq? n 'wb)   (disp "\\b")]
          [(eq? n 'bow)  (disp "\\b(?=\\w)")] ; unsupported pcre syntax
          [(eq? n 'eow)  (disp "\\b(?<=\\w)")] ; unsupported pcre syntax
          [(eq? n 'nwb)  (disp "\\B")]
          [(not (pair? n)) (err "invalid AST node" n)]
          [else (case (car n)
                  [(comp) (rlet1 s (write-to-string (cdr n))
                            (format p "[^~a" (substring s 2 -1)))]
                  [(seq)  (seq (cdr n))]
                  [(seq-uncase) (between "(?i:" (cdr n) ")")]
                  [(seq-case)   (between "(?-i:" (cdr n) ")")]
                  [(alt)
                   ;; empty alt always fails, and we represent it by
                   ;; empty negative lookahead assertion.
                   (if (null? (cdr n)) (disp "(?!)") (intersp (cdr n) "|"))]
                  [(rep rep-min rep-while) (apply unparse-rep n)]
                  [(cpat) (apply unparse-cpat (cdr n))]
                  [(backref)
                   ;; add dummy group (?:) in case if this is followed by
                   ;; other digits.
                   (if (integer? (cdr n))
                     (disp (format "\\~d(?:)" (cdr n)))
                     (err "invalid backref node in AST" n))]
                  [(once) (between "(?>" (cdr n) ")")]
                  [(assert nassert) (apply unparse-assert-like n)]
                  [else (if (integer? (car n))
                          (apply unparse-capture n)
                          (err "invalid AST node" n))]
                  )]))
  (unparse ast))
