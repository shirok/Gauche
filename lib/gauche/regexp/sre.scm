;;;
;;; SRE support
;;;
;;;   Copyright (c) 2019  Duy Nguyen <pclouds@gmail.com>
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

(define-module gauche.regexp.sre
  (use gauche.generator)
  (use gauche.unicode)
  (use scheme.list)
  (use scheme.charset)
  (export regexp-parse-sre regexp-unparse-sre
          sre->regexp regexp->sre
          make-grapheme-predicate
          <regexp-invalid-sre>))
(select-module gauche.regexp.sre)

(define ascii (make-parameter #f))
(define nocapture (make-parameter #f))
(define casefold (make-parameter #f))

(define-condition-type <regexp-invalid-sre> <error> #f
  (offending-item))

;; To be used internally by regexp.c. Returns a closure that takes a
;; position and returns true if grapheme boundary is right before that
;; position.
;;
;; Corner cases, positions zero and length, should be handled by the
;; caller.
;;
;; Note, if we assume "cur" only moves forward, we could keep at most
;; two cursors in saved-curs. That means backtracking MUST recreate a
;; new predicate because it breaks the "moving forward" assumption.
(define (make-grapheme-predicate str)
  (let ([get-more (call-with-input-string str
                    (lambda (port)
                      (let ([read-one (case (gauche-character-encoding)
                                        [(utf8 none) (cut read-char port)]
                                        [else (gmap char->ucs (cut read-char port))])])
                        (make-grapheme-cluster-reader read-one length))))]
        [saved-curs '()])
    (lambda (cur)
      ;; check old-curs first, then use gen-more to get one more
      ;; grapheme boundary while keep adding everything to
      ;; new-curs. Save new-curs to saved-curs before returning.
      ;;
      ;; yes we generate a bit of garbage here everytime this
      ;; procedure runs. Maybe we could set! get-more to #f when it
      ;; returns eof, and after that stop maintaining new-curs
      ;; completely...
      (let loop ([old-curs saved-curs]
                 [new-curs '()]
                 [last-cur (string-cursor-start str)])
        (if-let1 last-cur (if (null? old-curs)
                              (let ([len (get-more)])
                                (if (eof-object? len)
                                    #f
                                    (string-cursor-forward str last-cur len)))
                              (car old-curs))
          (let ([new-curs (cons last-cur new-curs)])
            (cond
             [(string-cursor=? cur last-cur)
              (set! saved-curs (reverse new-curs))
              #t]
             [(string-cursor<? cur last-cur)
              (set! saved-curs (reverse new-curs))
              #f]
             [(null? old-curs)
              (loop '() new-curs last-cur)]
             [else
              (loop (cdr old-curs) new-curs last-cur)]))
          (begin
            (set! saved-curs (reverse new-curs))
            #f))))))

(define (err msg item)
  (error <regexp-invalid-sre> :offending-item item msg item))

(define (->cset x)
  (cond
   [(pair? x) (if (eq? (car x) 'comp)
                  (char-set-complement (->cset (cdr x)))
                  (err "invalid SRE AST" x))]
   [(eq? x 'any) char-set:full]
   [(char? x) (->char-set x)]
   [(char-set? x) x]
   [else (err "invalid SRE AST" x)]))

;; parse <cset-sre>. Possible return values
;;
;; - 'any
;; - char
;; - <char-set>
;; - (comp . )
;; - #f (not recognized syntax)
(define (cset-sre sre)
  (define (flatten-ranges sre)
    (let loop ([lst (append-map (lambda (x)
                                  (cond
                                   [(string? x) (string->list x)]
                                   [(char? x) (list x)]
                                   [else (err "invalid <range-spec>" x)]))
                                sre)]
               [res '()])
      (cond
       [(null? lst) (reverse res)]
       [(pair? (cdr lst)) (loop (cddr lst)
                                (cons (cons (car lst)
                                            (cadr lst))
                                      res))]
       [else (err "uneven range" (list->string lst))])))

  (define (named-cset sym)
    (case sym
      [(any) 'any]
      [(nonl) (cset-sre '(~ #\newline #\return))]
      [(ascii) char-set:ascii]
      [(lower lower-case)
       (cond
        [(ascii) char-set:ascii-lower-case]
        [(casefold) char-set:LC]
        [else char-set:lower-case])]
      [(upper upper-case)
       (cond
        [(ascii) char-set:ascii-upper-case]
        [(casefold) char-set:LC]
        [else char-set:upper-case]) ]
      [(title title-case)
       (cond
        [(casefold) char-set:LC]
        [else char-set:title-case])]
      [(alpha alphabetic)
       (if (ascii) char-set:ascii-lower-case char-set:letter)]
      [(num numeric)
       (if (ascii) char-set:ascii-digit char-set:digit)]
      [(alnum alphanum alphanumeric)
       (if (ascii) char-set:ascii-letter+digit char-set:letter+digit)]
      [(punct punctuation)
       (if (ascii) char-set:ascii-punctuation char-set:punctuation)]
      [(symbol)
       (if (ascii) char-set:ascii-symbol char-set:symbol)]
      [(graph graphic)
       (if (ascii) char-set:ascii-graphic char-set:graphic)]
      [(space white whitespace) char-set:whitespace]
      [(print printing)
       (if (ascii) char-set:ascii-printing char-set:printing)]
      [(cntrl control) (ucs-range->char-set 0 32)]
      [(xdigit hex-digit) char-set:hex-digit]
      [else #f]))

  ;; convert all "rest" to <char-set> and apply proc. Return #f if any
  ;; item cannot be parsed as cset
  (define (apply-char-set proc rest)
    (let ([csets (map cset-sre rest)])
      (and (every (^x x) csets)
           (apply proc
                  (map ->cset csets)))))

  (define (cset-sre-1 sre)
    (and (pair? sre)
         (null? (cdr sre))
         (cset-sre (car sre))))

  (define (cset-list sym rest)
    (case sym
      [(char-set) (if (and (string? (car rest))
                           (null? (cdr rest)))
                      (string->char-set (car rest))
                      (err "expected (char-set <string>)" (cons sym rest)))]
      [(/ char-range) (apply char-set-union
                             (map (lambda (x)
                                    (ucs-range->char-set (char->integer (car x))
                                                         (+ (char->integer (cdr x)) 1)))
                                  (flatten-ranges rest)))]
      [(|\|| or) (apply-char-set char-set-union rest)]
      [(& and) (apply-char-set char-set-intersection rest)]
      [(- difference) (let ([rhs (apply-char-set char-set-union (cdr rest))])
                        (if (eq? (car rest) 'any)
                            `(comp . ,rhs)
                            (apply-char-set char-set-difference (list (car rest) rhs))))]
      ;; generate and take advantage of AST form (comp . <cset>)
      ;; delay calling char-set-complement until absolutely needed by
      ;; complex char-set algebra
      [(~ complement) (cons 'comp (apply-char-set char-set-union rest))]
      ;; note that w/* also appear in <sre> syntax, which has
      ;; different semantics. Accept (w/* <cset-sre>) form (i.e. one
      ;; <cset-sre>). Return #f on all other w/* forms and let the
      ;; <sre> parser deal with them.
      [(w/case) (cset-sre-1 rest)]
      [(w/nocase) (let ([cset (cset-sre-1 rest)])
                    (and cset
                         ((with-module gauche.internal %char-set-case-fold!)
                          (char-set-copy (->cset cset)))))]
      [(w/unicode) (cset-sre-1 rest)]
      [(w/ascii) (let1 cset (cset-sre-1 rest)
                   (and cset (cset-sre `(and ascii ,cset))))]
      [else #f]))

  (cond
   [(char? sre) sre]
   [(char-set? sre) sre]
   [(and (string? sre) (= (string-length sre) 1))
    (string-ref sre 0)]
   [(symbol? sre) (named-cset sre)]
   [(pair? sre)
    (cond
     [(string? (car sre))
      (cond
       [(null? (cdr sre))
        (string->char-set (car sre))]
       [else
        (err "expected (<string>)" sre)])]
     [(symbol? (car sre))
      (cset-list (car sre) (cdr sre))]
     [else #f])]
   [else #f]))

(define (regexp-parse-sre sre)
  (define id 0)

  (define (%sre->ast sre)
    (define (sre-sym sre)
      (case sre
        [(bos eos bol eol bow eow nwb bog eog) sre]
        [(grapheme) (%sre->ast
                     '(: bog (non-greedy-repeated 1 #f any) eog))]
        [(word) (%sre->ast '(word+ any))]
        [else (err "not supported" sre)]))

    (define (loop rest)
      (map %sre->ast rest))

    (define (seq-loop rest)
      `(seq ,@(map %sre->ast rest)))

    (define (cpat test rest)
      `(cpat ,(test (%sre->ast (car rest)))
             (,(%sre->ast (cadr rest)))
             ,(cond
               [(null? (cddr rest)) '()]
               [(null? (cdddr rest)) (list (%sre->ast (caddr rest)))]
               [else (err "unsupported syntax" sre)])))

    (define (sre-list sym rest)
      (case sym
        [(* zero-or-more) `(rep 0 #f ,@(loop rest))]
        [(+ one-or-more) `(rep 1 #f ,@(loop rest))]
        [(? optional) `(rep 0 1 ,@(loop rest))]
        [(= exactly) (let ([n (car rest)])
                       `(rep ,n ,n ,@(loop (cdr rest))))]
        [(>= at-least) `(rep ,(car rest)
                             #f
                             ,@(loop (cdr rest)))]
        [(** repeated) `(rep ,(car rest)
                             ,(cadr rest)
                             ,@(loop (cddr rest)))]
        [(|\|| or) `(alt ,@(loop rest))]
        [(: seq) (seq-loop rest)]
        [($ submatch) (if (nocapture)
                          (seq-loop rest)
                          (begin
                            (set! id (+ id 1))
                            `(,id #f ,@(loop rest))))]
        [(-> submatch-named) (if (nocapture)
                                 (seq-loop rest)
                                 (begin
                                   (set! id (+ id 1))
                                   `(,id ,(car rest) ,@(loop (cdr rest)))))]
        [(w/case) (parameterize ([casefold #f])
                    (seq-loop rest))]
        [(w/nocase) (parameterize ([casefold #t])
                      (seq-loop rest))]
        [(w/ascii) (parameterize ([ascii #t])
                     (seq-loop rest))]
        [(w/unicode) (parameterize ([ascii #f])
                       (seq-loop rest))]
        [(w/nocapture) (parameterize ([nocapture #t])
                         (seq-loop rest))]
        [(word) (%sre->ast `(: bow ,@rest eow))]
        [(word+) (%sre->ast `(word (+ (and (or alphanumeric "_")
                                           (or ,@rest)))))]
        [(?? non-greedy-optional) `(rep-min 0 1 ,@(loop rest))]
        [(*? non-greedy-zero-or-more) `(rep-min 0 #f ,@(loop rest))]
        [(**? non-greedy-repeated) `(rep-min ,(car rest)
                                             ,(cadr rest)
                                             ,@(loop (cddr rest)))]
        [(look-ahead) `(assert ,@(loop rest))]
        [(look-behind) `(assert (lookbehind ,@(loop rest)))]
        [(neg-look-ahead) `(nassert ,@(loop rest))]
        [(neg-look-behind) `(nassert (lookbehind ,@(loop rest)))]
        [(backref) (begin
                     (if (and (null? (cdr rest))
                              (or (number? (car rest))
                                  (symbol? (car rest))))
                         `(backref . ,(car rest))
                         (err "expected (backref <integer/symbol>)" (cons sym rest))))]
        ;; gauche extensions
        [(atomic) `(once ,@(cdr (seq-loop rest)))]
        [(if-backref) `(cpat ,(if (number? (car rest))
                                (car rest)
                                (err "if-backref can only take a number" sre))
                             (,(%sre->ast (cadr rest)))
                             ,(cond
                               [(null? (cddr rest)) '()]
                               [(null? (cdddr rest)) (list (%sre->ast (caddr rest)))]
                               [else (err "unsupported syntax" sre)]))]
        [(if-look-ahead) (cpat (^x `(assert ,x)) rest)]
        [(if-neg-look-ahead) (cpat (^x `(nassert ,x)) rest)]
        [(if-look-behind) (cpat (^x `(assert (lookbehind ,x))) rest)]
        [(if-neg-look-behind) (cpat (^x `(nassert (lookbehind ,x))) rest)]
        [else (err "invalid SRE" sym)]))

    (define (fold-case cset)
      (cond
       [(not (casefold)) cset]
       [(eq? cset 'any) cset]
       [else
        ((with-module gauche.internal %char-set-case-fold!)
         (char-set-copy (->cset cset)))]))

    (define (ascii-context cset)
      (cond
       [(not (ascii)) cset]
       ;; 'any technically should become char-set:ascii to avoid
       ;; matching non-ascii character, but we lose 'any' special
       ;; handling in the regexp engine. Not worth it
       [(eq? cset 'any) cset]
       [else
        (fold-case (cset-sre (list 'and char-set:ascii cset)))]))

    (define (finalize-cset cset)
      (fold-case (ascii-context cset)))

    (cond
     [(string? sre) `(seq ,@(map finalize-cset (string->list sre)))]
     [(cset-sre sre) => (cut finalize-cset <>)]
     [(symbol? sre) (sre-sym sre)]
     [(pair? sre) (sre-list (car sre) (cdr sre))]
     [else (err "invalid SRE" sre)]))

  `(0 #f ,(parameterize ([nocapture #f]
                         [casefold #f]
                         [ascii #f])
            (%sre->ast sre))))


(define (sre->regexp re :key (multi-line #t))
  (regexp-compile (regexp-optimize (regexp-parse-sre re))
                  :multi-line multi-line))

(define (regexp-unparse-sre ast)
  (define (parse-cpat test)
    (cond
     [(number? test)
      (values 'if-backref test)]
     [(not (pair? test))
      (err "unsupported AST" ast)]
     [(eq? (car test) 'assert)
      (if (and (pair? (cadr test))
               (eq? (caadr test) 'lookbehind))
        (values 'if-look-behind (cdadr test))
        (values 'if-look-ahead (cdr test)))]
     [(eq? (car test) 'nassert)
      (if (and (pair? (cadr test))
               (eq? (caadr test) 'lookbehind))
        (values 'if-neg-look-behind (cdadr test))
        (values 'if-neg-look-ahead (cdr test)))]
     [else
      (err "unsupported AST" ast)]))

  (define (unparse ast)
    (cond
     [(char? ast) ast]
     [(char-set? ast) ast]
     [(eq? ast 'wb) '(or bow eow)]
     [(symbol? ast) ast]
     [(pair? ast)
      (let ([sym (car ast)]
            [rest (cdr ast)])
        (case sym
          [(comp) `(~ ,(unparse rest))]
          [(seq) `(seq ,@(map unparse rest))]
          [(seq-uncase seq-case)
           (error "intermediate forms that are not generated by SRE" ast)]
          [(alt) `(or ,@(map unparse rest))]
          ;; note, 'rep-while' (aka no-backtrack 'rep') is only
          ;; produced by the AST optimizer, not directly by SRE->AST
          ;; transformation. So we can just return the SRE equivalent
          ;; form of 'rep'.
          [(rep rep-while) (let ([m (car rest)]
                                 [n (cadr rest)]
                                 [rest (cddr rest)])
                             (if n
                                 `(** ,m ,n ,@(map unparse rest))
                                 `(>= ,m ,@(map unparse rest))))]
          [(rep-min) (let ([m (car rest)]
                           [n (cadr rest)]
                           [rest (cddr rest)])
                       (cond
                        [n `(**? ,m ,n ,@(map unparse rest))]
                        [(zero? m) `(*? ,(map unparse rest))]
                        [else ; there's no >=? syntax
                         `(: (**? ,m ,m ,@(map unparse rest))
                             (*? ,(map unparse rest)))]))]
          [(assert) (if (and (pair? rest)
                             (pair? (car rest))
                             (eq? (caar rest) 'lookbehind))
                        `(look-behind ,@(map unparse (cdar rest)))
                        `(look-ahead ,@(map unparse rest)))]
          [(nassert) (if (and (pair? rest)
                             (pair? (car rest))
                             (eq? (caar rest) 'lookbehind))
                         `(neg-look-behind ,@(map unparse (cdar rest)))
                         `(neg-look-ahead ,@(map unparse rest)))]
          [(backref) `(backref ,rest)]
          [(once) `(atomic ,@(map unparse rest))]
          [(cpat) (call-with-values
                      (lambda () (parse-cpat (car rest)))
                    (lambda (name test)
                      `(,name
                        ,(if (pair? test)
                           `(seq ,@(map unparse test))
                           test)
                        (seq ,@(map unparse (cadr rest)))
                        (seq ,@(map unparse (caddr rest))))))]
          [else
           (cond
            ;; group number is ignored, which should be ok
            ;; if we walk ast the same way we walk sre?
            [(number? sym) (let ([sym (car rest)]
                                 [rest (cdr rest)])
                             (if sym
                                 `(submatch-named ,sym ,@(map unparse rest))
                                 `(submatch ,@(map unparse rest))))]
            [else (error "unsupported AST" ast)])]))]
     [else (error "unsupported AST" ast)]))

  (if (and (pair? ast) (zero? (car ast))
           (pair? (cdr ast)) (not (cadr ast)))
      (unparse (cons 'seq (cddr ast)))
      (unparse ast)))

(define (regexp->sre re)
  (regexp-unparse-sre (regexp-ast re)))
