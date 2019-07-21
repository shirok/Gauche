(define-module gauche.regexp.sre
  (use scheme.base)
  (use scheme.list)
  (use scheme.charset)
  (export regexp-parse-sre regexp-compile-sre))
(select-module gauche.regexp.sre)

(define (->cset x)
  (cond
   [(pair? x) (if (eq? (car x) 'comp)
                  (char-set-complement (->cset (cdr x)))
                  (error "internal error, invalid AST" x))]
   [(eq? x 'any) char-set:full]
   [(char? x) (->char-set x)]
   [(char-set? x) x]
   [else (error "internal error, invalid AST" x)]))

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
                                   [else (error "invalid sre, invalid <range-spec>" x)]))
                                sre)]
               [res '()])
      (cond
       [(null? lst) (reverse res)]
       [(pair? (cdr lst)) (loop (cddr lst)
                                (cons (cons (car lst)
                                            (cadr lst))
                                      res))]
       [else (error "invalid sre, uneven range" (list->string lst))])))

  (define (named-cset sym)
    (case sym
      [(any) 'any]
      [(nonl) (cset-sre '(~ #\newline #\return))]
      [(ascii) char-set:ascii]
      [(lower lower-case) char-set:lower-case]
      [(upper upper-case) char-set:upper-case]
      [(title title-case) char-set:title-case]
      [(alpha alphabetic) char-set:letter]
      [(num numeric) char-set:digit]
      [(alnum alphanum alphanumeric) char-set:letter+digit]
      [(punct punctuation) char-set:punctuation]
      [(symbol) char-set:symbol]
      [(graph graphic) char-set:graphic]
      [(space white whitespace) char-set:whitespace]
      [(print printing) char-set:printing]
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
                      (error "invalid sre, expected (char-set <string>)" (cons sym rest)))]
      [(/ char-range) (apply char-set-union
                             (map (lambda (x)
                                    (ucs-range->char-set (char->integer (car x))
                                                         (+ (char->integer (cdr x)) 1)))
                                  (flatten-ranges rest)))]
      [(|\|| or) (apply-char-set char-set-union rest)]
      [(& and) (apply-char-set char-set-intersection rest)]
      [(-) (let ([rhs (apply-char-set char-set-union (cdr rest))])
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
        (error "invalid sre, expected (<string>)" sre)])]
     [(symbol? (car sre))
      (cset-list (car sre) (cdr sre))]
     [else #f])]
   [else #f]))

(define (regexp-parse-sre sre)
  (define id 0)

  (define (sre-sym sre)
    (case sre
      [(bol eol bow eow nwb) sre]
      [else (error "invalid sre, not supported" sre)]))

  (define (%sre->ast sre nocapture casefold ascii)
    ;; FIXME: missing (w/* ...), (word* ...), ?? *? **?
    (define (sre-list sym rest)
      (define (loop :optional (rest rest))
        (map (cut %sre->ast <> nocapture casefold ascii) rest))

      (define (seq-loop :optional
                        (rest rest)
                        (nocapture nocapture)
                        (casefold casefold)
                        (ascii ascii))
        `(seq ,@(map (cut %sre->ast <> nocapture casefold ascii) rest)))

      (case sym
        [(* zero-or-more) `(rep 0 #f ,@(loop))]
        [(+ one-or-more) `(rep 1 #f ,@(loop))]
        [(? optional) `(rep 0 1 ,@(loop))]
        [(= exactly) (let ([n (car rest)])
                       `(rep ,n ,n ,@(loop (cdr rest))))]
        [(>= at-least) `(rep ,(car rest)
                             #f
                             ,@(loop (cdr rest)))]
        [(** repeated) `(rep ,(car rest)
                             ,(cadr rest)
                             ,@(loop (cddr rest)))]
        [(|\|| or) `(alt ,@(loop))]
        [(: seq) (seq-loop)]
        [($ submatch) (if nocapture
                          (seq-loop)
                          (begin
                            (set! id (+ id 1))
                            `(,id #f ,@(loop))))]
        [(-> submatch-named) (if nocapture
                                 (seq-loop)
                                 (begin
                                   (set! id (+ id 1))
                                   `(,id ,(car rest) ,@(loop (cdr rest)))))]
        [(w/case) (seq-loop rest nocapture #f)]
        [(w/nocase) (seq-loop rest nocapture #t)]
        [(w/ascii) (seq-loop rest nocapture casefold #t)]
        [(w/unicode) (seq-loop rest nocapture casefold #f)]
        [(w/nocapture) (seq-loop rest #t)]
        [(?? non-greedy-optional) `(rep-min 0 1 ,@(loop))]
        [(*? non-greedy-zero-or-more) `(rep-min 0 #f ,@(loop))]
        [(**? non-greedy-repeated) `(rep-min ,(car rest)
                                             ,(cadr rest)
                                             ,@(loop (cddr rest)))]
        [(look-ahead) `(assert ,@(loop))]
        [(look-behind) `(assert (lookbehind ,@(loop)))]
        [(neg-look-ahead) `(nassert ,@(loop))]
        [(neg-look-behind) `(nassert (lookbehind ,@(loop)))]
        [(backref) (begin
                     (if (null? (cdr rest))
                         (if (symbol? (car rest))
                             (error "invalid sre, (backref <name>) not supported yet")
                             `(backref . ,(car rest)))
                         (error "invalid sre, expected (backref <integer>)" (cons sym rest))))]
        [else (error "invalid sre" sym)]))

    (define (fold-case cset)
      (cond
       [(not casefold) cset]
       [(eq? cset 'any) cset]
       [else
        ((with-module gauche.internal %char-set-case-fold!)
         (char-set-copy (->cset cset)))]))

    (define (ascii-context cset)
      (cond
       [(not ascii) cset]
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
     [else (error "invalid sre" sre)]))

  `(0 #f ,(%sre->ast sre #f #f #f)))

(define (regexp-compile-sre re)
  ($ regexp-compile
     $ regexp-optimize
     $ regexp-parse-sre re))

