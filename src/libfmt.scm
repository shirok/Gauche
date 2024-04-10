;;;
;;; libfmt.scm - format
;;;
;;;   Copyright (c) 2013-2024  Shiro Kawai  <shiro@acm.org>
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

(declare) ;; a dummy form to suppress generation of "sci" file

(define-module gauche.format
  (use util.match))
(select-module gauche.format)
(inline-stub
 (.include "gauche/priv/configP.h"))

;; Parsing format string
;; The whole parsing business can be written much cleaner using parser.peg,
;; but we need this as a built-in, so we take low-level approach.

;; formatter-lex :: String -> [Directive]
;; Directive = String
;;         | (S flags mincol colinc minpad padchar maxcol)
;;         | (A flags mincol colinc minpad padchar maxcol)
;;         | (C flags)
;;         | (W flags)
;;         | (D flags mincol padchar commachar interval)
;;         | (B flags mincol padchar commachar interval)
;;         | (O flags mincol padchar commachar interval)
;;         | (X flags mincol padchar commachar interval)
;;         | (x flags mincol padchar commachar interval)
;;         | (R flags mincol padchar commachar interval)
;;         | (r flags mincol padchar commachar interval)
;;         | (F flags width digits scale ovfchar padchar)
;;         | ($ flags digits pre-digits width padchar)
;;         | (* flags count)
;;         | (? flags)
;;         | (& flags count)
;;         | (P flags)
;;         | (char flags <char> count) ; single-character insertion
;;         | (bra flags)   ; ~[, beginning of conditional
;;         | (sep flags)   ; ~;, separator of conditional
;;         | (ket flags)   ; ~], end of conditional
;; flags : '() | (Cons #\@ flags) | (Cons #\: flags)

;; NB: This procedure is written in a way that all closures can be
;; lifted (by the 0.9.3.3 compiler) so that we avoid runtime closure
;; allocation.
(define formatter-lex
  (let ()
    (define (fmtstr p) (port-attribute-ref p 'format-string))
    (define (directive? c)
      (string-scan "sSaAcCwWdDbBoOxXrR*?fF$~%&pPtT|(){}[];" c))
    (define directive-param-spec ; (type max-#-of-params [token-id])
      '((S 5) (A 5) (W 0) (C 0)
        (D 4) (B 4) (O 4) (X 4) (x 4) (* 1) (R 5) (r 5) (F 5) ($ 4) (? 0)
        (& 1) (P 0)
        ;; single-character instertion
        (~ 1 #\~) (% 1 #\newline) (T 1 #\tab) (|\|| 1 #\page)
        ;; tokens for structures
        (|(| 0 paren) (|)| 0 thesis) (|[| 1 bra) (|]| 0 ket)
        (|{| 0 curly) (|}| 0 brace) (|\;| 0 sep)))
    (define (flag? c) (memv c '(#\@ #\:)))
    (define (next p)
      (rlet1 c (read-char p)
        (if (eof-object? c)
          (error "Incomplete format directive in" (fmtstr p)))))
    (define (char p c)
      (let loop ([cs (list c)])
        (let1 c (read-char p)
          (cond [(eof-object? c) (list (list->string (reverse cs)))]
                [(eqv? c #\~) (cons (list->string (reverse cs))
                                    (directive p (next p)))]
                [else (loop (cons c cs))]))))
    ;; NB: this keeps params and flags in reverse order.
    ;; check-param reverses them.
    (define (directive p c)
      (let loop ([params '()] [c c])
        (receive (param c) (fc-param p c params)
          (cond [param (loop (cons param params) c)]
                [(eqv? c #\newline)  ;; tilde-newline
                 (if (memq #\@ params)
                   (if (memq #\: params)
                     (error "@ and : flags can't be specified simultaneously \
                             for the ~ + newline directive: " (fmtstr p))
                     (begin (skip-ws p) (cons "\n" (init p)))) ; ~@\newline
                   (if (memq #\: params)
                     (init p)           ; ~:\newline
                     (begin (skip-ws p) (init p))))] ; ~\newline
                [(directive? c)
                 (let1 dir ($ string->symbol $ string
                              $ if (memv c '(#\x #\r)) c (char-upcase c))
                   (acons dir params (init p)))]
                [else (errorf "Invalid format directive character `~a' in ~s"
                              c (fmtstr p))]))))
    ;; initial state
    (define (init p)
      (let1 c (read-char p)
        (cond [(eof-object? c) '()]
              [(eqv? c #\~) (directive p (next p))]
              [else (char p c)])))
    ;; parse one parameter, returns it and next char.
    ;; if we don't have param, return #f as param.
    (define (fc-param p c params)
      (if-let1 d (digit->integer c)
        (and (ensure-noflag p params)
             (digit-param p d 1))
        (case c
          [(#\+ #\-)
           (ensure-noflag p params)
           (let1 c (next p)
             (if-let1 d (digit->integer c)
               (digit-param p d (if (eqv? c #\+) 1 -1))
               (errorf "Invalid format directive in ~s" (fmtstr p))))]
          [(#\') (ensure-noflag p params)
           (let1 c (next p)
             (values c (ensure-param-delimiter p (next p))))]
          [(#\,) (ensure-noflag p params)
           (values 'empty (next p))]
          [(#\@ #\:)
           (when (memv c params)
             (errorf "Duplicate ~a flag in ~s" c (fmtstr p)))
           (values c (next p))]
          [(#\v) (ensure-noflag p params)
           (values 'variable (ensure-param-delimiter p (next p)))]
          [else (values #f c)])))
    (define (ensure-noflag p params)
      (if (or (memv #\@ params) (memv #\: params))
        (errorf "Invalid format directive in ~s" (fmtstr p))
        #t))
    (define (ensure-param-delimiter p c)
      (cond [(eqv? c #\,) (next p)]
            [(or (directive? c) (flag? c)) c]
            [else(errorf "Invalid format parameter syntax in ~s" (fmtstr p))]))
    (define (digit-param p val sign)
      (let loop ([val val])
        (let1 c (next p)
          (if-let1 d (digit->integer c)
            (loop (+ (* val 10) d))
            (values (* val sign) (ensure-param-delimiter p c))))))
    ;; for tilde-newline.  skip following whitespaces.
    (define (skip-ws p)
      (let loop ([c (peek-char p)])
        (when (char-whitespace? c)
          (begin (read-char p) (loop (peek-char p))))))
    ;; Check directive parameters vailidity.  This also normalize
    ;; the use of 'empty.  Returns normalized directive.
    (define (check-param directive p)
      (if (pair? directive)
        (receive (flags params)
            (let outer ([ps (cdr directive)] [fs '()])
              (cond [(null? ps) (values fs '())]
                    [(flag? (car ps)) (outer (cdr ps) (cons (car ps) fs))]
                    [(eq? (car ps) 'empty) (outer (cdr ps) fs)]
                    [else (values fs (reverse! ps))]))
          (receive (nparams type)
              (match (assq (car directive) directive-param-spec)
                [(_ nparams) (values nparams (car directive))]
                [(_ nparams dir) (values nparams dir)])
            (unless (length<=? params nparams)
              (errorf "Too many parameters for directive `~a' in ~s"
                      (car directive) (fmtstr p)))
            (if (char? type)
              (char-node flags params type)
              (list* type flags params))))
        directive))
    ;; single character node optimization
    (define (char-node flags params char)
      (cond [(null? params) (string char)]
            [(integer? (car params)) (make-string (car params) char)]
            [else (list* 'char flags char params)]))

    ;; Body of formatter-lex
    (^[format-str]
      (let1 p (open-input-string format-str)
        (port-attribute-set! p 'format-string format-str)
        ;; can be (map (cut check-param <> p) (init p)), but want to avoid
        ;; closure allocation
        (let loop ([directives (init p)] [r '()])
          (if (null? directives)
            (reverse! r)
            (loop (cdr directives)
                  (cons (check-param (car directives) p) r))))))))

;; formatter-parse :: [Directive] -> Tree
;;
;; Tree = String
;;      | (Seq Tree ...)
;;      | (S flags mincol colinc minpad padchar maxcol)
;;      | (A flags mincol colinc minpad padchar maxcol)
;;      | (C flags)
;;      | (W flags)
;;      | (D flags mincol padchar commachar interval)
;;      | (B flags mincol padchar commachar interval)
;;      | (O flags mincol padchar commachar interval)
;;      | (X flags mincol padchar commachar interval)
;;      | (x flags mincol padchar commachar interval)
;;      | (F flags width digits scale ovfchar padchar)
;;      | ($ flags digits pre-digits width padchar)
;;      | (* flags count)
;;      | (? flags)
;;      | (& flags count)
;;      | (P flags)
;;      | (char flags <char> count)
;;      | (case (Tree ...) Tree)
;;      | (if Tree Tree)    ; NB: if arg is #f, the first clause is chosen
;;      | (maybe Tree)
;;
(define formatter-parse
  (let ()
    ;; Plain string.  Concatenate consecutive strings.
    (define (string-node node ds)
      (let loop ([ds ds] [strs (list node)])
        (match ds
          [() (values (list (apply string-append (reverse strs))) '())]
          [((? string? node) . rest) (loop rest (cons node strs))]
          [_ (receive (trees rest) (parse ds)
               (values
                (cons (apply string-append (reverse strs)) trees)
                rest))])))
    ;; Non-structured, argument consuming nodes
    (define (simple-node node ds)
      (receive (trees rest) (parse ds)
        (values (cons node trees) rest)))
    ;; Parsing ~[...~;...~]
    (define (conditional-node node ds)
      (receive (tree rest)
          (match node
            [(_ ())     (case-tree ds)]
            [(_ (#\:))  (if-tree ds)]
            [(_ (#\@))  (maybe-tree ds)]
            [_ (error "Format directive ~[ may have either : or @ flag, \
                       but not both")])
        (receive (trees rest) (parse rest)
          (values (cons tree trees) rest))))
    (define (case-tree ds)
      (let loop ([ds ds] [clauses '()] [next-default? #f])
        (receive (trees rest) (parse ds)
          (match rest
            [(('ket . _) . rest)
             (values (if next-default?
                       `(case ,(reverse clauses) ,(treefy trees))
                       `(case ,(reverse (cons (treefy trees) clauses)) #f))
                     rest)]
            [(('sep fs) . rest)
             (when next-default?
               (error "Extra ~; directive after ~:;"))
             (loop rest (cons (treefy trees) clauses) (has-:? fs))]
            [_ (unterminated "~[")]))))
    (define (if-tree ds)
      (receive (trees1 rest) (parse ds)
        (match rest
          [(('sep . _) . rest)
           (receive (trees2 rest) (parse rest)
             (match rest
               [(('ket . _) . rest)
                (values `(if ,(treefy trees1) ,(treefy trees2)) rest)]
               [_ (unterminated "~:[")]))]
          [_ (unterminated "~:[")])))
    (define (maybe-tree ds)
      (receive (trees rest) (parse ds)
        (match rest
          [(('ket . _) . rest) (values `(maybe ,(treefy trees)) rest)]
          [_ (unterminated "~@[")])))
    ;; [Tree] -> Tree
    (define (treefy trees)
      (match trees
        [(tree) tree]
        [(tree ...) `(Seq ,@tree)]))
    (define (unterminated what) (errorf "Unterminated ~a directive" what))
    ;; master dispatcher.  returns [Tree] and the rest of ds.
    (define (parse ds)
      (match ds
        [() (values ds '())]
        [((? string? s) . rest) (string-node s rest)]
        [(n . rest)
         (match n
           [('bra flags) (conditional-node n rest)]
           [('ket . _)   (values '() ds)]
           [('sep . _)   (values '() ds)]
           [_ (simple-node n rest)])]))
    ;; Main body of formatter-parse.
    ;; (We don't utilize # of parameters yet).
    (^[directives]
      (receive (trees rest) (parse directives)
        (unless (null? rest)
          (errorf "Unbalanced formatter directive: ~~~a" (caar rest)))
        (treefy trees)))))

;; Runtime
;; Formatters have canonical signature:
;;   (ArgPtr, Port, Control) -> ArgPtr
;; ArgPtr is a structure to hold the argument list and the current position.

;; ArgPtr
;;  +---+---+
;;  | . | .-------------------------\
;;  +-|-+---+                       |
;;    |                             |
;;    v                             |
;;  +---+---+     arglist           v
;;  | . | .------> (arg0 arg1 .... argN ...)
;;  +-|-+---+
;;    |
;;    v
;;   ooo-flag
;;
;; ooo-flag (out-of-order flag) indicates whether the argument list
;; is accessed out-of-order, by ~* directive.

(define (fr-make-argptr args)
  (cons (cons #f args) args))

;; returns current arg, increment argptr.
(define (fr-next-arg! fmtstr argptr)
  (if (null? (cdr argptr))
    (errorf "Running out arguments for format string ~s" fmtstr)
    (rlet1 arg (cadr argptr)
      (set! (cdr argptr) (cddr argptr)))))

;; returns the current arg, without changing pointer
(define (fr-peek-arg fmtstr argptr)
  (if (null? (cdr argptr))
    (errorf "Running out arguments for format string ~s" fmtstr)
    (cadr argptr)))

;; check if we used all args.  if we've reordered args, this always succeeds.
(define (fr-args-used? argptr)
  (or (caar argptr)
      (null? (cdr argptr))))

(define (fr-jump-arg! argptr k)
  (if-let1 t (list-tail (cdar argptr) k #f)
    (begin (set! (cdr argptr) t)
           (set! (caar argptr) #t))
    (error "Format argument displacement out of range" k)))

(define (fr-arg-current-index argptr)
  (let loop ([k 0] [p (cdar argptr)])
    (if (eq? (cdr argptr) p)
      k
      (loop (+ k 1) (cdr p)))))

(define (fr-jump-arg-relative! argptr k)
  (fr-jump-arg! argptr (+ k (fr-arg-current-index argptr))))

;; Flag accessors
(define-inline (has-@? flags) (memv #\@ flags))
(define-inline (has-:? flags) (memv #\: flags))
(define-inline (no-flag? flags) (null? flags))

;;
;; Formatter generators
;;

;; TODO: strict check of invalid params/flags.

(define (make-format-seq fmtstr formatters)
  (^[argptr port ctrl] (dolist [f formatters] (f argptr port ctrl))))

;; (with-format-params ((var default [check]) ...) body ...)
;; Implicitly refers 'params' and 'fmtstr'.
;; Bounds 'port', 'argptr' and 'ctrl' in body.
(define-macro (with-format-params binds . body)
  (define (gen-extract var default :optional (check #f))
    `(,var (if (eq? ,var 'empty) ,default ,var)))
  (define (gen-check var default :optional (check #f))
    (and check
         `(unless (and (not (eq? ,var 'variable)) (,check ,var))
            (error "Bad type of argument for the format parameter ~a: ~s"
                   ',var ,var))))
  (define (gen-vbind var default :optional (check #f))
    `(,var
      (if (eq? ,var 'variable)
        ,(if check
           `(rlet1 ,var (fr-next-arg! fmtstr argptr)
              (unless (,check ,var)
                (error "Bad type of argument for the format parameter ~a: ~s"
                       ',var ,var)))
           '(fr-next-arg! fmtstr argptr))
        ,var)))
  (let ([binds1 (map (^b `(,(car b) 'empty)) binds)]
        [binds2 (map ($ apply gen-extract $) binds)]
        [checks (filter identity (map ($ apply gen-check $) binds))]
        [vbinds (map ($ apply gen-vbind $) binds)])
    `(let-optionals* params ,binds1
       (let* ,binds2
         ,@checks
         (if (memq 'variable params)
           (^[argptr port ctrl]
             (let* ,vbinds
               ,@body))
           (^[argptr port ctrl] ,@body))))))

;; ~S, ~A
(define (make-format-expr fmtstr params flags printer)
  (if (null? params)
    (^[argptr port ctrl]
      (let1 arg (fr-next-arg! fmtstr argptr)
        (if ctrl
          (printer arg port ctrl)
          (printer arg port))))
    ($ with-format-params ([mincol 0]
                           [colinc 1]
                           [minpad 0]
                           [padchar #\space]
                           [maxcol #f])
       ;; TODO: propagate write context from port to sport
       (let ([sport (open-output-string)]
             [arg (fr-next-arg! fmtstr argptr)])
         (when (and (> minpad 0) (has-@? flags))
           (dotimes [_ minpad] (write-char padchar sport)))
         (if ctrl
           (printer arg sport ctrl)
           (printer arg sport))
         (when (and (> minpad 0) (not (has-@? flags)))
           (dotimes [_ minpad] (write-char padchar sport)))
         (let* ([s (get-output-string sport)]
                [len (string-length s)])
           (if (and maxcol (> len maxcol))
             (chop-and-out s maxcol flags port)
             (let1 npad (* (ceiling (/ (max (- mincol len) 0) colinc)) colinc)
               (when (has-@? flags)
                 (dotimes [_ npad] (write-char padchar port)))
               (display s port)
               (unless (has-@? flags)
                 (dotimes [_ npad] (write-char padchar port))))))))))

(define (chop-and-out str limit flags port)
  ;; check if str chopped at limit contains terminated double-quote.
  ;; Returns one of the tree values:
  ;;  #t - Double-quote is terminated.  No special treatment needed.
  ;;  #f - Double quote is not terminated, and the last character is not
  ;;       a lone backslash.  We emit an extra double-quote.
  ;;  #\\ - Double quote is not termianted, and the last character is
  ;;       a lone backslash.  In this case, we need to emit extra backslash
  ;;       before the terminating double-quote.  Otherwise, the program
  ;;       that interprets the result (e.g. font colorizer) mistakes the
  ;;       closing backquote as escaped.
  (define (quote-terminated? str limit)
    (scan-out (open-input-string str) limit))
  (define (scan-out p limit)
    (or (<= limit 0)
        (let1 c (read-char p)
          (or (eof-object? c)
              (cond [(eqv? c #\") (scan-in p (- limit 1))]
                    [(eqv? c #\\) (read-char p) (scan-out p (- limit 2))]
                    [else (scan-out p (- limit 1))])))))
  (define (scan-in p limit)
    (and (> limit 0)
         (let1 c (read-char p)
           (and (not (eof-object? c))
                (cond [(eqv? c #\") (scan-out p (- limit 1))]
                      [(eqv? c #\\) (read-char p)
                       (if (= limit 1)
                         #\\
                         (scan-in p (- limit 2)))]
                      [else (scan-in p (- limit 1))])))))

  (if (and (has-:? flags) (>= limit 4))
    (begin (display (substring str 0 (- limit 4)) port)
           (case (quote-terminated? str (- limit 4))
             [(#t) (display " ..." port)]
             [(#f) (display "\"..." port)]
             [(#\\) (display "\\\".." port)]))
    (display (substring str 0 limit) port)))

;; ~W
;; Without flags, just behave like ~S.
;; With ':'-flag, pretty print.
;; With '@'-flag, remove level and length limits.
(define (make-format-pprint fmtstr flags)
  (let1 ctrl-args
      (cond-list [(has-:? flags) @ '(:pretty #t :width 79)]
                 [(has-@? flags) @ '(:length #f :level #f)])
    (^[argptr port ctrl]
      (let ([arg (fr-next-arg! fmtstr argptr)]
            [ctrl-args (list* :indent
                              ((with-module gauche.internal port-column) port)
                              ctrl-args)])
        (write-shared arg port
                      (if ctrl
                        (apply write-controls-copy ctrl ctrl-args)
                        (apply make-write-controls ctrl-args)))))))

;; ~C
;; The "spelling out" mode (~:C) isn't supported yet.
(define (make-format-char fmtstr flags)
  (define (char-formatter writer)
    (^[argptr port ctrl]
      (let1 c (fr-next-arg! fmtstr argptr)
        (unless (char? c)
          (error "Character required for ~c format directive, but got:" c))
        (writer c port))))
  (char-formatter (if (has-@? flags) write display)))

;; Common in ~X etc. and ~nR
(define-inline (format-num-body fmtstr argptr port ctrl radix upcase
                                flags mincol padchar comma interval point)
  (let* ([arg (fr-next-arg! fmtstr argptr)]
         [sarg (if (number? arg)
                 (number->string arg radix upcase)
                 (write-to-string arg display))])
    ;; In CL, ':' and '@' are only honored for exact integers.
    ;; We honor it on flonums as well.
    (when (or (exact-integer? arg) (flonum? arg))
      (when (has-:? flags)
        (set! sarg (insert-comma-in-digits sarg comma interval point)))
      (when (and (has-@? flags) (>= arg 0))
        (set! sarg (string-append "+" sarg))))
    (let1 len (string-length sarg)
      (when (< len mincol)
        (dotimes [_ (- mincol len)] (write-char padchar port)))
      (display sarg port))))

;; ~D, ~B, ~O, ~X, ~nR
(define (make-format-num fmtstr params flags radix upcase)
  (if (and (null? params) (no-flag? flags))
    (^[argptr port ctrl]
      (let1 arg (fr-next-arg! fmtstr argptr)
        (if (exact? arg)
          (display (number->string arg radix upcase) port)
          (display arg port))))
    ($ with-format-params ([mincol 0]
                           [padchar #\space]
                           [comma #\,]
                           [interval 3]
                           [point #\.])
       (format-num-body fmtstr argptr port ctrl radix upcase
                        flags mincol padchar comma interval point))))

(define (insert-comma-in-digits str comma interval point)
  (define (insert s)
    (let* ([len (string-length s)]
           [leading (modulo len interval)]
           [leading (if (and (= leading 1)  ;avoid -,100,000 etc.
                             (memv (string-ref s 0) '(#\+ #\-)))
                      (+ leading interval)
                      leading)])
      (let loop ([x leading]
                 [r (if (zero? leading)
                      '()
                      (list (substring s 0 leading)))])
        (if (= x len)
          (string-join (reverse r) (string comma))
          (loop (+ x interval) (cons (substring s x (+ x interval)) r))))))
  (receive (pre post) (string-scan str #\. 'both)
    (if pre
      (string-append (insert pre) (string point) post)
      (insert str))))

;; ~R
(define (make-format-r fmtstr params flags upcase)
  (if (null? params)
    (^[argptr port ctrl]
      (format-numeral-R port
                        (fr-next-arg! fmtstr argptr)
                        (has-@? flags)
                        (has-:? flags)
                        upcase))
    ($ with-format-params ([radix 10]
                           [mincol 0]
                           [padchar #\space]
                           [comma #\,]
                           [interval 3]
                           [point #\.])
       (unless (<= 2 radix 36)
         (error "Formatting ~nR: radix out of range (must be between 2 and 36):"
                radix))
       (format-num-body fmtstr argptr port ctrl radix upcase
                        flags mincol padchar comma interval point))))

;; ~F, ~E, ~G  ; we only support ~F for now
;; kind is 'E 'F or 'G
;; @ flag is used to force plus sign (CL)
;; : flag is used for notational rounding (Gauche only)
(define (make-format-flo fmtstr params flags kind)
  ($ with-format-params ([width 0]
                         [digits -1]
                         [scale 0]
                         [ovchar #f]
                         [padchar #\space])
     (let1 arg (fr-next-arg! fmtstr argptr)
       (cond [(real? arg)
              (flo-fmt (inexact (* arg (expt 10 scale)))
                       width digits 0 ovchar padchar
                       (has-@? flags) (has-:? flags) #f port)]
             [(complex? arg)
              (let1 s (call-with-output-string
                        (^p
                         (flo-fmt (* (real-part arg) (expt 10 scale))
                                  0 digits 0 ovchar padchar
                                  (has-@? flags) (has-:? flags) #f p)
                         (flo-fmt (* (imag-part arg) (expt 10 scale))
                                  0 digits 0 ovchar padchar
                                  #t (has-:? flags) #f p)
                         (display "i" p)))
                (let1 len (string-length s)
                  (when (< len width)
                    (dotimes [(- width len)]  (write-char padchar port)))
                  (display s port)))]
             [else
              (let1 sarg (write-to-string arg display)
                (let1 len (string-length sarg)
                  (when (< len width)
                    (dotimes [(- width len)] (write-char padchar port)))
                  (display sarg port)))]))))

;; ~$ (monetary floating point)
;; The order of parameters and their defaults differ from ~F
;; We use notatational rounding (:-flag is used for sign-front.
(define (make-format-currency fmtstr params flags)
  ($ with-format-params ([digits 2]
                         [pre-digits 1]
                         [width 0]
                         [padchar #\space])
     (let1 arg (fr-peek-arg fmtstr argptr)
       (if (real? arg)
         (flo-fmt (inexact (fr-next-arg! fmtstr argptr))
                  width digits pre-digits
                  #f padchar (has-@? flags) #t (has-:? flags) port)
         ;; if arg isn't real, use ~wD.
         (format-num-body fmtstr argptr port ctrl 10 #f
                          '() width #\space #\, 3 #\.)))))

(define (flo-fmt val width digits pre-digits
                 ovchar padchar plus? notational? sign-front? port)
  (let* ([s0 (number->string val 10
                             (cond-list
                              [plus? 'plus]
                              [notational? 'notational])
                             digits)]
         [s (if (> pre-digits 0)        ;special for ~$
              (let* ([m (#/^([+-]?)(\d+)/ s0)]
                     [pre (m 2)])
                (if (> pre-digits (string-length pre))
                  (string-append
                   (m 1)
                   (make-string (- pre-digits (string-length pre)) #\0)
                   pre (m 'after))
                  s0))
              s0)]
         [l (string-length s)])
    (if (< width l)
      (if ovchar
        (dotimes [width] (write-char ovchar port))
        (display s port))
      (receive (pre-sign body)
          (if (and sign-front? (memv (string-ref s 0) '(#\+ #\-)))
            (values (string-copy s 0 1) (string-copy s 1))
            (values "" s))
        (display pre-sign port)
        (dotimes [(- width l)] (write-char padchar port))
        (display body port)))))

;; ~?
(define (make-format-recur fmtstr flags)
  (define (fcompile str)
    (unless (string? str)
      (error "Argument for ~? must be a string, but got" str))
    ($ formatter-compile-rec str $ formatter-parse $ formatter-lex str))
  (if (has-@? flags)
    ;; take
    (^[argptr port ctrl]
      (let1 formatter (fcompile (fr-next-arg! fmtstr argptr))
        (formatter argptr port ctrl)))
    (^[argptr port ctrl]
      (let* ([formatter (fcompile (fr-next-arg! fmtstr argptr))]
             [xargptr (fr-make-argptr (fr-next-arg! fmtstr argptr))])
        (formatter xargptr port ctrl)))))

;; ~*
(define (make-format-jump fmtstr params flags)
  ($ with-format-params ([count 1])
     (let1 count (if (has-:? flags) (- count) count)
       (if (has-@? flags)
         (fr-jump-arg! argptr count)
         (fr-jump-arg-relative! argptr count)))))

;; ~&
(define (make-format-fresh-line fmtstr params flags)
  ($ with-format-params ([count 1])
     (when (>= count 1)
       (fresh-line port)
       (when (>= count 2)
         (dotimes [(- count 1)] (display "\n" port))))))

;; ~P
(define (make-format-plural fmtstr flags)
  (^[argptr port ctl]
    (when (has-:? flags)
      (fr-jump-arg-relative! argptr -1))
    (let1 arg (fr-next-arg! fmtstr argptr)
      ;; NB: Only exact 1 is singular.
      (if (eqv? arg 1)
        (when (has-@? flags)
          (display "y" port))
        (if (has-@? flags)
          (display "ies" port)
          (display "s" port))))))

;; ~t, ~%, ~~, ~|
(define (make-format-single fmtstr ch params flags)
  ($ with-format-params ([count 1])
     (display (make-string count ch) port)))

;; ~[...~;...~]
(define (make-format-case fmtstr clauses default-clause)
  (define branches
    (map (cut formatter-compile-rec fmtstr <>) clauses))
  (define fallback
    (if default-clause
      (formatter-compile-rec fmtstr default-clause)
      (^[argptr port ctl] argptr)))
  (^[argptr port ctl]
    (let1 arg (fr-next-arg! fmtstr argptr)
      (unless (exact-integer? arg)
        (error "Argument for ~[ must be an exact integer, but got" arg))
      (let1 branch-formatter (list-ref branches arg fallback)
        (branch-formatter argptr port ctl)))))

(define (make-format-if fmtstr alternative consequent)
  (define alt-formatter (formatter-compile-rec fmtstr alternative))
  (define csq-formatter (formatter-compile-rec fmtstr consequent))
  (^[argptr port ctl]
    (let1 arg (fr-next-arg! fmtstr argptr)
      (if arg
        (csq-formatter argptr port ctl)
        (alt-formatter argptr port ctl)))))

(define (make-format-maybe fmtstr consequent)
  (define csq-formatter (formatter-compile-rec fmtstr consequent))
  (^[argptr port ctl]
    (if-let1 arg (fr-peek-arg fmtstr argptr)
      (csq-formatter argptr port ctl)
      (begin (fr-jump-arg! argptr 1)
             (^[argptr port ctl] argptr)))))

;; Tree -> Formatter
;; src : source format string for error message
(define (formatter-compile-rec src tree)
  (match tree
    [(? string?) (^[a p c] (display tree p))]
    [(? null?)   (^[a p c] #f)]
    [('Seq . rest) ($ make-format-seq src
                      $ map (cut formatter-compile-rec src <>) rest)]
    [('S fs . ps) (make-format-expr src ps fs write)]
    [('A fs . ps) (make-format-expr src ps fs display)]
    [('C fs . ps) (make-format-char src fs)]
    [('W fs . ps) (make-format-pprint src fs)]
    [('D fs . ps) (make-format-num src ps fs 10 #f)]
    [('B fs . ps) (make-format-num src ps fs 2 #f)]
    [('O fs . ps) (make-format-num src ps fs 8 #f)]
    [('x fs . ps) (make-format-num src ps fs 16 #f)]
    [('X fs . ps) (make-format-num src ps fs 16 #t)]
    [('F fs . ps) (make-format-flo src ps fs 'F)]
    [('$ fs . ps) (make-format-currency src ps fs)]
    [((or 'R 'r) fs . ps) (make-format-r src ps fs (eq? (car tree) 'R))]
    [('? fs)      (make-format-recur src fs)]
    [('* fs . ps) (make-format-jump src ps fs)]
    [('& fs . ps) (make-format-fresh-line src ps fs)]
    [('P fs)      (make-format-plural src fs)]
    [('char fs c . ps) (make-format-single src c ps fs)]
    [('case cls default) (make-format-case src cls default)]
    [('if alt csq) (make-format-if src alt csq)]
    [('maybe csq) (make-format-maybe src csq)]
    [_ (error "Unsupported formatter directive:" tree)]))

;; Toplevel compiler
;; Returns formatter procedure
;;  Formatter :: Port, [Arg] -> Void
(define (formatter-compile fmtstr)
  (let1 fmt ($ formatter-compile-rec fmtstr $ formatter-parse
               $ formatter-lex fmtstr)
    (^[args port ctrl]
      (let1 argptr (fr-make-argptr args)
        (fmt argptr port ctrl)
        (unless (fr-args-used? argptr)
          (errorf "Too many arguments given to format string ~s" fmtstr))))))

(define (call-formatter shared? locking? formatter port ctrl args)
  (cond [((with-module gauche.internal %port-write-state) port)
         ;; We're in middle of shared writing.
         ;; TODO: If we're in the walk pass, all we need to do is to recurse
         ;; into arguments of aggregate types, so we can make this more
         ;; efficient than running the full formatter.
         (formatter args port ctrl)]
        [shared? ($ (with-module gauche.internal %with-2pass-setup) port
                    formatter formatter args port ctrl)]
        [locking? (with-port-locking port formatter args port ctrl)]
        [else (formatter args port ctrl)]))

(define (format-2 formatter-cache shared? out control fmtstr args)
  (let1 formatter (if formatter-cache
                    (or (unbox formatter-cache)
                        (rlet1 f (formatter-compile fmtstr)
                          (set-box! formatter-cache f)))
                    (formatter-compile fmtstr))
    (case out
      [(#t)
       (call-formatter shared? #t formatter (current-output-port) control args)]
      [(#f) (let1 out (open-output-string)
              (call-formatter shared? #f formatter out control args)
              (get-output-string out))]
      [else (call-formatter shared? #t formatter out control args)])))

;; This is a bridge to the internal API.  Although this isn't exported,
;; inliner or compiler macro may insert a call to this.  You shouldn't
;; change the signature across versions.
;;
;; CONTEXT is a list parameters statically constructed during compilation
;; phase.  ARGS is the actual parameter list passed.
;;
;; CONTEXT is currently three-argument list:
;;    shared?  - a flag to distinguish format (#f) and format/ss (#t).
;;    literal-str-pos - the position of the first string literal in
;;               the actual parameters, up to 2.  If the format directive
;;               string is a literal, we can cache the formatter.
;;               We may not be able to statically determine the format
;;               string, because of the optional port/write-controls arguments.
;;    formatter-cache - if there's a string literal in the 0, 1 or 2nd argument,
;;               the compiler macro allocates a box and passes it.
;;               This will be used to cache the formatter.
;;
;; We make CONTEXT a list so that we can extend it in future without changing
;; the signature.  Those CONTEXT can be computed at compile-time and becomes
;; a static literal list.
(define (format-internal context args)
  (match-let1 (shared? literal-str-pos format-cache . _) context
    (let loop ([port 'unseen]
               [control 'unseen]
               [as args]
               [pos 0])
      (cond [(null? as) (error "format: too few arguments" args)]
            [(string? (car as))
             (format-2 (and (eqv? pos literal-str-pos) format-cache)
                       shared?
                       (if (eq? port 'unseen) #f port)
                       (if (eq? control 'unseen) #f control)
                       (car as)
                       (cdr as))]
            [(or (port? (car as)) (boolean? (car as)))
             (if (eq? port 'unseen)
               (loop (car as) control (cdr as) (+ pos 1))
               (error "format: multiple ports given" args))]
            [(is-a? (car as) <write-controls>)
             (if (eq? control 'unseen)
               (loop port (car as) (cdr as) (+ pos 1))
               (error "format: multiple controls given" args))]
            [else (error "format: invalid argument" (car as))]))))

;; The external API, format and format/ss, are defined in libmacro.scm.
;; We want to make them hybrid macro, and the definition needs to come
;; after macro system initialization.
