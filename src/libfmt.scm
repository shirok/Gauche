;;;
;;; libfmt.scm - format
;;;
;;;   Copyright (c) 2013-2018  Shiro Kawai  <shiro@acm.org>
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
;;         | (* flags count)
;; flags : '() | (Cons #\@ flags) | (Cons #\: flags)

;; NB: This procedure is written in a way that all closures can be
;; lifted (by the 0.9.3.3 compiler) so that we avoid runtime closure
;; allocation.
(define formatter-lex
  (let ()
    (define (fmtstr p) (port-attribute-ref p 'format-string))
    (define (directive? c) (string-scan "sSaAcCwWdDbBoOxXrR*fF" c))
    (define directive-param-spec ; (type max-#-of-params)
      '((S 5) (A 5) (W 0) (C 0)
        (D 4) (B 4) (O 4) (X 4) (x 4) (* 1) (R 5) (r 5) (F 5)))
    (define (flag? c) (memv c '(#\@ #\:)))
    (define (next p)
      (rlet1 c (read-char p)
        (if (eof-object? c)
          (error "Incomplete format directive in" (fmtstr p)))))
    (define (char p c)
      (let loop ([cs (list c)])
        (let1 c (read-char p)
          (cond [(eof-object? c) (list (list->string (reverse cs)))]
                [(eqv? c #\~)
                 (cons (list->string (reverse cs))
                       (directive p (next p)))]
                [else (loop (cons c cs))]))))
    ;; NB: this keeps params and flags in reverse order.
    ;; check-param 
    (define (directive p c)
      (let loop ([params '()] [c c])
        (receive (param c) (fc-param p c params)
          (cond [param (loop (cons param params) c)]
                [(directive? c)
                 => (^_  (let1 directive
                             ($ string->symbol $ string
                                $ if (memv c '(#\x #\r)) c (char-upcase c))
                           (acons directive params (init p))))]
                [(eqv? c #\~) (cons (string c) (init p))]
                [(eqv? c #\%) (cons "\n" (init p))] ;TODO: handle param
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
          (let1 spec (assq (car directive) directive-param-spec)
            (unless (length<=? params (cadr spec))
              (errorf "Too many parameters for directive `~a' in ~s"
                      (car directive) (fmtstr p))))
          (list* (car directive) flags params))
        directive))

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

;; formatter-parse :: [Directive] -> (Tree, argcnt::Maybe Int)
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
;;      | (* flags count)
;;
;; argcnt : An integer if the formatter takes fixed number of arguments,
;;          #f otherwise.
;;
;; At this moment we don't have much to do here, but when we introduce
;; structured formatting directives such as ~{ ~}, this procedure will
;; recognize it.
(define formatter-parse
  (let ()
    ;; Plain string.  Concatenate consecutive strings.
    (define (string-node node ds r cnt)
      (if (null? ds)
        (values (cons node r) cnt)
        (if (string? (car ds))
          (let loop ([ds (cdr ds)] [strs (list (car ds) node)])
            (cond
             [(null? ds)
              (values (cons (apply string-append (reverse strs)) r) cnt)]
             [(string? (car ds))
              (loop (cdr ds) (cons (car ds) strs))]
             [else
              (receive (r cnt) (parse ds r cnt)
                (values (cons (apply string-append (reverse strs)) r) cnt))]))
          (receive (r cnt) (parse ds r cnt)
            (values (cons node r) cnt)))))
    ;; Non-structured, argument consuming nodes
    (define (simple-node node ds r cnt)
      (receive (r cnt) (parse ds r cnt)
        (values (cons node r)
                (and cnt (+ 1 (num-variable-params (cdr node)) cnt)))))
    ;; Jump nodes
    (define (jump-node node ds r cnt)
      (receive (r _) (parse ds r #f)
        (values (cons node r) #f)))
    ;; count "variable" parameters
    (define (num-variable-params params)
      (count (^p (eq? p 'variable)) params))
    ;; master dispatcher
    (define (parse ds r cnt)
      (cond [(null? ds) (values ds cnt)]
            [(string? (car ds)) (string-node (car ds) (cdr ds) r cnt)]
            [(eq? (caar ds) '*) (jump-node (car ds) (cdr ds) r cnt)]
            [else (simple-node (car ds) (cdr ds) r cnt)]))
    
    (^[directives]
      (let1 branches (parse directives '() 0)
        (if (and (pair? branches) (null? (cdr branches)))
          (car branches)
          (cons 'Seq branches))))))

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
           
;; ~S, ~A, ~W
;; NB: ~W doesn't take params, so justifying part won't be called for ~W.
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
  (if (has-:? flags)
    (begin (display (substring str 0 (- limit 4)) port)
           (display " ..." port))
    (display (substring str 0 limit) port)))

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
           (display sarg port))))))
           
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

;; ~R (roman numerals)
(define (make-format-r src flags upper)
  (^[argptr port ctrl] (error "not implemented yet")))

;; ~F, ~E, ~G  ; we only support ~F for now
;; kind is 'E 'F or 'G
;; @ flag is used to force plus sign (CL)
;; : flag is used for notational rounding (Gauche only)
(define (make-format-flo fmtstr params flags kind)
  (define (do-fmt n w d ovchar padchar flags port)
    (let* ([s (number->string n 10
                              (cond-list
                               [(has-@? flags) 'plus]
                               [(has-:? flags) 'notational])
                              d)]
           [l (string-length s)])
      (if (< w l)
        (if ovchar
          (dotimes [w] (write-char ovchar port))
          (display s port))
        (begin
          (dotimes [(- w l)] (write-char padchar port))
          (display s port)))))
  ($ with-format-params ([width 0]
                         [digits -1]
                         [scale 0]
                         [ovchar #f]
                         [padchar #\space])
     (let1 arg (fr-next-arg! fmtstr argptr)
       (if (real? arg)
         (do-fmt (inexact (* arg (expt 10 scale)))
                 width digits ovchar padchar flags port)
         (let1 sarg (write-to-string arg display)
           (let1 len (string-length sarg)
             (when (< len width)
               (dotimes [(- width len)] (write-char padchar port)))
             (display sarg port)))))))
           
;; ~*
(define (make-format-jump fmtstr params flags)
  ($ with-format-params ([count 1])
     (let1 count (if (has-:? flags) (- count) count)
       (if (has-@? flags)
         (fr-jump-arg! argptr count)
         (fr-jump-arg-relative! argptr count)))))

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
    [('W fs . ps) (make-format-expr src ps fs write-shared)]
    [('D fs . ps) (make-format-num src ps fs 10 #f)]
    [('B fs . ps) (make-format-num src ps fs 2 #f)]
    [('O fs . ps) (make-format-num src ps fs 8 #f)]
    [('x fs . ps) (make-format-num src ps fs 16 #f)]
    [('X fs . ps) (make-format-num src ps fs 16 #t)]
    [('F fs . ps) (make-format-flo src ps fs 'F)]
    [((or 'R 'r) fs . ps)
     (let ([upper (eq? (car tree) 'R)])
       (if (null? ps)
         (make-format-r src fs upper)
         (make-format-num src (cdr ps) fs (car ps) upper)))]
    [('* fs . ps) (make-format-jump src ps fs)]
    [_ (error "boo!")]))

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

(define (format-2 shared? out control fmtstr args)
  (let1 formatter (formatter-compile fmtstr)
    (case out
      [(#t)
       (call-formatter shared? #t formatter (current-output-port) control args)]
      [(#f) (let1 out (open-output-string)
              (call-formatter shared? #f formatter out control args)
              (get-output-string out))]
      [else (call-formatter shared? #t formatter out control args)])))

;; handle optional destination arg
(define (format-1 shared? args)
  (let loop ([port 'unseen]
             [control 'unseen]
             [as args])
    (cond [(null? as) (error "format: too few arguments" args)]
          [(string? (car as))
           (format-2 shared?
                     (if (eq? port 'unseen) #f port)
                     (if (eq? control 'unseen) #f control)
                     (car as)
                     (cdr as))]
          [(or (port? (car as)) (boolean? (car as)))
           (if (eq? port 'unseen)
             (loop (car as) control (cdr as))
             (error "format: multiple ports given" args))]
          [(is-a? (car as) <write-controls>)
           (if (eq? control 'unseen)
             (loop port (car as) (cdr as))
             (error "format: multiple controls given" args))]
          [else (error "format: invalid argument" (car as))])))

;; API
(define-in-module gauche (format . args) (format-1 #f args))
(define-in-module gauche (format/ss . args) (format-1 #t args))
