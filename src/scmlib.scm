;;;
;;; scmlib.scm - more Scheme libraries
;;;
;;;   Copyright (c) 2000-2010  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: scmlib.scm,v 1.20 2008-05-13 05:44:15 shirok Exp $
;;;

;; This file contains builtin library functions that are easier to be
;; written in Scheme instead of as stubs.
;;

(select-module gauche)

;;;=======================================================
;;; Extending procedures
;;;

;; Extended lambda formals (:optional, :key, :rest etc) are
;; expanded into the call of let-optionals* and let-keywords*
;; macros within the compiler.  Eventually the handling of the
;; optional and keyword arguments will be built in the VM.

;; NB: As of 0.8.13, Gauche cannot pre-compile R5RS macro, so
;; we use legacy macros here.

;; KLUDGE: We need these to include the compiled macro transformer
;; in the binary.  Will be gone in future.
(export let-keywords let-keywords* let-optionals* define-compiler-macro)

(define-macro (let-optionals* arg specs . body)
  (define (rec arg vars&inits rest)
    (cond
     [(null? (cdr vars&inits))
      `((let ((,(caar vars&inits)
               (if (null? ,arg) ,(cdar vars&inits) (car ,arg)))
              ,@(if (null? rest)
                  '()
                  `((,rest (if (null? ,arg) '() (cdr ,arg))))))
          ,@body))]
     [else
      (let ([g (gensym)]
            [v (caar vars&inits)]
            [i (cdar vars&inits)])
        ;; NB: if the compiler were more clever, we could use receive
        ;; or apply to make (null? ,arg) test once.  For now, testing it
        ;; twice is faster.
        `((let ((,v (if (null? ,arg) ,i (car ,arg)))
                (,g (if (null? ,arg) '() (cdr ,arg))))
            ,@(rec g (cdr vars&inits) rest))))]))
  (let1 g (gensym)
    `(let ((,g ,arg))
       ,@(rec g (map (lambda (s)
                       (cond [(and (pair? s) (pair? (cdr s)) (null? (cddr s)))
                              (cons (car s) (cadr s))]
                             [(or (symbol? s) (identifier? s))
                              (cons s '(undefined))]
                             [else (error "malformed let-optionals* bindings:"
                                          specs)]))
                     specs)
              (cdr (last-pair specs))))))
              

(define-macro (let-keywords arg specs . body)
  (%let-keywords-rec arg specs body 'let 'errorf))

(define-macro (let-keywords* arg specs . body)
  (%let-keywords-rec arg specs body 'let* 'errorf))

(define (%let-keywords-rec arg specs body %let %error/warn)
  (define (triplet var&default)
    (or (and-let* ([ (list? var&default) ]
                   [var (unwrap-syntax (car var&default))]
                   [ (symbol? var) ])
          (case (length var&default)
            [(2) (values (car var&default)
                         (make-keyword var)
                         (cadr var&default))]
            [(3) (values (car var&default)
                         (unwrap-syntax (cadr var&default))
                         (caddr var&default))]
            [else #f]))
        (and-let* ([var (unwrap-syntax var&default)]
                   [ (symbol? var) ])
          (values var (make-keyword var) (undefined)))
        (error "bad binding form in let-keywords" var&default)))
  (define (process-specs specs)
    (let loop ((specs specs)
               (vars '()) (keys '()) (defaults '()) (tmps '()))
      (define (finish restvar)
        (values (reverse! vars)
                (reverse! keys)
                (reverse! defaults)
                (reverse! tmps)
                restvar))
      (cond [(null? specs) (finish #f)]
            [(pair? specs)
             (receive (var key default) (triplet (car specs))
               (loop (cdr specs)
                     (cons var vars)
                     (cons key keys)
                     (cons default defaults)
                     (cons (gensym) tmps)))]
            [else (finish (or specs #t))])))

  (let ((argvar (gensym "args")) (loop (gensym "loop")))
    (receive (vars keys defaults tmps restvar) (process-specs specs)
      `(let ,loop ((,argvar ,arg)
                   ,@(if (boolean? restvar) '() `((,restvar '())))
                   ,@(map (cut list <> (undefined)) tmps))
            (cond
             [(null? ,argvar)
              (,%let ,(map (lambda (var tmp default)
                             `(,var (if (undefined? ,tmp) ,default ,tmp)))
                           vars tmps defaults)
                     ,@body)]
             [(null? (cdr ,argvar))
              (error "keyword list not even" ,argvar)]
             [else
              (case (car ,argvar)
                ,@(map (lambda (key)
                         `((,key)
                           (,loop (cddr ,argvar)
                                  ,@(if (boolean? restvar)
                                      '()
                                      `(,restvar))
                                  ,@(map (lambda (k t)
                                           (if (eq? key k)
                                             `(cadr ,argvar)
                                             t))
                                         keys tmps))))
                       keys)
                (else
                 ,(cond [(eq? restvar #t)
                         `(,loop (cddr ,argvar) ,@tmps)]
                        [(eq? restvar #f)
                         `(begin
                            (,%error/warn "unknown keyword ~S" (car ,argvar))
                            (,loop (cddr ,argvar) ,@tmps))]
                        [else
                         `(,loop
                           (cddr ,argvar)
                           (list* (car ,argvar) (cadr ,argvar) ,restvar)
                           ,@tmps)])))
              ]))))
  )

(define (delete-keywords ks kvlist)
  (define (rec kvs)
    (cond [(null? kvs) '()]
          [(null? (cdr kvs)) (error "incomplete key list" kvlist)]
          [(memv (car kvs) ks) (rec (cddr kvs))]
          [else (list* (car kvs) (cadr kvs) (rec (cddr kvs)))]))
  (rec kvlist))

(define (delete-keywords! ks kvlist)
  (define (head kvs)
    (cond [(null? kvs) '()]
          [(null? (cdr kvs)) (error "incomplete key list" kvlist)]
          [(memv (car kvs) ks) (head (cddr kvs))]
          [else (cut-tail! (cddr kvs) kvs) kvs]))
  (define (cut-tail! kvs prev)
    (cond [(null? kvs)]
          [(null? (cdr kvs)) (error "incomplete key list" kvlist)]
          [(memv (car kvs) ks)
           (set-cdr! (cdr prev) (cddr kvs))
           (cut-tail! (cddr kvs) prev)]
          [else (cut-tail! (cddr kvs) kvs)]))
  (head kvlist))

;; Tentative compiler macro.
;;
;;  (define-compiler-macro <name>
;;    (er-transformer
;;     (lambda (form rename compare) ...)))
;;
;; Er-transformer is a wrapper to indicate the transformer is
;; explicit-renaming.  It leaves room to support other type of macro
;; transformers in future.
;; The transformer itself must return <FORM> itself if it aborts
;; expansion.

(define-macro (define-compiler-macro name xformer-spec)
  ;; TODO: Rewrite this after we get builtin patter matching.
  (unless (and (= (length xformer-spec) 2)
               (eq? (unwrap-syntax (car xformer-spec)) 'er-transformer))
    (error "malformed define-compiler-macro: "
           `(define-compiler-macro ,name ,xformer-spec)))
  `((with-module gauche.internal %bind-inline-er-transformer)
    (current-module) ',name ,(cadr xformer-spec)))

;;;=======================================================
;;; Inline stub and declarations
;;;

;; The form (inline-stub ...) allows genstub directives embedded
;; within a Scheme source.  It is only valid when the source is
;; pre-compiled into C.  Technically we can kick C compiler at
;; runtime, but it'll need some more work, so we signal an error
;; when inline-stub form is evaluated at runtime.  The precompiler
;; (precomp) handles this form specially.

(define-macro (inline-stub . _)
  (warn "The inline-stub form can only be used for Scheme source \
         to be pre-compiled.  Since you're loading the file without \
         pre-compilation, the definitions and expressions in the \
         inline-stub form are ignored."))

;; The form (declare ...) may be used in wider purpose.  For the time
;; being we use it in limited purposes for compilers.  In interpreter
;; we just ignore it.
(define-macro (declare . _) #f)

;;;=======================================================
;;; List utilities
;;;

;; R5RS cxr's

;; NB: we avoid using getter-with-setter here, since
;;   - The current compiler doesn't take advantage of locked setters
;;   - Using getter-with-setter loses the inferred closure name
;; But this may change in future, of course.
(define-syntax %define-cxr
  (syntax-rules ()
    ((_ name a b)
     (begin
       (define-inline (name x) (a (b x)))
       (define-in-module scheme name name)
       (set! (setter name) (lambda (x v) (set! (a (b x)) v)))))))

(%define-cxr caaar  car  caar)
(%define-cxr caadr  car  cadr)
(%define-cxr cadar  car  cdar)
(%define-cxr caddr  car  cddr)
(%define-cxr cdaar  cdr  caar)
(%define-cxr cdadr  cdr  cadr)
(%define-cxr cddar  cdr  cdar)
(%define-cxr cdddr  cdr  cddr)
(%define-cxr caaaar caar caar)
(%define-cxr caaadr caar cadr)
(%define-cxr caadar caar cdar)
(%define-cxr caaddr caar cddr)
(%define-cxr cadaar cadr caar)
(%define-cxr cadadr cadr cadr)
(%define-cxr caddar cadr cdar)
(%define-cxr cadddr cadr cddr)
(%define-cxr cdaaar cdar caar)
(%define-cxr cdaadr cdar cadr)
(%define-cxr cdadar cdar cdar)
(%define-cxr cdaddr cdar cddr)
(%define-cxr cddaar cddr caar)
(%define-cxr cddadr cddr cadr)
(%define-cxr cdddar cddr cdar)
(%define-cxr cddddr cddr cddr)

;; Some srfi-1 functions that are used in the compiler
;; (hence we need to define here)

(define-inline (null-list? l)
  (cond [(null? l)]
        [(pair? l) #f]
        [else (error "argument must be a list, but got:" l)]))

(with-module gauche.internal
  (define (%zip-nary-args arglists . seed)
    (let loop ([as arglists]
               [cars '()]
               [cdrs '()])
      (cond [(null? as)
             (values (reverse! (if (null? seed) cars (cons (car seed) cars)))
                     (reverse! cdrs))]
            [(null? (car as)) (values #f #f)] ;;exhausted
            [(pair? (car as))
             (loop (cdr as) (cons (caar as) cars) (cons (cdar as) cdrs))]
            [else
             (error "argument lists contained an improper list ending with:"
                    (car as))])))
  )

(define (any pred lis . more)
  (if (null? more)
    (and (not (null-list? lis))
         (let loop ((head (car lis)) (tail (cdr lis)))
           (cond [(null-list? tail) (pred head)] ; tail call
                 [(pred head)]
                 [else (loop (car tail) (cdr tail))])))
    (let loop ((liss (cons lis more)))
      (receive (cars cdrs)
          ((with-module gauche.internal %zip-nary-args) liss)
        (cond [(not cars) #f]
              [(apply pred cars)]
              [else (loop cdrs)])))))

(define (every pred lis . more)
  (if (null? more)
    (or (null-list? lis)
        (let loop ((head (car lis)) (tail (cdr lis)))
          (cond [(null-list? tail) (pred head)] ; tail call
                [(not (pred head)) #f]
                [else (loop (car tail) (cdr tail))])))
    (let loop ((liss (cons lis more)))
      (receive (cars cdrs)
          ((with-module gauche.internal %zip-nary-args) liss)
        (cond [(not cars)]
              [(not (apply pred cars)) #f]
              [else (loop cdrs)])))))

(define (fold kons knil lis . more)
  (if (null? more)
    (let loop ((lis lis) (knil knil))
      (if (null-list? lis) knil (loop (cdr lis) (kons (car lis) knil))))
    (let loop ((liss (cons lis more)) (knil knil))
      (receive (cars cdrs)
          ((with-module gauche.internal %zip-nary-args) liss knil)
        (if cars
          (loop cdrs (apply kons cars))
          knil)))))

(define (fold-right kons knil lis . more)
  (if (null? more)
    (let rec ((lis lis))
      (if (null-list? lis)
        knil
        (kons (car lis) (rec (cdr lis)))))
    (let rec ((liss (cons lis more)))
      (receive (cars cdrs)
          ((with-module gauche.internal %zip-nary-args) liss)
        (if cars
          (apply kons (append! cars (list (rec cdrs))))
          knil)))))

(define (find pred lis)
  (let loop ((lis lis))
    (cond [(not (pair? lis)) #f]
          [(pred (car lis)) (car lis)]
          [else (loop (cdr lis))])))

(define (split-at lis i)
  (let loop ((i i) (rest lis) (r '()))
    (cond [(= i 0) (values (reverse! r) rest)]
          [(null? rest) (error "given list is too short:" lis)]
          [else (loop (- i 1) (cdr rest) (cons (car rest) r))])))

;;;=======================================================
;;; string stuff
;;;

;; String mutators.
;; They are just for backward compatibility, and they are expensive
;; anyway, so we provide them here instead of natively.

(define-in-module scheme (string-set! str k ch)
  (check-arg string? str)
  (check-arg integer? k)
  (check-arg exact? k)
  (check-arg char? ch)
  (let1 len (string-length str)
    (when (or (< k 0) (<= len k))
      (error "string index out of range:" k))
    (%string-replace-body! str
                           (string-append (substring str 0 k)
                                          (string ch)
                                          (substring str (+ k 1) len)))))

(set! (setter string-ref) string-set!)

(define (string-byte-set! str k b)
  (check-arg string? str)
  (check-arg integer? k)
  (check-arg exact? k)
  (check-arg integer? b)
  (let ([siz (string-size str)]
        [out (open-output-string :private? #t)])
    (when (or (< k 0) (<= siz k))
      (error "string index out of range:" k))
    (display (byte-substring str 0 k) out)
    (write-byte b out)
    (display (byte-substring str (+ k 1) siz) out)
    (%string-replace-body! str (get-output-byte-string out))))

(set! (setter string-byte-ref) string-byte-set!)

(define (string-fill! str c :optional (start 0) (end (string-length str)))
  (let1 len (string-length str)
    (when (or (< start 0) (< len start))
      (error "start index out of range:" start))
    (when (or (< end 0) (< len end))
      (error "end index out of range:" end))
    (when (< end start)
      (errorf "end index ~s is smaller than start index ~s" end start))
    (if (and (= start 0) (= end len))
      (%string-replace-body! str (make-string len c))
      (%string-replace-body! str
                             (string-append (substring str 0 start)
                                            (make-string (- end start) c)
                                            (substring str end len))))))

(define-reader-ctor 'string-interpolate
  (lambda (s) (string-interpolate s))) ;;lambda is required to delay loading

;;;=======================================================
;;; call/cc alias
;;;
(define-in-module scheme call/cc call-with-current-continuation)

;;;=======================================================
;;; error stuff, in terms of the condition system
;;;
(define-values (error errorf)
  (let ()
    (define (mkmsg msg args) ;; srfi-23 style message
      (let1 p (open-output-string)
        (display msg p)
        (dolist [obj args] (display " " p) (write/ss obj p))
        (get-output-string p)))
    (define (scan-keys args)
      (let loop ((args args)
                 (keys '()))
        (if (and (not (null? args))
                 (keyword? (car args))
                 (not (null? (cdr args))))
          (loop (cddr args) (list* (cadr args) (car args) keys))
          (values (reverse! keys) args))))
    
    (define (error msg . args)
      (raise
       (cond
        [(is-a? msg <condition-meta>)
         (receive (keys msgs) (scan-keys args)
           (if (null? msgs)
             (apply make msg keys)
             (apply make msg :message (mkmsg (car msgs) (cdr msgs)) keys)))]
        [else (make <error> :message (mkmsg msg args))])))

    (define (errorf fmt . args)
      (raise
       (cond
        [(is-a? fmt <condition-meta>)
         (receive (keys msgs) (scan-keys args)
           (if (null? msgs)
             (apply make fmt keys)
             (apply make fmt :message (apply format/ss #f msgs) keys)))]
        [else (make <error> :message (apply format/ss #f fmt args))])))

    (values error errorf)))

(define <exception> <condition>) ;; backward compatibility

;; exit handler.  we don't want to import the fluff with gauche.parameter,
;; so we manually allocate parameter slot.

(define exit-handler
  (receive (index id) (%vm-make-parameter-slot)
    ;; set default exit handler
    (%vm-parameter-set! index id
                        (lambda (code fmt args)
                          (when fmt
                            (apply format (standard-error-port) fmt args)
                            (newline (standard-error-port)))))
    (lambda maybe-arg
      (rlet1 old (%vm-parameter-ref index id)
        (when (pair? maybe-arg)
          (%vm-parameter-set! index id (car maybe-arg)))))))

(define (exit . args)
  (let-optionals* args ([code 0] [fmt #f] . args)
    (cond [(exit-handler)
           => (lambda (h) (guard (e [(<error> e) #f]) (h code fmt args)))])
    (%exit code)))

;;;=======================================================
;;; symbol-bound? (deprecated)
;;;
(define (symbol-bound? name . maybe-module)
  (global-variable-bound? (get-optional maybe-module #f) name))

;;;=======================================================
;;; call-with-values
;;;
(define-in-module scheme (call-with-values producer consumer)
  (receive vals (producer) (apply consumer vals)))

;;;=======================================================
;;; other system utilities
;;;
(define (sys-sigset . signals)
  (if (null? signals)
    (make <sys-sigset>)
    (apply sys-sigset-add! (make <sys-sigset>) signals)))

;; This is originally a part of shell-escape-string in gauche.process,
;; but the lower level function Scm_Exec() requires this to build
;; windows command line string from given argument list.   It would be
;; clumsy to implement this in C, so we provide this here to be shared
;; by Scm_Exec() and shell-escape-string.
;; NB:  There seems no reliable way to escape command line arguments on
;; windows, since the parsing is up to every application.  However,
;; the standard C runtime seems to obey that (a) whitespaces can be
;; embedded if the argument is surrounded by double quotes, and (b)
;; within double-quotes, consecutive two double-quotes are replaced
;; for one double-quote.
;; NB: The second condition would be clearer if we use string-index, but
;; it is in srfi-13.  string-split, otoh, is built-in.  The overhead of
;; using string-split here would be negligible.
(define (%sys-escape-windows-command-line s)
  (cond [(not (string? s))
         (%sys-escape-windows-command-line (write-to-string s))]
        [(equal? s "") "\"\""]
        [(null? (cdr (string-split s #[\s\"]))) s]
        [else (string-append "\"" (regexp-replace-all #/\"/ s "\"\"") "\"")]))

;;;=======================================================
;;; standard hash-bang tokens
;;;

(define-reader-directive 'r6rs
  (lambda (sym port ctx)
    (warn "Reading R6RS source file.  Note that Gauche is not R6RS compliant.")
    ;; TODO: we could do some adjustments, such as switching the semantics of
    ;; '#,' from srfi-10 to r6rs 'unsyntax'.
    (values)))

(define-reader-directive 'fold-case
  (lambda (sym port ctx)
    (port-case-fold-set! port #t)
    (values)))

(define-reader-directive 'no-fold-case
  (lambda (sym port ctx)
    (port-case-fold-set! port #f)
    (values)))

;;;=======================================================
;;; srfi-17
;;;
(define (getter-with-setter get set)
  (rlet1 proc (lambda x (apply get x))
    (set! (setter proc) set)))

;;;=======================================================
;;; srfi-38
;;;

(define read-with-shared-structure read)
(define read/ss read)

(define (write-with-shared-structure obj :optional (port (current-output-port)))
  (write* obj port))
(define write/ss write-with-shared-structure)

;;;=======================================================
;;; i/o utilities
;;;

(define (print . args) (for-each display args) (newline))

(define-values (format format/ss)
  (letrec ((format-int
            (lambda (port fmt args shared?)
              (cond [(eqv? port #f)
                     (let ((out (open-output-string :private? #t)))
                       (%format out fmt args shared?)
                       (get-output-string out))]
                    [(eqv? port #t)
                     (%format (current-output-port) fmt args shared?)]
                    [else (%format port fmt args shared?)])))
           (format
            (lambda (fmt . args)
              (if (string? fmt)
                (format-int #f fmt args #f) ;; srfi-28 compatible behavior
                (format-int fmt (car args) (cdr args) #f))))
           (format/ss
            (lambda (fmt . args)
              (if (string? fmt)
                (format-int #f fmt args #t) ;; srfi-28 compatible behavior
                (format-int fmt (car args) (cdr args) #t))))
           )
    (values format format/ss)))

;;;=======================================================
;;; regexp utilities
;;;

(define-values (regexp-replace regexp-replace-all)
  (let ()
    ;; aux routine for regexp-replace[-all]
    ;; "abc\\1de\\3" => '("abc" 1 "de" 3)
    (define (regexp-parse-subpattern sub)
      (cond
       [(string? sub)
        (let loop ((sub sub) (r '()))
          (cond [(rxmatch #/\\(?:(\d+)|k<([^>]+)>|(.))/ sub)
                 => (lambda (m)
                      (define (loop2 elem)
                        (loop (rxmatch-after m)
                              (list* elem (rxmatch-before m) r)))
                      (cond [(rxmatch-substring m 1)
                             => (lambda (d) (loop2 (string->number d)))]
                            [(rxmatch-substring m 2)
                             => (lambda (s) (loop2 (string->symbol s)))]
                            [else (loop2 (rxmatch-substring m 3))]))]
                [else (reverse (cons sub r))]))]
       [(procedure? sub) sub]
       [else (error "string or procedure required, but got" sub)]))

    ;; internal loop
    (define (regexp-replace-rec match subpat rec)
      (display (rxmatch-before match))
      (if (procedure? subpat)
        (display (subpat match))
        (dolist [pat subpat]
          (display (if (or (number? pat) (symbol? pat))
                     (rxmatch-substring match pat)
                     pat))))
      (rec (rxmatch-after match)))

    (define (regexp-replace rx string sub)
      (let ([subpat (regexp-parse-subpattern sub)]
            [match  (rxmatch rx string)])
        (if match
          (with-output-to-string (cut regexp-replace-rec match subpat display))
          string)))

    ;; The inner call is awkward to avoid creation of output string
    ;; when no match at all.
    (define (regexp-replace-all rx string sub)
      (let ([subpat (regexp-parse-subpattern sub)]
            [match  (rxmatch rx string)])
        (if match
          (with-output-to-string
            (lambda ()
              (define (loop str)
                (unless (equal? str "")
                  (cond [(rxmatch rx str)
                         => (lambda (match)
                              (when (= (rxmatch-start match) (rxmatch-end match))
                                (error "regexp-replace-all: matching zero-length string causes infinite loop:" rx))
                              (regexp-replace-rec match subpat loop))]
                        [else (display str)])))
              (regexp-replace-rec match subpat loop)))
          string)))
    (values regexp-replace regexp-replace-all)))

;; Multiple replacement
(define-values (regexp-replace* regexp-replace-all*)
  (let ()
    (define (regexp-replace-driver name func-1)
      (lambda (string rx sub . more)
        (cond [(null? more) (func-1 rx string sub)]
              [else
               (unless (zero? (modulo (length more) 2))
                 (errorf "~a: regexp and subsitution don't pair up" name))
               (let loop ([s (func-1 rx string sub)]
                          [args more])
                 (if (null? args)
                   s
                   (loop (func-1 (car args) s (cadr args))
                         (cddr args))))])))
    (values
     (regexp-replace-driver 'regexp-replace* regexp-replace)
     (regexp-replace-driver 'regexp-replace-all* regexp-replace-all))))

;; Contributed from Alex Shinn; modified a bit by shiro
(define (regexp-quote str)
  (with-string-io str
    (lambda ()
      (let loop ((c (read-char)))
        (unless (eof-object? c)
          (when (char-set-contains? #[\\|\[\](){}.*+?^$] c) (write-char #\\))
          (write-char c)
          (loop (read-char)))))))

(define (rxmatch->string rx str . sel)
  (cond [(null? sel) (rxmatch-substring (rxmatch rx str))]
        [(eq? (car sel) 'after)
         (apply rxmatch-after (rxmatch rx str) (cdr sel))]
        [(eq? (car sel) 'before)
         (apply rxmatch-before (rxmatch rx str) (cdr sel))]
        [else (rxmatch-substring (rxmatch rx str) (car sel))]))

;;;=======================================================
;;; with-something
;;;

;; R5RS open-{input|output}-file can be hooked by conversion port.
;; %open-{input|output}-file/conv are autoloaded.

(define-in-module scheme (open-input-file filename . args)
  (if (get-keyword :encoding args #f)
    (apply %open-input-file/conv filename args)
    (apply %open-input-file filename args)))

(define-in-module scheme (open-output-file filename . args)
  (if (get-keyword :encoding args #f)
    (apply %open-output-file/conv filename args)
    (apply %open-output-file filename args)))

;; File ports.

(define-in-module scheme (call-with-input-file filename proc . flags)
  (let1 port (apply open-input-file filename flags)
    (unwind-protect (proc port)
      (when port (close-input-port port)))))

(define-in-module scheme (call-with-output-file filename proc . flags)
  (let1 port (apply open-output-file filename flags)
    (unwind-protect (proc port)
      (when port (close-output-port port)))))

(define-in-module scheme (with-input-from-file filename thunk . flags)
  (let1 port (apply open-input-file filename flags)
    (and port
         (unwind-protect (with-input-from-port port thunk)
           (close-input-port port)))))

(define-in-module scheme (with-output-to-file filename thunk . flags)
  (let1 port (apply open-output-file filename flags)
    (and port
         (unwind-protect (with-output-to-port port thunk)
           (close-output-port port)))))

;; String ports

(define (with-output-to-string thunk)
  (let1 out (open-output-string)
    (with-output-to-port out thunk)
    (get-output-string out)))

(define (with-input-from-string str thunk)
  (with-input-from-port (open-input-string str) thunk))

(define (call-with-output-string proc)
  (let1 out (open-output-string)
    (proc out)
    (get-output-string out)))

(define (call-with-input-string str proc)
  (proc (open-input-string str)))

(define (call-with-string-io str proc)
  (let ((out (open-output-string))
        (in  (open-input-string str)))
    (proc in out)
    (get-output-string out)))

(define (with-string-io str thunk)
  (with-output-to-string (cut with-input-from-string str thunk)))

(define (write-to-string obj :optional (writer write))
  (with-output-to-string (cut writer obj)))

(define (read-from-string string . args)
  (with-input-from-string
      (if (null? args) string (apply %maybe-substring string args))
    read))

;; with-port

(define-syntax %with-ports
  (syntax-rules ()
    [(_ "tmp" (tmp ...) () (port ...) (param ...) thunk)
     (let ((tmp #f) ...)
       (dynamic-wind
           (lambda () (when port (set! tmp (param port))) ...)
           thunk
           (lambda () (when tmp (param tmp)) ...)))]
    [(_ "tmp" tmps (port . more) ports params thunk)
     (%with-ports "tmp" (tmp . tmps) more ports params thunk)]
    [(_ ((param port) ...) thunk)
     (%with-ports "tmp" () (port ...) (port ...) (param ...) thunk)]))

(define (with-input-from-port port thunk)
  (%with-ports ((current-input-port port)) thunk))

(define (with-output-to-port port thunk)
  (%with-ports ((current-output-port port)) thunk))

(define (with-error-to-port port thunk)
  (%with-ports ((current-error-port port)) thunk))

(define (with-ports iport oport eport thunk)
  (%with-ports ((current-input-port iport)
                (current-output-port oport)
                (current-error-port eport))
               thunk))

;; Get the file path currently loading from.
;; We may swap the implementation with more reliable way in future.
(define (current-load-path)
  (and-let* ([p (current-load-port)]
             [info (port-name p)]
             [ (string? info) ]
             [ (not (#/^\(.*\)$/ info)) ])
    info))

;;; TEMPORARY for 0.9.x series
;;; Remove this after 1.0 release!!!
;;;
;;; Add 0.9 directories, which doesn't follow the new directory structure,
;;; to support the extension modules that are installed with 0.9.  
;;; A few extension packages installs files into the "sys" directory hierarchy
;;; instead of "site" one.  It is banned after 0.9.1, but we need to add 0.9's
;;; sys directories for the backward compatibility.
;;; NB: we set! to *load-path* etc here, which is an emergency workaround.
;;; Ordinary programs should never modify *load-path*/*dynamic-load-path*
;;; directly.
(let* ([archdir (gauche-architecture-directory)]
       [m (rxmatch #/gauche-0\.9[\/\\]0\.9[^\/\\]*[\/\\]/ archdir)]
       [oldsitedir (string-append (rxmatch-before m)
                                  "gauche/site/0.9/"
                                  (rxmatch-after m))]
       [oldarchdir (string-append (rxmatch-before m)
                                  "gauche/0.9/"
                                  (rxmatch-after m))])
  (set! *dynamic-load-path*
        (append *dynamic-load-path* (list oldsitedir oldarchdir))))
(let* ([libdir (gauche-library-directory)]
       [m (rxmatch #/0\.9[^\/\\]*/ libdir)]
       [oldlibdir (string-append (rxmatch-before m)"0.9"(rxmatch-after m))])
  (set! *load-path* (append *load-path* (list oldlibdir))))
