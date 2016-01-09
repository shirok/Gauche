;;;
;;; comp-macros.scm - Utility macros for the compiler
;;;
;;;   Copyright (c) 2004-2015  Shiro Kawai  <shiro@acm.org>
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

;; This file is included in compile.scm.

;;;
;;;  Utility macros used in the main compiler.
;;;

;; Defines a bunch of constants, and the alist to map name to value.
(define-macro (define-enum name . syms)
  (let1 alist '()
    `(eval-when (:compile-toplevel)
       ,@(let loop ((syms syms) (i 0))
           (if (null? syms)
             '()
             (begin
               (push! alist (cons (car syms) i))
               (cons `(define-constant ,(car syms) ,i)
                     (loop (cdr syms) (+ i 1))))))
       (define-constant ,name ',(reverse alist)))))

;; We use integers, instead of symbols, as tags, for it allows
;; us to use jump table rather than 'case'.
;; This macro allows us to use symbolic constants instead of
;; the actual integers.
(define-macro (case/unquote obj . clauses)
  (let1 tmp (gensym)
    (define (expand-clause clause)
      (match clause
        [((item) . body)
         `((eqv? ,tmp ,item) ,@body)]
        [((item ...) . body)
         (let1 ilist (list 'quasiquote
                           (map (cut list 'unquote <>) item))
           `((memv ,tmp ,ilist) ,@body))]
        [('else . body)
         `(else ,@body)]))
    `(let ((,tmp ,obj))
       (cond ,@(map expand-clause clauses)))))

;; Similar to case/unquote, but this turns each clause into a
;; toplevel procedure and use jump vector, which is more efficient.
;; TODO: In future, we might want to compile case into jump vector
;; if the range of values are close together.  Once we have it,
;; we may no longer need this hack.
;;
;; ex. (define/case (foo iform y z)
;;       (iform-tag iform)
;;       [($DEFINE) expr ...]
;;       ...)
;;  =>
;;     (define-inline (foo iform y z)
;;       ((vector-ref *foo-dispatch-table* (iform-tag iform)) iform y z))
;;
;;     (define (foo/$DEFINE iform y z) expr ...)
;;     ...
;;     (define *foo-dispatch-table* (generate-dispatch-table foo))
(define-macro (define/case proc dispatch-expr . clauses)
  (define (generate-handler key clause)
    (let1 name (string->symbol #"~(car proc)/~key")
      `(define (,name ,@(cdr proc)) ,@clause)))
  (define (find-clause key)
    (if-let1 c (find (^c (memq key (car c))) clauses)
      (cdr c)
      (if-let1 c (assq 'else clauses)
        (cdr c)
        (errorf "No dispatch clause for ~a found during expanding ~a"
                key proc))))
  (define dispatch-table (string->symbol #"*~(car proc)-dispatch-table*"))

  `(begin
     (define-inline ,proc
       ((vector-ref ,dispatch-table ,dispatch-expr) ,@(cdr proc)))
     ,@(map (^t (generate-handler (car t) (find-clause (car t))))
            .intermediate-tags.)
     (define ,dispatch-table
       (vector ,@(map (^t (string->symbol #"~(car proc)/~(car t)"))
                      .intermediate-tags.)))))

;; Inlining map.  Combined with closure optimization, we can avoid
;; closure creation if we inline map call.
;; We once tried inlining map calls automatically, and found the
;; performance gain in general wasn't significant to justify the
;; amount of increase of the code.
;; However, it is worth to do in performance critical path.
;; NB: proc is evaluated every iteration.  Intended it to be a lambda form,
;; so that its call is inlined.

(define-macro (imap proc lis)
  (match proc
    [('cut p '<> c)      `(%map1c ,p ,lis ,c)]
    [('cut p '<> c1 c2)  `(%map1cc ,p ,lis ,c1 ,c2)]
    ['make-lvar+         `(%map-make-lvar ,lis)]
    [((or 'lambda '^) . _)
     (let ([p (gensym)] [r (gensym)] [loop (gensym)])
       `(let ,loop ((,r '()) (,p ,lis))
          (if (null? ,p)
            (reverse ,r)
            (,loop (cons (,proc (car ,p)) ,r) (cdr ,p)))))]
    [else `(map ,proc ,lis)]))

(define-macro (imap2 proc lis1 lis2)
  (let ([p1 (gensym)] [p2 (gensym)] [r (gensym)] [loop (gensym)])
    `(let ,loop ((,r '()) (,p1 ,lis1) (,p2 ,lis2))
       (if (null? ,p1)
         (reverse ,r)
         (,loop (cons (,proc (car ,p1) (car ,p2)) ,r) (cdr ,p1) (cdr ,p2))))))

(define-macro (ifor-each proc lis)
  (let ([p (gensym)] [loop (gensym)])
    `(let ,loop ((,p ,lis))
       (unless (null? ,p) (,proc (car ,p)) (,loop (cdr ,p))))))

(define-macro (ifor-each2 proc lis1 lis2)
  (let ([p1 (gensym)] [p2 (gensym)] [loop (gensym)])
    `(let ,loop ((,p1 ,lis1) (,p2 ,lis2))
       (unless (null? ,p1)
         (,proc (car ,p1) (car ,p2))
         (,loop (cdr ,p1) (cdr ,p2))))))

;; Inlining max
;; We only compare unsigned integers (in Pass 5), so we use the specialized
;; version of max.
(define-macro (imax x y . more)
  (if (null? more)
    `(%imax ,x ,y)
    `(%imax ,x (imax ,y ,@more))))

;; Generate dispatch table
(define-macro (generate-dispatch-table prefix)
  `(vector ,@(map (lambda (p) (string->symbol #"~|prefix|/~(car p)"))
                  .intermediate-tags.)))


;; Vector-as-struct
;;
;; We use a simple vector and manually defined accessors/modifiers,
;; since vector-{ref|set!} is very fast in Gauche.  We will rewrite
;; the simple-minded define-simple-struct macro with gauche.record
;; once we can compile vector-backed record efficiently.

;; Macro define-simple-struct creates a bunch of functions and macros
;; to emulate a structure by a vector.
;; NAME is a symbol to name the structure type.  TAG is some value
;; (usually a symbol or an integer) to indicate the type of the
;; structure.
;;
;; (define-simple-struct <name> <tag> <constructor> [(<slot-spec>*)])
;;
;; <constructor> : <symbol> | #f
;; <slot-spec>   : <slot-name> | (<slot-name> [<init-value>])
;;
;; For each <slot-spec>, the following accessor/modifier are automatially
;; generated.
;;
;;   NAME-SLOT      - accessor (macro)
;;   NAME-SLOT-set! - modifier (macro)
;;
;; If a symbol is given as <constructor>, it becomes a macro to construct
;; the structure.  It can take zero to as many arguments as the # of slots.
;; The arguments to the constructor initializes the slots in the order of
;; their appearance in define-simple-struct.  If not enough arguments are
;; given to the constructor, the rest of slots are initialized by each
;; <init-value> (or #f if <init-value> is omitted).

(define-macro (define-simple-struct name tag constructor :optional (slot-defs '()))
  (define (take l n) ; we can't use srfi-1 take, so here it is.
    (if (zero? n) '() (cons (car l) (take (cdr l) (- n 1)))))
  (define (make-constructor)
    (let ([args (gensym)]
          [num-slots  (length slot-defs)]
          [slot-names (map (^[s] (if (symbol? s) s (car s))) slot-defs)]
          [init-vals  (map (^[s] (if (symbol? s) #f (cadr s))) slot-defs)])
      `(define-macro (,constructor . ,args)
         (match ,args
           ,@(let loop ((n 0)
                        (r '()))
               (if (> n num-slots)
                 r
                 (let1 carg (take slot-names n)
                   (loop (+ n 1)
                         (cons
                          `(,carg
                            (list 'vector
                                  ,@(if tag `(',tag) '())
                                  ,@carg
                                  ,@(map (cut list 'quote <>)
                                         (list-tail init-vals n))))
                          r)))
                 ))))
      ))
  `(begin
     ,@(if constructor
         `((declare (keep-private-macro ,constructor))
           ,(make-constructor))
         '())
     ,@(let loop ((s slot-defs) (i (if tag 1 0)) (r '()))
         (if (null? s)
           (reverse r)
           (let* ([slot-name (if (pair? (car s)) (caar s) (car s))]
                  [acc (string->symbol #"~|name|-~|slot-name|")]
                  [mod (string->symbol #"~|name|-~|slot-name|-set!")])
             (loop (cdr s)
                   (+ i 1)
                   (list*
                    `(define-macro (,acc obj)
                       `(vector-ref ,obj ,,i))
                    `(define-macro (,mod obj val)
                       `(vector-set! ,obj ,,i ,val))
                    `(declare (keep-private-macro ,acc ,mod))
                    r))))))
  )

