;;;
;;; gauche.pp - pretty printer
;;;
;;;   Copyright (c) 2000-2017  Shiro Kawai  <shiro@acm.org>
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

;; Experimental

(define-module gauche.pp
  (use srfi-1)
  (use srfi-13)
  (use util.match)
  (export pprint))
(select-module gauche.pp)

;; List printing modes:
;;
;; oneline:
;;   (Lorem ipsum dolor sit amet consectetur adipisicing)
;;
;; fill:
;;   (Lorem ipsum dolor sit amet consectetur adipisicing elit
;;    sed do eiusmod tempor incididunt ut labore et dolore)
;;
;; linear:
;;   (Lorem
;;    ipsum
;;    dolor
;;    ...)
;;
;; (0) If all the elements can be printed in one line, do so.
;;
;; (1) If the list itself doesn't fit in one line, but each element
;;     does, then we use fill mode.
;;
;; (2) Otherwise, we fall back to linear mode.

;; Various states carried around while layouter is working
(define-class <pp-context> ()
  ([writer      :init-form write :init-keyword :writer]
   [shared-dict :init-keyword :shared-dict]
   [controls    :init-keyword :controls]))

(define *default-controls* (make-write-controls :print-length 40
                                                :print-level 10
                                                :print-width 79))

;; for internal convenience
(define-inline (rp-writer c) (~ c 'writer))
(define-inline (rp-dict c)   (~ c 'shared-dict))
(define-inline (rp-length c) (~ c 'controls 'print-length))
(define-inline (rp-level c)  (~ c 'controls 'print-level))
(define-inline (rp-width c)  (~ c 'controls 'print-width))

(define simple-obj?
  (any-pred number? boolean? char? port? symbol? null?
            (cut member <> '("" #()))))

;; We wrap layouter with a state monad to carry around 'seen' list,
;; the shared substructures that has already printed in this try
;; (thus we use #n# next time).
;; We can't use mutable dictionary for this, since the layouter may
;; retry a subtree with different strategies.
(define-syntax do*
  (syntax-rules (<-)
    [(_ () expr) expr]
    [(_ ((var <- init) . more) expr)
     (>>= init (lambda (var) (do* more expr)))]
    [(_ ((action) . more) expr)
     (>>= action (lambda (_) (do* more expr)))]))

(define (return x)    (lambda (s) (values x s)))
(define (>>= m f)     (lambda (s) (receive (r s.) (m s) ((f r) s.))))
(define (get-seen)    (lambda (s) (values s s)))
(define (put-seen s)  (lambda (_) (values #f s)))
(define (run m)       (values-ref (m '()) 0)) ; discard final 'seen' state.
(define (mapM f xs)   (if (null? xs)
                        (return '())
                        (do* ([r <- (f (car xs))]
                              [rs <- (mapM f (cdr xs))])
                             (return (cons r rs)))))

;; Maybe-ish monadic comparison
(define-inline (=* a b) (and a b (= a b)))
(define-inline (<* a b) (and a b (< a b)))
(define-inline (>* a b) (and a b (> a b)))
(define-inline (<=* a b) (and a b (<= a b)))
(define-inline (>=* a b) (and a b (>= a b)))
(define-inline (-* a b . args)
  (and a b (if (null? args) (- a b) (apply -* (- a b) args))))
(define-inline (min* a b) (if a (if b (min a b) a) b))

;; scan obj to find out shared structure and mark it in rp-dict.
(define (scan-shared obj level len c)
  (rlet1 dict (make-hash-table 'eq?)
    (define counter 0)
    (let rec ([obj obj] [level level] [len len])
      (cond [(hash-table-get dict obj #f)
             => (^n (unless (number? n)
                      (hash-table-put! dict obj counter)
                      (inc! counter)))]
            [(or (>* level (rp-level c))
                 (>* len (rp-length c))
                 (simple-obj? obj))]
            [else
             (hash-table-put! dict obj #t)
             (cond [(pair? obj)
                    (rec (car obj) (+ level 1) 0)
                    (rec (cdr obj) level (+ len 1))]
                   [(vector? obj)
                    (dotimes [i (min* (vector-length obj) (rp-level c))]
                      (rec (vector-ref obj i) (+ level 1) 0))])]))))

(define (shared-obj? obj c) (number? (hash-table-get (rp-dict c) obj #f)))

;; Creates a layout-procedure, that takes width and try to find the best
;; layout of obj.  A layout-procedure returns two values---a nested list
;; of strings, and the width the layout occupies.  The width may be #f
;; if the layout spills to more than one lines.  The list of strings
;; may contain a symbol 's or 'b, indicating inter-datum space and line
;; break.   A layout-procedure may be called more than once on the same
;; object if the layout is "retried".
;;
;; The layout function actually returns a state monad that wraps the
;; layout procedure.  The monad carries around a list of "seen" objects.
;; A seen object is a shared object and has been seen at least once.
;; We need to keep track of it separately from the hashtable for the
;; shared object, since the layout may be retried, so we can't use side
;; effects to track the "seen" state.

;; type Layouter = Integer -> (Formatted, Integer)

;; layout :: (Obj, Integer, Context) -> State Layouter
(define (layout obj level c)
  (do* ([seen <- (get-seen)])
       (cond [(and (shared-obj? obj c) (memq obj seen))
              (return (layout-ref obj c))]
             [(simple-obj? obj)
              (return (layout-simple (write-to-string obj (rp-writer c))))]
             [(>=* level (rp-level c)) (return (layout-simple "#"))]
             [else (layout-misc obj (cute layout <> (+ level 1) c) c)])))

;; layout-misc :: (Obj, ((Obj, Context) -> State Layouter), Context)
;;                   -> State Layouter
(define (layout-misc obj rec c)

  ;; mapi :: (Obj -> State Layouter), Vector -> State Layouter
  (define (mapi fn vec)
    (let1 s (vector-length vec)
      (do* ([rs <- (mapM (lambda (i) (fn (vector-ref vec i)))
                         (iota (min* s (rp-length c))))])
           (return (if (<* s (rp-length c)) rs `(,@rs ,dots))))))

  ;; map+ :: (Obj -> State Layouter), List -> State Layouter
  ;; map considering dotted list, print-length, and shared structure
  (define (map+ fn lis)
    (let loop ([lis lis] [len 0] [rs '()])
      (do* ([seen <- (get-seen)])
           (match lis
             [() (return (reverse rs))]
             [(l . lis)
              (if (>=* len (rp-length c))
                (return (reverse `(,dots ,@rs)))
                (do* ([r <- (fn l)])
                     (if (and (shared-obj? lis c) (memq obj seen))
                       (return (reverse `(,(layout-ref lis c) ,dot ,r ,@rs)))
                       (loop lis (+ len 1) (cons r rs)))))]
             [x (do* ([r <- (fn x)]) (return (reverse `(,r ,dot ,@rs))))]))))

  ;;: do-list :: (State String, State [Layouter]) -> State Layouter
  (define (do-list pref mapper)
    (do* ([str <- pref] [elts <- mapper]) (layout-list str elts c)))

  (cond [(pair? obj) (do-list (sprefix obj "(" c) (map+ rec obj))]
        [(vector? obj) (do-list (sprefix obj "#(" c) (mapi rec obj))]
        [(is-a? obj <uvector>)
         (let1 tag (rxmatch-substring
                     (#/[suf]\d+/ (x->string (class-name (class-of obj)))))
           (do-list (sprefix obj (format "#~a(" tag) c) (mapi rec obj)))]
        [else
         (do* ([str <- (sprefix obj (write-to-string obj (rp-writer c)) c)])
              (return (layout-simple str)))]))

;; :: Layouter
(define dots (lambda (w) (values "...." 4)))
(define dot  (lambda (w) (values "." 1)))

;; layout-simple :: String -> Layouter
(define (layout-simple str) (lambda (w) (values str (string-length str))))

;; layout-ref :: Object -> Layouter
(define (layout-ref obj c)
  (layout-simple (format "#~d#" (hash-table-get (rp-dict c) obj))))

;; layout-list :: String, [Layouter], Context -> State Layouter
(define (layout-list prefix elts c)
  (let1 plen (string-length prefix)
    (return (lambda (room)
              (receive (s w) (do-layout-elements (-* room plen) elts c)
                (values (cons prefix (reverse s)) (and w (+ w plen))))))))

;; sprefix :: (Object, String, Context) -> State String
(define (sprefix obj s c)
  (cond [(hash-table-get (rp-dict c) obj #f) number?
         => (lambda (cnt)
              (do* ([seen <- (get-seen)]
                    [ (put-seen (cons obj seen)) ])
                   (return (format "#~d=~a" cnt s))))]
        [else (return s)]))

;; do-layout-elements :: Integer, [Layouter], Context -> (Formatted, Integer)
(define (do-layout-elements room elts c)
  (define (do-oneline r es strs)
    (match es
      [() (values strs (-* room r))]
      [(e . es) (receive (s w) (e room)
                  (cond [(not w) (do-linear room elts)] ;giveup
                        [(>* w room)                            ;too big
                         (do-fill room es (list* 'b s 'b strs))]
                        [(>* w r)
                         (do-fill (-* room w) es (list* s 'b strs))]
                        [else
                         (do-oneline (-* r w 1) es (list* s 's strs))]))]))
  (define (do-fill r es strs)
    (match es
      [() (values strs #f)]
      [(e . es) (receive (s w) (e room)
                  (cond [(not w) (do-linear room elts)]
                        [(>* w (-* r 1))
                         (do-fill (-* room w 1) es (list* s 'b strs))]
                        [else (do-fill (-* r w 1) es (list* s 's strs))]))]))
  (define (do-linear r es)
    (values (fold (lambda (e strs) (receive (s w) (e room) (list* s 'b strs)))
                  '() es)
            #f))
  (receive (s w) (do-oneline room elts '())
    (values (cons ")" s) w)))

;; Render the nested list of strings.  Some trick: S's and b's right
;; after open paren are ignored.  S's right after b's are also ignored.
;; B's insert a newline and a proper indentation.
(define (render stree indent)
  (match stree
   [(prefix . es)
    (display prefix)
    (let1 ind (+ indent (string-length prefix))
      (let loop ([es (drop-while symbol? es)])
        (match es
          [() #f]
          [('b . es) (next-line ind) (loop (drop-while symbol? es))]
          [('s . es) (display " ") (loop es)]
          [(s . es)  (render s ind) (loop es)])))]
   [else (display stree)]))

(define (next-line col) (newline) (dotimes [i col] (display " ")))

;; Entry point of printer.
(define (pprint obj
                :key (controls *default-controls*)
                     print-width print-length print-level)
  (let* ([controls (if (or print-width print-length print-level)
                     (write-controls-copy controls
                                          :print-width print-width
                                          :print-length print-length
                                          :print-level print-level)
                     controls)]
         [context (make <pp-context> :controls controls)])
    (set! (~ context'shared-dict) (scan-shared obj 0 0 context))
    (let1 layouter (run (layout obj 0 context))
      (render (values-ref (layouter (rp-width context)) 0) 0))))
