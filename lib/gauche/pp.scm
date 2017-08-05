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

;; Pretty printer steps
;;
;; 1. Scan the tree and mark shared substrctures.
;; 2. Scan the tree again, to build a layouter network.
;; 3. Run the layouter network to produce "formatted" tree, which is
;;    a tree of strings with separator / line-break directives.
;; 4. Render the formatted tree into string.

;; Various states carried around while the layouter network is built.
;; 'Shared' table is used for shared/circular structure output.
;; The first pass maps object => # of encounter.  If # > 1, we use labels.
;; In the second pass we replace its value with -N where N is the label.
(define-class <pp-context> ()
  ([writer      :init-form write :init-keyword :writer]
   [shared      :init-form (make-hash-table 'eq?)]
   [counter     :init-value 0]          ;shared label counter
   [controls    :init-keyword :controls]))

(define *default-controls* (make-write-controls :print-length 40
                                                :print-level 10
                                                :print-width 79))

;; for internal convenience
(define-inline (rp-writer c) (~ c 'writer))
(define-inline (rp-shared c) (~ c 'shared))
(define-inline (rp-length c) (~ c 'controls 'print-length))
(define-inline (rp-level c)  (~ c 'controls 'print-level))
(define-inline (rp-width c)  (~ c 'controls 'print-width))

(define simple-obj?
  (any-pred number? boolean? char? port? symbol? null?
            (cut member <> '("" #()))))

;; Maybe-ish monadic comparison
(define-inline (=* a b) (and a b (= a b)))
(define-inline (<* a b) (and a b (< a b)))
(define-inline (>* a b) (and a b (> a b)))
(define-inline (<=* a b) (and a b (<= a b)))
(define-inline (>=* a b) (and a b (>= a b)))
(define-inline (-* a b . args)
  (and a b (if (null? args) (- a b) (apply -* (- a b) args))))
(define-inline (min* a b) (if a (if b (min a b) a) b))

;; scan obj to find out shared structure and mark it in rp-shared.
(define (scan-shared! obj level len c)
  (define dict (rp-shared c))
  (define counter 0)
  (let rec ([obj obj] [level level] [len len])
    (unless (or (>* level (rp-level c))
                (>* len (rp-length c))
                (simple-obj? obj))
      (hash-table-update! dict obj (cut + <> 1) 0)
      (cond [(pair? obj)
             (rec (car obj) (+ level 1) 0)
             (rec (cdr obj) level (+ len 1))]
            [(vector? obj)
             (dotimes [i (min* (vector-length obj) (rp-level c))]
               (rec (vector-ref obj i) (+ level 1) 0))]))))

(define (need-label? obj c) (> (hash-table-get (rp-shared c) obj 0) 1))
(define (obj-label obj c)
  (let1 n (hash-table-get (rp-shared c) obj 1)
    (and (<= n 0) (- n))))
(define (add-label! obj c)
  (rlet1 n (~ c'counter)
    (hash-table-put! (rp-shared c) obj (- n))
    (set! (~ c'counter) (+ n 1))))

;; Creates a layout-procedure, that takes width and try to find the best
;; layout of obj.  A layout-procedure returns two values---a nested list
;; of strings, and the width the layout occupies.  The width may be #f
;; if the layout spills to more than one lines.  The list of strings
;; may contain a symbol 's or 'b, indicating inter-datum space and line
;; break.   A layout-procedure may be called more than once on the same
;; object if the layout is "retried".

;; type Layouter = Integer -> (Formatted, Integer)

;; layout :: (Obj, Integer, Context) -> Layouter
(define (layout obj level c)
  (cond [(obj-label obj c) (layout-ref obj c)]
        [(simple-obj? obj) (layout-simple (write-to-string obj (rp-writer c)))]
        [(>=* level (rp-level c)) (layout-simple "#")]
        [else (layout-misc obj (cute layout <> (+ level 1) c) c)]))

;; layout-misc :: (Obj, ((Obj, Context) -> Layouter), Context) -> Layouter
(define (layout-misc obj rec c)

  ;; mapi :: (Obj -> Layouter), Vector -> Layouter
  (define (mapi fn vec)
    (let* ([s (vector-length vec)]
           [rs (map (^i (fn (vector-ref vec i)))
                    (iota (min* s (rp-length c))))])
      (if (<* s (rp-length c)) rs `(,@rs ,dots))))

  ;; map+ :: (Obj -> Layouter), List -> Layouter
  ;; map considering dotted list, print-length, and shared structure
  (define (map+ fn lis)
    (let loop ([lis lis] [len 0] [rs '()])
      (match lis
        [() (reverse rs)]
        [(l . lis)
         (if (>=* len (rp-length c))
           (reverse `(,dots ,@rs))
           (let1 r (fn l)
             (if (obj-label lis c)
               (reverse `(,(layout-ref lis c) ,dot ,r ,@rs))
               (loop lis (+ len 1) (cons r rs)))))]
        [x (reverse `(,(fn x) ,dot ,@rs))])))

  (cond [(pair? obj) (layout-list (sprefix obj "(" c) (map+ rec obj) c)]
        [(vector? obj) (layout-list (sprefix obj "#(" c) (mapi rec obj) c)]
        [(is-a? obj <uvector>)
         (let1 tag (rxmatch-substring
                     (#/[suf]\d+/ (x->string (class-name (class-of obj)))))
           (layout-list (sprefix obj (format "#~a(" tag) c) (mapi rec obj) c))]
        [else
         (layout-simple (sprefix obj (write-to-string obj (rp-writer c)) c))]))

;; :: Layouter
(define dots (^w (values "...." 4)))
(define dot  (^w (values "." 1)))

;; layout-simple :: String -> Layouter
(define (layout-simple str) (^w (values str (string-length str))))

;; layout-ref :: Object -> Layouter
(define (layout-ref obj c)
  (layout-simple (format "#~d#" (hash-table-get (rp-shared c) obj))))

;; layout-list :: (String, [Layouter], Context) -> Layouter
(define (layout-list prefix elts c)
  (let1 plen (string-length prefix)
    (^[room]
      (receive (s w) (do-layout-elements (-* room plen) elts)
        (values (cons prefix (reverse s)) (and w (+ w plen)))))))

;; sprefix :: (Object, String, Context) -> String
(define (sprefix obj s c)
  (if (need-label? obj c)
    (format "#~d=~a" (add-label! obj c) s)
    s))

;; do-layout-elements :: Integer, [Layouter] -> (Formatted, Integer)
;; This is the core of layout&retry algorithm.  Invoke layouters to
;; find out best fit.  Each layouter may be invoked more than once,
;; when retry happens.
(define (do-layout-elements room elts)
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
    (values (fold (^[e strs] (receive (s w) (e room) (list* s 'b strs)))
                  '() es)
            #f))
  (receive (s w) (do-oneline room elts '())
    (values (cons ")" s) w)))

;; Render the nested list of strings.  Some trick: S's and b's right
;; after open paren are ignored.  S's right after b's are also ignored.
;; B's insert a newline and a proper indentation.
(define (render stree indent)
  (define (next-line col) (newline) (dotimes [i col] (display " ")))
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
    (scan-shared! obj 0 0 context)
    (let* ([layouter (layout obj 0 context)]
           [stree (values-ref (layouter (rp-width context)) 0)])
      (render stree 0))))
