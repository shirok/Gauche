;;;
;;; gauche.pp - pretty printer
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

;; Experimental

(define-module gauche.pputil
  (use util.match)
  (export pprint))
(select-module gauche.pputil)

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
   [shared      :init-form (make-hash-table 'eq?) :init-keyword :shared]
   [counter     :init-value 0]          ;shared label counter
   [controls    :init-keyword :controls]))

;; for internal convenience
(define-inline (rp-shared c) (~ c 'shared))
(define-inline (rp-length c) (~ c 'controls 'length))
(define-inline (rp-level c)  (~ c 'controls 'level))
(define-inline (rp-width c)  (~ c 'controls 'width))

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

;; Recurse to the system's writer to handle objects other than
;; lists and vectors.  We want to pass down the controls (for
;; print-base etc.), but the system's writer directly recurse into
;; %pretty-print if print-pretty is true, causing infinite loop.
;; So we drop pretty-print.  It is, however, 
(define (rec-writer c)
  (let ([w  (~ c'writer)]
        [c2 (write-controls-copy (~ c'controls) :pretty #f)])
    (^x (w x c2))))

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
(define (has-label? obj c) (<= (hash-table-get (rp-shared c) obj 1) 0))
(define (add-label! obj c)
  (rlet1 n (~ c'counter)
    (hash-table-put! (rp-shared c) obj (- n))
    (set! (~ c'counter) (+ n 1))))

;; Creates a layouter that takes width and try to find the best
;; layout of obj.  A layouter returns a pair of two values---a formatted
;; string tree (FSTree), and the width the layout occupies.  The width may
;; be #f if the layout spills to more than one lines.  FSTree is a
;; tree of strings, with a symbol 's or 'b, indicating inter-datum space
;; and line break.   A layouter may be called more than once on
;; the same object if the layout is "retried".
;;
;; Because of retrying, running layouter might cost exponential time
;; in the worst case, when leaf layouter is invoked again and again with
;; the same width parameter.  So we also carry aronud a hashtable to
;; memoize the result. The table uses (cons Width Layouter) as key, and
;; (cons FStree Width) as result.

;; Layouter = Integer, Memo -> (FSTree . Integer)
;; FSTree = String | 's | 'b | (FSTree ...)

;; Memoizing lambda
(define-syntax memo^
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ (w m) . body)
        (quasirename r
          `(rec (fn ,w ,m)
             (or (hash-table-get ,m (cons ,w fn) #f)
                 (rlet1 p (begin ,@body)
                   (hash-table-put! ,m (cons ,w fn) p)))))]))))

(define (make-memo-hash)
  (make-hash-table
   (make-comparator pair?
                    (^[a b] (and (eqv? (car a) (car b))
                                 (eq? (cdr a) (cdr b))))
                    #f
                    (^p (combine-hash-value (eqv-hash (car p))
                                            (eq-hash (cdr p)))))))

;; layout :: (Obj, Integer, Context) -> Layouter
(define (layout obj level c)
  (cond [(has-label? obj c) (layout-ref obj c)]
        [(simple-obj? obj) (layout-simple (write-to-string obj (rec-writer c)))]
        [(>=* level (rp-level c)) (layout-simple "#")]
        [else (layout-misc obj (cute layout <> (+ level 1) c) c)]))

;; layout-misc :: (Obj, ((Obj, Context) -> Layouter), Context) -> Layouter
(define (layout-misc obj rec c)

  ;; mapi :: (Obj -> Layouter), Vector -> Layouter
  (define (mapi fn vec)
    (let* ([s (vector-length vec)]
           [rs (map (^i (fn (vector-ref vec i)))
                    (iota (min* s (rp-length c))))])
      (if (>=* s (rp-length c)) `(,@rs ,dots) rs)))

  ;; mapu :: (Obj -> Layouter), UVector -> Layouter
  (define (mapu fn vec)
    (let* ([s (uvector-length vec)]
           [rs (map (^i (fn (uvector-ref vec i)))
                    (iota (min* s (rp-length c))))])
      (if (>=* s (rp-length c)) `(,@rs ,dots) rs)))
  
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
             (if (has-label? lis c)
               (reverse `(,(layout-ref lis c) ,dot ,r ,@rs))
               (loop lis (+ len 1) (cons r rs)))))]
        [x (reverse `(,(fn x) ,dot ,@rs))])))

  (cond [(pair? obj) (layout-list (sprefix obj "(" c) (map+ rec obj) c)]
        [(vector? obj) (layout-list (sprefix obj "#(" c) (mapi rec obj) c)]
        [(is-a? obj <uvector>)
         (let1 tag (rxmatch-substring
                     (#/[csuf]\d+/ (x->string (class-name (class-of obj)))))
           (layout-list (sprefix obj (format "#~a(" tag) c) (mapu rec obj) c))]
        [else
         (layout-simple (sprefix obj (write-to-string obj (rec-writer c)) c))]))

;; :: Layouter
(define dots (^[w m] '("...." . 4)))
(define dot  (^[w m] '("." . 1)))

;; layout-simple :: String -> Layouter
(define (layout-simple str) (^[w m] (cons str (string-length str))))

;; layout-ref :: Object -> Layouter
(define (layout-ref obj c)
  (layout-simple (format "#~d#" (- (hash-table-get (rp-shared c) obj)))))

;; layout-list :: (String, [Layouter], Context) -> Layouter
(define (layout-list prefix elts c)
  (let1 plen (string-length prefix)
    (memo^ [room memo]
           (match-let1 (s . w) (do-layout-elements (-* room plen) memo elts)
             (cons (cons prefix (reverse s)) (and w (+ w plen)))))))

;; sprefix :: (Object, String, Context) -> String
(define (sprefix obj s c)
  (if (need-label? obj c)
    (format "#~d=~a" (add-label! obj c) s)
    s))

;; do-layout-elements :: Integer, [Layouter] -> (Formatted, Integer)
;; This is the core of layout&retry algorithm.  Invoke layouters to
;; find out best fit.  Each layouter may be invoked more than once,
;; when retry happens.
(define (do-layout-elements room memo elts)
  (define (do-oneline r es strs)
    (match es
      [() (cons strs (-* room r))]
      [(e . es) (match-let1 (s . w) (e room memo)
                  (cond [(not w) (do-linear room elts)] ;giveup
                        [(>* w room)                            ;too big
                         (do-fill room es (list* 'b s 'b strs))]
                        [(>* w r)
                         (do-fill (-* room w) es (list* s 'b strs))]
                        [else
                         (do-oneline (-* r w 1) es (list* s 's strs))]))]))
  (define (do-fill r es strs)
    (match es
      [() (cons strs #f)]
      [(e . es) (match-let1 (s . w) (e room memo)
                  (cond [(not w) (do-linear room elts)]
                        [(>* w (-* r 1))
                         (do-fill (-* room w 1) es (list* s 'b strs))]
                        [else (do-fill (-* r w 1) es (list* s 's strs))]))]))
  (define (do-linear r es)
    (cons (fold (^[e strs] (match-let1 (s . w) (e room memo) (list* s 'b strs)))
                '() es)
          #f))
  (match-let1 (s . w) (do-oneline room elts '())
    (cons (cons ")" s) w)))

;; Render the nested list of strings.  Some trick: S's and b's right
;; after open paren are ignored.  S's right after b's are also ignored.
;; B's insert a newline and a proper indentation.
(define (render stree indent port)
  (define (next-line col) (newline port) (dotimes [i col] (display " " port)))
  (define (drop-while pred xs) ; avoid depending on srfi-1 (for now)
    (cond [(null? xs) '()]
          [(pred (car xs)) (drop-while pred (cdr xs))]
          [else xs]))
  (match stree
    [(prefix . es)
     (display prefix port)
     (let1 ind (+ indent (string-length prefix))
       (let loop ([es (drop-while symbol? es)])
         (match es
           [() #f]
           [('b . es) (next-line ind) (loop (drop-while symbol? es))]
           [('s . es) (display " " port) (loop es)]
           [(s . es)  (render s ind port) (loop es)])))]
    [else (display stree port)]))

;; Stitch together
(define-in-module gauche (%pretty-print obj port shared-table controls)
  (let1 context (make <pp-context>
                  :controls controls
                  :shared (or shared-table (make-hash-table 'eq?)))
    (unless shared-table
      (scan-shared! obj 0 0 context))
    (let* ([layouter (layout obj 0 context)]
           [memo (make-memo-hash)]
           [fstree (car (layouter (rp-width context) memo))])
      (render fstree 0 port))))

;; Write controls used by pprint
(define *default-controls* (make-write-controls :length #f
                                                :level #f
                                                :width 79
                                                :pretty #t))

;; External API
(define (pprint obj
                :key (port (current-output-port))
                     (controls *default-controls*)
                     width length level
                     ((:newline nl) #t))
  (let1 controls (write-controls-copy controls
                                      :width width
                                      :length length
                                      :level level
                                      :pretty #t)
    (write obj port controls)
    (when nl (newline port))))
