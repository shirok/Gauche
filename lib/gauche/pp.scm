;;;
;;; gauche.pp - pretty printer
;;;
;;;   Copyright (c) 2000-2012  Shiro Kawai  <shiro@acm.org>
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
  (use srfi-42)
  (use gauche.parameter)
  (use gauche.sequence)
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

(define rp-writer (make-parameter write))
(define rp-dict   (make-parameter #f)) ;table to count shared structure

(define *rp-width* 79)
(define *rp-level* 7)
(define *rp-length* 40)

(define simple-obj?
  (any-pred number? boolean? char? port? symbol? null?
            (cut member <> '("" #()))))

;; crude state monad to simplify carry around 'seen' list.
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

;; scan obj to find out shared structure and mark it in rp-dict.
(define (scan-shared obj level len)
  (rlet1 dict (make-hash-table 'eq?)
    (define counter 0)
    (let rec ([obj obj] [level level] [len len])
      (cond [(hash-table-get dict obj #f)
             => (^n (unless (number? n)
                      (hash-table-put! dict obj counter)
                      (inc! counter)))]
            [(or (> level *rp-level*) (> len *rp-length*) (simple-obj? obj))]
            [else
             (hash-table-put! dict obj #t)
             (cond [(pair? obj)
                    (rec (car obj) (+ level 1) 0)
                    (rec (cdr obj) level (+ len 1))]
                   [(vector? obj)
                    (do-ec (: i (min (vector-length obj) *rp-length*))
                           (rec (vector-ref obj i) (+ level 1) 0))])]))))

(define (shared-obj? obj) (number? (hash-table-get (rp-dict) obj #f)))

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

;; kick-layouter :: (Layouter, Integer) -> Formatted
(define (kick-layouter layouter) (values-ref (layouter *rp-width*) 0))

;; layout :: (Obj, Integer) -> State Layouter
(define (layout obj level)
  (do* ([seen <- (get-seen)])
       (cond [(and (shared-obj? obj) (memq obj seen)) (return (layout-ref obj))]
             [(simple-obj? obj)
              (return (layout-simple (write-to-string obj (rp-writer))))]
             [(> level *rp-level*) (return (layout-simple "#"))]
             [else (layout-misc obj (cute layout <> (+ level 1)))])))

;; layout-misc :: (Obj, (Obj -> State Layouter)) -> State Layouter
(define (layout-misc obj rec)

  ;; mapi :: (Obj -> State Layouter), Vector -> State Layouter
  (define (mapi fn vec)
    (let1 s (size-of vec)
      (do* ([rs <- (mapM (lambda (i) (fn (vector-ref vec i)))
                         (iota (min s *rp-length*)))])
           (return (if (< s *rp-length*) rs `(,@rs ,dots))))))

  ;; map+ :: (Obj -> State Layouter), List -> State Layouter
  ;; map considering dotted list, *rp-length*, and shared structure
  (define (map+ fn lis)
    (let loop ([lis lis] [len 0] [rs '()])
      (do* ([seen <- (get-seen)])
           (match lis
             [() (return (reverse rs))]
             [(l . lis)
              (if (>= len *rp-length*)
                (return (reverse `(,dots ,@rs)))
                (do* ([r <- (fn l)])
                     (if (and (shared-obj? lis) (memq obj seen))
                       (return (reverse `(,(layout-ref lis) ,dot ,r ,@rs)))
                       (loop lis (+ len 1) (cons r rs)))))]
             [x (do* ([r <- (fn x)]) (return (reverse `(,r ,dot ,@rs))))]))))

  ;;: do-list :: (State String, State [Layouter]) -> State Layouter
  (define (do-list pref mapper)
    (do* ([str  <- pref] [elts <- mapper]) (layout-list str elts)))

  (cond [(pair? obj) (do-list (sprefix obj "(") (map+ rec obj))]
        [(vector? obj) (do-list (sprefix obj "#(") (mapi rec obj))]
        [(is-a? obj <uvector>)
         (let1 tag (rxmatch-substring
                     (#/[suf]\d+/ (x->string (class-name (class-of obj)))))
           (do-list (sprefix obj (format "#~a(" tag)) (mapi rec obj)))]
        [else
         (do* ([str <- (sprefix obj (write-to-string obj (rp-writer)))])
              (return (layout-simple str)))]))

;; :: Layouter
(define dots (lambda (w) (values "...." 4)))
(define dot  (lambda (w) (values "." 1)))

;; layout-simple :: String -> Layouter
(define (layout-simple str) (lambda (room) (values str (string-length str))))

;; layout-ref :: Object -> Layouter
(define (layout-ref obj)
  (layout-simple (format "#~d#" (hash-table-get (rp-dict) obj))))

;; layout-list :: String, [Layouter] -> State Layouter
(define (layout-list prefix elts)
  (let1 plen (string-length prefix)
    (return (lambda (room)
              (receive (s w) (do-layout-elements (- room plen) elts)
                (values (cons prefix (reverse s)) (and w (+ w plen))))))))

;; sprefix :: (Object, String) -> State String
(define (sprefix obj s)
  (cond [(hash-table-get (rp-dict) obj #f) number?
         => (lambda (cnt)
              (do* ([seen <- (get-seen)]
                    [ (put-seen (cons obj seen)) ])
                   (return (format "#~d=~a" cnt s))))]
        [else (return s)]))

;; do-layout-elements :: Integer, [Layouter] -> (Formatted, Integer)
(define (do-layout-elements room elts)
  (define (do-oneline r es strs)
    (match es
      [() (values strs (- room r))]
      [(e . es) (receive (s w) (e room)
                  (cond [(not w) (do-linear room elts)] ;giveup
                        [(> w room)                            ;too big
                         (do-fill room es (list* 'b s 'b strs))]
                        [(> w (- r 1))
                         (do-fill (- room w 1) es (list* s 'b strs))]
                        [else
                         (do-oneline (- r w 1) es (list* s 's strs))]))]))
  (define (do-fill r es strs)
    (match es
      [() (values strs #f)]
      [(e . es) (receive (s w) (e room)
                  (cond [(not w) (do-linear room elts)]
                        [(> w (- r 1))
                         (do-fill (- room w 1) es (list* s 'b strs))]
                        [else (do-fill (- r w 1) es (list* s 's strs))]))]))
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
(define (pprint obj)
  (parameterize ([rp-dict (scan-shared obj 0 0)])
    (let1 layouter (run (layout obj 0))
      (render (kick-layouter layouter) 0))))

