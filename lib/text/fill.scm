;;;
;;; text.fill - Fill paragraph
;;;
;;;   Copyright (c) 2024  Shiro Kawai  <shiro@acm.org>
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

(define-module text.fill
  (use gauche.unicode)
  (use srfi.13)
  (use text.tree)
  (use util.match)
  (export display-filled-text
          text->filled-stree)
  )
(select-module text.fill)

#|
  width
<------------------------------------------------------------------->

  hanging
<------>
         Lorem ipsum dolor sit amet, consectetur adipiscing elit,
   sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
   Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
   nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor
   in reprehenderit in voluptate velit esse cillum dolore eu fugiat
   nulla pariatur. Excepteur sint occaecat cupidatat non proident,
   sunt in culpa qui officia deserunt mollit anim id est laborum.
<->
  indent

NB: The first line does not consider 'indent' argument, but
it does consider the starting column if it's available.  So even
if you start printing a paragraph in the middle of the line, the text
still fits within the width.


  width
<--------------------------------------------------------------------->
             start column
                |   hanging
                v<---->
                        Lorem ipsum dolor sit amet, consectetur
          adipiscing elit, sed do eiusmod tempor incididunt ut labore
          et dolore magna aliqua.  Ut enim ad minim veniam, quis
          nostrud exercitation ullamco laboris nisi ut aliquip ex ea
          commodo consequat. Duis aute irure dolor in reprehenderit in
          voluptate velit esse cillum dolore eu fugiat nulla pariatur.
          Excepteur sint occaecat cupidatat non proident, sunt in culpa
          qui officia deserunt mollit anim id est laborum.
<-------->
   indent

The 'lead-in' is a leading text before paragraph.  If it is shorter than
(- hanging 1), it becomes run-in heading.  Otherwise, the paragraph begins
from the next line.  Compare the following two examples:


     hanging
<---------------->
THIS IS LEAD-IN.  Lorem ipsum dolor sit amet, consectetur
             adipiscing elit, sed do eiusmod tempor incididunt ut labore
             et dolore magna aliqua.
<----------->
    indent


     hanging
<---------------->
THIS IS LEAD-IN LONGER THAN HANGING.
                  Lorem ipsum dolor sit amet, consectetur
             adipiscing elit, sed do eiusmod tempor incididunt ut labore
             et dolore magna aliqua.
<----------->
    indent
|#

;; If the input text has double newlines, it marks a paragraph break.
;; The next line starts with hanging indent again.

;; TODO:
;;  - Customize east-asian-width

(define *default-width* 65)
(define *default-indent* 0)
(define *default-hanging* 0)

(define (display-filled-text text :key (port (current-output-port))
                             (indent #f) (hanging #f) (width #f)
                             (lead-in #f) (start-column #f))
  ($ write-tree
     ($ text->filled-stree text
        :start-column (or start-column (~ port'current-column))
        :indent indent
        :hanging hanging
        :lead-in lead-in
        :width width)
     port)
  (undefined))

(define (text->filled-stree text :key (indent #f) (hanging #f) (width #f)
                            (start-column 0) (lead-in #f))
  (let ([indent (or indent *default-indent*)]
        [hanging (or hanging indent *default-hanging*)]
        [width (or width *default-width*)])
    (assume (and (exact-integer? start-column) (>= start-column 0)))
    (assume (and (exact-integer? indent) (>= indent 0)))
    (assume (and (exact-integer? hanging) (>= hanging 0)))
    (assume (and (exact-integer? width) (> width indent)))
    (assume-type lead-in (<?> <string>))

    (let loop ([par (segment-paragraph text)]
               [start-column start-column]
               [lead-in lead-in]
               [r '()])
      (match par
        [()
         (if (and (null? r) lead-in)
           (list lead-in)               ;edge case - empty text
           (intersperse "\n" (reverse r)))]
        [(x . xs)
         (loop xs 0 #f
               (cons (fill-paragraph x indent hanging width start-column lead-in)
                     r))]))))

(define (fill-paragraph text indent hanging width start-column lead-in)
  ;; NB: This algorithm is similar to pretty printer, and we may integrate
  ;; the two in future.
  (let ([indenter (string-append "\n" (make-string indent #\space))]
        [hanging-indenter (if lead-in
                            (if (< (string-east-asian-width lead-in) hanging)
                              (format "~va" hanging lead-in)
                              (format "~a\n~va" lead-in hanging ""))
                            (make-string hanging #\space))])
    (let loop ([words (segment-text text)]
               [column (+ hanging start-column)]
               [r (list hanging-indenter)])
      (match words
        [() (reverse r)]
        [('s word . rest)
         (let1 w (string-east-asian-width word)
           (if (<= (+ column w 1) width)
             (loop rest (+ column w 1) (list* word " " r))
             (loop rest (+ indent w) (list* word indenter r))))]
        [(word . rest)
         (let1 w (string-east-asian-width word)
           (if (or (<= (+ column w) width)
                   (length=? r 1))      ;at the very beginning
             (loop rest (+ column w) (cons word r))
             (loop rest (+ indent w) (list* word indenter r))))]
        ))))

(define (segment-paragraph text)
  (map string-trim-both (string-split text "\n\n")))

;; Split text into unbreakable chunks.  We should also consider
;; hyphenations, but that's for future versions.
;;
;; Symbol 's' is inserted where we need to emit #\space if it's in
;; mid-line, but not before/after break.
(define (segment-text text)
  (define (spaces? x) (#/^\s+$/ x))
  (define (cat s) (and (= (string-length s) 1)
                       (char-general-category (string-ref s 0))))
  (define (Po? s) (eq? (cat s) 'Po))
  (define (Pe? s) (eq? (cat s) 'Pe))
  (let loop ([words (string->words text)]
             [r '()])
    (match words
      [() (reverse r)]
      [((? spaces? x)) (reverse r)]
      [(x) (loop '() (cons x r))]
      [((? spaces? x) . rest)
       (match r
         [('s . _) (loop rest r)]       ;merge consecutive 's's
         [_        (loop rest (cons 's r))])]
      [(x y . rest) (if (or (Po? y) (Pe? y))
                      (loop (cons (string-append x y) rest) r)
                      (loop (cons y rest) (cons x r)))]
      )))
