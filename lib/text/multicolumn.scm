;;;
;;; text.multicolumn - Show text in multicolumn format
;;;
;;;   Copyright (c) 2022-2025  Shiro Kawai  <shiro@acm.org>
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

(define-module text.multicolumn
  (use util.match)
  (use text.tree)
  (use scheme.division)
  (export display-multicolumn layout-multicolumn))
(select-module text.multicolumn)

;;
;; Show a list of words in multicolumn format (imagine output of 'ls').
;;

(define (display-multicolumn strs
                             :key (width 80) ; total width
                                  (minimum-width 8) ; minimum column width
                                  (max-columns 4)
                                  (order 'column)
                                  (indent 0)
                                  (style 'even))
  (define indent-string (make-string indent #\space))
  (dolist [line (layout-multicolumn strs
                                    :width (- width indent)
                                    :minimum-width minimum-width
                                    :max-columns max-columns :order order
                                    :style style)]
    (display indent-string)
    (for-each write-tree line)
    (newline)))

;; underlying utility of display-multicolumn.
;; Returns a list of string-list; each string-list is for a line.
(define (layout-multicolumn strs
                            :key (width 80)
                                 (minimum-width 8)
                                 (max-columns 4)
                                 (order 'column)
                                 (style 'even))
  (define num-items (length strs))
  [define max-col-width
    (max (+ 2 (apply max 0 (map string-length strs)))
         minimum-width)]
  (define (make-tr-matrix rows)         ; return transposed matrix
    (case order
      [(row) ($ map list
                   $* slices strs (ceiling->exact (/ num-items rows)) #t "")]
      [(column) (slices strs rows #t "")]))
  (define (column-widths tr-matrix)
    (map (^[ss] (apply max minimum-width (map (^s (+ 2 (string-length s))) ss)))
         tr-matrix))

  ;; Packers - Returns tr-matrix and column-widths
  ;;
  ;; even-packer - Use uniform column width
  (define (even-packer rows)
    (let1 trm (make-tr-matrix rows)
      (values trm
              (map (constantly max-col-width) (iota (length (car trm)))))))
  ;; dense-packer - Try more rows if possible.
  (define (dense-packer rows)
    (let try ([rows rows])
      (let* ([tr-matrix (make-tr-matrix rows)]
             [col-widths (column-widths tr-matrix)]
             [cols (length col-widths)]
             [total-width (apply + col-widths)])
        (cond [(> total-width width) (values #f #f)]
              [(and (< cols max-columns)
                    (<= total-width width)
                    (> rows 1))
               (receive (trm cws) (try (- rows 1))
                 (if trm
                   (values trm cws)
                   (values tr-matrix col-widths)))]
              [else (values tr-matrix col-widths)]))))

  (define (pad-items col-widths row)
    (let rec ([row row]
              [widths col-widths])
      ;; We avoid trailing spaces
      (match row
        [(w) (if (equal? w "") '() (list w))]
        [(w . ws) (let1 s (rec ws (cdr widths))
                    (if (null? s)
                      (if (equal? w "") '() (list w))
                      (cons (format "~va" (car widths) w) s)))])))

  (define (transpose tr-matrix)
    (apply map list tr-matrix))

  (if (null? strs)
    '()
    (let* ([min-cols (clamp (floor-quotient width max-col-width)
                            1 max-columns)]
           [max-rows (quotient (+ num-items min-cols -1) min-cols)]
           [widths (map (^_ max-col-width) (iota min-cols))])
      (receive (tr-matrix col-widths)
          (ecase style
            [(even) (even-packer max-rows)]
            [(packed) (dense-packer max-rows)])
        ($ map (cut pad-items col-widths <>) $ transpose tr-matrix)))))
