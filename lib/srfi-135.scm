;;;
;;; srfi-135 Immutable Texts
;;;
;;;   Copyright (c) 2020-2022  Shiro Kawai  <shiro@acm.org>
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

;; In Gauche, O(1)-access immutable texts is just an immutable string with
;; index attached.

(define-module srfi-135
  (use gauche.unicode)
  (use gauche.lazy)
  (use srfi-152)
  (export text? textual? textual-null? textual-every textual-any
          make-text text text-tabulate text-unfold text-unfold-right
          textual->text textual->string textual->vector textual->list
          string->text vector->text list->text reverse-list->text
          textual->utf8 textual->utf16be textual->utf16le textual->utf16
          utf8->text utf16be->text utf16le->text utf16->text

          text-length textual-length text-ref textual-ref
          subtext subtextual textual-copy
          textual-take textual-take-right textual-drop textual-drop-right
          textual-pad textual-pad-right
          textual-trim textual-trim-right textual-trim-both

          textual-replace

          textual=? textual-ci=?
          textual<? textual-ci<? textual<=? textual-ci<=?
          textual>? textual-ci>? textual>=? textual-ci>=?

          textual-prefix-length textual-suffix-length
          textual-prefix? textual-suffix?

          textual-index textual-index-right
          textual-skip textual-skip-right
          textual-contains textual-contains-right

          textual-upcase textual-downcase
          textual-foldcase textual-titlecase

          textual-append textual-concatenate textual-concatenate-reverse
          textual-join

          textual-fold textual-fold-right
          textual-map textual-for-each
          textual-map-index textual-for-each-index
          textual-count textual-filter textual-remove

          textual-replicate textual-split))
(select-module srfi-135)

(define (text? obj)
  (and (string-immutable? obj) (string-fast-indexable? obj)))
(define textual? string?)
(define textual-null? string-null?)
(define textual-every string-every)
(define textual-any string-any)

(define-inline (%textize obj)
  (string-build-index! (string-copy-immutable obj)))
(define-inline (%stringify obj)
  (cond [(char? obj) (string obj)]
        [(string? obj) obj]
        [else (error "procedure expected to return a character, a string \
                      or a text, but returned:" obj)]))

(define (make-text len char)
  (%textize (make-string len char)))
(define (text . chars)
  (%textize (list->string chars)))
(define (text-tabulate proc len)
  (%textize (string-tabulate proc len)))
(define (text-unfold p f g seed :optional (base "") (make-final (^_ "")))
  (%textize
   (with-output-to-string
     (^[] (display base)
       (let loop ((seed seed))
         (if (p seed)
           (display (make-final seed))
           (begin (display (f seed))
                  (loop (g seed)))))))))
(define (text-unfold-right p f g seed :optional (base "") (make-final (^_ "")))
  (let loop ((seed seed) (r '()))
    (if (p seed)
      (%textize (string-append (%stringify (make-final seed))
                               (string-concatenate r)
                               (%stringify base)))
      (loop (g seed)
            (cons (%stringify (f seed)) r)))))

(define (textual->text textual)
  (assume (textual? textual))
  (%textize textual))
(define (textual->string textual . args)
  (assume (textual? textual))
  (apply string-copy textual args))
(define (textual->list textual . args)
  (assume (textual? textual))
  (apply string->list textual args))
(define (textual->vector textual . args)
  (assume (textual? textual))
  (apply string->vector textual args))

(define (string->text string . args)
  (%textize (apply opt-substring string args)))
(define (vector->text vec . args)
  (%textize (apply vector->string vec args)))
(define (list->text lis :optional start end)
  (let* ([lis (cond [(and (integer? start) (>= start 0)) (drop lis start)]
                    [(undefined? start) lis]
                    [else (error "start argument must be an integer, but got:"
                                 start)])]
         [lis (cond [(integer? end)
                     (if (>= end start)
                       (take lis (- end start))
                       (errorf "end argument ~s is less than start argument ~s"
                               end start))]
                    [(undefined? end) lis]
                    [else (error "end argument must be an integer, but got:"
                                 end)])])
    (%textize (list->string lis))))
(define (reverse-list->text lis . args)
  (%textize (apply reverse-list->string lis args)))

(define textual->utf8 string->utf8)
(define (textual->utf16 textual . args)
  (apply string->utf16 textual (native-endian) #t args))
(define (textual->utf16be textual . args)
  (apply string->utf16 textual 'big-endian #f args))
(define (textual->utf16le textual . args)
  (apply string->utf16 textual 'little-endian #f args))

(define (utf8->text bv . args)
  (%textize (apply utf8->string bv args)))
(define (utf16->text bv . args)
  (%textize (apply utf16->string bv (native-endian) #f args)))
(define (utf16be->text bv . args)
  (%textize (apply utf16->string bv 'big-endian #t args)))
(define (utf16le->text bv . args)
  (%textize (apply utf16->string bv 'little-endian #t args)))

(define (text-length text)
  (assume (text? text))
  (string-length text))
(define (text-ref text index)
  (assume (text? text))
  (string-ref text index))
(define textual-length string-length)
(define textual-ref string-ref)

(define (subtext text start end)
  (assume (text? text))
  (%textize (substring text start end)))
(define (subtextual textual start end)
  (%textize (substring textual start end)))
(define (textual-copy textual . args)
  ;; textual-copy should copy once.
  (%textize (apply string-copy textual args)))

(define (textual-take textual nchars)
  (%textize (string-take textual nchars)))
(define (textual-drop textual nchars)
  (%textize (string-drop textual nchars)))
(define (textual-take-right textual nchars)
  (%textize (string-take-right textual nchars)))
(define (textual-drop-right textual nchars)
  (%textize (string-drop-right textual nchars)))

(define (textual-pad textual len . args)
  (%textize (apply string-pad textual len args)))
(define (textual-pad-right textual len . args)
  (%textize (apply string-pad-right textual len args)))
(define (textual-trim textual . args)
  (%textize (apply string-trim textual args)))
(define (textual-trim-right textual . args)
  (%textize (apply string-trim-right textual args)))
(define (textual-trim-both textual . args)
  (%textize (apply string-trim-both textual args)))

(define (textual-replace t1 t2 start1 end1 . args)
  (%textize (apply string-replace t1 t2 start1 end1 args)))

(define textual=? string=?)
(define textual<? string<?)
(define textual<=? string<=?)
(define textual>? string>?)
(define textual>=? string>=?)

(define textual-ci=? string-ci=?)
(define textual-ci<? string-ci<?)
(define textual-ci<=? string-ci<=?)
(define textual-ci>? string-ci>?)
(define textual-ci>=? string-ci>=?)

(define textual-prefix-length string-prefix-length)
(define textual-suffix-length string-suffix-length)
(define textual-prefix? string-prefix?)
(define textual-suffix? string-suffix?)

(define textual-index string-index)
(define textual-index-right string-index-right)
(define textual-skip string-skip)
(define textual-skip-right string-skip-right)
(define textual-contains string-contains)
(define textual-contains-right string-contains-right)

(define (textual-upcase textual) (%textize (string-upcase textual)))
(define (textual-downcase textual) (%textize (string-downcase textual)))
(define (textual-foldcase textual) (%textize (string-foldcase textual)))
(define (textual-titlecase textual) (%textize (string-titlecase textual)))

(define (textual-append . args)
  (%textize (string-concatenate args)))
(define (textual-concatenate args)
  (%textize (string-concatenate args)))
(define (textual-concatenate-reverse args :optional (final-textual "") end)
  (textual-concatenate (reverse args
                                (list
                                 (if (undefined? end)
                                   final-textual
                                   (subtext final-textual 0 end))))))
(define (textual-join lis . args)
  (%textize (apply string-join lis args)))

(define textual-fold string-fold)
(define textual-fold-right string-fold-right)

(define (textual-map proc textual . rest)
  (assume (textual? textual))
  (assume (every textual? rest))
  (%textize
   (string-concatenate
    (if (null? rest)
      (map (^t (%stringify (proc t))) (x->lseq textual))
      (apply map (^ ts (%stringify (apply proc ts)))
             (x->lseq textual)
             (map x->lseq rest))))))
(define (textual-for-each proc textual . rest)
  (assume (textual? textual))
  (assume (every textual? rest))
  (apply string-for-each proc textual rest))

(define (textual-map-index proc textual :optional (start 0)
                                                  (end (textual-length textual)))
  (%textize
   (string-concatenate
    (map (^i (%stringify (proc i)))
         (lrange start end)))))
(define (textual-for-each-index proc textual :optional (start 0)
                                             (end (textual-length textual)))
  (for-each proc (lrange start end)))

(define textual-count string-count)
(define (textual-filter pred textual . args)
  (%textize (apply string-filter pred textual args)))
(define (textual-remove pred textual . args)
  (%textize (apply string-remove pred textual args)))

(define (textual-replicate textual from to . args)
  (%textize (apply string-replicate textual from to args)))
(define (textual-split textual delimiter . args)
  (map %textize (apply string-split textual delimiter args)))
