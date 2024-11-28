;; srfi.29.format - SRFI-29 compatible 'format'.

;; Taken from SRFI-29 reference implementation.
;;
;; (c) 2002 Scott G. Miller.
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice (including the
;; next paragraph) shall be included in all copies or substantial
;; portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(define-module srfi.29.format
  (export format))
(select-module srfi.29.format)

(define (format format-string . objects)
  (let ((buffer (open-output-string)))
    (let loop ((format-list (string->list format-string))
               (objects objects)
               (object-override #f))
      (cond ((null? format-list) (get-output-string buffer))
            ((char=? (car format-list) #\~)
             (cond ((null? (cdr format-list))
                    (error 'format "Incomplete escape sequence"))
                   ((char-numeric? (cadr format-list))
                    (let posloop ((fl (cddr format-list))
                                  (pos (string->number
                                        (string (cadr format-list)))))
                      (cond ((null? fl)
                             (error 'format "Incomplete escape sequence"))
                            ((and (eq? (car fl) '#\@)
                                  (null? (cdr fl)))
                             (error 'format "Incomplete escape sequence"))
                            ((and (eq? (car fl) '#\@)
                                  (eq? (cadr fl) '#\*))
                             (loop (cddr fl) objects (list-ref objects pos)))
                            (else
                             (posloop (cdr fl)
                                      (+ (* 10 pos)
                                         (string->number
                                          (string (car fl)))))))))
                   (else
                    (case (cadr format-list)
                      ((#\a)
                       (cond (object-override
                              (begin
                                (display object-override buffer)
                                (loop (cddr format-list) objects #f)))
                             ((null? objects)
                              (error 'format "No value for escape sequence"))
                             (else
                              (begin
                                (display (car objects) buffer)
                                (loop (cddr format-list)
                                      (cdr objects) #f)))))
                      ((#\s)
                       (cond (object-override
                              (begin
                                (display object-override buffer)
                                (loop (cddr format-list) objects #f)))
                             ((null? objects)
                              (error 'format "No value for escape sequence"))
                             (else
                              (begin
                                (write (car objects) buffer)
                                (loop (cddr format-list)
                                      (cdr objects) #f)))))
                      ((#\%)
                       (if object-override
                         (error 'format "Escape sequence following positional override does not require a value"))
                       (display #\newline buffer)
                       (loop (cddr format-list) objects #f))
                      ((#\~)
                       (if object-override
                         (error 'format "Escape sequence following positional override does not require a value"))
                       (display #\~ buffer)
                       (loop (cddr format-list) objects #f))
                      (else
                       (error 'format "Unrecognized escape sequence"))))))
            (else (display (car format-list) buffer)
                  (loop (cdr format-list) objects #f))))))
