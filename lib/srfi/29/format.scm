;; srfi.29.format - SRFI-29 compatible 'format'.

;; Taken from SRFI-29 reference implementation.
;;
;; Copyright (C) Scott G. Miller (2002). All Rights Reserved.
;;
;; This document and translations of it may be copied and furnished to
;; others, and derivative works that comment on or otherwise explain it
;; or assist in its implementation may be prepared, copied, published and
;; distributed, in whole or in part, without restriction of any kind,
;  provided that the above copyright notice and this paragraph are
;; included on all such copies and derivative works. However, this
;; document itself may not be modified in any way, such as by removing
;; the copyright notice or references to the Scheme Request For
;; Implementation process or editors, except as needed for the purpose
;; of developing SRFIs in which case the procedures for copyrights
;; defined in the SRFI process must be followed, or as required to
;; translate it into languages other than English.
;;
;; The limited permissions granted above are perpetual and will not be
;; revoked by the authors or their successors or assigns.
;;
;; This document and the information contained herein is provided on an
;; "AS IS" basis and THE AUTHOR AND THE SRFI EDITORS DISCLAIM ALL
;; WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO ANY
;; WARRANTY THAT THE USE OF THE INFORMATION HEREIN WILL NOT INFRINGE
;; ANY RIGHTS OR ANY IMPLIED WARRANTIES OF MERCHANTABILITY OR FITNESS
;; FOR A PARTICULAR PURPOSE

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
