;;
;; test for charconv
;;

(use gauche.test)

(load "charconv")

(test-start "charconv")

(define (file->string file)
  (string-complete->incomplete
   (call-with-input-file file port->byte-string)))

(define (file->string-conv file from . to)
  (string-complete->incomplete
   (call-with-input-file file
     (lambda (f)
       (port->byte-string  (apply open-input-conversion-port f from to))))))

(define (map-test tester file from-codes to-codes)
  (for-each (lambda (from)
              (for-each (lambda (to) (tester file from to)) to-codes))
            from-codes))

;;--------------------------------------------------------------------
(test-section "input conversion")

(define (test-input file from to)
  (let ((infostr  (format #f "~a ~a => ~a" file from to))
        (fromfile (format #f "~a.~a" file from))
        (tofile   (format #f "~a.~a" file to)))
    (if (ces-conversion-supported? from to)
        (if (supported-character-encoding? to)
            (test infostr
                  (file->string tofile)
                  (lambda () (file->string-conv fromfile from)))
            (test infostr
                  (file->string tofile)
                  (lambda () (file->string-conv fromfile from to))))
        (test infostr "(not supported)"
              (lambda () "(not supported)")))
    ))

(map-test test-input "data/jp1"
          '("EUCJP" "UTF8" "SJIS" "CSISO2022JP")
          '("EUCJP" "UTF8" "SJIS" "CSISO2022JP"))
(map-test test-input "data/jp2"
          '("EUCJP" "UTF8" "SJIS" "CSISO2022JP")
          '("EUCJP" "UTF8" "SJIS" "CSISO2022JP"))
(map-test test-input "data/kr1"
          '("EUCKR" "UTF8")
          '("EUCKR" "UTF8" "ZIRKYU"))


;; WRITEME

(test-end)
