;;
;; test for charconv
;;

(use gauche.test)

(load "charconv")
(import gauche.charconv)

(test-start "charconv")

(define (file->string file)
  (string-complete->incomplete
   (call-with-input-file file port->byte-string)))

(define (file->string-conv/in file from . to)
  (string-complete->incomplete
   (call-with-input-file file
     (lambda (f)
       (port->byte-string  (apply open-input-conversion-port f from to))))))

(define (file->string-conv/out file to from reader writer)
  (string-complete->incomplete
   (call-with-output-string
     (lambda (out)
       (call-with-input-file file
         (lambda (in)
           (let ((cv (open-output-conversion-port out to :from-code from)))
             (let loop ((data (reader in)))
               (if (eof-object? data)
                   (close-output-port cv)
                   (begin (writer data cv) (loop (reader in))))))))))))

(define (map-test tester file from-codes to-codes)
  (for-each (lambda (from)
              (for-each (lambda (to) (tester file from to)) to-codes))
            from-codes))

;;--------------------------------------------------------------------
(test-section "input conversion")

(define (test-input file from to . guesser)
  (let* ((realfrom (if (null? guesser) from (car guesser)))
         (infostr  (format #f "~a.~a (~a) => ~a" file from realfrom to))
         (fromfile (format #f "~a.~a" file from))
         (tofile   (format #f "~a.~a" file to)))
    (if (ces-conversion-supported? from to)
        (if (supported-character-encoding? to)
            (test infostr
                  (file->string tofile)
                  (lambda () (file->string-conv/in fromfile realfrom)))
            (test infostr
                  (file->string tofile)
                  (lambda () (file->string-conv/in fromfile realfrom
                                                   :to-code to))))
        (test infostr "(not supported)"
              (lambda () "(not supported)")))
    ))

(map-test test-input "data/jp1"
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP")
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP"))
(map-test test-input "data/jp2"
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP")
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP"))
(map-test test-input "data/jp3"
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP")
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP"))
(map-test test-input "data/kr1"
          '("EUCKR" "UTF-8" "ISO2022KR")
          '("EUCKR" "UTF-8" "ISO2022KR"))

;; autodetect tester
(map-test (lambda (file from to)
            (test-input file from to "*JP"))
          "data/jp1"
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP")
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP"))
(map-test (lambda (file from to)
            (test-input file from to "*JP"))
          "data/jp2"
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP")
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP"))
(map-test (lambda (file from to)
            (test-input file from to "*JP"))
          "data/jp3"
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP")
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP"))

;;--------------------------------------------------------------------
(test-section "output conversion")

(define (test-output type reader writer file from to)
  (let ((infostr  (format #f "~a ~a => ~a (~a)" file from to type))
        (fromfile (format #f "~a.~a" file from))
        (tofile   (format #f "~a.~a" file to)))
    (if (ces-conversion-supported? from to)
        (test infostr
              (file->string tofile)
              (lambda ()
                (file->string-conv/out fromfile to from reader writer)))
        (test infostr "(not supported)"
              (lambda () "(not supported)")))
    ))

(define (test-output/byte file from to)
  (test-output "byte" read-byte write-byte file from to))
        
(define (test-output/char file from to)
  (test-output "char" read-char write-char file from to))
        
(define (test-output/chunk256 file from to)
  (test-output "chunk256" (lambda (p) (read-block 256 p)) display file from to))
        
(define (test-output/chunk20 file from to)
  (test-output "chunk20" (lambda (p) (read-block 20 p)) display file from to))

(define internal-enc
  (case (gauche-character-encoding)
    ((euc-jp) '("EUCJP"))
    ((sjis)   '("SJIS"))
    ((utf-8)  '("UTF-8"))
    (else '())))

(map-test test-output/byte "data/jp1"
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP")
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP"))
(map-test test-output/chunk256 "data/jp1"
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP")
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP"))
(map-test test-output/chunk20 "data/jp1"
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP")
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP"))
(map-test test-output/char "data/jp1"
          internal-enc
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP"))

(map-test test-output/byte "data/jp2"
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP")
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP"))
(map-test test-output/chunk256 "data/jp2"
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP")
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP"))
(map-test test-output/chunk20 "data/jp2"
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP")
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP"))
(map-test test-output/char "data/jp2"
          internal-enc
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP"))

(map-test test-output/byte "data/jp3"
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP")
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP"))
(map-test test-output/chunk256 "data/jp3"
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP")
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP"))
(map-test test-output/chunk20 "data/jp3"
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP")
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP"))
(map-test test-output/char "data/jp3"
          internal-enc
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP"))

(map-test test-output/byte "data/kr1"
          '("EUCKR" "UTF-8" "ISO2022KR")
          '("EUCKR" "UTF-8" "ISO2022KR"))
(map-test test-output/chunk256 "data/kr1"
          '("EUCKR" "UTF-8" "ISO2022KR")
          '("EUCKR" "UTF-8" "ISO2022KR"))
(map-test test-output/chunk20 "data/kr1"
          '("EUCKR" "UTF-8" "ISO2022KR")
          '("EUCKR" "UTF-8" "ISO2022KR"))

;;--------------------------------------------------------------------
(test-section "code guessing")

(define (test-guess file code scheme)
  (let ((infostr (format #f "guess ~a from ~a.~a" scheme file code))
        (infile  (format #f "~a.~a" file code)))
    (test infostr code
          (lambda ()
            (ces-guess-from-string (file->string infile) scheme)))))

(map-test test-guess "data/jp1"
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP")
          '("*JP"))
(map-test test-guess "data/jp2"
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP")
          '("*JP"))
(map-test test-guess "data/jp3"
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP")
          '("*JP"))

;;--------------------------------------------------------------------
(test-section "string conversion")

(define (test-string file from to)
  (let ((infostr (format #f "string(~a) ~a => ~a" file from to))
        (instr   (file->string (format #f "~a.~a" file from)))
        (outstr  (file->string (format #f "~a.~a" file to))))
    (if (ces-conversion-supported? from to)
        (test infostr
              outstr
              (lambda ()
                (string-complete->incomplete
                 (ces-convert instr from to))))
        (test infostr "(not supported)"
              (lambda () "(not supported)")))
    ))

(map-test test-string "data/jp1"
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP")
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP"))
(map-test test-string "data/jp2"
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP")
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP"))
(map-test test-string "data/jp3"
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP")
          '("EUCJP" "UTF-8" "SJIS" "ISO2022JP"))


(test-end)
