;;
;; Convert plain text COPYING into rtf format, for Wix needs it.
;;

(use file.util)
(use file.filter)
(use util.match)
(use srfi-13)

(define (main args)
  (match args
    [(_ in out) (file-filter doit :input in :output out)]
    [_ (exit 1 "Usage: ~a <input-file> <output-file>" (car args))])
  0)

(define (doit in out)
  (display "{\\rtf1\\ansi\\ansicpg1252\\deff0{\\fonttbl{\\f0\\fnil\\fcharset0 Tahoma;}}\r\n" out)
  (display "\\viewkind4\\uc1\\pard\\lang1033\\f0\\fs20 " out)
  (let loop ([prev-line (read-line in)])
    (unless (eof-object? prev-line)
      (let1 line (read-line in)
        (cond [(and (string? line) (#/^-------------------/ line))
               (format out "\\ul ~a\\ulnone\\par\r\n" prev-line)
               (loop (read-line in))]
              [(#/-------/ prev-line)
               (format out "\\ul ~a\\ulnone\\par\r\n"
                       (string-trim-right prev-line #[-]))
               (loop line)]
              [else
               (format out "~a\\par\r\n" prev-line)
               (loop line)]))))
  (display "}" out))
