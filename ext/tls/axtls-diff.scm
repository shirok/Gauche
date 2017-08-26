#!/usr/bin/env gosh

;;; Take diff of original axTLS source tree and the adapted version here.
;;; It is to make it easy to upgrade axTLS to the newer version

(use file.util)
(use gauche.process)
(use util.match)
(use srfi-1)
(use text.tr)

(define (usage)
  (print "Usage: ../../src/gosh -ftest ./axtls-diff.scm <original-axTLS-source-directory>")
  (print "Run this script under ext/tls.")
  (exit 1))

(define (main args)
  (match (cdr args)
    [(dir) (=> fail)
     (unless (and (file-is-directory? dir)
                  (file-is-directory? (build-path dir "ssl")))
       (fail))
     (for-each (cut do-diff <> dir) (gather-files))]
    [_ (usage)])
  0)

(define (gather-files)
  ($ remove #/\.mod\.c$/
     ($ get-files "ssl" $ get-files "crypto" $ get-files "config" '())))

(define (get-files subdir xs)
  (directory-fold (build-path "axTLS" subdir)
                  (^[p s] (if (#/(\.[ch]$)|(\.sh$)|(^Makefile$)/ p) (cons p s) s)) xs))

(define (fix-path name)
  (cond-expand
    [gauche.os.windows (string-tr name "\\\\" "/")]
    [else              name]))

(define (do-diff file dir)
  (let* ([orig (build-path dir (regexp-replace #/^axTLS[\/\\]/ file ""))]
         [p (run-process
             (cond-expand
              ;; for MSYS (mintty)
              [gauche.os.windows `("cmd.exe" "/c" diff -u -N ,orig ,file)]
              [else              `(diff -u -N ,orig ,file)])
             :wait #f :output :pipe)]
         ;; We replace pathnames in the diff header to be friendly for patch.
         ;; Especially, newer version of GNU patch rejects patchfile that has
         ;; absolute pathname, or pathname includes '..'.
         ;; The original diff header:
         ;;   --- /path/to/original/axtls/subdir/foo.c    <timestamp>
         ;;   +++ axTLS/subdir/foo.c    <timestamp>
         ;; Replaced diff header:
         ;;   --- a/axTLS/subdir/foo.c    <timestamp>
         ;;   +++ b/axTLS/subdir/foo.c    <timestamp>
         ;; By doing this, the result patch can be applied by
         ;;  patch -p1 < axtls.diff
         [line1 (read-line (process-output p))]
         [line2 (read-line (process-output p))])
    (unless (eof-object? line1)
      (let ([m1 (#/^--- [^\s]+\s+(.*)/ line1)]
            [m2 (#/^\+\+\+ [^\s]+\s+(.*)/ line2)]
            [path (fix-path file)])
        (unless (and m1 m2)
          (exit 1 #"Diff output parse error.  Check the output of \
                    `diff -u -N ~orig ~file'."))
        (print #"--- a/~path\t~(m1 1)")
        (print #"+++ b/~path\t~(m2 1)")
        (copy-port (process-output p) (current-output-port))))
    (process-wait p)))
