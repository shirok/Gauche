#!/usr/bin/env gosh

;;; Take diff of original axTLS source tree and the adapted version here.
;;; It is to make it easy to upgrade axTLS to the newer version

(use file.util)
(use gauche.process)
(use util.match)
(use srfi-1)

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
                  (^[p s] (if (#/(\.[ch]$)|(^Makefile$)/ p) (cons p s) s)) xs))

(define (do-diff file dir)
  (let1 orig (build-path dir (regexp-replace #/^axTLS\// file ""))
    (run-process `(diff -c -N ,orig ,file) :wait #t)))
 

  
