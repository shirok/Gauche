;;;
;;; srfi-13.scm - string library
;;;
;;;  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: srfi-13.scm,v 1.7 2001-06-29 20:32:47 shirok Exp $
;;;

;; Srfi-13 is a large set of functions, so I splitted it up to a number of
;; subfiles which will be autoloaded on demand.
;;
;; This implementation tries to use Gauche native functions as much as
;; possible.  Because of the way Gauche handles multibyte string,
;; traditional portable approach can be very inefficient (such like
;; pre-allocating a string by make-string and fill it by string-set!).
;;
;; Natively implemented functions:
;;   string? make-string string string->list list->string
;;   string-join string-length string-ref string-copy
;;   string-set! string-fill!
;;   string-append string-contains

(define-module srfi-13
  (use gauche.let-opt)
  (export string-null? string-every string-any
          string-tabulate reverse-list->string
          substring/shared string-copy!
          string-take string-take-right
          string-drop string-drop-right
          string-pad string-pad-right
          string-trim string-trim-right string-trim-both
          string-compare string-compare-ci
          string= string<> string< string> string<= string>=
          string-ci= string-ci<> string-ci< string-ci> stirng-ci<= string-ci>=
          string-hash string-hash-ci
          string-prefix-length string-suffix-length
          string-prefix-length-ci string-suffix-length-ci
          string-prefix? string-suffix?
          string-prefix-ci? string-suffix-ci?
          string-index string-index-right
          string-skip string-skip-right
          string-count string-contains string-contains-ci
          string-titlecase string-titlecase!
          string-upcase string-upcase!
          string-downcase string-downcase!
          string-reverse string-reverse!
          string-concatenate string-append/shared string-concatenate/shared
          string-concatenate-reverse string-concatenate-reverse/shared
          string-map string-map!
          string-fold string-fold-right
          string-unfold string-unfold-right
          string-for-each string-for-each-index
          xsubstring string-xcopy!
          string-replace string-tokenize
          string-filter string-delete
          string-parse-start+end string-parse-final-start+end
          let-string-start+end
          check-substring-spec substring-spec-ok?
          make-kmp-restart-vector
          kmp-step
          string-kmp-partial-search
          ))
(select-module srfi-13)

;; utility macro to handle char/char-set/pred match.
(define-syntax %get-char-pred
  (syntax-rules ()
    ((_ char/char-set/pred)
     (cond ((char? char/char-set/pred)
            (lambda (x) (char=? char/char-set/pred x)))
           ((char-set? char/char-set/pred)
            (lambda (x) (char-set-contains? char/char-set/pred x)))
           ((procedure? char/char-set/pred) char/char-set/pred)
           (else (error "argument needs to be either a character, a char-set, or a procedure: %S" char/char-set/pred))))
    ))

(autoload "srfi-13/pred"      string-null? string-every string-any)
(autoload "srfi-13/generator" string-tabulate reverse-list->string)
(autoload "srfi-13/selector"  substring/shared string-copy!
                              string-pad string-pad-right
                              string-take string-take-right
                              string-drop string-drop-right
                              string-trim string-trim-right string-trim-both)
(autoload "srfi-13/comparer"  string-compare string-compare-ci
                              string= string<> string<
                              string<= string> string>=
                              string-ci= string-ci<> string-ci<
                              stirng-ci<= string-ci> string-ci>=)
(autoload "srfi-13/hash"      string-hash string-hash-ci)
(autoload "srfi-13/prefix"    string-prefix-length string-suffix-length
                              string-prefix-length-ci string-suffix-length-ci
                              string-prefix? string-suffix?
                              string-prefix-ci? string-suffix-ci?)
(autoload "srfi-13/searcher"  string-index string-index-right
                              string-skip string-skip-right
                              string-count
                              string-contains string-contains-ci)
(autoload "srfi-13/casemap"   string-titlecase string-titlecase!
                              string-upcase string-upcase!
                              string-downcase string-downcase!)
(autoload "srfi-13/revapp"    string-reverse string-reverse!
                              string-concatenate string-concatenate/shared
                              string-append/shared
                              string-concatenate-reverse
                              string-concatenate-reverse/shared)
(autoload "srfi-13/folder"    string-map string-map!
                              string-fold string-fold-right
                              string-unfold string-unfold-right
                              string-for-each string-for-each-index)
(autoload "srfi-13/rotator"   xsubstring string-xcopy!)
(autoload "srfi-13/misc"      string-replace string-tokenize)
(autoload "srfi-13/filter"    string-filter string-delete)
(autoload "srfi-13/internal"  string-parse-start+end
                              string-parse-final-start+end
                              let-string-start+end
                              check-substring-spec
                              substring-spec-ok?)
(autoload "srfi-13/kmp"       make-kmp-restart-vector
                              kmp-step
                              string-kmp-partial-search)

(provide "srfi-13")

          
