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
;;;  $Id: srfi-13.scm,v 1.1 2001-04-23 09:35:55 shiro Exp $
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
;;   string-take string-drop string-take-right string-drop-right
;;   string-set! string-fill!
;;   string-append

(define-module srfi-13
  (export string-null? string-every string-any
          string-tabulate reverse-list->string
          substring/shared string-copy!
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


(provide "srfi-13")

          
