;;;
;;; srfi-13.scm - string library
;;;  
;;;   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
;;;  $Id: srfi-13.scm,v 1.11 2003-07-05 03:29:11 shirok Exp $
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
  (export string-null? string-every string-any
          string-tabulate reverse-list->string
          substring/shared string-copy!
          string-take string-take-right
          string-drop string-drop-right
          string-pad string-pad-right
          string-trim string-trim-right string-trim-both
          string-compare string-compare-ci
          string= string<> string< string> string<= string>=
          string-ci= string-ci<> string-ci< string-ci> string-ci<= string-ci>=
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
           (else (error "argument needs to be either a character, a char-set, or a procedure:" char/char-set/pred))))
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
                              string-ci<= string-ci> string-ci>=)
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

          
