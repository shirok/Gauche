;;;
;;; SRFI-238 - Codesets
;;;
;;;   Copyright (c) 2024  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi.238
  (use gauche.record)
  (use gauche.threads)
  (export codeset? codeset-symbols
          codeset-symbol codeset-number codeset-message)
  )
(select-module srfi.238)

;; Global mapping from name -> codeset
(define *codesets* (atom (make-hash-table 'eq?)))

(define (name->codeset name)
  (atomic *codesets* (cut hash-table-get <> name #f)))

(define (add-codeset! codeset)
  (atomic *codesets* (cut hash-table-put! <> (%codeset-name codeset) codeset)))

;; The record is internal.  We don't expose codeset object.
(define-record-type %codeset #t #t
  name
  symbols
  symbol->number
  number->symbol
  symbol->message
  number->message)

;; API
(define (codeset? name) (and (symbol? name) (boolean (name->codeset name))))

;; API
(define (codeset-symbols name)
  (assume-type name <symbol>)
  (if-let1 cs (name->codeset name)
    (%codeset-symbols cs)
    '()))

;; API
(define (codeset-symbol name code)
  (assume-type name <symbol>)
  (and-let1 cs (name->codeset name)
    (cond [(symbol? code) (and ((%codeset-symbol->number cs) code) code)]
          [(integer? code) ((%codeset-number->symbol cs) code)]
          [else #f])))

;; API
(define (codeset-number name code)
  (assume-type name <symbol>)
  (and-let1 cs (name->codeset name)
    (cond [(symbol? code) ((%codeset-symbol->number cs) code)]
          [(integer? code) (and ((%codeset-number->symbol cs) code) code)]
          [else #f])))

;; API
(define (codeset-message name code)
  (assume-type name <symbol>)
  (and-let1 cs (name->codeset name)
    (cond [(symbol? code) ((%codeset-symbol->message cs) code)]
          [(integer? code) ((%codeset-number->message cs) code)]
          [else #f])))

;;;
;;;  Actual codesets
;;;
(define (make-errno-codeset)
  (let* ([errno-alist (filter-map (^n (and-let1 s (sys-errno->symbol n)
                                        (cons s n)))
                                  (iota 500))]
         [sym->num (alist->hash-table errno-alist)]
         [num->sym (alist->hash-table (map (^p (cons (cdr p) (car p)))
                                           errno-alist)
                                      eqv-comparator)])
    (make-%codeset
     'errno
     (map car errno-alist)               ;symbols
     (cut hash-table-get sym->num <> #f) ;symbol->number
     (cut hash-table-get num->sym <> #f) ;number->symbol
     (^s (and-let1 n (hash-table-get sym->num s #f) ;symbol->message
           (sys-strerror n)))
     (^n (sys-strerror n)))))           ;number->message

(define (make-signal-codeset)
  (let* ([sig-alist (filter-map (^n (and-let1 s (sys-signal-name n)
                                      (cons (string->symbol s) n)))
                                (iota 200))]
         [sym->num (alist->hash-table sig-alist)]
         [num->sym (alist->hash-table (map (^p (cons (cdr p) (car p)))
                                           sig-alist)
                                      eqv-comparator)])
    (make-%codeset
     'signal
     (map car sig-alist)                 ;symbols
     (cut hash-table-get sym->num <> #f) ;symbol->number
     (cut hash-table-get num->sym <> #f) ;number->symbol
     (^s (and-let1 n (hash-table-get sym->num s #f) ;symbol->message
           (sys-strsignal n)))
     (^n (sys-strsignal n)))))          ;number->message

;; Register to global index
(add-codeset! (make-errno-codeset))
(add-codeset! (make-signal-codeset))
