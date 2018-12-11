;;;
;;; text.edn - read and write Clojure's Extensible Data Notation
;;;
;;;   Copyright (c) 2018  Shiro Kawai  <shiro@acm.org>
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

;; https://github.com/edn-format/edn

(define-module text.edn
  (use gauche.mop.singleton)
  (use gauche.record)
  (use gauche.lazy)
  (use gauche.generator)
  (use srfi-13)
  (use scheme.set)
  (use parser.peg)

  (export edn-equal? edn-comparator
          edn-nil edn-nil? edn-map edn-set
          edn-object edn-object? edn-object-tag edn-object-map
          edn-symbol-prefix edn-symbol-name

          parse-edn parse-edn* parse-edn-string
          construct-edn construct-edn-string))
(select-module text.edn)

;;;
;;; Comparison
;;;
(define (edn-equal? a b)
  (cond [(hash-table? a) (and (hash-table? b) (hash-table=? edn-comparator a b))]
        [(set? a) (and (set? b) (set=? a b))]
        [(edn-nil? a) (edn-nil? b)]
        [else (equal? a b)]))

(define edn-comparator 
  (make-comparator #t edn-equal? #f default-hash))

;;;
;;; EDN-specific types
;;;

;; nil is mapped to a singleton instance #<edn-nil>.
(define-class <edn-nil> ()
  ()
  :metaclass <singleton-meta>)
(define-method write-object ((obj <edn-nil>) port)
  (format port "#<edn-nil>"))

(define edn-nil (make <edn-nil>))
(define (edn-nil? obj) (eq? obj edn-nil))

;; EDN map is a hashtable with edn-comparator.
(define (edn-map . kv-list)
  (rlet1 h (make-hash-table edn-comparator)
    (doplist [[k v] kv-list]
      ;; todo: duplicate key check
      (hash-table-put! h k v))))

;; EDN set is a set with edn-comparator.
(define (edn-set . vals)
  (list->set edn-comparator vals))

;; EDN tagged object
(define-record-type edn-object #t #t
  tag
  map)

;; EDN symbol is mapped to a symbol; we just treat prefix as a part
;; of symbol names.  We do provide a utility to extract prefix
;; and real name.
(define (edn-symbol-prefix sym)
  (and-let1 p (string-scan (symbol->string sym) "/" 'before)
    (string->symbol p)))

(define (edn-symbol-name sym)
  (or (and-let1 p (string-scan (symbol->string sym) "/" 'after)
        (string->symbol p))
      sym))

;; EDN keywords are mapped to Gauche keywords.  EDN symbols never
;; begins with ':' so they won't conflict.
;;
;; EDN true and false map to #t and #f.
;;
;; EDN list and vector map to Gauche list and vector.

;;;
;;; Parser
;;;

(define (parse lseq)
  (define %term
    ($lazy ($seq ($skip-many %ws)
                 ($or %list %vec %map %set
                      %atom %tagged %str %char))))
  (define %ws
    ($or ($skip-many1 ($one-of #[\s,]))
         ($seq ($c #\;)
               ($skip-many ($one-of #[^\newline])))
         ($seq ($s "#_") %term)))
  (define %list
    ($between ($c #\() ($many %term) ($c #\))))
  (define %vec
    ($between ($c #\[) ($lift list->vector ($many %term)) ($c #\])))
  (define %set
    ($between ($s "#{") ($lift ($ apply edn-set $) ($many %term)) ($c #\})))
  (define %map
    ($between ($c #\{) ($lift ($ apply edn-map $) ($many %term)) ($c #\})))
  (define %word ; intermediate - used by %atom and %char
    ($->string ($many1 ($one-of #[\w*!?$%&=<>:#+.-]))))
  (define %atom
    ($do [val %word]
         (cond
          [(equal? val "true")  ($return #t)]
          [(equal? val "false") ($return #f)]
          [(equal? val "nil")   ($return edn-nil)]
          [(string-prefix? ":" val) ($return 
                                     (make-keyword (string-drop val 1)))]
          [(%parse-num val) => $return]
          [else ($return (string->symbol val))])))
  (define %tagged
    ($do [ ($c #\#) ]
         [tag %word]
         [content %map]
         ($return (make-edn-object tag map))))
  (define %str 
    ($between ($c #\")
              ($->string
               ($many ($or ($seq ($s "\\\"") ($return "\""))
                           ($seq ($s "\\t") ($return "\t"))
                           ($seq ($s "\\r") ($return "\r"))
                           ($seq ($s "\\n") ($return "\n"))
                           ($seq ($s "\\\\") ($return "\\"))
                           ($one-of #[^\\\"]))))
              ($c #\")))
  (define %char
    ($do [w ($seq ($c #\\) %word)] 
         (rxmatch-case w
           [#/^newline$/ (_) ($return #\newline)]
           [#/^return$/  (_) ($return #\return)]
           [#/^space$/   (_) ($return #\space)]
           [#/^tab$/     (_) ($return #\tab)]
           [#/^u([0-9a-fA-F]+)$/ (_ v)
                ($return (ucs->char (string->number v 16)))]
           [#/^.$/       (s) ($return (~ s 0))]
           [else ($fail (format "invalid char name: ~a" w))])))
  (peg-run-parser %term lseq))

(define (%parse-num word)
  (rxmatch-if (#/^([+-])?(\d+)(?:\.(\d+))?(?:[eE]([+-]?\d+))?([MN])?$/ word)
      [_ sign int frac exp sfx]
    (* (if (equal? sign "-") -1 1)
       (if (or frac exp (equal? sfx "M"))
         (string->number (format "~a.~aE~a" int (or frac "0") (or exp "0")))
         (string->number int)))
    #f))

(define (parse-edn :optional (iport (current-input-port)))
  (values-ref (parse (port->char-lseq iport)) 0))

(define (parse-edn* :optional (iport (current-input-port)))
  (let1 s (port->char-lseq iport)
    (generator->list (^[] (if (null? s)
                            (eof-object)
                            (receive (v s.) (parse s)
                              (set! s s.)
                              v))))))


(define (parse-edn-string str)
  (values-ref (parse (x->lseq str)) 0))

;;;
;;; Writer
;;;

(define-method edn-write ((x <boolean>)) (display (if x 'true 'false)))
(define-method edn-write ((x <edn-nil>)) (display 'nil))
(define-method edn-write ((x <string>)) (write x))
(define-method edn-write ((x <symbol>)) (write x))
(define-method edn-write ((x <keyword>)) (write x))
(define-method edn-write ((x <real>)) (write x))

(define-method edn-write ((x <char>))
  (cond
   [(char-set-contains? #[!-~] x) (format #t "\\~c" x)]
   [(eqv? x #\newline) (display "\\newline")]
   [(eqv? x #\return)  (display "\\return")]
   [(eqv? x #\space)   (display "\\space")]
   [(eqv? x #\tab)     (display "\\tab")]
   [else (format #t "\\u~04x" (char->ucs x))]))

(define (do-intersperse delim proc xs)
  (unless (null? xs)
    (proc (car xs))
    (dolist [x (cdr xs)]
      (display delim)
      (proc x))))

(define-method edn-write ((x <list>))
  (display "(") (do-intersperse " " edn-write x) (display ")"))
(define-method edn-write ((x <vector>))
  (display "[") (do-intersperse " " edn-write (vector->list x)) (display "]"))
(define-method edn-write ((x (with-module scheme.set <set>)))
  (display "#{") (do-intersperse " " edn-write (set->list x)) (display "}"))
(define-method edn-write ((x <hash-table>))
  (display "{")
  (do-intersperse ", " (^e (edn-write (car e)) 
                           (display " ")
                           (edn-write (cdr e)))
                  (hash-table->alist x))
  (display "}"))

(define-method edn-write (obj)
  (errorf "Cannot render ~s in EDN" obj))

(define (construct-edn obj :optional (oport (current-output-port)))
  (with-output-to-port oport (^[] (edn-write obj))))
(define (construct-edn-string obj)
  (with-output-to-string (^[] (edn-write obj))))
