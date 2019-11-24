;;;
;;; text.edn - read and write Clojure's Extensible Data Notation
;;;
;;;   Copyright (c) 2018-2019  Shiro Kawai  <shiro@acm.org>
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

;;; Spec:
;;;   https://github.com/edn-format/edn
;;;
;;; Mapping:
;;;     EDN          Gauche
;;;     true         #t
;;;     false        #f
;;;     nil          nil       (symbol; but client must be aware that
;;;                             it is not a symbol in Clojure world)
;;;     <number>     <real>
;;;     <symbol>     <symbol>  (some restrictions in symbol names)
;;;     <keyword>    <keyword> (Clojure symbols never begin with ':', so
;;;                             it won't conflict with symbols)
;;;     <list>       <list>    (no improper list)
;;;     <vector>     <vector>
;;;     <map>        <hash-table> with edn-comparator
;;;     <set>        <set> with edn-comparator
;;;     <taggged>    <edn-object>  (customizable)
;;;
;;;
;;; Customization:
;;;   - register-edn-object-handler! registers mapping from tagged object
;;;     to Scheme object
;;;   - edn-write method can be used to write Scheme object in edn

(define-module text.edn
  (use gauche.record)
  (use gauche.lazy)
  (use gauche.generator)
  (use gauche.threads)
  (use srfi-13)
  (use scheme.set)
  (use parser.peg)

  (export <edn-parse-error>

          edn-equal? edn-comparator
          edn-map edn-set
          <edn-object> edn-object? make-edn-object
          edn-object-tag edn-object-payload

          register-edn-object-handler! edn-object-handler
          
          edn-symbol-prefix edn-symbol-basename edn-valid-symbol-name?

          parse-edn parse-edn* parse-edn-string
          construct-edn construct-edn-string edn-write))
(select-module text.edn)

;;;
;;; Comparison
;;;

(define (edn-equal? a b)
  (cond [(hash-table? a) (and (hash-table? b)
                              (hash-table=? edn-comparator a b))]
        [(set? a)        (and (set? b) (set=? a b))]
        [(edn-object? a) (and (edn-object? b)
                              (edn-equal? (edn-object-tag a)
                                          (edn-object-tag b))
                              (edn-equal? (edn-object-payload a)
                                          (edn-object-payload b)))]
        [else (equal? a b)]))

(define edn-comparator 
  (make-comparator #t edn-equal? #f default-hash))

;;;
;;; Conditions
;;;

(define-condition-type <edn-parse-error> <error> #f
  (position))

(define (wrap-parser thunk)
  (guard (e [(<parse-error> e)
             (error <edn-parse-error>
                    :position (~ e'position)
                    :message (~ e'message))])
    (thunk)))

;;;
;;; EDN-specific types
;;;

;; EDN map is a hashtable with edn-comparator.
(define (edn-map . kv-list)
  (rlet1 h (make-hash-table edn-comparator)
    (do-plist [[k v] kv-list]
      ;; todo: duplicate key check
      (hash-table-put! h k v))))

;; EDN set is a set with edn-comparator.
(define (edn-set . vals)
  (list->set edn-comparator vals))

;; EDN tagged object
(define-record-type <edn-object> make-edn-object edn-object?
  (tag     edn-object-tag)
  (payload edn-object-payload))

(define-method write-object ((x <edn-object>) port)
  (format port "#<edn-object #~s ~a>" 
          (edn-object-tag x)
          (write-to-string (edn-object-payload x) edn-write)))

;; EDN symbol is mapped to a symbol; we just treat prefix as a part
;; of symbol names.  We do provide a utility to extract prefix
;; and real name.
(define (edn-symbol-prefix sym)
  (and-let1 p (string-scan (symbol->string sym) "/" 'before)
    (string->symbol p)))

(define (edn-symbol-basename sym)
  (or (and-let1 p (string-scan (symbol->string sym) "/" 'after)
        (string->symbol p))
      sym))

(define (edn-valid-symbol-name? str)
  (define valid-component?
    #/^([[:alpha:]*!_?$%*<>]|[+.-][[:alpha:]*!_?$%*<>:#+.-])[\w*!_?$%*<>:#+.-]*$/)
  (let1 s (string-split str "/" 'infix)
    (and (<= 1 (length s) 2)
         (every valid-component? s)
         (not (member (last s) '("nil" "true" "false"))))))

;;;
;;; Customization hook
;;;

;; handler :: Symbol, EdnObject -> Obj

(define *edn-handlers* (atom (make-hash-table 'string=?)))

(define (register-edn-object-handler! tag handler)
  (assume-type tag <symbol>)
  (let1 s (symbol->string tag)
    (assume (edn-valid-symbol-name? s))
    (atomic *edn-handlers* (cut hash-table-put! <> s handler))))

(define (edn-object-handler tag)
  (let1 s (if (string? tag) tag (x->string tag))
    (atomic *edn-handlers* (cut hash-table-get <> tag #f))))

;;;
;;; Parser
;;;

(define (parse lseq)
  (define %term
    ($lazy ($seq ($skip-many %ws)
                 ($or %list %vec %map %set
                      %tagged %atom %str %char))))
  (define %ws
    ($or ($skip-many1 ($. #[\s,]))
         ($seq ($. #\;)
               ($skip-many ($. #[^\newline])))
         ($seq ($. "#_") %term)))
  (define %list
    ($between ($. #\() ($many %term) ($. #\))))
  (define %vec
    ($between ($. #\[) ($lift list->vector ($many %term)) ($. #\])))
  (define %set
    ($between ($. "#{") ($lift ($ apply edn-set $) ($many %term)) ($. #\})))
  (define %map
    ($between ($. #\{) ($lift ($ apply edn-map $) ($many %term)) ($. #\})))
  (define %word ; intermediate - used by %atom, %char and %tagged
    ($->string ($many1 ($. #[\w*!?$%&=<>:#/+.-]))))
  (define %atom
    ($do [val %word]
         (cond
          [(equal? val "true")  ($return #t)]
          [(equal? val "false") ($return #f)]
          [(equal? val "nil")   ($return 'nil)]
          [(string-prefix? ":" val) ($return 
                                     (make-keyword (string-drop val 1)))]
          [(%parse-num val) => $return]
          [(edn-valid-symbol-name? val) ($return (string->symbol val))]
          [else ($fail (format "invalid token: ~s" val))])))
  (define %tagged
    ($do [ ($. #\#) ]
         [tag %word]
         [content %term]
         (if (edn-valid-symbol-name? tag)
           (if-let1 h (edn-object-handler tag)
             ($return (h tag content))
             ($return (make-edn-object (string->symbol tag) content)))
           ($fail (format "invalid tag: ~s" tag)))))
  (define %str 
    ($between ($. #\")
              ($->string
               ($many ($or ($seq ($. "\\\"") ($return "\""))
                           ($seq ($. "\\t") ($return "\t"))
                           ($seq ($. "\\r") ($return "\r"))
                           ($seq ($. "\\n") ($return "\n"))
                           ($seq ($. "\\\\") ($return "\\"))
                           ($. #[^\\\"]))))
              ($. #\")))
  (define %char
    ($do [w ($seq ($. #\\) %word)] 
         (rxmatch-case w
           [#/^newline$/ (_) ($return #\newline)]
           [#/^return$/  (_) ($return #\return)]
           [#/^space$/   (_) ($return #\space)]
           [#/^tab$/     (_) ($return #\tab)]
           [#/^u([0-9a-fA-F]+)$/ (_ v)
                ($return (ucs->char (string->number v 16)))]
           [#/^.$/       (s) ($return (~ s 0))]
           [else ($fail (format "invalid char name: ~a" w))])))
  (wrap-parser (cut peg-run-parser %term lseq)))

(define (%parse-num word)
  (rxmatch-case word
    [#/^([+-])?0[xX]([0-9a-fA-F]+)$/ (_ sign digs)
        (* (if (equal? sign "-") -1 1) (string->number digs 16))]
    [#/^([+-])?0([0-7]+)$/ (_ sign digs)
        (* (if (equal? sign "-") -1 1) (string->number digs 8))]
    [#/^([+-])?(\d+)(?:\.(\d+))?(?:[eE]([+-]?\d+))?([MN])?$/
        (_ sign int frac exp sfx)
        (* (if (equal? sign "-") -1 1)
           (if (or frac exp (equal? sfx "M"))
             (string->number (format "~a.~aE~a" int (or frac "0") (or exp "0")))
             (string->number int)))]
    [else #f]))

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
(define-method edn-write ((x <edn-object>))
  (display "#")
  (edn-write (edn-object-tag x))
  (display " ")
  (edn-write (edn-object-payload x)))

(define-method edn-write (obj)
  (errorf "Cannot render ~s in EDN" obj))

(define (construct-edn obj :optional (oport (current-output-port)))
  (with-output-to-port oport (^[] (edn-write obj))))
(define (construct-edn-string obj)
  (with-output-to-string (^[] (edn-write obj))))
