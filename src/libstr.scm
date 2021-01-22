;;;
;;;libstr.scm - built-in string library
;;;
;;;   Copyright (c) 2000-2020  Shiro Kawai  <shiro@acm.org>
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

(select-module gauche.internal)

(inline-stub
 (declcode (.include <gauche/bignum.h>
                     <gauche/vminsn.h>
                     <gauche/priv/stringP.h>)))

;;
;; Predicates
;;

(select-module scheme)
(define-cproc string? (obj) ::<boolean> :fast-flonum :constant
  (inliner STRINGP) SCM_STRINGP)

(select-module gauche)
(define-cproc string-incomplete? (obj) ::<boolean>
  (return (and (SCM_STRINGP obj) (SCM_STRING_INCOMPLETE_P obj))))
(define-cproc string-immutable? (obj) ::<boolean>
  (return (and (SCM_STRINGP obj) (SCM_STRING_IMMUTABLE_P obj))))

;;
;; Constructors
;;

(select-module scheme)
(define-cproc make-string (len::<fixnum> :optional (c::<char> #\space))
  Scm_MakeFillString)
(define-cproc string (:rest chars) Scm_ListToString)
(define-cproc string-copy (str::<string> :optional start end)
  (return (Scm_CopyString (SCM_STRING (Scm_MaybeSubstring str start end)))))

(define-cproc string-append (:rest args) Scm_StringAppend)

(select-module gauche)
(define-cproc string-copy-immutable (str::<string> :optional start end)
  (let* ([s (Scm_MaybeSubstring str start end)])
    (if (SCM_STRING_IMMUTABLE_P s)
      (return s)
      (return (Scm_CopyStringWithFlags (SCM_STRING s)
                                       SCM_STRING_IMMUTABLE
                                       SCM_STRING_IMMUTABLE)))))

(define-cproc string-join (strs::<list>
                           :optional (delim::<string> " ") (grammar infix))
  (let* ([gm::int 0])
    (cond
     [(SCM_EQ grammar 'infix) (set! gm SCM_STRING_JOIN_INFIX)]
     [(SCM_EQ grammar 'strict-infix) (set! gm SCM_STRING_JOIN_STRICT_INFIX)]
     [(SCM_EQ grammar 'suffix) (set! gm SCM_STRING_JOIN_SUFFIX)]
     [(SCM_EQ grammar 'prefix) (set! gm SCM_STRING_JOIN_PREFIX)]
     [else (SCM_TYPE_ERROR grammar "one of the symbols infix, strict-infix, \
                                     suffix, or prefix")])
    (return (Scm_StringJoin strs delim gm))))

(define-reader-ctor 'string-interpolate
  (^ args
    (apply string-interpolate args))) ;;lambda is required to delay loading

;;
;; Conversions
;;

(select-module scheme)
(define-cproc string->list (str::<string> :optional start end)
  (return (Scm_StringToList (SCM_STRING (Scm_MaybeSubstring str start end)))))
(define-cproc list->string (list::<list>) Scm_ListToString)

;;
;; Accessors
;;

(select-module scheme)
(define-cproc string-length (str::<string>) ::<fixnum> :constant
  (return (SCM_STRING_BODY_LENGTH (SCM_STRING_BODY str))))
(define-cproc string-ref (str::<string> k :optional fallback)
  :constant
  (let* ([r::ScmChar (Scm_StringRefCursor str k (SCM_UNBOUNDP fallback))])
    (return (?: (== r SCM_CHAR_INVALID) fallback (SCM_MAKE_CHAR r)))))
(define-cproc substring (str::<string> start end)
  (return (Scm_SubstringCursor str start end)))

(select-module gauche)
(define-cproc string-size (str::<string>) ::<fixnum> :constant
  (return (SCM_STRING_BODY_SIZE (SCM_STRING_BODY str))))

(define-cproc %maybe-substring (str::<string> :optional start end)
  Scm_MaybeSubstring)

;; bound argument is for srfi-13
(define-cproc %hash-string (str::<string> :optional bound) ::<ulong>
  (let* ([modulo::u_long 0])
    (cond [(or (SCM_UNBOUNDP bound) (SCM_UNDEFINEDP bound))
           (set! modulo 0)]
          [(SCM_INTP bound) (set! modulo (SCM_INT_VALUE bound))]
          [(SCM_BIGNUMP bound)
           (set! modulo
                 (Scm_BignumToUI (SCM_BIGNUM bound) SCM_CLAMP_BOTH NULL))]
          [else (Scm_Error "argument out of domain: %S" bound)])
    (return (Scm_HashString str modulo))))

(select-module gauche)
(inline-stub
 (define-cfn string-scan-mode (mode) ::int
   (let* ([rmode::int 0])
     (cond
      [(SCM_EQ mode 'index)   (set! rmode SCM_STRING_SCAN_INDEX)]
      [(SCM_EQ mode 'cursor)  (set! rmode SCM_STRING_SCAN_CURSOR)]
      [(SCM_EQ mode 'before)  (set! rmode SCM_STRING_SCAN_BEFORE)]
      [(SCM_EQ mode 'after)   (set! rmode SCM_STRING_SCAN_AFTER)]
      [(SCM_EQ mode 'before*) (set! rmode SCM_STRING_SCAN_BEFORE2)]
      [(SCM_EQ mode 'after*)  (set! rmode SCM_STRING_SCAN_AFTER2)]
      [(SCM_EQ mode 'both)    (set! rmode SCM_STRING_SCAN_BOTH)]
      [else (Scm_Error "bad value in mode argumet: %S, must be one of \
                 'index, 'before, 'after, 'before*, 'after* or 'both." mode)])
     (return rmode))))

;; primitive scanner
(define-cproc string-scan (s1::<string> s2 :optional (mode index))
  (let* ([rmode::int (string-scan-mode mode)])
    (cond
     [(SCM_STRINGP s2) (return (Scm_StringScan s1 (SCM_STRING s2) rmode))]
     [(SCM_CHARP s2)
      (return (Scm_StringScanChar s1 (SCM_CHAR_VALUE s2) rmode))]
     [else (Scm_Error "bad type of argument for s2: %S, must be \
                       either string or character" s2)
           (return SCM_UNDEFINED)])))

(define-cproc string-scan-right (s1::<string> s2 :optional (mode index))
  (let* ([rmode::int (string-scan-mode mode)])
    (cond
     [(SCM_STRINGP s2) (return (Scm_StringScanRight s1 (SCM_STRING s2) rmode))]
     [(SCM_CHARP s2)
      (return (Scm_StringScanCharRight s1 (SCM_CHAR_VALUE s2) rmode))]
     [else (Scm_Error "bad type of argument for s2: %S, must be \
                       either string or character" s2)
           (return SCM_UNDEFINED)])))

;; string-split
;; Generic string-split
;;  splitter can be a character, a char-set, a string, or a regexp.
;;  NB: To adapt to srfi-152, we changed the optional argument - now the
;;  'grammar' argument comes before 'limit'.  For the bacward compatibility,
;;  we recognize an integer argument in 'grammar' as 'limit'.
(define (string-split string splitter
                      :optional (grammar 'infix) (limit #f) start end)
  (if (or (not grammar) (integer? grammar))
    ((with-module gauche.internal %string-split)
     string splitter 'infix grammar limit start)
    ((with-module gauche.internal %string-split)
     string splitter grammar limit start end)))

(select-module gauche.internal)
(define-cproc %string-split-by-char (s::<string> ch::<char>
                                     :optional (limit::<int> -1))
  Scm_StringSplitByCharWithLimit)

(define (%string-split string splitter grammar limit start end)
  (unless (memq grammar '(infix strict-infix prefix suffix))
    (error "grammar argument must be one of (infix strict-infix prefix suffix), but got" grammar))
  (unless (or (not limit) (and (integer? limit) (>= limit 0)))
    (error "limit argument must be a nonnegative integer or #f, but got" limit))
  (let1 s ((with-module gauche.internal %maybe-substring) string start end)
    (if (equal? s "")
      (if (eq? grammar 'strict-infix)
        (error "string must not be empty with strict-infix grammar")
        '())
      (let1 r (if (char? splitter)
                (%string-split-by-char s splitter (or limit -1))
                (%string-split-by-scanner s (%string-split-scanner splitter)
                                          (or limit -1)))
        (case grammar
          [(prefix) (if (and (pair? r) (equal? (car r) ""))
                      (cdr r)
                      r)]
          [(suffix) (if (and (pair? r) (equal? (car (last-pair r)) ""))
                      (drop-right r 1)
                      r)]
          [else r])))))

;; aux fns
(define (%string-split-scanner splitter)
  (cond [(string? splitter)
         (if (string=? splitter "")
           (^s (if (<= (string-length s) 1)
                 (values s #f)
                 (values (string-copy s 0 1) (string-copy s 1))))
           (^s (receive (before after) (string-scan s splitter 'both)
                 (if before (values before after) (values s #f)))))]
        [(char-set? splitter)
         (%string-split-scanner-each-char
          (cut char-set-contains? splitter <>))]
        [(regexp? splitter)
         (^s (cond [(rxmatch splitter s)
                    => (^m (let ([before (m 'before)]
                                 [after  (m 'after)])
                             (if (string=? s after)
                               (if (<= (string-length s) 1)
                                 (values s #f)
                                 (values (string-copy s 0 1)
                                         (string-copy s 1)))
                               (values before after))))]
                   [else (values s #f)]))]
        [else ;; assume splitter is a predicate
         (%string-split-scanner-each-char splitter)]))

(define (%string-split-scanner-each-char pred)
  (define (cursor-substr s cur)
    (substring s (string-cursor-start s) cur))

  (define (scan-in s cur end)
    (cond [(string-cursor=? cur end)
           (values (cursor-substr s cur) #f)]
          [(pred (string-ref s cur))
           (scan-out s (string-cursor-next s cur) end (cursor-substr s cur))]
          [else
           (scan-in s (string-cursor-next s cur) end)]))

  (define (scan-out s cur end before)
    (cond [(string-cursor=? cur end)
           (values before "")]
          [(pred (string-ref s cur))
           (scan-out s (string-cursor-next s cur) end before)]
          [else
           (values before (substring s cur end))]))

  (^s (scan-in s (string-cursor-start s) (string-cursor-end s))))

(define (%string-split-by-scanner string scanner limit)
  (let loop ([s string]
             [r '()]
             [limit limit])
    (if (zero? limit)
      (reverse! (cons s r))
      (receive (before after) (scanner s)
        (if after
          (loop after (cons before r) (- limit 1))
          (reverse! (cons before r)))))))

;; Modifying string
;;  They are just for backward compatibility, and they are expensive
;;  anyway, so we don't care their performance.

(select-module gauche.internal)
(define-cproc %string-replace-body! (target::<string> source::<string>)
  (return (Scm_StringReplaceBody target (SCM_STRING_BODY source))))

(define-in-module scheme (string-set! str k ch)
  (check-arg string? str)
  (check-arg char? ch)
  (let1 cur (string-index->cursor str k)
    (%string-replace-body! str
                           (string-append (substring str 0 cur)
                                          (string ch)
                                          (string-copy str (string-cursor-next str k))))))

(set! (setter string-ref) string-set!)

(define-in-module gauche (string-byte-set! str k b)
  (check-arg string? str)
  (check-arg integer? k)
  (check-arg exact? k)
  (check-arg integer? b)
  (let ([siz (string-size str)]
        [out (open-output-string :private? #t)])
    (when (or (< k 0) (<= siz k))
      (error "string index out of range:" k))
    (display (byte-substring str 0 k) out)
    (write-byte b out)
    (display (byte-substring str (+ k 1) siz) out)
    (%string-replace-body! str (get-output-byte-string out))))

(set! (setter string-byte-ref) string-byte-set!)

(define-in-module scheme (string-fill! str c
                                       :optional (start 0)
                                                 (end (string-length str)))
  (let ([start  (string-index->cursor str start)]
        [end    (string-index->cursor str end)]
        [istart (string-cursor->index str start)]
        [iend   (string-cursor->index str end)]
        [len    (string-length str)])
    (when (< iend istart)
      (errorf "end index ~s is smaller than start index ~s" iend istart))
    (if (and (= istart 0) (= iend len))
      (%string-replace-body! str (make-string len c))
      (%string-replace-body! str
                             (string-append (substring str 0 start)
                                            (make-string (- iend istart) c)
                                            (string-copy str end))))))

;; Build index.
;; Technically, string-build-index mutates StringBody, but it's an idempotent
;; operation and can be applied on immutable strings.
(select-module gauche)
(define-cproc string-build-index! (s::<string>) ::<string>
  (Scm_StringBodyBuildIndex (cast ScmStringBody* (SCM_STRING_BODY s)))
  (return s))

(define-cproc string-fast-indexable? (s::<string>) ::<boolean>
  (return (Scm_StringBodyFastIndexableP (SCM_STRING_BODY s))))

(select-module gauche.internal)
(define-cproc %string-index-dump (s::<string> :optional (p::<port> (current-output-port))) ::<void>
  (Scm_StringBodyIndexDump (SCM_STRING_BODY s) p))

;;
;; Comparison
;;

(select-module scheme)
(inline-stub
 (define-cise-expr strcmp  [(_ op) `(,op (Scm_StringCmp s1 s2) 0)])
 (define-cise-expr strcmpi [(_ op) `(,op (Scm_StringCiCmp s1 s2) 0)])

 (define-cise-stmt strcmp-multiarg
   [(_ cmp)
    `(while TRUE
       (if ,cmp
         (cond [(SCM_NULLP ss) (return TRUE) break]
               [(not (SCM_STRINGP (SCM_CAR ss)))
                (SCM_TYPE_ERROR (SCM_CAR ss) "string")]
               [else (set! s1 s2) (set! s2 (SCM_STRING (SCM_CAR ss)))
                     (set! ss (SCM_CDR ss))])
         (begin (return FALSE) break)))])
 )

(define-cproc string=? (s1::<string> s2::<string> :rest ss)
  ::<boolean> :constant
  (strcmp-multiarg (Scm_StringEqual s1 s2)))
(define-cproc string<? (s1::<string> s2::<string> :rest ss)
  ::<boolean> :constant
  (strcmp-multiarg (strcmp <)))
(define-cproc string>? (s1::<string> s2::<string> :rest ss)
  ::<boolean> :constant
  (strcmp-multiarg (strcmp >)))
(define-cproc string<=? (s1::<string> s2::<string> :rest ss)
  ::<boolean> :constant
  (strcmp-multiarg (strcmp <=)))
(define-cproc string>=? (s1::<string> s2::<string> :rest ss)
  ::<boolean> :constant
  (strcmp-multiarg (strcmp >=)))

(define-cproc string-ci=? (s1::<string> s2::<string> :rest ss)
  ::<boolean> :constant
  (strcmp-multiarg (strcmpi ==)))
(define-cproc string-ci<? (s1::<string> s2::<string> :rest ss)
  ::<boolean> :constant
  (strcmp-multiarg (strcmpi <)))
(define-cproc string-ci>? (s1::<string> s2::<string> :rest ss)
  ::<boolean> :constant
  (strcmp-multiarg (strcmpi >)))
(define-cproc string-ci<=? (s1::<string> s2::<string> :rest ss)
  ::<boolean> :constant
  (strcmp-multiarg (strcmpi <=)))
(define-cproc string-ci>=? (s1::<string> s2::<string> :rest ss)
  ::<boolean> :constant
  (strcmp-multiarg (strcmpi >=)))

;;
;; Mapping
;;

;; We have two kinds of string-{map|for-each}.  SRFI-13 takes one string
;; with optional start/end arguments.  R7RS takes one or more string arguments.
;; In retrospect, R7RS is more consistent, but srfi-13 is very frequently
;; imported and it is bothersome to avoid name conflicts.  So we have
;; single implementation that handles both.

(select-module gauche.internal)
(define (%string-map-dispatch name proc str rest proc-single proc-multi)
  (cond [(null? rest) (proc-single proc str)]
        [(or (integer? (car rest))
             (string-cursor? (car rest)))
         (if (null? (cdr rest))
           (proc-single proc (%maybe-substring str (car rest) (undefined)))
           (if (or (integer? (cadr rest))
                   (string-cursor? (cadr rest)))
             (if (null? (cddr rest))
               (proc-single proc (%maybe-substring str
                                                   (car rest)
                                                   (cadr rest)))
               (errorf "Too many arguments for srfi-13 style ~a" name))
             (error "Integer or string-cursor expected, but got:" (cadr rest))))]
        [(string? (car rest))
         (if-let1 bad (find (^s (not (string? s))) (cdr rest))
           (error "string expected, but got:" bad)
           (apply proc-multi proc str rest))]
        [else (error "string expected, but got:" (car rest))]))

(select-module gauche)
(define (string-map proc str . rest)
  (define (string-map-1 proc str)
    ;; go reverse so we don't have to reverse the final list
    ;; slightly slower than iterating forward, but less temp.
    ;; objects
    (let ([start (string-cursor-start str)])
      (let loop ([cur (string-cursor-end str)]
                 [lst '()])
        (if (string-cursor=? cur start)
          (list->string lst)
          (let1 cur (string-cursor-prev str cur)
            (loop cur (cons (proc (string-ref str cur)) lst)))))))
  (define (string-map-n proc . strs)
    ;; the multi-strs version is already more complicated/less
    ;; efficient with lots of lists, just go from left to right
    (let1 ends (map string-cursor-end strs)
      (let loop ([curs (map string-cursor-start strs)]
                 [lst '()])
        (if (any string-cursor=? curs ends)
          (list->string (reverse lst))
          (loop (map string-cursor-next strs curs)
                (cons (apply proc (map string-ref strs curs)) lst))))))
  ((with-module gauche.internal %string-map-dispatch)
   'string-map proc str rest string-map-1 string-map-n))

(define (string-for-each proc str . rest)
  (define (string-for-each-1 proc str)
    (let ([end (string-cursor-end str)])
      (let loop ([cur (string-cursor-start str)])
        (if (string-cursor=? cur end)
          (undefined)
          (begin (proc (string-ref str cur))
                 (loop (string-cursor-next str cur)))))))
  (define (string-for-each-n proc . strs)
    (let1 ends (map string-cursor-end strs)
      (let loop ([curs (map string-cursor-start strs)])
        (if (any string-cursor=? curs ends)
          (undefined)
          (begin (apply proc (map string-ref strs curs))
                 (loop (map string-cursor-next strs curs)))))))
  ((with-module gauche.internal %string-map-dispatch)
   'string-for-each proc str rest string-for-each-1 string-for-each-n))

;;
;; Byte string
;;

(select-module gauche)
;; DEPRECATED, only kept for backward compatibility.
;; We allocate a new string and swap the body, in order to avoid MT-hazard.
;; (So it is _not_ allocation-free, and we no longer have reason to keep
;; this procedure.)
(define (string-incomplete->complete! str)
  (and-let1 s (string-incomplete->complete str)
    ((with-module gauche.internal %string-replace-body!) str s)
    s))

(define-cproc string-complete->incomplete (str::<string>)
  (return (Scm_CopyStringWithFlags str 
                                   SCM_STRING_INCOMPLETE 
                                   SCM_STRING_INCOMPLETE)))

;; handling : #f | :omit | :replace | :escape
;; filler   : #f | <char> | <string>
(define-cproc string-incomplete->complete (str::<string>
                                           :optional (handling #f)
                                                     (filler #f))
  ;; backward compatibility
  (when (SCM_CHARP handling)
    (set! filler handling
          handling ':replace))
  ;; argument check
  (unless (or (SCM_FALSEP handling)
              (Scm_Memq handling '(:omit :replace :escape)))
    (Scm_Error "Either #f, :omit, :replace or :escape required, \
                but got:" handling))
  (unless (or (SCM_FALSEP filler) (SCM_CHARP filler))
    (Scm_Error "Either a character or #f required, but got:" filler))
  (let* ([b::(const ScmStringBody*) (SCM_STRING_BODY str)])
    (unless (SCM_STRING_BODY_INCOMPLETE_P b)
      ;; We do simple copy
      (return (Scm_CopyString str)))
    (let* ([s::(const char*) (SCM_STRING_BODY_START b)]
           [siz::ScmSmallInt (SCM_STRING_BODY_SIZE b)]
           [len::ScmSmallInt (Scm_MBLen s (+ s siz))])
      (when (>= len 0)
        ;; The body can be interpreted as a complete string.
        (return (Scm_MakeString s siz len 0)))
      (when (SCM_FALSEP handling)
        ;; Return #f for string that can't be complete
        (return SCM_FALSE))
      (let* ([p::(const char*) s]
             [ds::ScmDString]
             [echar::ScmChar (?: (SCM_CHARP filler)
                                 (SCM_CHAR_VALUE filler)
                                 #\?)])
        (Scm_DStringInit (& ds))
        (while (< p (+ s siz))
          (let* ([ch::ScmChar])
            (if (>= (+ p (SCM_CHAR_NFOLLOWS (* p))) (+ s siz))
              (set! ch SCM_CHAR_INVALID)
              (SCM_CHAR_GET p ch))
            (cond
             [(and (SCM_EQ handling ':escape) ; escaping escape char
                   (== ch echar))
              (Scm_DStringPutc (& ds) ch)
              (Scm_DStringPutc (& ds) ch)
              (+= p (SCM_CHAR_NBYTES ch))]
             [(!= ch SCM_CHAR_INVALID)  ; ok
              (Scm_DStringPutc (& ds) ch)
              (+= p (SCM_CHAR_NBYTES ch))]
             [(SCM_EQ handling ':omit) (inc! p)] ;skip
             [(SCM_EQ handling ':replace)        ;subs with echar
              (Scm_DStringPutc (& ds) echar)
              (inc! p)]
             [(SCM_EQ handling ':escape)         ;escape
              (let* ([octet::u_char (* p)])
                (Scm_DStringPutc (& ds) echar)
                (Scm_DStringPutc (& ds) (Scm_IntToDigit (>> octet 4) 16 0 0))
                (Scm_DStringPutc (& ds) (Scm_IntToDigit (logand octet #xf) 16 0 0))
                (inc! p))])))
        (return (Scm_DStringGet (& ds) 0))))))

(define-cproc make-byte-string (size::<int32> :optional (byte::<uint8> 0))
  (let* ([s::char*])
    (when (< size 0) (Scm_Error "size out of bound: %d" size))
    (set! s (SCM_NEW_ATOMIC2 (C: char*) size))
    (memset s byte size)
    (return (Scm_MakeString s size size SCM_STRING_INCOMPLETE))))

(define-cproc string-byte-ref (str::<string> k::<fixnum> :optional fallback)
  (let* ([r::int (Scm_StringByteRef str k (SCM_UNBOUNDP fallback))])
    (return (?: (< r 0) fallback (SCM_MAKE_INT r)))))

(define-cproc byte-substring (str::<string> start::<fixnum> end::<fixnum>)
  (return (Scm_Substring str start end TRUE)))

;;
;; Complete-sexp?
;;

;; Check if the given string can be parsed as a complete sexp.
;; Note that this test doesn't rule out all invalid sexprs.
;;
;; This used to be in gauche.listener, but is also used in
;; gauche.interactive.editable-reader so moved here.
;;
;; This should eventually be folded into build-in read, so that
;; any nontrivial syntax can be handled consistently.

(select-module gauche)
(define (complete-sexp? str)
  (with-input-from-string str
    (^[]
      ;; charset that delimits token
      (define special-chars #[\u0000-\u0020\"\'()\,\;\[\\\]\`{|}\u007f])

      ;; main loop
      (define (rec closer)
        (let1 ch (read-char)
          (cond [(eof-object? ch) (if closer #f #t)]
                [(eqv? closer ch) #t]
                [(eqv? #\( ch) (and (rec #\) ) (rec closer))]
                [(eqv? #\[ ch) (and (rec #\] ) (rec closer))]
                [(eqv? #\{ ch) (and (rec #\} ) (rec closer))]
                [(eqv? #\" ch) (and (rec-escaped #\") (rec closer))]
                [(eqv? #\| ch) (and (rec-escaped #\|) (rec closer))]
                [(eqv? #\; ch) (skip-to-nl) (rec closer)]
                [(eqv? #\# ch)
                 (let1 c2 (read-char)
                   (cond [(eof-object? c2) #f]
                         [(eqv? c2 #\\)
                          (and (not (eof-object? (read-char)))
                               (begin (skip-token) (rec closer)))]
                         [(eqv? c2 #\/) (and (rec-escaped #\/) (rec closer))]
                         [(eqv? c2 #\[) (and (rec-escaped #\]) (rec closer))]
                         [(eqv? c2 #\,)
                          (let1 c3 (skip-ws)
                            (cond [(eof-object? c3) #f]
                                  [(eqv? #\( c3) (and (rec #\) ) (rec closer))]
                                  [(eqv? #\[ c3) (and (rec #\] ) (rec closer))]
                                  [(eqv? #\{ c3) (and (rec #\} ) (rec closer))]
                                  [else (skip-token) (rec closer)]))]
                         [(eqv? c2 #\() (and (rec #\)) (rec closer))]
                         [(eqv? c2 #\<)
                          (errorf "unreadable sequence #<~a..."
                                  (read-block 10))]
                         [(eqv? c2 closer) #t]
                         [else (rec closer)]))]
                [else (rec closer)])))

      (define (rec-escaped closer)
        (let1 ch (read-char)
          (cond [(eof-object? ch) #f]
                [(eqv? closer ch) #t]
                [(eqv? #\\ ch) (read-char) (rec-escaped closer)]
                [else (rec-escaped closer)])))

      (define (skip-token)
        (let loop ([ch (peek-char)])
          (unless (or (eof-object? ch)
                      (char-set-contains? special-chars ch))
            (read-char)
            (loop (peek-char)))))

      (define (skip-ws)
        (let loop ([ch (read-char)])
          (if (or (eof-object? ch)
                  (char-set-contains? #[\S] ch))
            ch
            (loop (read-char)))))

      (define (skip-to-nl)
        (let loop ([ch (read-char)])
          (unless (or (eof-object? ch)
                      (eqv? ch #\newline))
            (loop (read-char)))))

      ;; body
      (rec #f)
      )))

;;
;; String cursors
;;

(select-module gauche)

(define-cproc string-cursor? (obj) ::<boolean>
  SCM_STRING_CURSOR_P)
(define-cproc string-cursor-start (s::<string>)
  (return (Scm_MakeStringCursorFromIndex s 0)))
(define-cproc string-cursor-end (s::<string>)
  Scm_MakeStringCursorEnd)
(define-cproc string-cursor-next (s::<string> cursor)
  (return (Scm_StringCursorForward s cursor 1)))
(define-cproc string-cursor-prev (s::<string> cursor)
  (return (Scm_StringCursorBack s cursor 1)))
(define-cproc string-cursor-forward (s::<string> cursor nchars::<fixnum>)
  Scm_StringCursorForward)
(define-cproc string-cursor-back (s::<string> cursor nchars::<fixnum>)
  Scm_StringCursorBack)
(define-cproc string-index->cursor (s::<string> index)
  (if (SCM_STRING_CURSOR_P index)
    (return index)
    (return (Scm_MakeStringCursorFromIndex s (Scm_GetInteger index)))))
(define-cproc string-cursor->index (s::<string> cursor)
  Scm_StringCursorIndex)

(define-cproc string-cursor=? (cursor1 cursor2) ::<boolean>
  (return (Scm_StringCursorCompare cursor1 cursor2 Scm_NumEq)))

(define-cproc string-cursor<? (cursor1 cursor2) ::<boolean>
  (return (Scm_StringCursorCompare cursor1 cursor2 Scm_NumLT)))

(define-cproc string-cursor>? (cursor1 cursor2) ::<boolean>
  (return (Scm_StringCursorCompare cursor1 cursor2 Scm_NumGT)))

(define-cproc string-cursor<=? (cursor1 cursor2) ::<boolean>
  (return (Scm_StringCursorCompare cursor1 cursor2 Scm_NumLE)))

(define-cproc string-cursor>=? (cursor1 cursor2) ::<boolean>
  (return (Scm_StringCursorCompare cursor1 cursor2 Scm_NumGE)))

(define-cproc string-cursor-diff (s::<string> start end)
  (return (Scm_Sub (Scm_StringCursorIndex s end)
                   (Scm_StringCursorIndex s start))))
