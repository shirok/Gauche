;;;
;;;libstr.scm - built-in string library
;;;
;;;   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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
                     <gauche/vminsn.h>)))

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
(define-cproc string-ref (str::<string> k::<fixnum> :optional fallback)
  :constant
  (let* ([r::ScmChar (Scm_StringRef str k (SCM_UNBOUNDP fallback))])
    (return (?: (== r SCM_CHAR_INVALID) fallback (SCM_MAKE_CHAR r)))))
(define-cproc substring (str::<string> start::<fixnum> end::<fixnum>)
  (return (Scm_Substring str start end FALSE)))

(select-module gauche)
(define-cproc string-size (str::<string>) ::<fixnum> :constant
  (return (SCM_STRING_BODY_SIZE (SCM_STRING_BODY str))))

(select-module gauche.internal)
;; see lib/gauche/stringutil.scm for generic string-split
(define-cproc %string-split-by-char (s::<string> ch::<char>
                                     :optional (limit::<int> -1))
  Scm_StringSplitByCharWithLimit)

(define-cproc %maybe-substring (str::<string> :optional start end)
  Scm_MaybeSubstring)

;; bound argument is for srfi-13
(define-cproc %hash-string (str::<string> :optional bound) ::<ulong>
  (let* ([modulo::u_long 0])
    (cond [(or (SCM_UNBOUNDP bound) (SCM_UNDEFINEDP bound))
           (set! modulo SCM_SMALL_INT_MAX)]
          [(SCM_INTP bound) (set! modulo (SCM_INT_VALUE bound))]
          [(SCM_BIGNUMP bound)
           (set! modulo
                 (Scm_BignumToUI (SCM_BIGNUM bound) SCM_CLAMP_BOTH NULL))])
    (when (== modulo 0) (Scm_Error "argument out of domain: %S" bound))
    (return (Scm_HashString str modulo))))

(select-module gauche)
(inline-stub
 (define-cfn string-scan-mode (mode) ::int
   (let* ([rmode::int 0])
     (cond
      [(SCM_EQ mode 'index)   (set! rmode SCM_STRING_SCAN_INDEX)]
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

;;
;; Modifying string
;;  They are just for backward compatibility, and they are expensive
;;  anyway, so we don't care their performance.

(select-module gauche.internal)
(define-cproc %string-replace-body! (target::<string> source::<string>)
  (return (Scm_StringReplaceBody target (SCM_STRING_BODY source))))

(define-in-module scheme (string-set! str k ch)
  (check-arg string? str)
  (check-arg integer? k)
  (check-arg exact? k)
  (check-arg char? ch)
  (let1 len (string-length str)
    (when (or (< k 0) (<= len k))
      (error "string index out of range:" k))
    (%string-replace-body! str
                           (string-append (substring str 0 k)
                                          (string ch)
                                          (substring str (+ k 1) len)))))

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

(define-in-module gauche (string-fill! str c
                                       :optional (start 0)
                                                 (end (string-length str)))
  (let1 len (string-length str)
    (when (or (< start 0) (< len start))
      (error "start index out of range:" start))
    (when (or (< end 0) (< len end))
      (error "end index out of range:" end))
    (when (< end start)
      (errorf "end index ~s is smaller than start index ~s" end start))
    (if (and (= start 0) (= end len))
      (%string-replace-body! str (make-string len c))
      (%string-replace-body! str
                             (string-append (substring str 0 start)
                                            (make-string (- end start) c)
                                            (substring str end len))))))

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
;; Byte string
;;

(select-module gauche)
;; DEPRECATED, only kept for backward compatibility.
;; We allocate a new string and swap the body, in order to avoid MT-hazard.
;; (So it is _not_ allocation-free, and we no longer have reason to keep
;; this procedure.)
(define-cproc string-incomplete->complete! (str::<string>)
  (let* ([s (Scm_StringIncompleteToComplete str SCM_ILLEGAL_CHAR_REJECT
                                            (SCM_CHAR 0))])
    (unless (SCM_FALSEP s) (set! (-> str body) (SCM_STRING_BODY s)))
    (return s)))

(define-cproc string-complete->incomplete (str::<string>)
  Scm_StringCompleteToIncomplete)

(define-cproc string-incomplete->complete (str::<string>
                                           :optional (handling #f))
  (let* ([h::int 0] [sub::ScmChar (SCM_CHAR 0)])
    (cond [(SCM_EQ handling ':omit) (set! h SCM_ILLEGAL_CHAR_OMIT)]
          [(SCM_FALSEP handling)    (set! h SCM_ILLEGAL_CHAR_REJECT)]
          [(SCM_CHARP handling)     (set! h SCM_ILLEGAL_CHAR_REPLACE
                                          sub (SCM_CHAR_VALUE handling))]
          [else (SCM_TYPE_ERROR handling ":omit, #f, or a character")])
    (return (Scm_StringIncompleteToComplete str h sub))))

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
;; String pointers
;;

(select-module gauche)
(inline-stub
 ;; string pointer
 (define-type <string-pointer> "ScmStringPointer*" "string pointer"
   "SCM_STRING_POINTERP" "SCM_STRING_POINTER")
 )

(define-cproc make-string-pointer (str::<string>
                                   :optional (index::<fixnum> 0)
                                   (start::<fixnum> 0)
                                   (end::<fixnum> -1))
  Scm_MakeStringPointer)
(define-cproc string-pointer? (obj) ::<boolean> SCM_STRING_POINTERP)

(define-cproc string-pointer-ref (sp::<string-pointer>)
  Scm_StringPointerRef)
(define-cproc string-pointer-next! (sp::<string-pointer>)
  Scm_StringPointerNext)
(define-cproc string-pointer-prev! (sp::<string-pointer>)
  Scm_StringPointerPrev)
(define-cproc string-pointer-set! (sp::<string-pointer> index::<fixnum>)
  Scm_StringPointerSet)
(define-cproc string-pointer-substring (sp::<string-pointer> :key (after #f))
  (return (Scm_StringPointerSubstring sp (not (SCM_FALSEP after)))))
(define-cproc string-pointer-index (sp::<string-pointer>) ::<int>
  (return (-> sp index)))
(define-cproc string-pointer-copy (sp::<string-pointer>)
  Scm_StringPointerCopy)
(define-cproc string-pointer-byte-index (sp::<string-pointer>) ::<int>
  (return (cast int (- (-> sp current) (-> sp start)))))

(select-module gauche.internal)
(define-cproc %string-pointer-dump (sp::<string-pointer>) ::<void>
  Scm_StringPointerDump)
