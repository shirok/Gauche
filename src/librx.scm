;;;
;;; librx.scm - builtin regular-expression procedures
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

(select-module gauche)

(inline-stub
 (declcode (.include <gauche/regexp.h>)))

(define-cproc regexp? (obj)   ::<boolean> :constant SCM_REGEXPP)
(define-cproc regmatch? (obj) ::<boolean> SCM_REGMATCHP)

(define-cproc string->regexp (str::<string> :key (case-fold #f))
  (let* ([flags::int (?: (SCM_BOOL_VALUE case-fold) SCM_REGEXP_CASE_FOLD 0)])
    (return (Scm_RegComp str flags))))
(define-cproc regexp-ast (regexp::<regexp>) (return (-> regexp ast)))
(define-cproc regexp-case-fold? (regexp::<regexp>) ::<boolean>
  (return (logand (-> regexp flags) SCM_REGEXP_CASE_FOLD)))

(define-cproc regexp-parse (str::<string> :key (case-fold #f))
  (let* ([flags::int (?: (SCM_BOOL_VALUE case-fold) SCM_REGEXP_CASE_FOLD 0)])
    (return (Scm_RegComp str (logior flags SCM_REGEXP_PARSE_ONLY)))))
(define-cproc regexp-compile (ast)  Scm_RegCompFromAST)
(define-cproc regexp-optimize (ast) Scm_RegOptimizeAST)

(define-cproc regexp-num-groups (regexp::<regexp>) ::<int>
  (return (-> regexp numGroups)))
(define-cproc regexp-named-groups (regexp::<regexp>)
  (return (-> regexp grpNames)))

(define-cproc rxmatch (regexp str::<string>)
  (let* ([rx::ScmRegexp* NULL])
    (cond [(SCM_STRINGP regexp) (set! rx (SCM_REGEXP (Scm_RegComp
                                                      (SCM_STRING regexp) 0)))]
          [(SCM_REGEXPP regexp) (set! rx (SCM_REGEXP regexp))]
          [else (SCM_TYPE_ERROR regexp "regexp")])
    (return (Scm_RegExec rx str))))

(inline-stub
 (define-cise-stmt rxmatchop
   [(_ (exp ...)) (template exp)]
   [(_ fn)        (template `(,fn (SCM_REGMATCH match) obj))]
   :where
   (define (template result)
     `(cond [(SCM_FALSEP match) (return '#f)]
            [(SCM_REGMATCHP match) (return ,result)]
            [else (SCM_TYPE_ERROR match "regmatch object or #f")
                  (return SCM_UNDEFINED)])))
 )

(define-cproc rxmatch-substring (match :optional (obj 0))
  (rxmatchop Scm_RegMatchSubstr))
(define-cproc rxmatch-start (match :optional (obj 0))
  (rxmatchop Scm_RegMatchStart))
(define-cproc rxmatch-end (match :optional (obj 0))
  (rxmatchop Scm_RegMatchEnd))
(define-cproc rxmatch-before (match :optional (obj 0))
  (rxmatchop Scm_RegMatchBefore))
(define-cproc rxmatch-after (match :optional (obj 0))
  (rxmatchop Scm_RegMatchAfter))
(define-cproc rxmatch-num-matches (match)
  (if (SCM_FALSEP match)
    (return (SCM_MAKE_INT 0))
    (rxmatchop (SCM_MAKE_INT (-> (SCM_REGMATCH match) numMatches)))))
(define-cproc rxmatch-named-groups (match)
  (if (SCM_FALSEP match)
    (return SCM_NIL)
    (rxmatchop (-> (SCM_REGMATCH match) grpNames))))

(select-module gauche.internal)
(define-cproc %regexp-dump (rx::<regexp>) ::<void> Scm_RegDump)
(define-cproc %regmatch-dump (rm::<regmatch>) ::<void> Scm_RegMatchDump)

(define-cproc %regexp-pattern (regexp::<regexp>)
  (setter (regexp::<regexp> pat::<string>) ::<void>
          (set! (-> regexp pattern) (SCM_OBJ pat)))
  (return (-> regexp pattern)))
(define-cproc %regexp-laset (regexp::<regexp>) ; for testing
  (return (-> regexp laset)))

(select-module gauche.internal)
;; aux routine for regexp-replace[-all]
;; "abc\\1de\\3" => '("abc" 1 "de" 3)
(define (%regexp-parse-subpattern sub)
  (cond
   [(string? sub)
    (let loop ((sub sub) (r '()))
      (cond [(rxmatch #/\\(?:(\d+)|k<([^>]+)>|(.))/ sub)
             => (^m
                 (define (loop2 elem)
                   (loop (rxmatch-after m)
                         (list* elem (rxmatch-before m) r)))
                 (cond [(rxmatch-substring m 1)
                        => (^d (loop2 (string->number d)))]
                       [(rxmatch-substring m 2)
                        => (^s (loop2 (string->symbol s)))]
                       [else (loop2 (rxmatch-substring m 3))]))]
            [else (reverse (cons sub r))]))]
   [(procedure? sub) sub]
   [else (error "string or procedure required, but got" sub)]))

;; internal loop
(define (%regexp-replace-rec match subpat rec)
  (display (rxmatch-before match))
  (if (procedure? subpat)
    (display (subpat match))
    (dolist [pat subpat]
      (display (if (or (number? pat) (symbol? pat))
                 (rxmatch-substring match pat)
                 pat))))
  (rec (rxmatch-after match)))

(define-in-module gauche (regexp-replace rx string sub)
  (let ([subpat (%regexp-parse-subpattern sub)]
        [match  (rxmatch rx string)])
    (if match
      (with-output-to-string (cut %regexp-replace-rec match subpat display))
      string)))

(define-in-module gauche (regexp-replace-all rx string sub)
  (let ([subpat (%regexp-parse-subpattern sub)]
        [match  (rxmatch rx string)])
    (if match
      (with-output-to-string
        (^[]
          (define (loop str)
            (unless (equal? str "")
              (cond [(rxmatch rx str)
                     => (^[match]
                          (when (= (rxmatch-start match) (rxmatch-end match))
                            (error "regexp-replace-all: matching zero-length string causes infinite loop:" rx))
                          (%regexp-replace-rec match subpat loop))]
                    [else (display str)])))
          (%regexp-replace-rec match subpat loop)))
      string)))

(define (regexp-replace-driver name func-1)
  (^[string rx sub . more]
    (cond [(null? more) (func-1 rx string sub)]
          [else
           (unless (even? (length more))
             (errorf "~a: regexp and subsitution don't pair up" name))
           (let loop ([s (func-1 rx string sub)]
                      [args more])
             (if (null? args)
               s
               (loop (func-1 (car args) s (cadr args))
                     (cddr args))))])))

(define-in-module gauche regexp-replace*
  (regexp-replace-driver 'regexp-replace* regexp-replace))

(define-in-module gauche regexp-replace-all*
  (regexp-replace-driver 'regexp-replace-all* regexp-replace-all))

;; Contributed from Alex Shinn; modified a bit by shiro
(define-in-module gauche (regexp-quote str)
  (with-string-io str
    (^[] (let loop ((c (read-char)))
           (unless (eof-object? c)
             (when (char-set-contains? #[\\|\[\](){}.*+?^$] c) (write-char #\\))
             (write-char c)
             (loop (read-char)))))))

(define-in-module gauche (regexp->string rx)
  (or (%regexp-pattern rx)
      (rlet1 s (regexp-unparse (regexp-ast rx))
        (set! (%regexp-pattern rx) s))))

(define-in-module gauche (rxmatch->string rx str . sel)
  (cond [(null? sel) (rxmatch-substring (rxmatch rx str))]
        [(eq? (car sel) 'after)
         (apply rxmatch-after (rxmatch rx str) (cdr sel))]
        [(eq? (car sel) 'before)
         (apply rxmatch-before (rxmatch rx str) (cdr sel))]
        [else (rxmatch-substring (rxmatch rx str) (car sel))]))

