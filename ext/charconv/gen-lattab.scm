;;;
;;; Generate Unicode <-> Latin-N tables
;;;

;; The tables can be obtained from
;; https://unicode.org/Public/MAPPINGS/ISO8859/

(use file.util)
(use gauche.dictionary)
(use gauche.collection)
(use gauche.cgen)
(use util.match)
(use srfi.42)

(define-class <latin-ces> ()
  ((n :init-keyword :n)                 ; iso8859-N
   (bimap :init-keyword :bimap)         ; code <-> ucs
   ))

;; Returns bimap between iso8859-N (upper plane) <-> UCS
(define (parse-txt file)
  (rlet1 b (make-bimap (make-hash-table 'eqv?) (make-hash-table 'eqv?))
    (dolist [line (file->string-list file)]
      (rxmatch-case line
        [#/^0x([[:xdigit:]]+)\s+0x([[:xdigit:]]+)/ (_ scode sucs)
         (let ([code (string->number scode 16)]
               [ucs  (string->number sucs 16)])
           (when (>= code #xa0)
             (bimap-put! b code ucs)))]))))

(define (load-ces-data dir n)
  (make <latin-ces> :n n :bimap (parse-txt #"~|dir|/8859-~|n|.TXT")))

(define (latN->utf8 ces)
  (define procname #"lat~(~ ces'n)_utf8")
  (cgen-extern #"static ScmConvProc ~|procname|;")

  (cgen-body ""
             #"static ScmSize ~|procname|(ScmConvInfo *cinfo,"
             #"    const char *inptr, ScmSize inroom SCM_UNUSED,"
             #"    char *outptr, ScmSize outroom, ScmSize *outchars)"
             #"{"
             #"    static u_char tab[] = {")
  (do-ec (: code #xa0 #x100)
         (let* ([ucs (hash-table-get (bimap-left (~ ces'bimap)) code 0)]
                [utf8 (take* (ucs4->utf8 ucs) 3 #t 0)])
           (cgen-body
            (format "        0x~2,'0x,0x~2,'0x,0x~2,'0x, /* ~2,'0x */"
                    (car utf8) (cadr utf8) (caddr utf8)
                    code))))
  (cgen-body #"    };"
             #"    u_char c = *inptr;"
             #"    if (c < 0x80) {"
             #"        *outptr = c;"
             #"        *outchars = 1;"
             #"    } else if (c < 0xa0) {"
             #"        OUTCHK(2);"
             #"        outptr[0] = 0xc2;"
             #"        outptr[1] = c;"
             #"        *outchars = 2;"
             #"    } else {"
             #"        int index = (c - 0xa0) * 3;"
             #"        if (tab[index] == 0) {"
             #"            DO_SUBST;"
             #"        } else if (tab[index + 2] == 0) {"
             #"            OUTCHK(2);"
             #"            outptr[0] = tab[index];"
             #"            outptr[1] = tab[index+1];"
             #"            *outchars = 2;"
             #"        } else {"
             #"            OUTCHK(3);"
             #"            outptr[0] = tab[index];"
             #"            outptr[1] = tab[index+1];"
             #"            outptr[2] = tab[index+2];"
             #"            *outchars = 3;"
             #"        }"
             #"    }"
             #"    return 1;"
             #"}"))

(define (utf8->latN ces)
  (define procname #"utf8_lat~(~ ces'n)")
  ;; grouped :: ((first-octet branch ...) ...)
  ;; branch  :: (second-octet . code) | (second-octet third-octet . code)
  (define grouped
    (sort (map (^g (cons (caar g) (sort (map cdr g) < car)))
               (group-collection
                (map (^p (match-let1 (ucs . code) p
                           (append (ucs4->utf8 ucs) code)))
                     (hash-table->alist (bimap-right (~ ces'bimap))))
                :key car))
          < car))

  ;; If branch has more than 8 entries and all of them are 2-octet sequences,
  ;; we use lookup table.
  (define (gen-lookup pfx branches)
    (receive (lo hi) (find-min&max (map car branches))
      (cgen-decl ""
                 #"/* offset ~|lo|*/"
                 #"static u_char ~|procname|_~|pfx|[] = {")
      (do-ec (: n lo (+ hi 1))
             (:let e (assv n branches))
             (cgen-decl (format "    0x~2,'0x, /* [~a ~2,'0x] */"
                                (if e (cdr e) 0) pfx n)))
      (cgen-decl "};")
      (cgen-body #"        INCHK(2);"
                 #"        u_char u1 = inptr[1];"
                 #"        u_char out = 0;"
                 #"        if (u1 >= ~|lo| && u1 <= ~|hi|) {"
                 #"            out = utf8_lat~(~ ces'n)_~|pfx|[u1 - ~|lo|];"
                 #"        }"
                 #"        if (out == 0) DO_SUBST;"
                 #"        else outptr[0] = out;")))

  ;; Else we do linear search
  (define (gen-linear pfx branches)
    (cgen-body "        u_char u1 = inptr[1];")
    (dolist [br branches]
      (if (not (pair? (cdr br)))
        (cgen-body (format "        if (u1 == 0x~2,'0x) {" (car br))
                   (format "            *outptr = 0x~2,'0x;" (cdr br)))
        (cgen-body (format "        if (u1 == 0x~2,'0x &&" (car br))
                   (format "            (u_char)inptr[2] == 0x~2,'0x) {" (cadr br))
                   (format "            *outptr = 0x~2,'0x;" (cddr br))))
      (cgen-body "        } else"))
    (cgen-body "        { DO_SUBST; }"))

  (cgen-extern #"static ScmConvProc ~|procname|;")
  (cgen-body #"static ScmSize ~|procname|(ScmConvInfo *cinfo,"
             #"    const char *inptr, ScmSize inroom,"
             #"    char *outptr, ScmSize outroom, ScmSize *outchars)"
             #"{"
             #"    u_char u0 = inptr[0];"
             #"    ScmSize nread;"
             #"    if (u0 < 0x80) {"
             #"        outptr[0] = u0;"
             #"        *outchars = 1;"
             #"        return 1;"
             #"    }"
             #"    else if (u0 < 0xc0) { return ILLEGAL_SEQUENCE; }"
             #"    else if (u0 < 0xe0) { nread = 2; }"
             #"    else if (u0 < 0xf0) { nread = 3; }"
             #"    else if (u0 < 0xf8) { nread = 4; }"
             #"    else if (u0 < 0xfc) { nread = 5; }"
             #"    else if (u0 < 0xfe) { nread = 6; }"
             #"    else { return ILLEGAL_SEQUENCE; }"
             #"    INCHK(nread);")

  (cgen-body "    if (u0 == 0xc2 && (u_char)inptr[1] >= 0x80 && (u_char)inptr[1] < 0xa0) {")
  (cgen-body "        outptr[0] = inptr[1];  /* 0x80 - 0x9f */")
  (cgen-body "    } else")
  (dolist [group grouped]
    (define pfx (format "~2,'0x" (car group)))
    (cgen-body #"    if (u0 == 0x~|pfx|) {")
    (if (or (length<? (cdr group) 8)
            (any (^e (pair? (cdr e))) (cdr group)))
      (gen-linear pfx (cdr group))
      (gen-lookup pfx (cdr group)))
    (cgen-body "    } else"))
  (cgen-body "    { DO_SUBST; }")
  (cgen-body "    *outchars = 1;")
  (cgen-body "    return nread;")
  (cgen-body "}"))

;; This is in gauche.unicode, but we can't depend on it yet during
;; building charconv.
;; For Latin-N set, we only need to take care up to 3-octet ranges.
(define (ucs4->utf8 code)
  (cond [(< code #x80)  `(,code)]
        [(< code #x800) `(,(logior (ash code -6) #xc0)
                          ,(logior (logand code #x3f) #x80))]
        [else           `(,(logior (ash code -12) #xe0)
                          ,(logior (logand (ash code -6) #x3f) #x80)
                          ,(logior (logand code #x3f) #x80))]))

(define (main args)
  (match (cdr args)
    [(datadir)
     (parameterize ([cgen-current-unit (make <cgen-unit>
                                         :name "latin_tab"
                                         :preamble "/* Generated by gen-lattab.scm */"
                                         :init-prologue ""
                                         :init-epilogue "")])
       (dolist [n '(2 3 4 5 6 7 8 9 10 11 13 14 15 16)]
         (let1 ces (load-ces-data datadir n)
           (latN->utf8 ces)
           (utf8->latN ces)))
       (cgen-emit-c (cgen-current-unit))
       (cgen-emit-h (cgen-current-unit)))]
    )
  0)
