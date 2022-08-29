;;;
;;; gauche.cgen.cbe - C back-end
;;;
;;;   Copyright (c) 2022  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.cgen.cbe
  (use gauche.cgen)
  (use gauche.cgen.bbb)
  (use gauche.vm.insn)
  (use gauche.sequence)
  (use gauche.package.compile)
  (use scheme.list)
  (use scheme.set)
  (use srfi-13)
  (use srfi-27)
  (use srfi-42)
  (use util.match)
  (use file.util)
  (use data.ulid)
  (use text.tr)
  (export compile->c compile-link-toplevel))
(select-module gauche.cgen.cbe)

(define name-gen
  (let1 rs (make-random-source)
    (random-source-randomize! rs)
    (make-ulid-generator rs)))

;; API is experimental
(define (compile->c source.scm)
  (parameterize ([cgen-current-unit (make <cgen-unit>
                                      :name (path-sans-extension source.scm))])
    (cgen-decl "#include <gauche.h>"
               "#include <gauche/precomp.h>"
               "")
    (with-input-from-file source.scm
      (cut generator-for-each compile-toplevel read))
    (cgen-emit-c (cgen-current-unit))))

;; For easier experiment.
(define (compile-link-toplevel form)
  (let1 name #"cgen~(ulid->string (name-gen))"
    (parameterize ([cgen-current-unit (make <cgen-unit> :name name)])
      (cgen-decl "#include <gauche.h>"
                 "#include <gauche/precomp.h>"
                 "")
      (compile-toplevel form)
      (cgen-emit-c (cgen-current-unit)))
    (print #"Code generated in ~|name|.c")
    (let1 cppflags (cond-expand
                    [gauche.in-place
                     (string-append "-I"
                                    (sys-dirname ((with-module gauche.internal
                                                    %gauche-libgauche-path))))]
                    [else #f])
      (gauche-package-compile-and-link name `(,#"~|name|.c")
                                       :verbose #t
                                       :cppflags cppflags
                                       :cflags "-O2"))
    (dynamic-load #"./~|name|" :init-function #"Scm__Init_~|name|")))

(define (compile-toplevel form)
  (let1 toplevel-cfn (benv->c (compile-b form))
    (cgen-init #"  ~|toplevel-cfn|(NULL, 0, NULL);")))

(define (benv->c benv)                  ;returns benv's entry cfn name
  (for-each benv->c (~ benv'children))
  (for-each cluster->c (~ benv'clusters))
  (gen-entry benv))

;; Each benv has one C function as subr.
(define (benv-cfn-name benv)
  (format "~a_ENTRY" (cgen-safe-name (x->string (~ benv'name)))))

(define (cluster->c cluster)
  (define cfn-name (cluster-cfn-name cluster))
  (cgen-decl #"static ScmObj ~|cfn-name|(ScmObj, void**);")
  (cgen-body ""
             #"static ScmObj ~|cfn-name|(ScmObj VAL0, void **DATA)"
             #"{")
  (cluster-prologue cluster)
  (for-each block->c (reverse (~ cluster'blocks)))
  (cgen-body #"}"))

(define (block->c block)
  (cgen-body #" ~(block-label block):")
  (for-each (cute insn->c (~ block'cluster) <>)
            (reverse (~ block'insns))))

(define (block-label block)
  (cgen-safe-name-friendly (bb-name block)))

(define (insn->c c insn)
  (match insn
    [('MOV rd rs) (cgen-body #"  ~(R rd) = ~(R rs);")]
    [('LD r id) (let1 c-id (cgen-literal id)
                  (cgen-body
                   #"  {"
                   #"    /* ~(cgen-safe-comment (~ id'name)) */"
                   #"    static ScmGloc *g = NULL;"
                   #"    if (!g) {"
                   #"      ~(R r) = Scm_IdentifierGlobalRef(SCM_IDENTIFIER(~(cgen-cexpr c-id)), &g);"
                   #"    } else {"
                   #"      ~(R r) = Scm_GlocGetValue(g);"
                   #"    }"
                   #"  }"))]
    [('ST r id)   (let1 c-id (cgen-literal id)
                    (cgen-body
                     #"  /* ~(cgen-safe-comment (~ id'name)) */"
                     #"  Scm_GlobalVariableSet(~(cgen-cexpr c-id),"
                     #"                        ~(R r),"
                     #"                        NULL);"))]
    [('CLOSE r b) (cgen-body #"  ~(R r) ="
                             #"    Scm_MakeSubr(~(benv-cfn-name b),"
                             #"                 NULL,"
                             #"                 ~(~ b'input-reqargs),"
                             #"                 ~(~ b'input-optargs),"
                             #"                 SCM_FALSE);")]
    [('BR r b1 b2)(cgen-body #"  if (SCM_FALSEP(~(R r)))")
                  (gen-jump-cstmt c b2)
                  (cgen-body #"  else")
                  (gen-jump-cstmt c b1)]
    [('JP b)      (gen-jump-cstmt c b)]
    [('CONT b)    (gen-cont-cstmt c b)]
    [('CALL bb proc r ...) (gen-vmcall c proc r)]
    [('RET r . rs)(cgen-body #"  return ~(R r);")]
    [('DEF id flags r)
     (let ([c-mod (cgen-literal (~ id'module))]
           [c-name (cgen-literal (identifier->symbol id))])
       (cgen-body #"  /* ~(cgen-safe-comment (~ c-name'value)) */"
                  #"  Scm_Define(SCM_MODULE(~(cgen-cexpr c-mod)),"
                  #"             SCM_SYMBOL(~(cgen-cexpr c-name)),"
                  #"             ~(R r));"))]
    ;; Builtin operations
    [('CONS r x y) (builtin-2arg c "Scm_Cons" r x y)]
    [('CAR r x) (builtin-1arg c "Scm_Car" r x)]
    [('CDR r x) (builtin-1arg c "Scm_Cdr" r x)]
    [('CAAR r x) (builtin-1arg c "Scm_Caar" r x)]
    [('CADR r x) (builtin-1arg c "Scm_Cadr" r x)]
    [('CDAR r x) (builtin-1arg c "Scm_Cdar" r x)]
    [('CDDR r x) (builtin-1arg c "Scm_Cddr" r x)]
    [('LIST r . xs) (cgen-body #"  ~(R r) = ~(gen-list c xs);")]
    [('LIST* r . xs) (cgen-body #"  ~(R r) = ~(gen-list* c xs);")]
    [('LENGTH r x) (builtin-1arg c "Scm_Length" r x)]
    [('MEMQ r x y) (builtin-2arg c "Scm_Memq" r x y)]
    [('MEMV r x y) (builtin-2arg c "Scm_Memv" r x y)]
    [('ASSQ r x y) (builtin-2arg c "Scm_Assq" r x y)]
    [('ASSV r x y) (builtin-2arg c "Scm_Assv" r x y)]
    [('EQ r x y) (builtin-2arg/bool c "SCM_EQ" r x y)]
    [('EQV r x y) (builtin-2arg/bool c "Scm_EqvP" r x y)]
    [('APPEND r . xs) (cgen-body #"  /* WRITEME: APPEND */")]
    [('NOT r x) (cgen-body #"  ~(R r) = SCM_MAKE_BOOL(SCM_FALSEP(~(R x)));")]
    [('REVERSE r x) (builtin-1arg c "Scm_Reverse" r x)]
    [('APPLY r . xs) (cgen-body #"  /* WRITEME: APPLY */")]
    [('TAIL-APPLY r . xs) (cgen-body #"  /* WRITEME: TAIL-APPLY */")]
    [('IS-A r x y) (builtin-2arg/bool c "SCM_ISA" r x y)]
    [('NULLP r x) (builtin-1arg/bool c "SCM_NULLP" r x)]
    [('PAIRP r x) (builtin-1arg/bool c "SCM_PAIRP" r x)]
    [('CHARP r x) (builtin-1arg/bool c "SCM_CHARP" r x)]
    [('EOFP r x) (builtin-1arg/bool c "SCM_EOFP" r x)]
    [('STRINGP r x) (builtin-1arg/bool c "SCM_STRINGP" r x)]
    [('SYMBOLP r x) (builtin-1arg/bool c "SCM_SYMBOLP" r x)]
    [('VECTORP r x) (builtin-1arg/bool c "SCM_VECTORP" r x)]
    [('NUMBERP r x) (builtin-1arg/bool c "SCM_NUMBERP" r x)]
    [('REALP r x) (builtin-1arg/bool c "SCM_REALP" r x)]
    [('IDENTIFIERP r x) (builtin-1arg/bool c "SCM_IDENTIFIERP" r x)]
    [('SETTER r x) (builtin-1arg c "Scm_Setter" r x)]
    [('VEC r . xs) (cgen-body #"  /* WRITEME: VEC */")]
    [('LIST->VEC r x) (cgen-body #"  /* WRITEME: LIST->VEC */")]
    [('APP-VEC r . xs) (cgen-body #"  /* WRITEME: APP-VEC */")]
    [('VEC-LEN r x) (builtin-1arg c "SCM_VECTOR_SIZE" r x)]
    [('VEC-REF r x y)
     (let ([n (gensym 'n)]
           [v (gensym 'v)])
       (cgen-body #"  ScmSmallInt ~n = SCM_PC_GET_INDEX(~(R y));"
                  #"  ScmVector *~v = SCM_PC_ENSURE_VEC(~(R x)));"
                  #"  SCM_PC_BOUND_CHECK(SCM_VECTOR_SIZE(~v), ~n);"
                  #"  ~(R r) = SCM_VECTOR_ELEMENT(~v, ~n);"))]
    [('VEC-SET r x y z)
     (let ([n (gensym 'n)]
           [v (gensym 'v)])
       (cgen-body #"  ScmSmallInt ~n = SCM_PC_GET_INDEX(~(R y));"
                  #"  ScmVector *~v = SCM_PC_ENSURE_VEC(~(R x)));"
                  #"  SCM_PC_BOUND_CHECK(SCM_VECTOR_SIZE(~v), ~n);"
                  #"  SCM_VECTOR_ELEMENT(~v, ~n) = ~(R z);"
                  #"  ~(R r) = SCM_UNDEFINED;"))]
    [('UVEC-REF r x y z) (cgen-body #"  /* WRITEME: UVEC-REF */")]
    [('NUMEQ2 r x y) (builtin-2arg c "SCM_PC_NUMEQ2" r x y)]
    [('NUMLT2 r x y) (builtin-2arg c "SCM_PC_NUMLT2" r x y)]
    [('NUMLE2 r x y) (builtin-2arg c "SCM_PC_NUMLE2" r x y)]
    [('NUMGT2 r x y) (builtin-2arg c "SCM_PC_NUMGT2" r x y)]
    [('NUMGE2 r x y) (builtin-2arg c "SCM_PC_NUMGE2" r x y)]
    [('NUMADD2 r x y) (builtin-2arg c "Scm_Add" r x y)]
    [('NUMSUB2 r x y) (builtin-2arg c "Scm_Sub" r x y)]
    [('NUMMUL2 r x y) (builtin-2arg c "Scm_Mul" r x y)]
    [('NUMDIV2 r x y) (builtin-2arg c "Scm_Div" r x y)]
    [('NUMMOD2 r x y) (builtin-2arg c "SCM_MOD2" r x y)]
    [('NUMREM2 r x y) (builtin-2arg c "SCM_REM2" r x y)]
    [('NEGATE r x) (builtin-1arg c "Scm_Negate" r x)]
    [('ASH r x y) (builtin-2arg c "Scm_Ash" r x y)]
    [('LOGAND r x y) (builtin-2arg c "Scm_LogAnd" r x y)]
    [('LOGIOR r x y) (builtin-2arg c "Scm_LogIor" r x y)]
    [('LOGXOR r x y) (builtin-2arg c "Scm_LogXor" r x y)]
    [('CURIN r) (builtin-0arg c "SCM_CURIN" r)]
    [('CUROUT r) (builtin-0arg c "SCM_CUROUT" r)]
    [('CURERR r) (builtin-0arg c "SCM_CURERR" r)]
    [('UNBOX r x) (builtin-1arg c "Scm_Unbox" r x)]
    ))

(define (builtin-0arg c v r)
  (cgen-body #"  ~(R r) = SCM_OBJ(~|v|);"))

(define (builtin-1arg c fn r x)
  (cgen-body #"  ~(R r) = ~|fn|(~(R x));"))

(define (builtin-1arg/bool c fn r x)
  (cgen-body #"  ~(R r) = SCM_MAKE_BOOL(~|fn|(~(R x)));"))

(define (builtin-2arg c fn r x y)
  (cgen-body #"  ~(R r) = ~|fn|(~(R x), ~(R y));"))

(define (builtin-2arg/bool c fn r x y)
  (cgen-body #"  ~(R r) = SCM_MAKE_BOOL(~|fn|(~(R x), ~(R y)));"))

(define (cluster-cfn-name c)
  (cgen-safe-name (x->string (~ c'id))))

(define (cluster-env-type-name c)
  #"struct ~(cluster-cfn-name c)_ENV")

(define (cluster-prologue c)
  ;; Set up registers
  (when (cluster-has-env? c)
    (let1 off (if (cluster-needs-dispatch? c) 1 0)
      (for-each-with-index
       (^[i r] (cgen-body #"  ScmObj ~(R r) = SCM_OBJ(DATA[~(+ i off)]);"))
       (cluster-env c))))
  (set-for-each
   (^r (cgen-body #"  ScmObj ~(R r);"))
   (~ c'lregs))
  ;; Jump table
  (when (cluster-needs-dispatch? c)
    (cgen-body #"  switch ((intptr_t)DATA[0]) {")
    (for-each-with-index
     (^[i bb] (cgen-body #"    case ~|i|: goto ~(block-label bb);"))
     (~ c'entry-blocks))
    (cgen-body #"  }"))
  )

;; Returns bb's entry index.
(define (entry-block-index bb)
  (assume (find-index (cut eq? bb <>) (~ bb'cluster'entry-blocks))
          "entry-block-index fails to find index of:" bb))

(define (gen-entry benv)                ;returns cfn name
  (and-let* ([entry-cluster (find (^c (memq (~ benv'entry) (~ c'blocks)))
                                  (~ benv'clusters))]
             [entry-cfn (cluster-cfn-name entry-cluster)])
    (cgen-body #"static ScmObj ~(benv-cfn-name benv)("
               #"                  ScmObj *SCM_FP,"
               #"                  int SCM_ARGCNT SCM_UNUSED,"
               #"                  void *data_ SCM_UNUSED)"
               #"{")
    (if (cluster-has-env? entry-cluster)
      ;; set up env
      (let* ([off (if (cluster-needs-dispatch? entry-cluster) 1 0)]
             [env-size (+ (cluster-env-size entry-cluster) off)])
        (cgen-body #"  ScmWord data[~(+ env-size 1)];")
        (when (cluster-needs-dispatch? entry-cluster)
          (let1 i (entry-block-index (last (~ entry-cluster'blocks)))
            (cgen-body #"  data[0] = SCM_WORD(~i);")))
        (do-ec [: ireg (index i) (~ benv'input-regs)]
               (let1 pos
                   (find-index (cut eq? ireg <>) (cluster-env entry-cluster))
                 (assume pos)
                 (cgen-body #"  data[~(+ pos off)] = SCM_WORD(SCM_FP[~i]);")))
        (cgen-body #"  return ~|entry-cfn|(SCM_FALSE, (void**)data);"))
      ;; no need to set up env
      (if (cluster-needs-dispatch? entry-cluster)
        (cgen-body #"  ScmWord data[1];"
                   #"  data[0] = SCM_WORD(0);"
                   #"  return ~|entry-cfn|(SCM_FALSE, (void**)data);")
        (cgen-body #"  return ~|entry-cfn|(SCM_FALSE, NULL);")))
    (cgen-body "}" "")
    (benv-cfn-name benv)))

(define (gen-jump-cstmt c dest-bb)
  (if (eq? c (~ dest-bb'cluster))
    (cgen-body #"    goto ~(block-label dest-bb);")
    (let* ([dest-c (~ dest-bb 'cluster)]
           [index (entry-block-index dest-bb)]
           [cfn (cluster-cfn-name dest-c)]
           [off (if (cluster-needs-dispatch? dest-c) 1 0)]
           [env-size (+ (cluster-env-size dest-c) off)])
      (cgen-body #"  {"
                 #"    ScmWord data[~|env-size|];")
      (when (= off 1)
        (cgen-body #"    data[0] = SCM_WORD(~index);"))
      (prepare-env c (~ dest-bb'cluster) off)
      (cgen-body #"    return ~|cfn|(SCM_FALSE, (void**)data);"
                 #"  }"))))

(define (gen-cont-cstmt c dest-bb)
  (let* ([dest-c (~ dest-bb'cluster)]
         [index (entry-block-index dest-bb)]
         [cfn (cluster-cfn-name dest-c)]
         [off (if (cluster-needs-dispatch? dest-c) 1 0)]
         [env-size (+ (cluster-env-size dest-c) off)])
    (cgen-body #"  {"
               #"    ScmVM *vm = Scm_VM();"
               #"    ScmWord *data = (ScmWord*)(Scm_vPushCC(vm, ~|cfn|, ~|env-size|));")
    (when (= off 1)
      (cgen-body #"    data[0] = SCM_OBJ(~index);"))
    (prepare-env c dest-c off)
    (cgen-body #"  }")))

(define (gen-vmcall c proc regs)
  (case (length regs)
    [(0)
     (cgen-body #"  return Scm_VMApply0(~(R proc));")]
    [(1)
     (cgen-body #"  return Scm_VMApply1(~(R proc), ~(R (car regs)));")]
    [(2)
     (cgen-body #"  return Scm_VMApply2(~(R proc), ~(R (car regs)), ~(R (cadr regs)));")]
    [(3)
     (cgen-body #"  return Scm_VMApply3(~(R proc), ~(R (car regs)), ~(R (cadr regs)), ~(R (caddr regs)));")]
    [(4)
     (cgen-body #"  return Scm_VMApply4(~(R proc), ~(R (car regs)), ~(R (cadr regs)), ~(R (caddr regs)), ~(R (cadddr regs)));")]
    [else
     (cgen-body #"  return Scm_VMApply(~(R proc), ~(gen-list c regs));")]))

;; Generate code that construct env struct for destination cluster (dest-c)
;; from the env of current cluster (c).  The C variable name for the new
;; env is ENV2.
(define (prepare-env c dest-c offset)
  (define env-type-name (cluster-env-type-name dest-c))
  (for-each-with-index
   (^[i r] (cgen-body #"    data[~(+ i offset)] = SCM_WORD(~(R r));"))
   (cluster-env dest-c)))

(define (gen-list c regs)
  (define (rec regs)
    (match regs
      [() '("SCM_NIL")]
      [(reg . regs) `("Scm_Cons(" ,(R reg) ", " ,@(rec regs) ")")]))
  (string-concatenate (rec regs)))

(define (gen-list* c regs)
  (define (rec regs)
    (match regs
      [(reg) (R reg)]
      [(reg . regs) `("Scm_Cons(" ,(R reg) ", " ,@(rec regs) ")")]))
  (string-concatenate (rec regs)))

(define *constant-literals* (make-hash-table 'eq?)) ;; const -> <literal>

;; Register name
(define (R r)
  (cond
   [(is-a? r <reg>)
    (let1 m (#/^%(\d+)\.(\d+)(?:\.(.*))?/ (x->string (~ r'name)))
      (format "R~d_~d~a" (m 1) (m 2)
              (if-let1 name (m 3)
                #"_~(cgen-safe-name-friendly name)"
                "")))]
   [(is-a? r <const>)
    (let1 lit (or (hash-table-get *constant-literals* r #f)
                  (rlet1 lit (cgen-literal (const-value r))
                    (hash-table-put! *constant-literals* r lit)))
      (cgen-cexpr lit))]
   [(eq? r '%VAL0) "VAL0"]
   [else (error "Invalid register: " r)]))
