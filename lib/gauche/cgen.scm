;;;
;;; gauche.cgen - support for C code generation
;;;  
;;;   Copyright (c) 2004 Shiro Kawai, All rights reserved.
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
;;;  $Id: cgen.scm,v 1.1 2004-01-18 12:07:31 shirok Exp $
;;;

;; *EXPERIMENTAL*
;; gauche.cgen.* modules is intended to provide a common framework
;; to generate C code from Scheme.  They will be used for stub
;; generators or any other kind of automatic code generation.

(define-module gauche.cgen
  (use srfi-1)
  (use srfi-2)
  (use srfi-13)
  (use srfi-27)
  (use gauche.parameter)
  (export-all) ;; for now
  )
(select-module gauche.cgen)

;; Required features for C code generator differ greatly among
;; applications, and too much scaffolding could be a constraint
;; for the module users.  So the bottom layer of this module
;; assumes pretty loose model about the generated C source.
;; If you need lots of manual hardwiring, you can work on the bottom
;; layer.  On top of that, the module provides some utilities
;; which support common idioms.
;;
;; [Unit]
;;   A unit (<cgen-unit>) is an object that corresponds to each
;;   C source file.  It can optionally generate a corresponding
;;   header file.  You can set a global "current unit" using
;;   a parameter cgen-current-unit.
;;
;; [Node]
;;   A node (<cgen-node>) is a source of C code piece.  It has
;;   lots of subclasses, each of which handles a specific case.
;;
;; [Building code]
;;   1. Create a unit and make it the current unit.
;;   2. Call node creation functions.  The created nodes are
;;      accumulated within the current unit.
;;   3. Call "emit" method on the unit, which generates a C file
;;      and optionally a header file.
;;
;; [Code arrangement]
;;   Basically, C code appears in the same order that the nodes
;;   are created.   However, C requires declaration and such, so
;;   the generated C is conceptually divided into the following
;;   four parts:
;;
;;   'Extern': This part is put into the header file, if exists.
;;   'Decl':   Placed at the beginning of the C source.
;;   'Body':   Placed in the C source, following the 'decl' part.
;;   'Init':   Placed at the end of C source, following the 'body' part.
;;
;;   The unit's list of nodes are scanned for every part to generate
;;   a C code piece.  Each node "knows" what code it should emit
;;   at each part.
;;
;;   The easiest way to control where the code is put is to use
;;   a node constructors cgen-extern, cgen-decl, cgen-body and cgen-init,
;;   each of which creates a node that emits the given code literally
;;   into the specified part.
;;
;;   More complex structure requires different piece of code to
;;   be generated in different parts.  For example, if you want to
;;   use a literal Scheme symbol within the code, you need a
;;   static string declaration code for the symbol's name, and
;;   the declaration of C variable that holds the symbol reference,
;;   within the 'decl' part.  Furthermore, you need a call to
;;   Scm_Intern within the 'init' part to initialize the symbol
;;   reference variable.  A single node, <cgen-scheme-symbol>,
;;   takes care of these code pieces.
;;
;; [Initialization function]
;;   The name and arguments of the initialization function also depends
;;   on the application.   The unit's init-prologue and init-epilogue
;;   slot will handle the prologue (including function name, arguments,
;;   and the opening curly brace) and the epilogue (including the closing
;;   curly brace).


;;=============================================================
;; Unit
;;

;; A 'cgen-unit' is the unit of C source.  It generates one .c file,
;; and optionally one .h file.

(define-class <cgen-unit> ()
  ((name     :init-keyword :name   :init-value "cgen")
   (c-file   :init-keyword :c-file :init-value #f)
   (h-file   :init-keyword :h-file :init-value #f)
   (preamble :init-keyword :preamble
             :init-value '("/* Generated by gauche.cgen $Revision: 1.1 $ */"))
   (init-prologue :init-keyword :init-prologue :init-value #f)
   (init-epilogue :init-keyword :init-epilogue :init-value #f)
   (toplevels :init-value '()) ;; toplevel nodes to be realized
   ))

(define cgen-current-unit (make-parameter '()))

(define-method cgen-unit-c-file ((unit <cgen-unit>))
  (or (ref unit 'c-file)
      #`",(ref unit 'name).c"))

(define-method cgen-unit-init-name ((unit <cgen-unit>))
  (format "Scm__Init_~a"
          (or (ref unit 'init-name)
              (cgen-safe-name (ref unit 'name)))))

(define-method cgen-unit-h-file ((unit <cgen-unit>))
  (ref unit 'h-file))

(define (cgen-add! node)
  (and-let* ((unit (cgen-current-unit)))
    (slot-push! unit 'toplevels node))
  node)

(define-method cgen-emit ((unit <cgen-unit>) part)
  (let1 context (make-hash-table)
    (define (walker node)
      (unless (hash-table-get context node #f)
        (hash-table-put! context node #t)
        (cgen-emit node part walker)))
    (for-each walker (reverse (ref unit 'toplevels)))))

(define-method cgen-emit-h ((unit <cgen-unit>))
  (and-let* ((h-file (cgen-unit-h-file unit)))
    (cgen-with-output-file h-file
      (lambda ()
        (cond ((ref unit 'preamble) => emit-raw))
        (cgen-emit unit 'extern)))))

(define-method cgen-emit-c ((unit <cgen-unit>))
  (cgen-with-output-file (cgen-unit-c-file unit)
    (lambda ()
      (cond ((ref unit 'preamble) => emit-raw))
      (cgen-emit unit 'decl)
      (cgen-emit unit 'body)
      (cond ((ref unit 'init-prologue) => emit-raw)
            (else
             (print "Scm__Init_"(cgen-safe-name (ref unit 'name))"(void)")
             (print "{")))
      (cgen-emit unit 'init)
      (cond ((ref unit 'init-epilogue) => emit-raw)
            (else (print "}")))
      )))

;;=============================================================
;; Base class
;;
(define-class <cgen-node> ()
  ((extern?  :init-keyword :extern? :init-value #f)))

;; fallback methods
(define-method cgen-decl-common ((node <cgen-node>)) #f)

(define-method cgen-emit ((node <cgen-node>) part walker)
  (case part
    ((extern) (when (ref node 'extern?) (cgen-decl-common node)))
    ((decl)   (unless (ref node 'extern?) (cgen-decl-common node)))
    ((body init) #f)))

;;=============================================================
;; Raw nodes - can be used to insert a raw piece of code
;;

(define-class <cgen-raw-node> (<cgen-node>)
  ((parts :init-keyword :parts :init-value '())
   (code  :init-keyword :code :init-value "")))

(define-method cgen-emit ((node <cgen-raw-node>) part walker)
  (when (memq part (ref node 'parts))
    (emit-raw (ref node 'code))))

(define (cgen-extern . code)
  (cgen-add! (make <cgen-raw-node> :parts '(extern) :code code)))

(define (cgen-decl . code)
  (cgen-add! (make <cgen-raw-node> :parts '(decl) :code code)))

(define (cgen-body . code)
  (cgen-add! (make <cgen-raw-node> :parts '(body) :code code)))

(define (cgen-init . code)
  (cgen-add! (make <cgen-raw-node> :parts '(init) :code code)))

;;=============================================================
;; cpp
;;

;; #include ---------------------------------------------------
(define-class <cgen-include> (<cgen-node>)
  ((path        :init-keyword :path)))

(define-method cgen-decl-common ((node <cgen-include>))
  (print "#include "
         (if (string-prefix? "<" (ref node 'path))
           (ref node 'path)
           #`"\",(ref node 'path)\"")))

(define (cgen-include path)
  (cgen-add! (make <cgen-include> :path path)))

;; #if --------------------------------------------------------
;(define-class <cgen-cpp-if> (<cgen-node>)
;  ((condition :init-keyword :condition :init-value #f)
;   (then      :init-keyword :then :init-value '())
;   (else      :init-keyword :else :init-value '())))

;(define-method cgen-emit ((node <cgen-cpp-if>) part walker)
;  (if (ref node 'condition)
;    (begin
;      (print "#if " (ref node 'condition))
;      (for-each walker (ref node 'then))
;      (if (null? (ref node 'else))
;        (print "#endif /*" (ref node 'condition) "*/")
;        (begin
;          (print "#else  /* !" (ref node 'condition) "*/")
;          (for-each walker (ref node 'else))
;          (print "#endif /* !" (ref node 'condition) "*/"))))
;    (for-each walker (ref node 'then))))

;; #define -----------------------------------------------------
(define-class <cgen-cpp-define> (<cgen-node>)
  ((name   :init-keyword :name)
   (value  :init-keyword :value)
   ))

(define-method cgen-decl-common ((node <cgen-cpp-define>))
  (print "#define " (ref node 'name) " " (ref node 'value)))

(define (cgen-define name . maybe-value)
  (cgen-add!
   (make <cgen-cpp-define> :name name :value (get-optional maybe-value ""))))

;;=============================================================
;; Scheme static values
;;

(define-class <cgen-scheme-value> (<cgen-node>)
  ((scope  :init-keyword :scope  :init-value 'local)
   (c-name :init-keyword :c-name
           :init-form (format "scm__~a" (cgen-unique-name)))
   (value  :init-keyword :value :init-form #f)
   ))

(define-method initialize ((node <cgen-scheme-value>) initargs)
  (next-method)
  (when (ref node 'c-name)
    (and-let* ((unit (cgen-current-unit)))
      (slot-push! unit 'toplevels node))))

(define-method cgen-cexpr ((node <cgen-scheme-value>))
  (ref node 'c-name))

(define-method cgen-emit ((node <cgen-scheme-value>) part walker)
  (case part
    ((extern)
     (when (ref node 'extern?)
       (print "extern ScmObj " (ref node 'c-name) ";")))
    ((decl)
     (print (if (ref node 'extern?) "" "static ")
            "ScmObj " (ref node 'c-name) " = SCM_UNDEFINED;"))
    (else (next-method))))

;; method cgen-literal returns a <cgen-scheme-value> node for the
;; literal value of given Scheme value.  It first scans the current
;; unit's toplevel nodes with the same value, and returns it if found.
;; Otherwise, it creates a new node and register it to the toplevel if
;; necessary.

(define (cgen-literal value)
  (or (and-let* ((unit (cgen-current-unit)))
        (find (lambda (node)
                (and (is-a? node <cgen-scheme-value>)
                     (equal? (ref node 'value) value)))
              (ref unit 'toplevels)))
      (cgen-make-literal value)))

(define-method cgen-make-literal (top)
  (error "you can't create a literal node for:" top))


;; primitive values -------------------------------------------
(define-class <cgen-scheme-boolean> (<cgen-scheme-value>) ())

(define-method cgen-cexpr ((node <cgen-scheme-boolean>))
  (if (ref node 'value) "SCM_TRUE" "SCM_FALSE"))
(define-method cgen-emit ((node <cgen-scheme-boolean>) part walker) #f)

(define *cgen-scheme-true*
  (make <cgen-scheme-boolean> :c-name #f :value #t))
(define *cgen-scheme-false*
  (make <cgen-scheme-boolean> :c-name #f :value #f))

(define-method cgen-make-literal ((value <boolean>))
  (if value *cgen-scheme-true* *cgen-scheme-false*))

(define-class <cgen-scheme-char> (<cgen-scheme-value>) ())

(define-method cgen-cexpr ((node <cgen-scheme-boolean>))
  (format "SCM_MAKE_CHAR(~a)" (char->integer (ref node 'value))))
(define-method cgen-emit ((node <cgen-scheme-boolean>) part walker) #f)

(define-method cgen-make-literal ((value <char>))
  (make <cgen-scheme-char> :c-name #f :value value))

;; string ------------------------------------------------------
;; (for now, we just deal with ASCII string w/o NUL.
(define-class <cgen-scheme-string> (<cgen-scheme-value>) ())

(define-method cgen-cexpr ((node <cgen-scheme-string>))
  #`"SCM_OBJ(&,(ref node 'c-name))")

(define-method cgen-emit ((node <cgen-scheme-string>) part walker)
  (case part
    ((extern)
     (when (ref node 'extern?)
       (print "extern ScmString " (ref node 'c-name) ";")))
    ((decl)
     (let1 s (ref node 'value)
       (print (if (ref node 'extern?) "" "static ")
              (format "SCM_DEFINE_STRING_CONST(~a, ~s, ~a, ~a);"
                      (ref node 'c-name) s
                      (string-size s) (string-length s)))))
    (else (next-method))))

(define-method cgen-make-literal ((value <string>))
  (make <cgen-scheme-string> :value value))


;; symbol ------------------------------------------------------
(define-class <cgen-scheme-symbol> (<cgen-scheme-value>)
  ((symbol-name :init-keyword :symbol-name))) ;; <cgen-scheme-string>

(define-method initialize ((node <cgen-scheme-symbol>) initargs)
  (next-method)
  (unless (slot-bound? node 'symbol-name)
    (set! (ref node 'symbol-name)
          (make <cgen-scheme-string>
            :scope (ref node 'scope)
            :value (symbol->string (ref node 'value))))))

(define-method cgen-emit ((node <cgen-scheme-symbol>) part walker)
  (case part
    ((extern decl)
     (walker (ref node 'symbol-name))
     (next-method))
    ((init)
     (print "  " (ref node 'c-name)
            " = Scm_Intern(SCM_STRING("
            (cgen-cexpr (ref node 'symbol-name))
            "));"))
    (else (next-method))))

(define-method cgen-make-literal ((value <symbol>))
  (make <cgen-scheme-symbol> :value value))

;; keyword ------------------------------------------------------
(define-class <cgen-scheme-keyword> (<cgen-scheme-value>)
  ((keyword-name :init-keyword :keyword-name))) ;; <cgen-scheme-string>

(define-method initialize ((node <cgen-scheme-keyword>) initargs)
  (next-method)
  (unless (slot-bound? node 'keyword-name)
    (set! (ref node 'keyword-name)
          (make <cgen-scheme-string>
            :scope (ref node 'scope)
            :value (keyword->string (ref node 'value))))))

(define-method cgen-emit ((node <cgen-scheme-keyword>) part walker)
  (case part
    ((extern decl)
     (walker (ref node 'keyword-name))
     (next-method))
    ((init)
     (print "  " (ref node 'c-name)
            " = Scm_MakeKeyword(SCM_STRING("
            (cgen-cexpr (ref node 'keyword-name))
            "));"))
    (else (next-method))))

(define-method cgen-make-literal ((value <keyword>))
  (make <cgen-scheme-keyword> :value value))

;;=============================================================
;; Utilities
;;

(define (cgen-with-output-file file thunk)
  (receive (port tmpfile) (sys-mkstemp file)
    (with-error-handler
        (lambda (e) (sys-unlink tmpfile) (raise e))
      (lambda ()
        (with-output-to-port port thunk)
        (sys-rename tmpfile file)))))

(define cgen-unique-name
  (let ((counter 0))
    (lambda ()
      (format "~5,'0d" (inc! counter)))))

;; creates a C-safe name from Scheme string str
(define (cgen-safe-name str)
  (with-string-io str
    (lambda ()
      (let loop ((b (read-byte)))
        (cond ((eof-object? b))
              ((or (<= 48 b 57)
                   (<= 65 b 90)
                   (<= 97 b 122))
               (write-byte b) (loop (read-byte)))
              (else
               (format #t "_~2,'0x" b) (loop (read-byte))))))))

(define (emit-raw code)
  (if (list? code)
    (for-each print code)
    (print code)))

(provide "gauche/cgen")
