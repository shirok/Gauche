;;;
;;; gauche.cgen - support for C code generation
;;;
;;;   Copyright (c) 2004-2019  Shiro Kawai  <shiro@acm.org>
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

;; *EXPERIMENTAL*
;; gauche.cgen.* modules are intended to provide a common framework
;; to generate C code from Scheme.  They will be used for stub
;; generators or any other kind of automatic code generation.

;; NB: gauche.cgen is used by genstub, which is used to build extension
;; modules bundled in Gauche.  That means gauche.cgen cannot rely on
;; any extension modules.  Keep this in mind if you make modifications here.

(define-module gauche.cgen
  (extend gauche.cgen.unit gauche.cgen.literal gauche.cgen.type
          gauche.cgen.cise)
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
;;   <cgen-unit> keeps some more values that are put into C files.
;;   In a short summary, the C files are constructed like this:
;;
;;   header file:
;;     - (ref unit 'preamble)     ;; for "generated by" message etc.
;;     - <extern part>
;;
;;   c file:
;;     - (ref unit 'preamble)     ;; for "generated by" message etc.
;;     - (ref unit 'pre-decl)     ;; #includes or #defines that are
;;                                ;;   needed before #include <gauche.h>
;;     - <decl part>
;;     - definitions of static array
;;     - <body part>
;;     - (ref unit 'init-prologue) ;; void Scm__InitFoobar etc.
;;     - <init part>
;;     - (ref unit 'init-epilogue) ;; closing brace etc.
;;
;; [Initialization function]
;;   The name and arguments of the initialization function also depends
;;   on the application.   The unit's init-prologue and init-epilogue
;;   slot will handle the prologue (including function name, arguments,
;;   and the opening curly brace) and the epilogue (including the closing
;;   curly brace).

