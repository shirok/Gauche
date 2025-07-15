;;;
;;; gauche.vm.block - Basic block analysis
;;;
;;;   Copyright (c) 2025  Shiro Kawai  <shiro@acm.org>
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

;; EXPERIMENTAL

;; Unlike gauche.vm.bbb, which replaces compiler's pass5, this module
;; analyzes instruction sequence and split it into basic blocks.

(define-module gauche.vm.block
  (use gauche.vm.code)
  (use gauche.vm.insn)
  (use util.match)
  (use scheme.list)
  (export-all) ; for now
  )
(select-module gauche.vm.block)

(define (vm-code->insns code)
  ;; Returns a list of
  ;;  (addr (insn-name params ...) operands ...)
  ;; and a list of "merge point addresses", where control flow may merge
  ;; from other than the previous insn.
  (let loop ([codelis (vm-code->list code)]
             [addr 0]
             [insns '()]
             [merge-pts '()])
    (match codelis
      [() (values (reverse insns) (delete-duplicates (sort merge-pts)))]
      [((and (opc . params) insn) . rest)
       (case (~ (vm-find-insn-info opc) 'operand-type)
         [(none) (loop rest (+ addr 1) `((,addr ,insn) ,@insns) merge-pts)]
         [(obj code codes)
          (loop (cdr rest) (+ addr 2) `((,addr ,insn ,(car rest)) ,@insns)
                merge-pts)]
         [(label)
          (loop (cdr rest) (+ addr 2) `((,addr ,insn ,(car rest)) ,@insns)
                (cons (car rest) merge-pts))]
         [(obj+label)
          (loop (cddr rest) (+ addr 3)
                `((,addr ,insn ,(car rest) ,(cadr rest)) ,@insns)
                (cons (cadr rest) merge-pts))]
         [(obj+native)
          (loop (cddr rest) (+ addr 3)

                `((,addr ,insn ,(car rest) ,(cadr rest)) ,@insns)
                merge-pts)])])))

(define (insns->blocks insns merge-points)
  (let loop ([insns insns]
             [merge-points merge-points])
    (if (null? merge-points)
      (list insns)
      (receive (block rest)
          (span (^[entry] (< (car entry) (car merge-points))) insns)
        (cons block (loop rest (cdr merge-points)))))))

(define (remove-dead-code block)
  (match block
    [() '()]
    [((and (addr (op . params) . operands) insn) . rest)
     (if (~ (vm-find-insn-info op)'terminal)
       (list insn)
       (cons insn (remove-dead-code rest)))]))
