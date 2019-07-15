;;;
;;; gauche.cgen.optimize - Extra optimization
;;;
;;;   Copyright (c) 2013-2019  Shiro Kawai  <shiro@acm.org>
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

;; This is a part of precompiler (gauche.cgen.precomp) and to be autoloaded
;; when necessary.  This module provides some experimental routines that
;; does optimizations which is expensive in the on-the-fly compiler.

;; NOTE: This module heavily depends on vm instructions and vm compiledcode
;; format.

(define-module gauche.cgen.optimizer
  (use util.match)
  (use gauche.vm.insn)
  (use gauche.sequence)
  (import gauche.vm.code)
  (export optimize-compiled-code))
(select-module gauche.cgen.optimizer)

;; API
(define (optimize-compiled-code compiled-code)
  (eliminate-dead-code compiled-code))

;; Dead code elimination.  The core compile tends to leave a few percents
;; of dead code from the result of final pass optimization; it won't bother
;; to remove them in favor of compilation speed.  In AOT compilation, we
;; have luxury to remove them...
(define (eliminate-dead-code orig-code)
  ;; convert code list into #(#(code visited? label) ...)
  (let ([cvec (map-to <vector> (^c `#(,c #f #f)) (vm-code->list orig-code))]
        [cc   (make-compiled-code-builder (~ orig-code'required-args)
                                          (~ orig-code'optional-args)
                                          (~ orig-code'name)
                                          (~ orig-code'parent)
                                          (~ orig-code'intermediate-form))])
    (define jumps
      '(JUMP LOCAL-ENV-JUMP))
    (define returns
      '(RET VALUES-RET LREF-RET CONST-RET CONSTF-RET CONSTU-RET
        TAIL-CALL LOCAL-ENV-TAIL-CALL GREF-TAIL-CALL PUSH-GREF-TAIL-CALL
        LREF0-PUSH-GREF-TAIL-CALL VALUES-APPLY TAIL-APPLY))
    (define branches
      '(PUSH-PRE-CALL PRE-CALL RECEIVE RECEIVE-ALL BF BT BNEQ BNEQV BNNULL
        BNUMNE BNLT BNLE BNGT BNGE
        LREF-VAL0-BNUMNE LREF-VAL0-BNLT LREF-VAL0-BNLE
        LREF-VAL0-BNGT LREF-VAL0-BNGE
        BNUMNEI BNEQC BNEQVC))

    ;; returns addr value
    (define (get-addr opcode i)
      (let1 insn-info (vm-find-insn-info opcode)
        (case (~ insn-info'operand-type)
          [(addr) (~ cvec (+ i 1) 0)]
          [(obj+addr) (~ cvec (+ i 2) 0)]
          [else (errorf "Opcode ~a doesn't take addr operand" opcode)])))
    ;; a kind of priority queue; we pop the least number first.
    (define (make-q) (make-tree-map = <))
    (define (empty? q) (tree-map-empty? q))
    (define (enq! q x) (tree-map-put! q x #t))
    (define (deq! q)   (car (tree-map-pop-min! q)))
    (define (mark-destination q addr)
      (unless (~ cvec addr 2)
        (set! (~ cvec addr 2) (compiled-code-new-label cc)))
      (enq! q addr))
    (define (label-at i) (~ cvec i 2))
    (define visited?
      (getter-with-setter (^i (~ cvec i 1)) (^[i v] (set! (~ cvec i 1) v))))
    ;; mark phase
    (define (mark i q)
      (if (or (>= i (vector-length cvec)) (visited? i))
        (unless (empty? q) (mark (deq! q) q))
        (let1 opcode (car (~ cvec i 0))
          (set! (visited? i) #t)
          (cond [(memq opcode jumps)
                 (mark-destination q (get-addr opcode i))
                 (mark (deq! q) q)]
                [(memq opcode returns)
                 (unless (empty? q) (mark (deq! q) q))]
                [(memq opcode branches)
                 (mark-destination q (get-addr opcode i))
                 (mark (+ i (vm-insn-size opcode)) q)]
                [else (mark (+ i (vm-insn-size opcode)) q)]))))
    ;; sweep phase
    (define (sweep)
      (let1 cc (make-compiled-code-builder (~ orig-code'required-args)
                                           (~ orig-code'optional-args)
                                           (~ orig-code'name)
                                           (~ orig-code'parent)
                                           (~ orig-code'intermediate-form))
        (do ([i 0 i])
            [(= i (vector-length cvec))
             (compiled-code-finish-builder cc (~ orig-code'max-stack))
             cc]
          (let* ([insn (~ cvec i 0)]
                 [opcode (car insn)])
            (unless (not (visited? i))
              (and-let* ([lab (label-at i)])
                (compiled-code-set-label! cc lab))
              (let1 info (vm-find-insn-info opcode)
                (emit! cc i insn (~ info'code) (~ info'operand-type))))
            (inc! i (vm-insn-size opcode))))))
    (define (emit! cc i insn code type)
      (match insn
        [(_)
         (emit-1! i type (cut compiled-code-emit0! cc code)
                  (cut compiled-code-emit0o! cc code <>))]
        [(_ arg0)
         (emit-1! i type (cut compiled-code-emit1! cc code arg0)
                  (cut compiled-code-emit1o! cc code arg0 <>))]
        [(_ arg0 arg1)
         (emit-1! i type (cut compiled-code-emit2! cc code arg0 arg1)
                  (cut compiled-code-emit2o! cc code arg0 arg1 <>))]))
    (define (emit-1! i type emit-simple emit/operand)
      (ecase type
        [(none)  (emit-simple)]
        [(obj)   (emit/operand (~ cvec (+ i 1) 0))]
        [(code)  (emit/operand (eliminate-dead-code (~ cvec (+ i 1) 0)))]
        [(codes) (emit/operand (map (^c (if (is-a? c <compiled-code>)
                                          (eliminate-dead-code c)
                                          c))
                                    (~ cvec (+ i 1) 0)))]
        [(addr)  (emit/operand (label-at (~ cvec (+ i 1) 0)))]
        [(obj+addr) (emit/operand (list (~ cvec (+ i 1) 0)
                                        (label-at (~ cvec (+ i 2) 0))))]))
    (mark 0 (make-q))
    (compiled-code-copy! orig-code (sweep))
    orig-code))




