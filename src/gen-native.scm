;;;
;;; Generate native code vector for FFI
;;;

;; Currently, the generated code should be manually copy&pasted into
;; libnative.scm.  Eventually we'll streamline the build process to automate it.

(use scheme.list)
(use util.match)
(use gauche.uvector)
(use file.util)
(use lang.asm.x86_64)

;; The code tail jumps to the target, so the target's return directly
;; returns to the caller of call-amd64.  Thus we can reuse the same
;; codepad area without worrying the recursive calls overwrite active
;; code.

;; For SYSV AMD64 calling convention: Section 3.2 of 
;; http://refspecs.linux-foundation.org/elf/x86_64-abi-0.95.pdf

(define (gen-stub-amd64 port)
  ;; When all args can be on registers
  (define-values (reg-code reg-labels)
    (asm '(func: (.dataq 0)
           arg0: (.dataq 0)
           arg1: (.dataq 0)
           arg2: (.dataq 0)
           arg3: (.dataq 0)
           arg4: (.dataq 0)
           arg5: (.dataq 0)
           farg0: (.dataq 0)
           farg1: (.dataq 0)
           farg2: (.dataq 0)
           farg3: (.dataq 0)
           farg4: (.dataq 0)
           farg5: (.dataq 0)
           farg6: (.dataq 0)
           farg7: (.dataq 0)
           entry6f7: (movsd (farg7:) %xmm7)
           entry6f6: (movsd (farg6:) %xmm6)
           entry6f5: (movsd (farg5:) %xmm5)
           entry6f4: (movsd (farg4:) %xmm4)
           entry6f3: (movsd (farg3:) %xmm3)
           entry6f2: (movsd (farg2:) %xmm2)
           entry6f1: (movsd (farg1:) %xmm1)
           entry6f0: (movsd (farg0:) %xmm0)
           entry6:   (movq (arg5:) %r9)
           entry5:   (movq (arg4:) %r8)
           entry4:   (movq (arg3:) %rcx)
           entry3:   (movq (arg2:) %rdx)
           entry2:   (movq (arg1:) %rsi)
           entry1:   (movq (arg0:) %rdi)
           entry0:   (jmp (func:))
           end:)))
  ;; Spill case.  We fill these after 
  (define-values (spill-code spill-labels)
    (asm '(func: (.dataq 0)
           arg0: (.dataq 0)
           arg1: (.dataq 0)
           arg2: (.dataq 0)
           arg3: (.dataq 0)
           arg4: (.dataq 0)
           arg5: (.dataq 0)
           farg0: (.dataq 0)
           farg1: (.dataq 0)
           farg2: (.dataq 0)
           farg3: (.dataq 0)
           farg4: (.dataq 0)
           farg5: (.dataq 0)
           farg6: (.dataq 0)
           farg7: (.dataq 0)
           entry6f7: (movsd (farg7:) %xmm7)
           entry6f6: (movsd (farg6:) %xmm6)
           entry6f5: (movsd (farg5:) %xmm5)
           entry6f4: (movsd (farg4:) %xmm4)
           entry6f3: (movsd (farg3:) %xmm3)
           entry6f2: (movsd (farg2:) %xmm2)
           entry6f1: (movsd (farg1:) %xmm1)
           entry6f0: (movsd (farg0:) %xmm0)
           entry6:   (movq (arg5:) %r9)
           entry5:   (movq (arg4:) %r8)
           entry4:   (movq (arg3:) %rcx)
           entry3:   (movq (arg2:) %rdx)
                     (movq (spill-size:) %rax)
                     (leaq (spill: %rip) %rsi)
           loop:     (movq (%rsi) %rdi)
                     (push %rdi)
                     (addq 8 %rsi)
                     (subq 8 %rax)
                     (jnz loop:)
           entry2:   (movq (arg1:) %rsi)
           entry1:   (movq (arg0:) %rdi)
           entry0:   (call (func:))
                     (addq (spill-size:) %rsp)
                     (ret)
                     (.align 8)
           spill-size: (.dataq 0)
           spill:)))
           
  (define (entry-offsets labels)  ;; numargs -> code vector offset
    (map (cut assq-ref labels <>) 
         '(entry0: entry1: entry2: entry3: entry4: entry5: entry6:
                   entry6f0: entry6f1: entry6f2: entry6f3:
                   entry6f4: entry6f5: entry6f6: entry6f7:)))
  (define (arg-offsets labels)    ;; arg# -> offset
    (map (cut assq-ref labels <>)
         '(arg0: arg1: arg2: arg3: arg4: arg5:)))
  (define (farg-offsets labels)    ;; farg# -> offset
    (map (cut assq-ref labels <>)
         '(farg0: farg1: farg2: farg3: farg4: farg5: farg6: farg7:)))
  (define (end-addr labels) (assq-ref labels 'end:))
  (define (size-offset labels)
    (assq-ref labels 'spill-size:))
  (display ";; libnative.scm supplemental code.\n" port)
  (display ";; Generated automatically by gen-native.scm.  DO NOT EDIT.\n" port)
  (display "\n" port)
  (display ";; Register-only calling" port)
  (display ";; label    offset\n" port)
  (dolist [p reg-labels]
    (format port ";; ~10a  ~3d\n" (car p) (cdr p)))
  (pprint `(define *amd64-call-reg-code*
             ',(list->u8vector reg-code))
          :port port
          :controls (make-write-controls :pretty #t :width 75 
                                         :base 16 :radix #t))

  (display ";; Spill-to-stack case" port)
  (display ";; label    offset\n" port)
  (dolist [p spill-labels]
    (format port ";; ~10a  ~3d\n" (car p) (cdr p)))
  (pprint `(define *amd64-call-spill-code*
             ',(list->u8vector spill-code))
          :port port
          :controls (make-write-controls :pretty #t :width 75 
                                         :base 16 :radix #t))
  
  ;; (call-amd64 <dlptr> args rettype)
  ;;  args : ((type value) ...)
  ;; NB: In the final form, we won't expose this function to the user; it's
  ;; too error-prone.  You can wreck havoc just by passing a wrong type.
  ;; Instead, we'll require the user to parse the C function declaration
  ;; and we automatically extract the type info.
  (pprint
   `(define call-amd64
      (^[ptr args rettype]
        (let* ([num-iargs (count (^p (memq (car p) '(o p i s))) args)]
               [num-fargs (count (^p (memq (car p) '(d))) args)]
               [num-spills (+ (max 0 (- num-iargs 6))
                              (max 0 (- num-fargs 8)))])
          (if (zero? num-spills)
            (call-amd64-regs  ptr args num-iargs num-fargs rettype)
            (call-amd64-spill ptr args
                              (min num-iargs 6)
                              (min num-fargs 8)
                              num-spills rettype)))))
   :port port)
  (pprint 
   `(define call-amd64-regs
      (let ((%%call-native (global-variable-ref (find-module 'gauche.bootstrap)
                                                '%%call-native))
            (entry-offsets ',(entry-offsets reg-labels))
            (arg-offsets ',(arg-offsets reg-labels))
            (farg-offsets ',(farg-offsets reg-labels)))
        (^[ptr args num-iargs num-fargs rettype]
          (let* ([effective-nargs (if (zero? num-fargs)
                                    num-iargs
                                    (+ 6 num-fargs))]
                 [entry (~ entry-offsets effective-nargs)]
                 [patcher
                  (let loop ([args args] [icount 0] [fcount 0] [r '()])
                    (cond [(null? args) r]
                          [(memq (caar args) '(o p i s))
                           (loop (cdr args) (+ icount 1) fcount
                                 (cons `(,(~ arg-offsets icount) ,@(car args))
                                       r))]
                          [(memq (caar args) '(d))
                           (loop (cdr args) icount (+ fcount 1)
                                 (cons `(,(~ farg-offsets fcount) ,@(car args))
                                       r))]
                          [else (error "bad arg entry:" (car args))]))])
            (%%call-native entry 0
                           *amd64-call-reg-code*
                           entry
                           ,(end-addr reg-labels)
                           entry
                           (cons `(0 p ,ptr) patcher)
                           rettype)))))
   :port port)
  (pprint
   `(define call-amd64-spill
      (let ((%%call-native (global-variable-ref (find-module 'gauche.bootstrap)
                                                '%%call-native))
            (entry-offsets ',(entry-offsets spill-labels))
            (arg-offsets ',(arg-offsets spill-labels))
            (farg-offsets ',(farg-offsets spill-labels))
            (size-offset ',(size-offset spill-labels))
            (spill-offset (^n (+ ,(assq-ref spill-labels 'spill:)
                                 (* n 8)))))
        (^[ptr args num-iargs num-fargs num-spills rettype]
          (let* ([effective-nargs (if (zero? num-fargs)
                                    num-iargs
                                    (+ 6 num-fargs))]
                 [entry (~ entry-offsets effective-nargs)]
                 [patcher
                  (let loop ([args args] [icount 0] [fcount 0] [scount 0]
                             [r '()])
                    (cond [(null? args) r]
                          [(memq (caar args) '(o p i s))
                           (if (< icount 6)
                             (loop (cdr args) (+ icount 1) fcount scount
                                   (cons `(,(~ arg-offsets icount) ,@(car args))
                                         r))
                             (loop (cdr args) (+ icount 1) fcount
                                   (+ scount 1)
                                   (cons `(,(spill-offset (- num-spills scount 1)) ,@(car args))
                                         r)))]
                          [(memq (caar args) '(d))
                           (if (< fcount 8)
                             (loop (cdr args) icount (+ fcount 1) scount
                                   (cons `(,(~ farg-offsets fcount) ,@(car args))
                                         r))
                             (loop (cdr args) icount (+ fcount 1)
                                   (+ scount 1)
                                   (cons `(,(spill-offset (- num-spills scount 1)) ,@(car args))
                                         r)))]
                          [else (error "bad arg entry:" (car args))]))])
            (%%call-native entry 
                           (+ ,(assq-ref spill-labels 'spill:)
                              (* num-spills 8))
                           *amd64-call-spill-code*
                           entry
                           ,(size-offset spill-labels)
                           entry
                           (list* `(0 p ,ptr)
                                  `(,',(size-offset spill-labels) i ,(* 8 num-spills))
                                  patcher)
                           rettype)))))
   :port port)
  )

;; gosh ./gen-native.scm <dir>
(define (main args)
  (match (cdr args)
    [(dir) (call-with-temporary-file 
            (^[port tmpname]
              (gen-stub-amd64 port)
              (sys-rename tmpname #"~|dir|/native-supp.scm"))
            :directory dir :prefix "native-supp.scm")]
    [else  (exit 1 "Usage: gosh ./gen-native.scm <directory>")])
  0)

