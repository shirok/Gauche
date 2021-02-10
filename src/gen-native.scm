;;;
;;; Generate native code vector for FFI
;;;

;; Currently, the generated code should be manually copy&pasted into
;; libnative.scm.  Eventually we'll streamline the build process to automate it.

(use scheme.list)
(use util.match)
(use gauche.uvector)
(use lang.asm.x86_64)

;; The code tail jumps to the target, so the target's return directly
;; returns to the caller of call-amd64.  Thus we can reuse the same
;; codepad area without worrying the recursive calls overwrite active
;; code.

;; For SYSV AMD64 calling convention: Section 3.2 of 
;; http://refspecs.linux-foundation.org/elf/x86_64-abi-0.95.pdf

(define (gen-stub-amd64 port)
  ;; Support up to 6 args, integer register passing only for now.
  (receive (code labels)
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
             end:))
    (define entry-offsets   ;; numargs -> code vector offset
      (map (cut assq-ref labels <>) 
           '(entry0: entry1: entry2: entry3: entry4: entry5: entry6:
             entry6f0: entry6f1: entry6f2: entry6f3:
             entry6f4: entry6f5: entry6f6: entry6f7:)))
    (define arg-offsets     ;; arg# -> offset
      (map (cut assq-ref labels <>)
           '(arg0: arg1: arg2: arg3: arg4: arg5:)))
    (define farg-offsets     ;; farg# -> offset
      (map (cut assq-ref labels <>)
           '(farg0: farg1: farg2: farg3: farg4: farg5: farg6: farg7:)))
    (define end-addr (assq-ref labels 'end:))

    (display ";; libnative.scm supplemental code.\n" port)
    (display ";; Generated automatically by gen-native.scm.  DO NOT EDIT.\n" port)
    (display "\n" port)
    (display ";; label    offset\n" port)
    (dolist [p labels]
      (format port ";; ~10a  ~3d\n" (car p) (cdr p)))
    (pprint `(define *amd64-call-code*
               ',(list->u8vector code))
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
        (let ((%%call-native (global-variable-ref (find-module 'gauche.bootstrap)
                                                  '%%call-native))
              (entry-offsets ',entry-offsets)
              (arg-offsets ',arg-offsets)
              (farg-offsets ',farg-offsets))
          (^[ptr args rettype]
            (let* ([num-iargs (count (^p (memq (car p) '(o p i s))) args)]
                   [num-fargs (count (^p (memq (car p) '(d))) args)]
                   [effective-nargs (if (zero? num-fargs)
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
              (%%call-native entry
                             *amd64-call-code*
                             entry
                             ,end-addr
                             entry
                             (cons `(0 p ,ptr) patcher)
                             rettype)))))
     :port port)
    ))

;; gosh ./gen-native.scm <dir>
(define (main args)
  (match (cdr args)
    [(dir) (call-with-output-file #"~|dir|/native-supp.scm"
             (cut gen-stub-amd64 <>))]
    [else  (exit 1 "Usage: gosh ./gen-native.scm <directory>")])
  0)

