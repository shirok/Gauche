;;
;; Low-level unit tests for the native FFI subsystem internals.
;;
;; This file exercises the bootstrap primitives directly (codepad
;; install/destroy, callback assembly fragments, etc.), bypassing
;; with-ffi.

(use gauche.test)
(use gauche.uvector)
(use gauche.native-type)
(use gauche.ffi)
;; call-sysvx64 lazily resolves bindings in lang.asm.linker via
;; module-binding-ref; pull it in so the lookup succeeds.
(use lang.asm.x86_64)

(test-start "FFI native internals")

(if (not (ffi-subsystem-available? :native))
  (format #t "skipping: native FFI subsystem not available on this platform~%")
  (begin

;;----------------------------------------------------------
(test-section "Single-callback install")

;; Bootstrap-installed bindings (captured in gauche.internal during init).
(define %install-one
  (with-module gauche.internal %%install-ffi-callback-one))
(define %pad-entry
  (with-module gauche.internal %%ffi-callback-pad-entry))
(define %destroy-one!
  (with-module gauche.internal %%destroy-ffi-callback-pad!))
;; call-sysvx64 expects to run inside (with-module gauche.typeutil
;; native-ptr-fill-enabled?) → #t, the same way make-native-ffi-proc
;; wraps its callsites.  On Windows x64 the runtime uses the Win64 ABI,
;; so dispatch to call-winx64 there.
(define call-sysvx64
  (let ([raw (cond-expand
              [gauche.os.windows (with-module gauche.internal call-winx64)]
              [else              (with-module gauche.internal call-sysvx64)])]
        [param (with-module gauche.typeutil native-ptr-fill-enabled?)])
    (^[ptr args rettype]
      (parameterize ([param #t])
        (raw ptr args rettype)))))

;; mov $42, %eax ; ret  (no prologue/epilogue, no PDATA needed)
(define stub-return-42
  (u8vector #xb8 #x2a #x00 #x00 #x00     ; mov $42, %eax
            #xc3))                       ; ret

;; Type tag for a C function returning intptr_t with no args; used to
;; wrap the codepad entry as a <native-handle>.
(define fn-type-int-of-void
  (make-c-function-type <intptr_t> '()))

(test* "install + invoke + destroy: literal stub returns 42"
       42
       (let* ([pad (%install-one stub-return-42 0 0 0)]
              [hdl (%pad-entry pad fn-type-int-of-void)]
              [val (call-sysvx64 hdl '() <intptr_t>)])
         (%destroy-one! pad)
         val))

(test* "double-destroy is a no-op"
       'ok
       (let1 pad (%install-one stub-return-42 0 0 0)
         (%destroy-one! pad)
         (%destroy-one! pad)
         'ok))

(test* "entry of destroyed pad raises"
       (test-error)
       (let1 pad (%install-one stub-return-42 0 0 0)
         (%destroy-one! pad)
         (%pad-entry pad fn-type-int-of-void)))

;;----------------------------------------------------------
(test-section "Batched install of multiple callbacks")

(define %install-context
  (with-module gauche.internal %%install-ffi-callback-context))
(define %context-entry
  (with-module gauche.internal %%ffi-callback-context-entry))
(define %destroy-context!
  (with-module gauche.internal %%destroy-ffi-callback-context!))

(define (stub-return-imm32 n)
  ;; mov $n, %eax ; ret  (n in [0, 2^31))
  (u8vector #xb8
            (logand n          #xff)
            (logand (ash n -8)  #xff)
            (logand (ash n -16) #xff)
            (logand (ash n -24) #xff)
            #xc3))

(test* "context with 3 stubs: each returns its own constant"
       '(1 2 3)
       (let* ([specs (list (list (stub-return-imm32 1) 0 0 0)
                           (list (stub-return-imm32 2) 0 0 0)
                           (list (stub-return-imm32 3) 0 0 0))]
              [ctx (%install-context specs)]
              [results (map (^i (call-sysvx64 (%context-entry ctx i
                                                            fn-type-int-of-void)
                                            '() <intptr_t>))
                            '(0 1 2))])
         (%destroy-context! ctx)
         results))

(test* "destroyed context: entry call raises"
       (test-error)
       (let* ([specs (list (list (stub-return-imm32 7) 0 0 0))]
              [ctx (%install-context specs)])
         (%destroy-context! ctx)
         (%context-entry ctx 0 fn-type-int-of-void)))

;;----------------------------------------------------------
(test-section "Callback trampoline: zero-arg invocation and return")

;; The trampoline assembler must follow the host's C calling convention,
;; because the trampoline calls back into the Gauche C runtime
;; (Scm_Cons, Scm_MakeFlonum, Scm__FFINativeCallCallback, …).  On Windows
;; those helpers use the Win64 ABI; using the SysV trampoline there
;; passes proc/args in the wrong registers and crashes immediately.
(define assemble-callback-sysvx64
  (cond-expand
   [gauche.os.windows
    (with-module gauche.internal assemble-callback-winx64)]
   [else
    (with-module gauche.internal assemble-callback-sysvx64)]))

;; The handle's <c-function> tag is consulted by call-sysvx64 only to
;; satisfy the :func patch's pointer-type check; the explicit rettype
;; argument is what drives return-value handling.  We therefore reuse
;; fn-type-int-of-void — what matters is just that the trampoline's
;; address is wrapped in *some* <c-function>-tagged native handle.

;; Build, install, call, destroy.  Returns the value call-sysvx64 saw.
(define (run-callback body arg-canons args rettype)
  (receive (code entry win-pe win-fs)
      (assemble-callback-sysvx64 body arg-canons rettype)
    (let* ([pad (%install-one code entry win-pe win-fs)]
           [hdl (%pad-entry pad fn-type-int-of-void)])
      (begin0 (call-sysvx64 hdl args rettype)
        (%destroy-one! pad)))))

(test* "trampoline invokes its body procedure (no args)"
       'invoked
       (let1 sentinel #f
         (run-callback (^[] (set! sentinel 'invoked)) '() '() <top>)
         sentinel))

(test* "trampoline returns its body's value (no args)"
       'returned
       (run-callback (^[] 'returned) '() '() <top>))

;;----------------------------------------------------------
(test-section "Callback trampoline: per-kind argument boxing")

;; For each scalar kind, build an identity callback whose Scheme body
;; just returns its single argument unchanged.  Calling the callback
;; from the C-style site with rettype=<top> hands us back the boxed
;; ScmObj; the test verifies the round trip.

(test* "intptr arg round-trips" 42
       (run-callback (^[x] x)
                     (list <intptr_t>)
                     `((,<intptr_t> 42))
                     <top>))

(test* "fixnum arg round-trips (cheap shl/incq encoding)" 99
       (run-callback (^[x] x)
                     (list <fixnum>)
                     `((,<intptr_t> 99))     ; passed as raw intptr at C level
                     <top>))

(test* "uintptr arg round-trips" #x80000001
       (run-callback (^[x] x)
                     (list <uintptr_t>)
                     `((,<uintptr_t> ,#x80000001))
                     <top>))

(test* "double arg round-trips" 3.14
       (run-callback (^[x] x)
                     (list <double>)
                     `((,<double> 3.14))
                     <top>))

(test* "float arg round-trips (within float precision)" #t
       (let1 result (run-callback (^[x] x)
                                  (list <float>)
                                  `((,<float> 1.5))
                                  <top>)
         (< (abs (- result 1.5)) 1e-6)))

(test* "c-string arg round-trips" "hello"
       (run-callback (^[s] s)
                     (list <c-string>)
                     `((,<c-string> "hello"))
                     <top>))

(test* "pointer arg round-trips (same address survives boxing)"
       #t
       (let* ([ptype  (make-c-pointer-type <int>)]
              [u      (u32vector 7 8 9)]
              [handle (uvector->native-handle u ptype)]
              [result (run-callback (^[h] h)
                                    (list ptype)
                                    `((,ptype ,handle))
                                    <top>)])
         (and (c-pointer-handle? result)
              (c-pointer=? result handle))))

(test* "<top> arg passes through raw"
       'a-symbol
       (run-callback (^[x] x)
                     (list <top>)
                     `((,<top> a-symbol))
                     <top>))

;;----------------------------------------------------------
(test-section "Callback trampoline: multi-arg mix")

(test* "(int, double, c-string) all visible"
       '(7 2.5 "world")
       (run-callback (^[i d s] (list i d s))
                     (list <intptr_t> <double> <c-string>)
                     `((,<intptr_t> 7) (,<double> 2.5) (,<c-string> "world"))
                     <top>))

(test* "(double, int, double) — interleave int/fp slots"
       '(1.0 5 2.0)
       (run-callback (^[a b c] (list a b c))
                     (list <double> <intptr_t> <double>)
                     `((,<double> 1.0) (,<intptr_t> 5) (,<double> 2.0))
                     <top>))

;; SysV gives 6 int arg registers; Win64 only 4, and the winx64
;; trampoline doesn't yet support spilled args.
(cond-expand
 [gauche.os.windows]
 [else
  (test* "six int args (full int reg file)"
         '(1 2 3 4 5 6)
         (run-callback (^[a b c d e f] (list a b c d e f))
                       (list <intptr_t> <intptr_t> <intptr_t>
                             <intptr_t> <intptr_t> <intptr_t>)
                       `((,<intptr_t> 1) (,<intptr_t> 2) (,<intptr_t> 3)
                         (,<intptr_t> 4) (,<intptr_t> 5) (,<intptr_t> 6))
                       <top>))])

;;----------------------------------------------------------
(test-section "Callback trampoline: per-kind return unboxing")

;; For each retkind, build a callback whose body produces a Scheme value
;; of the matching type; declare that retkind on the trampoline; let the
;; C-side caller (call-sysvx64) re-box the C value back to Scheme using
;; the same rettype.  Round-trip == correct unbox.

(test* "<top> return passes ScmObj through unchanged"
       'sym
       (run-callback (^[] 'sym) '() '() <top>))

(test* "<fixnum> return uses sar-2 unbox (positive)"
       42
       (run-callback (^[] 42) '() '() <fixnum>))

(test* "<fixnum> return uses sar-2 unbox (negative, signed >>)"
       -7
       (run-callback (^[] -7) '() '() <fixnum>))

(test* "<intptr_t> return goes through Scm_IntegerToIntptr"
       #x100000000
       (run-callback (^[] #x100000000) '() '() <intptr_t>))

(test* "<uintptr_t> return goes through Scm_IntegerToUintptr"
       #x80000001
       (run-callback (^[] #x80000001) '() '() <uintptr_t>))

(test* "<double> return goes through Scm_GetDouble"
       3.14
       (run-callback (^[] 3.14) '() '() <double>))

(test* "<float> return: Scm_GetDouble + cvtsd2ss, within float precision"
       #t
       (let1 r (run-callback (^[] 1.5) '() '() <float>)
         (< (abs (- r 1.5)) 1e-6)))

(test* "<void> return: result discarded; trampoline still terminates"
       'ok
       (begin (run-callback (^[] 'whatever) '() '() <void>) 'ok))

(test* "<c-string> return: Scm_GetStringConst unbox round-trips"
       "hello"
       (run-callback (^[] "hello") '() '() <c-string>))

(test* "pointer return: Scm_NativeHandlePtr unbox preserves address"
       #t
       (let* ([ptype  (make-c-pointer-type <int>)]
              [u      (u32vector 11 22 33)]
              [handle (uvector->native-handle u ptype)]
              [result (run-callback (^[] handle) '() '() ptype)])
         (and (c-pointer-handle? result)
              (c-pointer=? result handle))))

;;----------------------------------------------------------
(test-section "Callback trampoline: exception propagation")

(test* "error raised in body propagates through C frames"
       'caught
       (guard (e [else 'caught])
         (run-callback (^[x] (error "boom" x))
                       (list <intptr_t>)
                       `((,<intptr_t> 7))
                       <top>)))

(test* "exception condition carries the body's message"
       "boom"
       (guard (e [(<error> e) (~ e'message)])
         (run-callback (^[] (error "boom"))
                       '() '() <top>)))

)) ;; if (ffi-subsystem-available? :native)

(test-end)
