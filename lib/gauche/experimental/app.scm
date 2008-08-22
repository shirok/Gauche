;; *HIGHLY EXPERIMENTAL*
;; Haskell-ish application.  May change later.
;;
;;  ($ f a b c)         => (f a b c)
;;  ($ f a b c $)       => (lambda args (apply f a b c args))
;;  ($ f $ g a b c)     => (f (g a b c))
;;  ($ f $ g a b c $)   => (lambda args (f (apply g a b c args)))
;;  ($ f $ g $ h a b c) => (f (g (h a b c)))
;;  ($ f a $ g b $ h c) => (f a (g b (h c)))
;;  ($ f a $ g b $ h $) => (lambda args (f a (g b (apply h args))))
;;
;;  ($* f a b c)         => (apply f a b c)
;;  ($* f a $ g b c)     => (apply f a (g b c))
;;  ($* f $* g $ h a b)  => (apply f (apply g (h a b)))
;;  ($* f $* g $* h a b) => (apply f (apply g (apply h a b)))

(define-module gauche.experimental.app
  (export $ $*))
(select-module gauche.experimental.app)

(define-syntax $
  (syntax-rules ()
    [(_ f . rest) (%$-rec () () f . rest)]
    [(_)          (syntax-error "Invalid $-form: ($)")]))

(define-syntax $*
  (syntax-rules ()
    [(_ f . rest) (%$-rec (apply) () f . rest)]
    [(_)          (syntax-error "Invalid $*-form: ($*)")]))

(define-syntax %$-rec
  (syntax-rules ($ $*)
    ;[(_ (app ...) es $)              (app ... pa$ . es)]
    ;[(_ (app ...) es $*)             (app ... pa$ apply . es)]
    [(_ (app ...) (e ...) $ . rest)  (app ... e ... ($ . rest))]
    [(_ (app ...) (e ...) $* . rest) (app ... e ... ($* . rest))]
    [(_ apps      (e ...) x . rest)  (%$-rec apps (e ... x) . rest)]
    [(_ (app ...) es)                (app ... . es)]))

(provide "gauche/experimental/app")
