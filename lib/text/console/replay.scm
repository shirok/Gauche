(define-module text.console.replay
  (use text.console)
  (export <replay-console>
          start-recording finish-recording play-recording))
(select-module text.console.replay)

(define-class <replay-console> ()
  ((commands :init-value '())
   ))

(define (start-recording con)
  (set! (~ con'commands) '()))

(define (finish-recording con)
  (set! (~ con'commands) (reverse (~ con'commands))))

(define (play-recording con real-con)
  (let loop ((cmds (~ con'commands)))
    (unless (null? cmds)
      (let ([cmd #?=(car cmds)]
            [rest (cdr cmds)])
        ((cdr cmd) real-con)
        (loop rest)))))

(define (save-cmd con name proc)
  (set! (~ con'commands) (cons (cons name proc) (~ con'commands))))

(define-method clear-to-eos ((con <replay-console>))
  (save-cmd con 'clear-to-eos
            (cut clear-to-eos <>)))

(define-method cursor-down/scroll-up ((con <replay-console>) y height full-column-flag)
  (save-cmd con 'cursor-down/scroll-up
            (cut cursor-down/scroll-up <> y height full-column-flag))
  ;; copied from text.console.generic, maybe there's a better way?
  (if (and y height (>= y (- height 1))) 0 1))

(define-method move-cursor-to ((con <replay-console>) y x)
  (save-cmd con 'move-cursor-to
            (cut move-cursor-to <> y x)))

(define-method putch ((con <replay-console>) c)
  (save-cmd con 'putch
            (cut putch <> c)))

(define-method putstr ((con <replay-console>) s)
  (save-cmd con 'putstr
            (cut putstr <> #?=s)))

(define-method reset-character-attribute ((con <replay-console>))
  (save-cmd con 'reset-character-attribute
            (cut reset-character-attribute <>)))

(define-method set-character-attribute ((con <replay-console>) newattr)
  (save-cmd con 'set-character-attribute
            (cut set-character-attribute <> newattr)))
