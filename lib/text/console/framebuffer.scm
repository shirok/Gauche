(define-module text.console.framebuffer
  (use srfi-13)
  (use text.console)
  (use text.console.wide-char-setting)
  (export <framebuffer-console> init-framebuffer draw-framebuffer))
(select-module text.console.framebuffer)

;;; Framebuffer console.
;; This is a virtual console that records all updates in an internal
;; buffer, then updates a real console later with draw-framebuffer
;; call. The last draw is kept, so next time it only updates the
;; changed parts.
;;
;; This is made for text.line-edit and supports just enough operations
;; for it to work. It also assumes that init-framebuffer starts with a
;; cleared console. This is true for line-edit, but not always.
;;
;; It is undefined behavior if something is written outside the
;; console's view, aka (or (> x width) (> y height)).
(define-class <framebuffer-console> ()
  ((width)
   (height)
   (wide-char-setting :init-value #f)
   ;; Two dimensional array of cons whose car is the character and cdr
   ;; is the attribute. If car is #f, the position is part of a wide
   ;; char character. If cdr is #f, the attribute is reset. If a
   ;; vector item is #f instead of a cons, then it's the same as
   ;; (#\space . #f)
   (buffer :init-form (make-vector 0))
   (prev-buffer :init-form (make-vector 0))

   ;; current cursor / attribute
   (x :init-value 0)
   (y :init-value 0)
   (attr :init-value #f)
   (cursor-hidden :init-value #f)
   ))

(define-syntax yx->pos
  (syntax-rules ()
    ((_ con row col)
     (+ col (* row (~ con'width))))))

(define-syntax char-width
  (syntax-rules ()
    ((_ con ch) (if (~ con'wide-char-setting)
                  (get-char-width (~ con'wide-char-setting) ch)
                  1))))

(define (init-framebuffer fb w h wide-char-setting)
  (set! (~ fb'width) w)
  (set! (~ fb'height) h)
  (set! (~ fb'cursor-hidden) #f)
  (set! (~ fb'wide-char-setting) wide-char-setting)
  (let ([size (* w h)])
    (if (and (vector? (~ fb'buffer))
             (>= (vector-length (~ fb'buffer)) size))
      ;; reuse the vector to reduce GC pressure a bit
      (begin
        (vector-fill! (~ fb'buffer) #f)
        (vector-fill! (~ fb'prev-buffer) #f))
      (begin
        (set! (~ fb'buffer) (make-vector size #f))
        (set! (~ fb'prev-buffer) (make-vector size #f))))))

(define (draw-framebuffer con console init-row init-col)
  (let ([curbuf (~ con'buffer)]
        [prevbuf (~ con'prev-buffer)]
        [width (~ con'width)]
        [height (~ con'height)]
        [clear-to-eos? #f]              ; FIXME
        [empty (cons #\space #f)]
        [rattr #f]
        [rx 0]
        [ry 0])

    (hide-cursor console)

    ;; in case we detect clearing the screen or something (e.g. the
    ;; majority of updates is to write whitespaces) then better to
    ;; just clear the whole thing first and skip all whitespace
    ;; updates
    (when clear-to-eos?
      (move-cursor-to console init-row init-col)
      (clear-to-eos console))

    (let loop ((x init-col) (y init-row))
      (let ([prev (or (vector-ref prevbuf (+ x (* y width))) empty)]
            [cur (or (vector-ref curbuf (+ x (* y width))) empty)])
        (unless (or (equal? prev cur)
                    (and (pair? cur) (eq? (car cur) #f))
                    (and clear-to-eos?
                         (eq? (car cur) #\space)))
          (let ([newattr (if (pair? cur) (cdr cur) #f)])
            (unless (equal? rattr newattr)
              (if newattr
                (set-character-attribute console newattr)
                (reset-character-attribute console))
              (set! rattr newattr)))
          (unless (and (= x rx) (= y ry))
            (move-cursor-to console y x))
          (let ([ch (if (pair? cur) (car cur) #\space)])
            (putch console ch)
            (set! rx (+ x (char-width con ch))))
          (set! ry y))
        (let ([next-x (+ x 1)]
              [next-y (+ y 1)])
          (cond
           [(and (eq? next-x width)
                 (eq? next-y height)) #t]
           [(eq? next-x width)
            (loop 0 next-y)]
           [else
            (loop next-x y)]))))

    (move-cursor-to console (~ con'y) (~ con'x))
    (unless (~ con'cursor-hidden)
      (show-cursor console))
    (vector-copy! (~ con'prev-buffer) 0 (~ con'buffer))))

(define-method clear-to-eos ((con <framebuffer-console>))
  (vector-fill! (~ con'buffer)
                '(#\space . #f)
                (+ (~ con'x) (* (~ con'width) (~ con'y)))))

(define-method cursor-down/scroll-up ((con <framebuffer-console>)
                                      :optional (y #f) (height #f)
                                      (full-column-flag #f))
  ;; FIXME: needs more work here, probably
  (if (< (~ con'y) (- (~ con'height) 1))
    (inc! (~ con'y)))
  ;; copied from text.console.generic, maybe there's a better way?
  (if (and y height (>= y (- height 1))) 0 1))

(define-method move-cursor-to ((con <framebuffer-console>) y x)
  (set! (~ con'x) x)
  (set! (~ con'y) y))

(define-method putch ((con <framebuffer-console>) c)
  (let ([pos (yx->pos con  (~ con'y) (~ con'x))])
    (vector-set! (~ con'buffer)
                 pos
                 (cons c (~ con'attr)))
    (let ([w (char-width con c)])
      (when (> w 1)
        (vector-fill! (~ con'buffer)
                      (cons #f #f) ; blank, occupied by the previous character
                      (+ pos 1)
                      (+ pos w)))
      (inc! (~ con'x) w))))

(define-method putstr ((con <framebuffer-console>) s)
  (string-for-each (cut putch con <>) s))

(define-method reset-character-attribute ((con <framebuffer-console>))
  (set! (~ con'attr) #f))

(define-method set-character-attribute ((con <framebuffer-console>) newattr)
  (set! (~ con'attr) newattr))

(define-method hide-cursor ((con <framebuffer-console>))
  (set! (~ con'cursor-hidden) #t))

(define-method show-cursor ((con <framebuffer-console>))
  (set! (~ con'cursor-hidden) #f))
