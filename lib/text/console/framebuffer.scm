(define-module text.console.framebuffer
  (use srfi-13)
  (use text.console)
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
(define-class <framebuffer-console> ()
  ((width)
   (height)
   ;; Two dimensional array of cons whose car is the character and cdr
   ;; is the attribute.
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

(define (init-framebuffer fb w h)
  (set! (~ fb'width) w)
  (set! (~ fb'height) h)
  (set! (~ fb'cursor-hidden) #f)
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
        [empty (cons #\space #f)])

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
                    (and clear-to-eos?
                         (eq? (car cur) #\space)))
          (move-cursor-to console y x)
          (putch console (if (pair? cur) (car cur) #\space)))
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
  ;; fixme: wide char support
  (vector-set! (~ con'buffer)
               (yx->pos con  (~ con'y) (~ con'x))
               (cons c (~ con'attr)))
  (inc! (~ con'x)))

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
